/**
 * Generate `TypeInfo` objects, which are needed for run-time introspection of types.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/typeinf.d, _typeinf.d)
 * Documentation:  https://dlang.org/phobos/dmd_typinf.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/typinf.d
 */

module dmd.typinf;

import dmd.astenums;
import dmd.declaration;
import dmd.dmodule;
import dmd.dscope;
import dmd.dclass;
import dmd.dstruct;
import dmd.errors;
import dmd.expression;
import dmd.globals;
import dmd.gluelayer;
import dmd.location;
import dmd.mtype;
import dmd.visitor;
import core.stdc.stdio;

/****************************************************
 * Generates the `TypeInfo` object associated with `torig` if it
 * hasn't already been generated
 * Params:
 *      e     = if not null, then expression for pretty-printing errors
 *      loc   = the location for reporting line numbers in errors
 *      torig = the type to generate the `TypeInfo` object for
 *      sc    = the scope
 *      genObjCode = if true, object code will be generated for the obtained TypeInfo
 */
extern (C++) void genTypeInfo(Expression e, const ref Loc loc, Type torig, Scope* sc, bool genObjCode = true)
{
    // printf("genTypeInfo() %s\n", torig.toChars());

    // Even when compiling without `useTypeInfo` (e.g. -betterC) we should
    // still be able to evaluate `TypeInfo` at compile-time, just not at runtime.
    // https://issues.dlang.org/show_bug.cgi?id=18472
    if (!sc || !(sc.flags & SCOPE.ctfe))
    {
        if (!global.params.useTypeInfo)
        {
            if (e)
                .error(loc, "expression `%s` uses the GC and cannot be used with switch `-betterC`", e.toChars());
            else
                .error(loc, "`TypeInfo` cannot be used with -betterC");
            fatal();
        }
    }

    if (!Type.dtypeinfo)
    {
        .error(loc, "`object.TypeInfo` could not be found, but is implicitly used");
        fatal();
    }

    Type t = torig.merge2(); // do this since not all Type's are merge'd
    if (!t.vtinfo)
    {
        if (t.isShared()) // does both 'shared' and 'shared const'
            t.vtinfo = TypeInfoSharedDeclaration.create(t);
        else if (t.isConst())
            t.vtinfo = TypeInfoConstDeclaration.create(t);
        else if (t.isImmutable())
            t.vtinfo = TypeInfoInvariantDeclaration.create(t);
        else if (t.isWild())
            t.vtinfo = TypeInfoWildDeclaration.create(t);
        else
            t.vtinfo = getTypeInfoDeclaration(t);
        assert(t.vtinfo);

        // ClassInfos are generated as part of ClassDeclaration codegen
        const isUnqualifiedClassInfo = (t.ty == Tclass && !t.mod);

        // generate a COMDAT for other TypeInfos not available as builtins in
        // druntime
        if (!isUnqualifiedClassInfo && !builtinTypeInfo(t) && genObjCode)
        {
            if (sc) // if in semantic() pass
            {
                // Find module that will go all the way to an object file
                Module m = sc._module.importedFrom;
                m.members.push(t.vtinfo);
            }
            else // if in obj generation pass
            {
                toObjFile(t.vtinfo, global.params.multiobj);
            }
        }
    }
    if (!torig.vtinfo)
        torig.vtinfo = t.vtinfo; // Types aren't merged, but we can share the vtinfo's
    assert(torig.vtinfo);
}

/****************************************************
 * Gets the type of the `TypeInfo` object associated with `t`
 * Params:
 *      loc = the location for reporting line nunbers in errors
 *      t   = the type to get the type of the `TypeInfo` object for
 *      sc  = the scope
 *      genObjCode = if true, object code will be generated for the obtained TypeInfo
 * Returns:
 *      The type of the `TypeInfo` object associated with `t`
 */
extern (C++) Type getTypeInfoType(const ref Loc loc, Type t, Scope* sc, bool genObjCode = true);

private TypeInfoDeclaration getTypeInfoDeclaration(Type t)
{
    //printf("Type::getTypeInfoDeclaration() %s\n", t.toChars());
    switch (t.ty)
    {
    case Tpointer:
        return TypeInfoPointerDeclaration.create(t);
    case Tarray:
        return TypeInfoArrayDeclaration.create(t);
    case Tsarray:
        return TypeInfoStaticArrayDeclaration.create(t);
    case Taarray:
        return TypeInfoAssociativeArrayDeclaration.create(t);
    case Tstruct:
        return TypeInfoStructDeclaration.create(t);
    case Tvector:
        return TypeInfoVectorDeclaration.create(t);
    case Tenum:
        return TypeInfoEnumDeclaration.create(t);
    case Tfunction:
        return TypeInfoFunctionDeclaration.create(t);
    case Tdelegate:
        return TypeInfoDelegateDeclaration.create(t);
    case Ttuple:
        return TypeInfoTupleDeclaration.create(t);
    case Tclass:
        if ((cast(TypeClass)t).sym.isInterfaceDeclaration())
            return TypeInfoInterfaceDeclaration.create(t);
        else
            return TypeInfoClassDeclaration.create(t);

    default:
        return TypeInfoDeclaration.create(t);
    }
}

/**************************************************
 * Returns:
 *      true if any part of type t is speculative.
 *      if t is null, returns false.
 */
bool isSpeculativeType(Type t)
{
    static bool visitVector(TypeVector t)
    {
        return isSpeculativeType(t.basetype);
    }

    static bool visitAArray(TypeAArray t)
    {
        return isSpeculativeType(t.index) ||
               isSpeculativeType(t.next);
    }

    static bool visitStruct(TypeStruct t)
    {
        StructDeclaration sd = t.sym;
        if (auto ti = sd.isInstantiated())
        {
            if (!ti.needsCodegen())
            {
                if (ti.minst || sd.requestTypeInfo)
                    return false;

                /* https://issues.dlang.org/show_bug.cgi?id=14425
                 * TypeInfo_Struct would refer the members of
                 * struct (e.g. opEquals via xopEquals field), so if it's instantiated
                 * in speculative context, TypeInfo creation should also be
                 * stopped to avoid 'unresolved symbol' linker errors.
                 */
                /* When -debug/-unittest is specified, all of non-root instances are
                 * automatically changed to speculative, and here is always reached
                 * from those instantiated non-root structs.
                 * Therefore, if the TypeInfo is not auctually requested,
                 * we have to elide its codegen.
                 */
                return true;
            }
        }
        else
        {
            //assert(!sd.inNonRoot() || sd.requestTypeInfo);    // valid?
        }
        return false;
    }

    static bool visitClass(TypeClass t)
    {
        ClassDeclaration sd = t.sym;
        if (auto ti = sd.isInstantiated())
        {
            if (!ti.needsCodegen() && !ti.minst)
            {
                return true;
            }
        }
        return false;
    }


    static bool visitTuple(TypeTuple t)
    {
        if (t.arguments)
        {
            foreach (arg; *t.arguments)
            {
                if (isSpeculativeType(arg.type))
                    return true;
            }
        }
        return false;
    }

    if (!t)
        return false;
    Type tb = t.toBasetype();
    switch (tb.ty)
    {
        case Tvector:   return visitVector(tb.isTypeVector());
        case Taarray:   return visitAArray(tb.isTypeAArray());
        case Tstruct:   return visitStruct(tb.isTypeStruct());
        case Tclass:    return visitClass(tb.isTypeClass());
        case Ttuple:    return visitTuple(tb.isTypeTuple());
        case Tenum:     return false;
        default:
        return isSpeculativeType(tb.nextOf());

        /* For TypeFunction, TypeInfo_Function doesn't store parameter types,
         * so only the .next (the return type) is checked here.
         */
    }
}

/* ========================================================================= */

/* Indicates whether druntime already contains an appropriate TypeInfo instance
 * for the specified type (in module rt.util.typeinfo).
 */
extern (C++) bool builtinTypeInfo(Type t)
{
    if (!t.mod) // unqualified types only
    {
        // unqualified basic types + typeof(null)
        if (t.isTypeBasic() || t.ty == Tnull)
            return true;
        // some unqualified arrays
        if (t.ty == Tarray)
        {
            Type next = t.nextOf();
            return (next.isTypeBasic() && !next.mod)                     // of unqualified basic types
                || (next.ty == Tchar && next.mod == MODFlags.immutable_) // string
                || (next.ty == Tchar && next.mod == MODFlags.const_);    // const(char)[]
        }
    }
    return false;
}
