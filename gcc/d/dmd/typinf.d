/**
 * Generate `TypeInfo` objects, which are needed for run-time introspection of types.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/typinf.d, _typinf.d)
 * Documentation:  https://dlang.org/phobos/dmd_typinf.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/typinf.d
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
import dmd.location;
import dmd.mtype;
import dmd.typesem;
import core.stdc.stdio;

/****************************************************
 * Generates the `TypeInfo` object associated with `torig` if it
 * hasn't already been generated
 * Params:
 *      e     = if not null, then expression for pretty-printing errors
 *      loc   = the location for reporting line numbers in errors
 *      torig = the type to generate the `TypeInfo` object for
 *      sc    = the scope
 * Returns:
 *      true if `TypeInfo` was generated and needs compiling to object file
 */
bool genTypeInfo(Expression e, Loc loc, Type torig, Scope* sc)
{
    // printf("genTypeInfo() %s\n", torig.toChars());

    // Even when compiling without `useTypeInfo` (e.g. -betterC) we should
    // still be able to evaluate `TypeInfo` at compile-time, just not at runtime.
    // https://issues.dlang.org/show_bug.cgi?id=18472
    if (!sc || !sc.ctfe)
    {
        if (!global.params.useTypeInfo)
        {
            global.gag = 0;
            if (e)
                .error(loc, "expression `%s` uses the GC and cannot be used with switch `-betterC`", e.toChars());
            else
                .error(loc, "`TypeInfo` cannot be used with -betterC");

            if (sc && sc.tinst)
                sc.tinst.printInstantiationTrace(Classification.error, uint.max);

            fatal();
        }
    }

    if (!Type.dtypeinfo)
    {
        .error(loc, "`object.TypeInfo` could not be found, but is implicitly used");
        fatal();
    }

    import dmd.typesem : merge2;
    Type t = torig.merge2(); // do this since not all Type's are merge'd
    if (t.ty == Taarray)
        t = makeNakedAssociativeArray(cast(TypeAArray)t);

    bool needsCodegen = false;
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
            t.vtinfo = getTypeInfoDeclaration(t, sc);
        assert(t.vtinfo);

        // ClassInfos are generated as part of ClassDeclaration codegen
        const isUnqualifiedClassInfo = (t.ty == Tclass && !t.mod);

        if (!isUnqualifiedClassInfo && !builtinTypeInfo(t))
            needsCodegen = true;
    }
    if (!torig.vtinfo)
        torig.vtinfo = t.vtinfo; // Types aren't merged, but we can share the vtinfo's
    assert(torig.vtinfo);
    return needsCodegen;
}

/****************************************************
 * Gets the type of the `TypeInfo` object associated with `t`
 * Params:
 *      loc = the location for reporting line nunbers in errors
 *      t   = the type to get the type of the `TypeInfo` object for
 *      sc  = the scope
 * Returns:
 *      The type of the `TypeInfo` object associated with `t`
 */
extern (C++) Type getTypeInfoType(Loc loc, Type t, Scope* sc);

private TypeInfoDeclaration getTypeInfoDeclaration(Type t, Scope* sc)
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
        return getTypeInfoAssocArrayDeclaration(cast(TypeAArray)t, sc);
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

/******************************************
 * Instantiate TypeInfoAssociativeArrayDeclaration and fill
 * the entry with TypeInfo_AssociativeArray.Entry!(t.index, t.next)
 *
 * Params:
 *      t = TypeAArray to generate TypeInfo_AssociativeArray for
 *      sc = context
 * Returns:
 *      a TypeInfoAssociativeArrayDeclaration with field entry initialized
 */
TypeInfoDeclaration getTypeInfoAssocArrayDeclaration(TypeAArray t, Scope* sc)
{
    import dmd.arraytypes;
    import dmd.expressionsem;
    import dmd.id;

    assert(sc); // must not be called in the code generation phase

    auto ti = TypeInfoAssociativeArrayDeclaration.create(t);
    t.vtinfo = ti; // assign it early to avoid recursion in expressionSemantic
    Loc loc = t.loc;
    auto tiargs = new Objects();
    tiargs.push(t.index); // always called with naked types
    tiargs.push(t.next);

    Expression id = new IdentifierExp(loc, Id.empty);
    id = new DotIdExp(loc, id, Id.object);
    id = new DotIdExp(loc, id, Id.TypeInfo_AssociativeArray);
    auto tempinst = new DotTemplateInstanceExp(loc, id, Id.Entry, tiargs);
    auto e = expressionSemantic(tempinst, sc);
    assert(e.type);
    ti.entry = e.type;
    if (auto ts = ti.entry.isTypeStruct())
    {
        ts.sym.requestTypeInfo = true;
        if (auto tmpl = ts.sym.isInstantiated())
            tmpl.minst = sc._module.importedFrom; // ensure it get's emitted
    }
    getTypeInfoType(loc, ti.entry, sc);
    assert(ti.entry.vtinfo);

    return ti;
}

/******************************************
 * Find or create a TypeAArray with index and next without
 * any head modifiers, tail `inout` is replaced with `const`
 *
 * Params:
 *      t = TypeAArray to convert
 * Returns:
 *      t = found type
 */
Type makeNakedAssociativeArray(TypeAArray t)
{
    Type tindex = t.index.toBasetype().nakedOf().substWildTo(MODFlags.const_);
    Type tnext = t.next.toBasetype().nakedOf().substWildTo(MODFlags.const_);
    if (tindex == t.index && tnext == t.next)
        return t;

    t = new TypeAArray(tnext, tindex);
    return t.merge();
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
bool builtinTypeInfo(Type t)
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
