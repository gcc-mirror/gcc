/**
 * Handle introspection functionality of the `__traits()` construct.
 *
 * Specification: $(LINK2 https://dlang.org/spec/traits.html, Traits)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/traits.d, _traits.d)
 * Documentation:  https://dlang.org/phobos/dmd_traits.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/traits.d
 */

module dmd.traits;

import core.stdc.stdio;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.astcodegen;
import dmd.astenums;
import dmd.attrib;
import dmd.canthrow;
import dmd.dclass;
import dmd.declaration;
import dmd.dimport;
import dmd.dinterpret;
import dmd.dmangle;
import dmd.dmodule;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.errorsink;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.funcsem;
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.nogc;
import dmd.optimize;
import dmd.parse;
import dmd.root.array;
import dmd.root.speller;
import dmd.root.stringtable;
import dmd.target;
import dmd.tokens;
import dmd.typesem;
import dmd.visitor;
import dmd.rootobject;
import dmd.common.outbuffer;
import dmd.root.string;

enum LOGSEMANTIC = false;

/************************ TraitsExp ************************************/

/**************************************
 * Convert `Expression` or `Type` to corresponding `Dsymbol`, additionally
 * stripping off expression contexts.
 *
 * Some symbol related `__traits` ignore arguments expression contexts.
 * For example:
 * ----
 *  struct S { void f() {} }
 *  S s;
 *  pragma(msg, __traits(isNested, s.f));
 *  // s.f is `DotVarExp`, but `__traits(isNested)`` needs a `FuncDeclaration`.
 * ----
 *
 * This is used for that common `__traits` behavior.
 *
 * Input:
 *      oarg     object to get the symbol for
 * Returns:
 *      Dsymbol  the corresponding symbol for oarg
 */
private Dsymbol getDsymbolWithoutExpCtx(RootObject oarg)
{
    if (auto e = isExpression(oarg))
    {
        if (auto dve = e.isDotVarExp())
            return dve.var;
        if (auto dte = e.isDotTemplateExp())
            return dte.td;
    }
    return getDsymbol(oarg);
}

/**
 * Fill an array of target size_t values that indicate possible pointer words in memory
 *  if interpreted as the type given as argument.
 *  One bit in the array per pointer-sized piece of memory
 * Params:
 *      loc = location for error messages
 *      t = type to generate pointer bitmap from
 *      data = array forming the bitmap
 *      eSink = error message sink
 * Returns:
 *      size of the type `t` in bytes, ulong.max on error
 */
ulong getTypePointerBitmap(Loc loc, Type t, ref Array!(ulong) data, ErrorSink eSink)
{
    auto tc = t.isTypeClass();
    const ulong sz = (tc && !tc.sym.isInterfaceDeclaration())
        ? tc.sym.AggregateDeclaration.size(loc)
        : t.size(loc);
    if (sz == SIZE_INVALID)
        return ulong.max;

    const sz_size_t = Type.tsize_t.size(loc); // size of target's size_t
    assert(sz_size_t <= ulong.sizeof);
    if (sz > sz.max - sz_size_t)
    {
        eSink.error(loc, "size overflow for type `%s`", t.toChars());
        return ulong.max;
    }

    const ulong bitsPerElement = sz_size_t * 8;  // bits used in each array element
    const ulong cntptr = (sz + sz_size_t - 1) / sz_size_t; // pointers have same size as sz_size_t
    const ulong length = (cntptr + bitsPerElement - 1) / bitsPerElement; // a bit per pointer

    data.setDim(cast(size_t)length);
    data.zero();

    ulong offset;
    bool error;    // sticky error indicator

    void visit(Type t)
    {
        void setpointer(ulong off)
        {
            ulong ptroff = off / sz_size_t;
            data[cast(size_t)(ptroff / bitsPerElement)] |= 1L << (ptroff % bitsPerElement);
        }

        void visitType(Type t)
        {
            Type tb = t.toBasetype();
            if (tb != t)
                visit(tb);
        }

        void visitError(TypeError t)
        {
            visitType(t);
        }

        void visitBasic(TypeBasic t)
        {
            if (t.ty == Tvoid)
                setpointer(offset);
        }

        void visitVector(TypeVector t)
        {
        }

        void visitSArray(TypeSArray t)
        {
            ulong arrayoff = offset;
            ulong nextsize = t.next.size();
            if (nextsize == SIZE_INVALID)
                error = true;
            ulong dim = t.dim.toInteger();
            for (ulong i = 0; i < dim; i++)
            {
                offset = arrayoff + i * nextsize;
                visit(t.next);
            }
            offset = arrayoff;
        }

        void visitDArray(TypeDArray t)
        {
            setpointer(offset + sz_size_t);
        }

        // dynamic array is {length,ptr}
        void visitAArray(TypeAArray t)
        {
            setpointer(offset);
        }

        void visitPointer(TypePointer t)
        {
            if (t.nextOf().ty != Tfunction) // don't mark function pointers
                setpointer(offset);
        }

        void visitReference(TypeReference t)
        {
            setpointer(offset);
        }

        void visitClass(TypeClass t)
        {
            setpointer(offset);
        }

        void visitFunction(TypeFunction t)
        {
        }

        void visitDelegate(TypeDelegate t)
        {
            setpointer(offset);
        }

        void visitEnum(TypeEnum t)
        {
            visitType(t);
        }

        void visitTuple(TypeTuple t)
        {
            visitType(t);
        }

        void visitNull(TypeNull t)
        {
            // always a null pointer
        }

        void visitNoreturn(TypeNoreturn t)
        {
        }

        void visitStruct(TypeStruct t)
        {
            ulong structoff = offset;
            foreach (v; t.sym.fields)
            {
                offset = structoff + v.offset;
                if (v.type.ty == Tclass)
                    setpointer(offset);
                else
                    visit(v.type);
            }
            offset = structoff;
        }

        void visitDefaultCase(Type t)
        {
            //printf("ty = %d\n", t.ty);
            assert(0);
        }

        mixin VisitType!void visit;
        visit.VisitType(t);
    }

    if (auto tcx = t.isTypeClass())
    {
        // a "toplevel" class is treated as an instance, while TypeClass fields are treated as references
        void visitTopLevelClass(TypeClass t)
        {
            ulong classoff = offset;
            // skip vtable-ptr and monitor
            if (t.sym.baseClass)
                visitTopLevelClass(t.sym.baseClass.type.isTypeClass());
            foreach (v; t.sym.fields)
            {
                offset = classoff + v.offset;
                visit(v.type);
            }
            offset = classoff;
        }

        visitTopLevelClass(tcx);
    }
    else
        visit(t);
    return error ? ulong.max : sz;
}

/**
 * get an array of size_t values that indicate possible pointer words in memory
 *  if interpreted as the type given as argument
 * the first array element is the size of the type for independent interpretation
 *  of the array
 * following elements bits represent one word (4/8 bytes depending on the target
 *  architecture). If set the corresponding memory might contain a pointer/reference.
 *
 *  Returns: [T.sizeof, pointerbit0-31/63, pointerbit32/64-63/128, ...]
 */
private Expression pointerBitmap(TraitsExp e, ErrorSink eSink)
{
    if (!e.args || e.args.length != 1)
    {
        eSink.error(e.loc, "a single type expected for trait pointerBitmap");
        return ErrorExp.get();
    }

    Type t = getType((*e.args)[0]);
    if (!t)
    {
        eSink.error(e.loc, "`%s` is not a type", (*e.args)[0].toChars());
        return ErrorExp.get();
    }

    Array!(ulong) data;
    const ulong sz = getTypePointerBitmap(e.loc, t, data, eSink);
    if (sz == ulong.max)
        return ErrorExp.get();

    auto exps = new Expressions(data.length + 1);
    (*exps)[0] = new IntegerExp(e.loc, sz, Type.tsize_t);       // [0] is size in bytes of t
    foreach (size_t i; 1 .. exps.length)
        (*exps)[i] = new IntegerExp(e.loc, data[cast(size_t) (i - 1)], Type.tsize_t);

    auto ale = new ArrayLiteralExp(e.loc, Type.tsize_t.sarrayOf(data.length + 1), exps);
    return ale;
}

Expression semanticTraits(TraitsExp e, Scope* sc)
{
    static if (LOGSEMANTIC)
    {
        printf("TraitsExp::semantic() %s\n", e.toChars());
    }

    if (e.ident != Id.compiles &&
        e.ident != Id.isSame &&
        e.ident != Id.identifier &&
        e.ident != Id.getProtection && e.ident != Id.getVisibility &&
        e.ident != Id.getAttributes)
    {
        // Pretend we're in a deprecated scope so that deprecation messages
        // aren't triggered when checking if a symbol is deprecated
        const save = sc.stc;
        if (e.ident == Id.isDeprecated)
            sc.stc |= STC.deprecated_;
        if (!TemplateInstance.semanticTiargs(e.loc, sc, e.args, 1))
        {
            sc.stc = save;
            return ErrorExp.get();
        }
        sc.stc = save;
    }
    size_t dim = e.args ? e.args.length : 0;

    Expression dimError(int expected)
    {
        error(e.loc, "expected %d arguments for `%s` but had %d", expected, e.ident.toChars(), cast(int)dim);
        return ErrorExp.get();
    }

    static IntegerExp True()
    {
        return IntegerExp.createBool(true);
    }

    static IntegerExp False()
    {
        return IntegerExp.createBool(false);
    }

    /********
     * Gets the function type from a given AST node
     * if the node is a function of some sort.
     * Params:
     *   o = an AST node to check for a `TypeFunction`
     *   fdp = if `o` is a FuncDeclaration then fdp is set to that, otherwise `null`
     * Returns:
     *   a type node if `o` is a declaration of
     *   a delegate, function, function-pointer or a variable of the former.
     *   Otherwise, `null`.
     */
    static TypeFunction toTypeFunction(RootObject o, out FuncDeclaration fdp)
    {
        Type t;
        if (auto s = getDsymbolWithoutExpCtx(o))
        {
            if (auto fd = s.isFuncDeclaration())
            {
                t = fd.type;
                fdp = fd;
            }
            else if (auto vd = s.isVarDeclaration())
                t = vd.type;
            else
                t = isType(o);
        }
        else
            t = isType(o);

        if (t)
        {
            if (auto tf = t.isFunction_Delegate_PtrToFunction())
                return tf;
        }

        return null;
    }

    IntegerExp isX(T)(bool delegate(T) fp)
    {
        if (!dim)
            return False();
        foreach (o; *e.args)
        {
            static if (is(T == Type))
                auto y = getType(o);

            static if (is(T : Dsymbol))
            {
                auto s = getDsymbolWithoutExpCtx(o);
                if (!s)
                    return False();
            }
            static if (is(T == Dsymbol))
                alias y = s;
            static if (is(T == Declaration))
                auto y = s.isDeclaration();
            static if (is(T == FuncDeclaration))
                auto y = s.isFuncDeclaration();

            if (!y || !fp(y))
                return False();
        }
        return True();
    }

    alias isTypeX = isX!Type;
    alias isDsymX = isX!Dsymbol;
    alias isDeclX = isX!Declaration;
    alias isFuncX = isX!FuncDeclaration;

    Expression isPkgX(bool function(Package) fp)
    {
        return isDsymX((Dsymbol sym) {
            Package p = resolveIsPackage(sym);
            return (p !is null) && fp(p);
        });
    }

    if (e.ident == Id.isArithmetic)
    {
        return isTypeX(t => t.isintegral() || t.isfloating());
    }
    if (e.ident == Id.isFloating)
    {
        return isTypeX(t => t.isfloating());
    }
    if (e.ident == Id.isIntegral)
    {
        return isTypeX(t => t.isintegral());
    }
    if (e.ident == Id.isScalar)
    {
        return isTypeX(t => t.isscalar());
    }
    if (e.ident == Id.isUnsigned)
    {
        return isTypeX(t => t.isunsigned());
    }
    if (e.ident == Id.isAssociativeArray)
    {
        return isTypeX(t => t.toBasetype().ty == Taarray);
    }
    if (e.ident == Id.isDeprecated)
    {
        if (isTypeX(t => t.iscomplex() || t.isimaginary()).toBool().hasValue(true))
            return True();
        return isDsymX(t => t.isDeprecated());
    }
    if (e.ident == Id.isFuture)
    {
       return isDeclX(t => t.isFuture());
    }
    if (e.ident == Id.isStaticArray)
    {
        return isTypeX(t => t.toBasetype().ty == Tsarray);
    }
    if (e.ident == Id.isAbstractClass)
    {
        return isTypeX(t => t.toBasetype().isTypeClass() &&
                            t.toBasetype().isTypeClass().sym.isAbstract());
    }
    if (e.ident == Id.isFinalClass)
    {
        return isTypeX(t => t.toBasetype().isTypeClass() &&
                            (t.toBasetype().isTypeClass().sym.storage_class & STC.final_) != 0);
    }
    if (e.ident == Id.isTemplate)
    {
        if (dim != 1)
            return dimError(1);

        return isDsymX((s)
        {
            if (!s.toAlias().isOverloadable())
                return false;
            return overloadApply(s,
                sm => sm.isTemplateDeclaration() !is null) != 0;
        });
    }
    if (e.ident == Id.isPOD)
    {
        if (dim != 1)
            return dimError(1);

        auto o = (*e.args)[0];
        auto t = isType(o);
        if (!t)
        {
            error(e.loc, "type expected as second argument of __traits `%s` instead of `%s`",
                e.ident.toChars(), o.toChars());
            return ErrorExp.get();
        }

        Type tb = t.baseElemOf();
        auto ts = tb.isTypeStruct();
        if (auto sd = ts ? ts.sym : null)
        {
            return sd.isPOD() ? True() : False();
        }
        return True();
    }
    if (e.ident == Id.hasCopyConstructor || e.ident == Id.hasPostblit)
    {
        if (dim != 1)
            return dimError(1);

        auto o = (*e.args)[0];
        auto t = isType(o);
        if (!t)
        {
            error(e.loc, "type expected as second argument of __traits `%s` instead of `%s`",
                e.ident.toChars(), o.toChars());
            return ErrorExp.get();
        }

        Type tb = t.baseElemOf();
        auto ts = tb.isTypeStruct();
        if (auto sd = ts ? ts.sym : null)
        {
            return (e.ident == Id.hasPostblit) ? (sd.postblit ? True() : False())
                 : (sd.hasCopyCtor ? True() : False());
        }
        return False();
    }
    if (e.ident == Id.isCopyable)
    {
        if (dim != 1)
            return dimError(1);

        auto o = (*e.args)[0];
        auto t = isType(o);
        if (!t)
        {
            error(e.loc, "type expected as second argument of __traits `%s` instead of `%s`",
                    e.ident.toChars(), o.toChars());
            return ErrorExp.get();
        }

        t = t.toBasetype();     // get the base in case `t` is an `enum`

        if (auto ts = t.isTypeStruct())
        {
            ts.sym.dsymbolSemantic(sc);
        }

        return isCopyable(t) ? True() : False();
    }

    if (e.ident == Id.isNested)
    {
        if (dim != 1)
            return dimError(1);

        auto o = (*e.args)[0];
        auto s = getDsymbolWithoutExpCtx(o);
        if (!s)
        {
        }
        else if (auto ad = s.isAggregateDeclaration())
        {
            return ad.isNested() ? True() : False();
        }
        else if (auto fd = s.isFuncDeclaration())
        {
            return fd.isNested() ? True() : False();
        }

        error(e.loc, "aggregate or function expected instead of `%s`", o.toChars());
        return ErrorExp.get();
    }
    if (e.ident == Id.isDisabled)
    {
        if (dim != 1)
            return dimError(1);

        return isDeclX(f => f.isDisabled());
    }
    if (e.ident == Id.isAbstractFunction)
    {
        if (dim != 1)
            return dimError(1);

        return isFuncX(f => f.isAbstract());
    }
    if (e.ident == Id.isVirtualFunction)
    {
        // @@@DEPRECATED2.121@@@
        // Deprecated in 2.101 - Can be removed from 2.121
        deprecation(e.loc, "`traits(isVirtualFunction)` is deprecated. Use `traits(isVirtualMethod)` instead");

        if (dim != 1)
            return dimError(1);

        return isFuncX(f => f.isVirtual());
    }
    if (e.ident == Id.isVirtualMethod)
    {
        if (dim != 1)
            return dimError(1);

        return isFuncX(f => f.isVirtualMethod());
    }
    if (e.ident == Id.isFinalFunction)
    {
        if (dim != 1)
            return dimError(1);

        return isFuncX(f => f.isFinalFunc());
    }
    if (e.ident == Id.isOverrideFunction)
    {
        if (dim != 1)
            return dimError(1);

        return isFuncX(f => f.isOverride());
    }
    if (e.ident == Id.isStaticFunction)
    {
        if (dim != 1)
            return dimError(1);

        return isFuncX(f => !f.needThis() && !f.isNested());
    }
    if (e.ident == Id.isModule)
    {
        if (dim != 1)
            return dimError(1);

        return isPkgX(p => p.isModule() || p.isPackageMod());
    }
    if (e.ident == Id.isPackage)
    {
        if (dim != 1)
            return dimError(1);

        return isPkgX(p => p.isModule() is null);
    }
    if (e.ident == Id.isRef)
    {
        if (dim != 1)
            return dimError(1);

        return isDeclX(d => d.isRef());
    }
    if (e.ident == Id.isOut)
    {
        if (dim != 1)
            return dimError(1);

        return isDeclX(d => d.isOut());
    }
    if (e.ident == Id.isLazy)
    {
        if (dim != 1)
            return dimError(1);

        return isDeclX(d => (d.storage_class & STC.lazy_) != 0);
    }
    if (e.ident == Id.identifier)
    {
        // Get identifier for symbol as a string literal
        /* Specify 0 for bit 0 of the flags argument to semanticTiargs() so that
         * a symbol should not be folded to a constant.
         * Bit 1 means don't convert Parameter to Type if Parameter has an identifier
         */
        if (!TemplateInstance.semanticTiargs(e.loc, sc, e.args, 2))
            return ErrorExp.get();
        if (dim != 1)
            return dimError(1);

        auto o = (*e.args)[0];
        Identifier id;
        if (auto po = isParameter(o))
        {
            if (!po.ident)
            {
                error(e.loc, "argument `%s` has no identifier", po.type.toChars());
                return ErrorExp.get();
            }
            id = po.ident;
        }
        else
        {
            Dsymbol s = getDsymbolWithoutExpCtx(o);
            if (!s || !s.ident)
            {
                error(e.loc, "argument `%s` has no identifier", o.toChars());
                return ErrorExp.get();
            }
            id = s.ident;
        }

        auto se = new StringExp(e.loc, id.toString());
        return se.expressionSemantic(sc);
    }
    if (e.ident == Id.fullyQualifiedName) // https://dlang.org/spec/traits.html#fullyQualifiedName
    {
        if (dim != 1)
            return dimError(1);

        Scope* sc2 = sc.push();
        sc2.flags = sc.flags | SCOPE.noaccesscheck | SCOPE.ignoresymbolvisibility;
        bool ok = TemplateInstance.semanticTiargs(e.loc, sc2, e.args, 1);
        sc2.pop();
        if (!ok)
            return ErrorExp.get();

        const(char)[] fqn;
        auto o = (*e.args)[0];
        if (auto s = getDsymbolWithoutExpCtx(o))
        {
            if (s.semanticRun == PASS.initial)
                s.dsymbolSemantic(null);

            fqn = s.toPrettyChars().toDString();
        }
        else if (auto t = getType(o))
        {
            fqn = t.toPrettyChars(true).toDString();
        }
        else
        {
            if (!isError(o))
                error(e.loc, "argument `%s` has no identifier", o.toChars());
            return ErrorExp.get();
        }
        assert(fqn);
        auto se = new StringExp(e.loc, fqn);
        return se.expressionSemantic(sc);

    }
    if (e.ident == Id.getProtection || e.ident == Id.getVisibility)
    {
        if (dim != 1)
            return dimError(1);

        Scope* sc2 = sc.push();
        sc2.flags = sc.flags | SCOPE.noaccesscheck | SCOPE.ignoresymbolvisibility;
        bool ok = TemplateInstance.semanticTiargs(e.loc, sc2, e.args, 1);
        sc2.pop();
        if (!ok)
            return ErrorExp.get();

        auto o = (*e.args)[0];
        auto s = getDsymbolWithoutExpCtx(o);
        if (!s)
        {
            if (!isError(o))
                error(e.loc, "argument `%s` has no visibility", o.toChars());
            return ErrorExp.get();
        }
        if (s.semanticRun == PASS.initial)
            s.dsymbolSemantic(null);

        auto protName = visibilityToString(s.visible().kind); // TODO: How about package(names)
        assert(protName);
        auto se = new StringExp(e.loc, protName);
        return se.expressionSemantic(sc);
    }
    if (e.ident == Id.parent)
    {
        if (dim != 1)
            return dimError(1);

        auto o = (*e.args)[0];
        auto s = getDsymbolWithoutExpCtx(o);
        if (s)
        {
            // https://issues.dlang.org/show_bug.cgi?id=12496
            // Consider:
            // class T1
            // {
            //     class C(uint value) { }
            // }
            // __traits(parent, T1.C!2)
            if (auto ad = s.isAggregateDeclaration())  // `s` is `C`
            {
                if (ad.isNested())                     // `C` is nested
                {
                    if (auto p = s.toParent())         // `C`'s parent is `C!2`, believe it or not
                    {
                        if (auto ti = p.isTemplateInstance())    // `C!2` is a template instance
                        {
                            s = p;                     // `C!2`'s parent is `T1`
                            auto td = ti.tempdecl;
                            if (td)
                                s = td;                // get the declaration context just in case there's two contexts
                        }
                    }
                }
            }

            if (auto fd = s.isFuncDeclaration()) // https://issues.dlang.org/show_bug.cgi?id=8943
                s = fd.toAliasFunc();
            if (!s.isImport()) // https://issues.dlang.org/show_bug.cgi?id=8922
                s = s.toParent();
        }
        if (!s || s.isImport())
        {
            error(e.loc, "argument `%s` has no parent", o.toChars());
            return ErrorExp.get();
        }

        if (auto f = s.isFuncDeclaration())
        {
            if (auto td = getFuncTemplateDecl(f))
            {
                if (td.overroot) // if not start of overloaded list of TemplateDeclaration's
                    td = td.overroot; // then get the start
                Expression ex = new TemplateExp(e.loc, td, f);
                ex = ex.expressionSemantic(sc);
                return ex;
            }
            if (auto fld = f.isFuncLiteralDeclaration())
            {
                // Directly translate to VarExp instead of FuncExp
                Expression ex = new VarExp(e.loc, fld, true);
                return ex.expressionSemantic(sc);
            }
        }
        return symbolToExp(s, e.loc, sc, false);
    }
    if (e.ident == Id.child)
    {
        if (dim != 2)
            return dimError(2);

        Expression ex;
        auto op = (*e.args)[0];
        if (auto symp = getDsymbol(op))
            ex = new DsymbolExp(e.loc, symp);
        else if (auto exp = op.isExpression())
            ex = exp;
        else
        {
            error(e.loc, "symbol or expression expected as first argument of __traits `child` instead of `%s`", op.toChars());
            return ErrorExp.get();
        }

        ex = ex.expressionSemantic(sc);
        auto oc = (*e.args)[1];
        auto symc = getDsymbol(oc);
        if (!symc)
        {
            error(e.loc, "symbol expected as second argument of __traits `child` instead of `%s`", oc.toChars());
            return ErrorExp.get();
        }

        if (auto d = symc.isDeclaration())
            ex = new DotVarExp(e.loc, ex, d);
        else if (auto td = symc.isTemplateDeclaration())
            ex = new DotExp(e.loc, ex, new TemplateExp(e.loc, td));
        else if (auto ti = symc.isScopeDsymbol())
            ex = new DotExp(e.loc, ex, new ScopeExp(e.loc, ti));
        else
            assert(0);

        ex = ex.expressionSemantic(sc);
        return ex;
    }
    if (e.ident == Id.toType)
    {
        if (dim != 1)
            return dimError(1);

        auto ex = isExpression((*e.args)[0]);
        if (!ex)
        {
            error(e.loc, "expression expected as second argument of __traits `%s`", e.ident.toChars());
            return ErrorExp.get();
        }
        ex = ex.ctfeInterpret();

        StringExp se = semanticString(sc, ex, "__traits(toType, string)");
        if (!se)
        {
            return ErrorExp.get();
        }
        Type t = decoToType(se.toUTF8(sc).peekString());
        if (!t)
        {
            error(e.loc, "cannot determine `%s`", e.toChars());
            return ErrorExp.get();
        }
        return (new TypeExp(e.loc, t)).expressionSemantic(sc);
    }
    if (e.ident == Id.hasMember ||
        e.ident == Id.getMember ||
        e.ident == Id.getOverloads ||
        e.ident == Id.getVirtualMethods ||
        e.ident == Id.getVirtualFunctions)
    {
        if (dim != 2 && !(dim == 3 && e.ident == Id.getOverloads))
            return dimError(2);

        auto o = (*e.args)[0];
        auto ex = isExpression((*e.args)[1]);
        if (!ex)
        {
            error(e.loc, "expression expected as second argument of __traits `%s`", e.ident.toChars());
            return ErrorExp.get();
        }
        ex = ex.ctfeInterpret();

        bool includeTemplates = false;
        if (dim == 3 && e.ident == Id.getOverloads)
        {
            auto b = isExpression((*e.args)[2]);
            b = b.ctfeInterpret();
            if (!b.type.equals(Type.tbool))
            {
                error(e.loc, "`bool` expected as third argument of `__traits(getOverloads)`, not `%s` of type `%s`", b.toChars(), b.type.toChars());
                return ErrorExp.get();
            }
            includeTemplates = b.toBool().get();
        }

        StringExp se = ex.toStringExp();
        if (!se || se.len == 0)
        {
            error(e.loc, "string expected as second argument of __traits `%s` instead of `%s`", e.ident.toChars(), ex.toChars());
            return ErrorExp.get();
        }
        se = se.toUTF8(sc);

        if (se.sz != 1)
        {
            error(e.loc, "string must be chars");
            return ErrorExp.get();
        }
        auto id = Identifier.idPool(se.peekString());

        /* Prefer a Type, because getDsymbol(Type) can lose type modifiers.
           Then a Dsymbol, because it might need some runtime contexts.
         */

        Dsymbol sym = getDsymbol(o);

        if (sym && e.ident == Id.hasMember)
        {
            if (auto sm = sym.search(e.loc, id))
                return True();

            // https://issues.dlang.org/show_bug.cgi?id=23951
            if (auto decl = sym.isDeclaration())
            {
                ex = typeDotIdExp(e.loc, decl.type, id);
                goto doSemantic;
            }
        }

        if (auto t = isType(o))
            ex = typeDotIdExp(e.loc, t, id);
        else if (sym)
        {
            ex = new DsymbolExp(e.loc, sym);
            ex = new DotIdExp(e.loc, ex, id);
        }
        else if (auto ex2 = isExpression(o))
            ex = new DotIdExp(e.loc, ex2, id);
        else
        {
            error(e.loc, "invalid first argument");
            return ErrorExp.get();
        }
    doSemantic:
        // ignore symbol visibility and disable access checks for these traits
        Scope* scx = sc.push();
        scx.flags |= SCOPE.ignoresymbolvisibility | SCOPE.noaccesscheck;
        scope (exit) scx.pop();

        if (e.ident == Id.hasMember)
        {
            /* Take any errors as meaning it wasn't found
             */
            ex = ex.trySemantic(scx);
            return ex ? True() : False();
        }
        else if (e.ident == Id.getMember)
        {
            if (auto die = ex.isDotIdExp())
                // Prevent semantic() from replacing Symbol with its initializer
                die.wantsym = true;
            ex = ex.expressionSemantic(scx);
            return ex;
        }
        else if (e.ident == Id.getVirtualFunctions ||
                 e.ident == Id.getVirtualMethods ||
                 e.ident == Id.getOverloads)
        {
            uint errors = global.errors;
            Expression eorig = ex;
            ex = ex.expressionSemantic(scx);
            if (errors < global.errors)
                error(e.loc, "`%s` cannot be resolved", eorig.toChars());

            if (e.ident == Id.getVirtualFunctions)
            {
                // @@@DEPRECATED2.121@@@
                // Deprecated in 2.101 - Can be removed from 2.121
                deprecation(e.loc, "`traits(getVirtualFunctions)` is deprecated. Use `traits(getVirtualMethods)` instead");
            }

            /* Create tuple of functions of ex
             */
            auto exps = new Expressions();
            Dsymbol f;
            if (auto ve = ex.isVarExp)
            {
                if (ve.var.isFuncDeclaration() || ve.var.isOverDeclaration())
                    f = ve.var;
                ex = null;
            }
            else if (auto dve = ex.isDotVarExp)
            {
                if (dve.var.isFuncDeclaration() || dve.var.isOverDeclaration())
                    f = dve.var;
                if (dve.e1.op == EXP.dotType || dve.e1.op == EXP.this_)
                    ex = null;
                else
                    ex = dve.e1;
            }
            else if (auto te = ex.isTemplateExp)
            {
                auto td = te.td;
                f = td;
                if (td && td.funcroot)
                    f = td.funcroot;
                ex = null;
            }
            else if (auto dte = ex.isDotTemplateExp)
            {
                auto td = dte.td;
                f = td;
                if (td && td.funcroot)
                    f = td.funcroot;
                ex = null;
                if (dte.e1.op != EXP.dotType && dte.e1.op != EXP.this_)
                    ex = dte.e1;
            }
            bool[string] funcTypeHash;

            /* Compute the function signature and insert it in the
             * hashtable, if not present. This is needed so that
             * traits(getOverlods, F3, "visit") does not count `int visit(int)`
             * twice in the following example:
             *
             * =============================================
             * interface F1 { int visit(int);}
             * interface F2 { int visit(int); void visit(); }
             * interface F3 : F2, F1 {}
             *==============================================
             */
            void insertInterfaceInheritedFunction(FuncDeclaration fd, Expression e)
            {
                auto signature = fd.type.toString();
                //printf("%s - %s\n", fd.toChars, signature);
                if (signature !in funcTypeHash)
                {
                    funcTypeHash[signature] = true;
                    exps.push(e);
                }
            }

            int dg(Dsymbol s)
            {
                auto fd = s.isFuncDeclaration();
                if (!fd)
                {
                    if (includeTemplates)
                    {
                        if (auto td = s.isTemplateDeclaration())
                        {
                            // if td is part of an overload set we must take a copy
                            // which shares the same `instances` cache but without
                            // `overroot` and `overnext` set to avoid overload
                            // behaviour in the result.
                            if (td.overnext !is null)
                            {
                                if (td.instances is null)
                                {
                                    // create an empty AA just to copy it
                                    scope ti = new TemplateInstance(Loc.initial, Id.empty, null);
                                    auto tib = TemplateInstanceBox(ti);
                                    td.instances[tib] = null;
                                    td.instances.clear();
                                }
                                td = td.syntaxCopy(null);
                                import core.stdc.string : memcpy;
                                memcpy(cast(void*) td, cast(void*) s,
                                        __traits(classInstanceSize, TemplateDeclaration));
                                td.overroot = null;
                                td.overnext = null;
                            }

                            auto e = ex ? new DotTemplateExp(Loc.initial, ex, td)
                                        : new DsymbolExp(Loc.initial, td);
                            exps.push(e);
                        }
                    }
                    return 0;
                }
                if (e.ident == Id.getVirtualFunctions && !fd.isVirtual())
                    return 0;
                if (e.ident == Id.getVirtualMethods && !fd.isVirtualMethod())
                    return 0;

                auto fa = new FuncAliasDeclaration(fd.ident, fd, false);
                fa.visibility = fd.visibility;

                auto e = ex ? new DotVarExp(Loc.initial, ex, fa, false)
                            : new DsymbolExp(Loc.initial, fa, false);

                // if the parent is an interface declaration
                // we must check for functions with the same signature
                // in different inherited interfaces
                if (sym && sym.isInterfaceDeclaration())
                    insertInterfaceInheritedFunction(fd, e);
                else
                    exps.push(e);
                return 0;
            }

            InterfaceDeclaration ifd = null;
            if (sym)
                ifd = sym.isInterfaceDeclaration();
            // If the symbol passed as a parameter is an
            // interface that inherits other interfaces
            overloadApply(f, &dg);
            if (ifd && ifd.interfaces && f)
            {
                // check the overloads of each inherited interface individually
                foreach (bc; ifd.interfaces)
                {
                    if (auto fd = bc.sym.search(e.loc, f.ident))
                        overloadApply(fd, &dg);
                }
            }

            auto tup = new TupleExp(e.loc, exps);
            return tup.expressionSemantic(scx);
        }
        else
            assert(0);
    }
    if (e.ident == Id.classInstanceSize || e.ident == Id.classInstanceAlignment)
    {
        if (dim != 1)
            return dimError(1);

        auto o = (*e.args)[0];
        auto s = getDsymbol(o);
        auto cd = s ? s.isClassDeclaration() : null;
        if (!cd)
        {
            error(e.loc, "first argument is not a class");
            return ErrorExp.get();
        }
        if (cd.sizeok != Sizeok.done)
        {
            cd.size(e.loc);
        }
        if (cd.sizeok != Sizeok.done)
        {
            error(e.loc, "%s `%s` is forward referenced", cd.kind(), cd.toChars());
            return ErrorExp.get();
        }

        return new IntegerExp(e.loc, e.ident == Id.classInstanceSize ? cd.structsize : cd.alignsize, Type.tsize_t);
    }
    if (e.ident == Id.getAliasThis)
    {
        if (dim != 1)
            return dimError(1);

        auto o = (*e.args)[0];
        auto s = getDsymbol(o);
        auto ad = s ? s.isAggregateDeclaration() : null;

        auto exps = new Expressions();
        if (ad && ad.aliasthis)
            exps.push(new StringExp(e.loc, ad.aliasthis.ident.toString()));
        Expression ex = new TupleExp(e.loc, exps);
        ex = ex.expressionSemantic(sc);
        return ex;
    }
    if (e.ident == Id.getAttributes)
    {
        /* Specify 0 for bit 0 of the flags argument to semanticTiargs() so that
         * a symbol should not be folded to a constant.
         * Bit 1 means don't convert Parameter to Type if Parameter has an identifier
         */
        if (!TemplateInstance.semanticTiargs(e.loc, sc, e.args, 3))
            return ErrorExp.get();

        if (dim != 1)
            return dimError(1);

        auto o = (*e.args)[0];
        auto po = isParameter(o);
        auto s = getDsymbolWithoutExpCtx(o);
        auto typeOfArg = isType(o);
        UserAttributeDeclaration udad = null;
        if (po)
        {
            udad = po.userAttribDecl;
        }
        else if (s)
        {
            // @@@DEPRECATION 2.100.2
            if (auto fd = s.isFuncDeclaration())
            {
                if (fd.overnext)
                {
                    deprecation(e.loc, "`__traits(getAttributes)` may only be used for individual functions, not the overload set `%s`", fd.toChars());
                    deprecationSupplemental(e.loc, "the result of `__traits(getOverloads)` may be used to select the desired function to extract attributes from");
                }
            }

            // @@@DEPRECATION 2.100.2
            if (auto td = s.isTemplateDeclaration())
            {
                if (td.overnext || td.overroot)
                {
                    deprecation(e.loc, "`__traits(getAttributes)` may only be used for individual functions, not the overload set `%s`", td.ident.toChars());
                    deprecationSupplemental(e.loc, "the result of `__traits(getOverloads)` may be used to select the desired function to extract attributes from");
                }
            }
            if (s.isImport())
            {
                s = s.isImport().mod;
            }
            //printf("getAttributes %s, attrs = %p, scope = %p\n", s.toChars(), s.userAttribDecl, s._scope);
            udad = s.userAttribDecl;
        }
        else if (typeOfArg)
        {
            // If there is a type but no symbol, do nothing rather than erroring.
        }
        else
        {
            version (none)
            {
                Expression x = isExpression(o);
                Type t = isType(o);
                if (x)
                    printf("e = %s %s\n", EXPtoString(x.op).ptr, x.toChars());
                if (t)
                    printf("t = %d %s\n", t.ty, t.toChars());
            }
            error(e.loc, "first argument is not a symbol");
            return ErrorExp.get();
        }

        auto exps = udad ? udad.getAttributes() : new Expressions();
        auto tup = new TupleExp(e.loc, exps);
        return tup.expressionSemantic(sc);
    }
    if (e.ident == Id.getFunctionAttributes)
    {
        /* Extract all function attributes as a tuple (const/shared/inout/pure/nothrow/etc) except UDAs.
         * https://dlang.org/spec/traits.html#getFunctionAttributes
         */
        if (dim != 1)
            return dimError(1);

        FuncDeclaration fd;
        TypeFunction tf = toTypeFunction((*e.args)[0], fd);

        if (!tf)
        {
            error(e.loc, "first argument is not a function");
            return ErrorExp.get();
        }

        // https://issues.dlang.org/show_bug.cgi?id=19706
        // When getting the attributes of the instance of a
        // templated member function semantic tiargs does
        // not perform semantic3 on the instance.
        // For more information see FuncDeclaration.functionSemantic.
        // For getFunctionAttributes it is mandatory to do
        // attribute inference.
        if (fd && fd.parent && fd.parent.isTemplateInstance)
        {
            functionSemantic3(fd);
            tf = fd.type.isTypeFunction();
        }

        auto mods = new Expressions();

        void addToMods(string str)
        {
            mods.push(new StringExp(Loc.initial, str));
        }
        tf.modifiersApply(&addToMods);
        tf.attributesApply(&addToMods, TRUSTformatSystem);

        auto tup = new TupleExp(e.loc, mods);
        return tup.expressionSemantic(sc);
    }
    if (e.ident == Id.isReturnOnStack)
    {
        /* Extract as a boolean if function return value is on the stack
         * https://dlang.org/spec/traits.html#isReturnOnStack
         */
        if (dim != 1)
            return dimError(1);

        RootObject o = (*e.args)[0];
        FuncDeclaration fd;
        TypeFunction tf = toTypeFunction(o, fd);

        if (!tf)
        {
            error(e.loc, "argument to `__traits(isReturnOnStack, %s)` is not a function", o.toChars());
            return ErrorExp.get();
        }

        bool value = target.isReturnOnStack(tf, fd && fd.needThis());
        return IntegerExp.createBool(value);
    }
    if (e.ident == Id.getFunctionVariadicStyle)
    {
        /* Accept a symbol or a type. Returns one of the following:
         *  "none"      not a variadic function
         *  "argptr"    extern(D) void dstyle(...), use `__argptr` and `__arguments`
         *  "stdarg"    extern(C) void cstyle(int, ...), use core.stdc.stdarg
         *  "typesafe"  void typesafe(T[] ...)
         *  "KR"        old K+R style
         */
        // get symbol linkage as a string
        if (dim != 1)
            return dimError(1);

        LINK link;
        VarArg varargs;
        auto o = (*e.args)[0];

        FuncDeclaration fd;
        TypeFunction tf = toTypeFunction(o, fd);

        if (tf)
        {
            link = tf.linkage;
            varargs = tf.parameterList.varargs;
        }
        else
        {
            if (!fd)
            {
                error(e.loc, "argument to `__traits(getFunctionVariadicStyle, %s)` is not a function", o.toChars());
                return ErrorExp.get();
            }
            link = fd._linkage;
            varargs = fd.getParameterList().varargs;
        }
        string style;
        final switch (varargs)
        {
            case VarArg.none:     style = "none";           break;
            case VarArg.variadic: style = (link == LINK.d)
                                             ? "argptr"
                                             : "stdarg";    break;
            case VarArg.KRvariadic: style = "KR";           break;
            case VarArg.typesafe: style = "typesafe";       break;
        }
        auto se = new StringExp(e.loc, style);
        return se.expressionSemantic(sc);
    }
    if (e.ident == Id.getParameterStorageClasses)
    {
        /* Accept a function symbol or a type, followed by a parameter index.
         * Returns a tuple of strings of the parameter's storage classes.
         */
        // get symbol linkage as a string
        if (dim != 2)
            return dimError(2);

        auto o = (*e.args)[0];
        auto o1 = (*e.args)[1];

        ParameterList fparams;

        CallExp ce;
        if (auto exp = isExpression(o))
            ce = exp.isCallExp();

        if (ce)
        {
            fparams = ce.f.getParameterList();
        }
        else
        {
            FuncDeclaration fd;
            auto tf = toTypeFunction(o, fd);
            if (tf)
                fparams = tf.parameterList;
            else if (fd)
                fparams = fd.getParameterList();
            else
            {
                error(e.loc, "first argument to `__traits(getParameterStorageClasses, %s, %s)` is not a function or a function call",
                    o.toChars(), o1.toChars());
                return ErrorExp.get();
            }
        }

        // Avoid further analysis for invalid functions leading to misleading error messages
        if (!fparams.parameters)
            return ErrorExp.get();

        StorageClass stc;

        // Set stc to storage class of the ith parameter
        auto ex = isExpression((*e.args)[1]);
        if (!ex)
        {
            error(e.loc, "expression expected as second argument of `__traits(getParameterStorageClasses, %s, %s)`",
                o.toChars(), o1.toChars());
            return ErrorExp.get();
        }
        ex = ex.ctfeInterpret();
        auto ii = ex.toUInteger();
        if (ii >= fparams.length)
        {
            error(e.loc, "parameter index must be in range 0..%u not %s", cast(uint)fparams.length, ex.toChars());
            return ErrorExp.get();
        }

        uint n = cast(uint)ii;
        Parameter p = fparams[n];
        stc = p.storageClass;

        // This mirrors hdrgen.visit(Parameter p)
        if (p.type && p.type.mod & MODFlags.shared_)
            stc &= ~STC.shared_;

        auto exps = new Expressions;

        void push(string s)
        {
            exps.push(new StringExp(e.loc, s));
        }

        if (stc & STC.auto_)
            push("auto");
        if (stc & STC.return_)
            push("return");

        if (stc & STC.out_)
            push("out");
        else if (stc & STC.in_)
            push("in");
        else if (stc & STC.ref_)
            push("ref");
        else if (stc & STC.lazy_)
            push("lazy");
        else if (stc & STC.alias_)
            push("alias");

        if (stc & STC.const_)
            push("const");
        if (stc & STC.immutable_)
            push("immutable");
        if (stc & STC.wild)
            push("inout");
        if (stc & STC.shared_)
            push("shared");
        if (stc & STC.scope_ && !(stc & STC.scopeinferred))
            push("scope");

        auto tup = new TupleExp(e.loc, exps);
        return tup.expressionSemantic(sc);
    }
    if (e.ident == Id.getLinkage)
    {
        // get symbol linkage as a string
        if (dim != 1)
            return dimError(1);

        LINK link;
        auto o = (*e.args)[0];

        FuncDeclaration fd;
        TypeFunction tf = toTypeFunction(o, fd);

        if (tf)
        {
            link = fd ? fd.toAliasFunc()._linkage : tf.linkage;
        }
        else
        {
            auto s = getDsymbol(o);
            Declaration d;
            AggregateDeclaration agg;
            if (!s || ((d = s.isDeclaration()) is null && (agg = s.isAggregateDeclaration()) is null))
            {
                error(e.loc, "argument to `__traits(getLinkage, %s)` is not a declaration", o.toChars());
                return ErrorExp.get();
            }

            if (d !is null)
                link = d._linkage;
            else
            {
                // Resolves forward references
                if (agg.sizeok != Sizeok.done)
                {
                    agg.size(e.loc);
                    if (agg.sizeok != Sizeok.done)
                    {
                        error(e.loc, "%s `%s` is forward referenced", agg.kind(), agg.toChars());
                        return ErrorExp.get();
                    }
                }

                final switch (agg.classKind)
                {
                    case ClassKind.d:
                        link = LINK.d;
                        break;
                    case ClassKind.cpp:
                        link = LINK.cpp;
                        break;
                    case ClassKind.objc:
                        link = LINK.objc;
                        break;
                    case ClassKind.c:
                        link = LINK.c;
                        break;
                }
            }
        }
        auto linkage = linkageToChars(link);
        auto se = new StringExp(e.loc, linkage.toDString());
        return se.expressionSemantic(sc);
    }
    if (e.ident == Id.allMembers ||
        e.ident == Id.derivedMembers)
    {
        if (dim != 1)
            return dimError(1);

        auto o = (*e.args)[0];
        auto s = getDsymbol(o);
        if (!s)
        {
            error(e.loc, "in expression `%s` `%s` can't have members", e.toChars(), o.toChars());
            errorSupplemental(e.loc, "`%s` must evaluate to either a module, a struct, an union, a class, an interface or a template instantiation", o.toChars());

            return ErrorExp.get();
        }
        if (auto imp = s.isImport())
        {
            // https://issues.dlang.org/show_bug.cgi?id=9692
            // https://issues.dlang.org/show_bug.cgi?id=20008
            if (imp.pkg)
                s = imp.pkg;
        }

        // https://issues.dlang.org/show_bug.cgi?id=16044
        if (auto p = s.isPackage())
        {
            if (auto pm = p.isPackageMod())
                s = pm;
        }

        auto sds = s.isScopeDsymbol();
        if (!sds || sds.isTemplateDeclaration())
        {
            error(e.loc, "in expression `%s` %s `%s` has no members", e.toChars(), s.kind(), s.toChars());
            errorSupplemental(e.loc, "`%s` must evaluate to either a module, a struct, an union, a class, an interface or a template instantiation", s.toChars());
            return ErrorExp.get();
        }

        auto idents = new Identifiers();

        int pushIdentsDg(size_t n, Dsymbol sm)
        {
            if (!sm)
                return 1;

            // skip local symbols, such as static foreach loop variables
            if (auto decl = sm.isDeclaration())
            {
                if (decl.storage_class & STC.local)
                {
                    return 0;
                }
                // skip 'this' context pointers
                else if (decl.isThisDeclaration())
                    return 0;
            }

            // https://issues.dlang.org/show_bug.cgi?id=20915
            // skip version and debug identifiers
            if (sm.isVersionSymbol() || sm.isDebugSymbol())
                return 0;

            //printf("\t[%i] %s %s\n", i, sm.kind(), sm.toChars());
            if (sm.ident)
            {
                // https://issues.dlang.org/show_bug.cgi?id=10096
                // https://issues.dlang.org/show_bug.cgi?id=10100
                // Skip over internal members in __traits(allMembers)
                if ((sm.isCtorDeclaration() && sm.ident != Id.ctor) ||
                    (sm.isDtorDeclaration() && sm.ident != Id.dtor) ||
                    (sm.isPostBlitDeclaration() && sm.ident != Id.postblit) ||
                    sm.isInvariantDeclaration() ||
                    sm.isUnitTestDeclaration())

                {
                    return 0;
                }
                if (sm.ident == Id.empty)
                {
                    return 0;
                }
                if (sm.isTypeInfoDeclaration()) // https://issues.dlang.org/show_bug.cgi?id=15177
                    return 0;
                if ((!sds.isModule() && !sds.isPackage()) && sm.isImport()) // https://issues.dlang.org/show_bug.cgi?id=17057
                    return 0;

                //printf("\t%s\n", sm.ident.toChars());

                /* Skip if already present in idents[]
                 */
                foreach (id; *idents)
                {
                    if (id == sm.ident)
                        return 0;

                    // Avoid using strcmp in the first place due to the performance impact in an O(N^2) loop.
                    debug
                    {
                        import core.stdc.string : strcmp;
                        assert(strcmp(id.toChars(), sm.ident.toChars()) != 0);
                    }
                }
                idents.push(sm.ident);
            }
            else if (auto ed = sm.isEnumDeclaration())
            {
                _foreach(null, ed.members, &pushIdentsDg);
            }
            return 0;
        }

        _foreach(sc, sds.members, &pushIdentsDg);
        auto cd = sds.isClassDeclaration();
        if (cd && e.ident == Id.allMembers)
        {
            if (cd.semanticRun < PASS.semanticdone)
                cd.dsymbolSemantic(null); // https://issues.dlang.org/show_bug.cgi?id=13668
                                   // Try to resolve forward reference

            void pushBaseMembersDg(ClassDeclaration cd)
            {
                for (size_t i = 0; i < cd.baseclasses.length; i++)
                {
                    auto cb = (*cd.baseclasses)[i].sym;
                    assert(cb);
                    _foreach(null, cb.members, &pushIdentsDg);
                    if (cb.baseclasses.length)
                        pushBaseMembersDg(cb);
                }
            }

            pushBaseMembersDg(cd);
        }

        // Turn Identifiers into StringExps reusing the allocated array
        assert(Expressions.sizeof == Identifiers.sizeof);
        auto exps = cast(Expressions*)idents;
        foreach (i, id; *idents)
        {
            auto se = new StringExp(e.loc, id.toString());
            (*exps)[i] = se;
        }

        /* Making this a tuple is more flexible, as it can be statically unrolled.
         * To make an array literal, enclose __traits in [ ]:
         *   [ __traits(allMembers, ...) ]
         */
        Expression ex = new TupleExp(e.loc, exps);
        ex = ex.expressionSemantic(sc);
        return ex;
    }
    if (e.ident == Id.compiles)
    {
        /* Determine if all the objects - types, expressions, or symbols -
         * compile without error
         */
        if (!dim)
            return False();

        foreach (o; *e.args)
        {
            uint errors = global.startGagging();
            Scope* sc2 = sc.push();
            sc2.tinst = null;
            sc2.minst = null;   // this is why code for these are not emitted to object file
            sc2.flags = (sc.flags & ~(SCOPE.ctfe | SCOPE.condition)) | SCOPE.compile | SCOPE.fullinst;

            bool err = false;

            auto t = isType(o);
            auto ex = isExpression(o);
            if (t)
            {
                Dsymbol s;
                t.resolve(e.loc, sc2, ex, t, s);
                if (t)
                {
                    t.typeSemantic(e.loc, sc2);
                    if (t.ty == Terror)
                        err = true;
                }
                else if (s && s.errors)
                    err = true;
            }
            if (ex)
            {
                ex = ex.expressionSemantic(sc2);
                ex = resolvePropertiesOnly(sc2, ex);
                ex = ex.optimize(WANTvalue);
                if (sc2.func && sc2.func.type.isTypeFunction())
                {
                    const tf = sc2.func.type.isTypeFunction();
                    err |= tf.isnothrow && canThrow(ex, sc2.func, null);
                }
                ex = checkGC(sc2, ex);
                if (ex.op == EXP.error)
                    err = true;
            }

            // Carefully detach the scope from the parent and throw it away as
            // we only need it to evaluate the expression
            // https://issues.dlang.org/show_bug.cgi?id=15428
            sc2.detach();

            if (global.endGagging(errors) || err)
            {
                return False();
            }
        }
        return True();
    }
    if (e.ident == Id.isSame)
    {
        /* Determine if two symbols are the same
         */
        if (dim != 2)
            return dimError(2);

        // https://issues.dlang.org/show_bug.cgi?id=20761
        // tiarg semantic may expand in place the list of arguments, for example:
        //
        //     before tiarg sema:  __traits(isSame, seq!(0,0), seq!(1,1))
        //     after            :  __traits(isSame, 0, 0, 1, 1)
        //
        // so we split in two lists
        Objects ob1;
        ob1.push((*e.args)[0]);
        Objects ob2;
        ob2.push((*e.args)[1]);
        if (!TemplateInstance.semanticTiargs(e.loc, sc, &ob1, 0))
            return ErrorExp.get();
        if (!TemplateInstance.semanticTiargs(e.loc, sc, &ob2, 0))
            return ErrorExp.get();
        if (ob1.length != ob2.length)
            return False();
        foreach (immutable i; 0 .. ob1.length)
            if (!ob1[i].isSame(ob2[i], sc))
                return False();
        return True();
    }
    if (e.ident == Id.getUnitTests)
    {
        if (dim != 1)
            return dimError(1);

        auto o = (*e.args)[0];
        auto s = getDsymbolWithoutExpCtx(o);
        if (!s)
        {
            error(e.loc, "argument `%s` to __traits(getUnitTests) must be a module or aggregate",
                o.toChars());
            return ErrorExp.get();
        }
        if (auto imp = s.isImport()) // https://issues.dlang.org/show_bug.cgi?id=10990
            s = imp.mod;

        auto sds = s.isScopeDsymbol();
        if (!sds || sds.isTemplateDeclaration())
        {
            error(e.loc, "argument `%s` to __traits(getUnitTests) must be a module or aggregate, not a %s",
                s.toChars(), s.kind());
            return ErrorExp.get();
        }

        auto exps = new Expressions();
        if (global.params.useUnitTests)
        {
            bool[void*] uniqueUnitTests;

            void symbolDg(Dsymbol s)
            {
                if (auto ad = s.isAttribDeclaration())
                {
                    ad.include(null).foreachDsymbol(&symbolDg);
                }
                else if (auto tm = s.isTemplateMixin())
                {
                    tm.members.foreachDsymbol(&symbolDg);
                }
                else if (auto ud = s.isUnitTestDeclaration())
                {
                    if (cast(void*)ud in uniqueUnitTests)
                        return;

                    uniqueUnitTests[cast(void*)ud] = true;

                    auto ad = new FuncAliasDeclaration(ud.ident, ud, false);
                    ad.visibility = ud.visibility;

                    auto e = new DsymbolExp(Loc.initial, ad, false);
                    exps.push(e);
                }
            }

            sds.members.foreachDsymbol(&symbolDg);
        }
        auto te = new TupleExp(e.loc, exps);
        return te.expressionSemantic(sc);
    }
    if (e.ident == Id.getVirtualIndex)
    {
        if (dim != 1)
            return dimError(1);

        auto o = (*e.args)[0];
        auto s = getDsymbolWithoutExpCtx(o);

        auto fd = s ? s.isFuncDeclaration() : null;
        if (!fd)
        {
            error(e.loc, "first argument to __traits(getVirtualIndex) must be a function");
            return ErrorExp.get();
        }

        fd = fd.toAliasFunc(); // Necessary to support multiple overloads.
        return new IntegerExp(e.loc, fd.vtblIndex, Type.tptrdiff_t);
    }
    if (e.ident == Id.getPointerBitmap)
    {
        return pointerBitmap(e, global.errorSink);
    }
    if (e.ident == Id.initSymbol)
    {
        if (dim != 1)
            return dimError(1);

        auto o = (*e.args)[0];

        ErrorExp badArgument()
        {
            error(e.loc, "struct / class type expected as argument to __traits(initSymbol) instead of `%s`", o.toChars());
            return ErrorExp.get();
        }

        Type t = isType(o);

        if (!t || t.isTypeEnum())
            return badArgument();

        AggregateDeclaration ad = isAggregate(t);

        // Interfaces don't have an init symbol and hence cause linker errors
        if (!ad || ad.isInterfaceDeclaration())
            return badArgument();

        Declaration d = new SymbolDeclaration(ad.loc, ad);
        d.type = Type.tvoid.arrayOf().constOf();
        d.storage_class |= STC.rvalue;
        return new VarExp(e.loc, d);
    }
    if (e.ident == Id.isZeroInit)
    {
        if (dim != 1)
            return dimError(1);

        auto o = (*e.args)[0];
        Type t = isType(o);
        if (!t)
        {
            error(e.loc, "type expected as second argument of __traits `%s` instead of `%s`",
                e.ident.toChars(), o.toChars());
            return ErrorExp.get();
        }

        // https://issues.dlang.org/show_bug.cgi?id=23534
        //
        // For enums, we need to get the enum initializer
        // (the first enum member), not the initializer of the
        // type of the enum members.
        Type tb = t.isTypeEnum ? t : t.baseElemOf();
        return tb.isZeroInit(e.loc) ? True() : False();
    }
    if (e.ident == Id.getTargetInfo)
    {
        if (dim != 1)
            return dimError(1);

        auto ex = isExpression((*e.args)[0]);
        StringExp se = ex ? ex.ctfeInterpret().toStringExp() : null;
        if (!ex || !se || se.len == 0)
        {
            error(e.loc, "string expected as argument of __traits `%s` instead of `%s`", e.ident.toChars(), (*e.args)[0].toChars());
            return ErrorExp.get();
        }
        se = se.toUTF8(sc);

        const slice = se.peekString();
        Expression r = target.getTargetInfo(slice.ptr, e.loc); // BUG: reliance on terminating 0
        if (!r)
        {
            error(e.loc, "`getTargetInfo` key `\"%.*s\"` not supported by this implementation",
                cast(int)slice.length, slice.ptr);
            return ErrorExp.get();
        }
        return r.expressionSemantic(sc);
    }
    if (e.ident == Id.getLocation)
    {
        if (dim != 1)
            return dimError(1);
        auto arg0 = (*e.args)[0];
        Dsymbol s = getDsymbolWithoutExpCtx(arg0);
        if (!s || !s.loc.isValid())
        {
            error(e.loc, "can only get the location of a symbol, not `%s`", arg0.toChars());
            return ErrorExp.get();
        }

        const fd = s.isFuncDeclaration();
        // FIXME:td.overnext is always set, even when using an index on it
        //const td = s.isTemplateDeclaration();
        if ((fd && fd.overnext) /*|| (td && td.overnext)*/)
        {
            error(e.loc, "cannot get location of an overload set, " ~
                    "use `__traits(getOverloads, ..., \"%s\"%s)[N]` " ~
                    "to get the Nth overload",
                    arg0.toChars(), /*td ? ", true".ptr :*/ "".ptr);
            return ErrorExp.get();
        }

        auto exps = new Expressions(3);
        (*exps)[0] = new StringExp(e.loc, s.loc.filename.toDString());
        (*exps)[1] = new IntegerExp(e.loc, s.loc.linnum,Type.tint32);
        (*exps)[2] = new IntegerExp(e.loc, s.loc.charnum,Type.tint32);
        auto tup = new TupleExp(e.loc, exps);
        return tup.expressionSemantic(sc);
    }
    if (e.ident == Id.getCppNamespaces)
    {
        auto o = (*e.args)[0];
        auto s = getDsymbolWithoutExpCtx(o);
        auto exps = new Expressions(0);
        if (auto d = s.isDeclaration())
        {
            if (d.inuse)
            {
                .error(d.loc, "%s `%s` circular reference in `__traits(GetCppNamespaces,...)`", d.kind, d.toPrettyChars);
                return ErrorExp.get();
            }
            d.inuse = 1;
        }

        /**
         Prepend the namespaces in the linked list `ns` to `es`.

         Returns: true if `ns` contains an `ErrorExp`.
         */
        bool prependNamespaces(Expressions* es, CPPNamespaceDeclaration ns)
        {
            // Semantic processing will convert `extern(C++, "a", "b", "c")`
            // into `extern(C++, "a") extern(C++, "b") extern(C++, "c")`,
            // creating a linked list what `a`'s `cppnamespace` points to `b`,
            // and `b`'s points to `c`. Our entry point is `a`.
            for (; ns !is null; ns = ns.cppnamespace)
            {
                ns.dsymbolSemantic(sc);

                if (ns.exp.isErrorExp())
                    return true;

                auto se = ns.exp.toStringExp();
                // extern(C++, (emptyTuple))
                // struct D {}
                // will produce a blank ident
                if (!se.len)
                    continue;
                es.insert(0, se);
            }
            return false;
        }
        for (auto p = s; !p.isModule(); p = p.toParent())
        {
            p.dsymbolSemantic(sc);
            auto pp = p.toParent();
            if (pp.isTemplateInstance())
            {
                if (!p.cppnamespace)
                    continue;
                //if (!p.toParent().cppnamespace)
                //    continue;
                auto inner = new Expressions(0);
                auto outer = new Expressions(0);
                if (prependNamespaces(inner,  p.cppnamespace)) return ErrorExp.get();
                if (prependNamespaces(outer, pp.cppnamespace)) return ErrorExp.get();

                size_t i = 0;
                while(i < outer.length && ((*inner)[i]) == (*outer)[i])
                    i++;

                foreach_reverse (ns; (*inner)[][i .. $])
                    exps.insert(0, ns);
                continue;
            }

            if (p.isNspace())
                exps.insert(0, new StringExp(p.loc, p.ident.toString()));

            if (prependNamespaces(exps, p.cppnamespace))
                return ErrorExp.get();
        }
        if (auto d = s.isDeclaration())
            d.inuse = 0;
        auto tup = new TupleExp(e.loc, exps);
        return tup.expressionSemantic(sc);
    }
    //https://issues.dlang.org/show_bug.cgi?id=22291
    if (e.ident == Id.parameters)
    {
        //No args are valid
        if (e.args)
        {
            char[] contents = cast(char[]) e.args.toString();
            contents = contents[1..$];
            contents[$-1] = '\0';
            error(e.loc, "`__traits(parameters)` cannot have arguments, but `%s` was supplied", contents.ptr);
            return ErrorExp.get();
        }

        auto fd = sc.getEnclosingFunction();
        if (!fd)
        {
            error(e.loc, "`__traits(parameters)` may only be used inside a function");
            return ErrorExp.get();
        }

        auto tf = fd.type.isTypeFunction();
        assert(tf);
        auto exps = new Expressions(0);
        int addParameterDG(size_t idx, Parameter x)
        {
            assert(x.ident);
            exps.push(new IdentifierExp(e.loc, x.ident));
            return 0;
        }
        /*
            This is required since not all "parameters" actually have a name
            until they (tuples) are expanded e.g. an anonymous tuple parameter's
            contents get given names but not the tuple itself.
        */
        Parameter._foreach(tf.parameterList.parameters, &addParameterDG);
        auto tup = new TupleExp(e.loc, exps);
        return tup.expressionSemantic(sc);
    }

    /* Can't find the identifier. Try a spell check for a better error message
     */
    traitNotFound(e);

    return ErrorExp.get();
}

/// compare arguments of __traits(isSame)
private bool isSame(RootObject o1, RootObject o2, Scope* sc)
{
    static FuncLiteralDeclaration isLambda(RootObject oarg)
    {
        if (auto t = isDsymbol(oarg))
        {
            if (auto td = t.isTemplateDeclaration())
            {
                if (td.members && td.members.length == 1)
                {
                    if (auto fd = (*td.members)[0].isFuncLiteralDeclaration())
                        return fd;
                }
            }
        }
        else if (auto ea = isExpression(oarg))
        {
            if (ea.op == EXP.function_)
            {
                if (auto fe = ea.isFuncExp())
                    return fe.fd;
            }
        }
        return null;
    }

    auto l1 = isLambda(o1);
    auto l2 = isLambda(o2);

    if (l1 && l2)
    {
        import dmd.lambdacomp : isSameFuncLiteral;
        if (isSameFuncLiteral(l1, l2, sc))
            return true;
    }

    // https://issues.dlang.org/show_bug.cgi?id=12001, allow isSame, <BasicType>, <BasicType>
    Type t1 = isType(o1);
    Type t2 = isType(o2);
    if (t1 && t2 && t1.equals(t2))
        return true;

    auto s1 = getDsymbol(o1);
    auto s2 = getDsymbol(o2);
    //printf("isSame: %s, %s\n", o1.toChars(), o2.toChars());
    version (none)
    {
        printf("o1: %p\n", o1);
        printf("o2: %p\n", o2);
        if (!s1)
        {
            if (auto ea = isExpression(o1))
                printf("%s\n", ea.toChars());
            if (auto ta = isType(o1))
                printf("%s\n", ta.toChars());
            return false;
        }
        else
            printf("%s %s\n", s1.kind(), s1.toChars());
    }
    if (!s1 && !s2)
    {
        auto ea1 = isExpression(o1);
        auto ea2 = isExpression(o2);
        if (ea1 && ea2)
        {
            if (ea1.equals(ea2))
                return true;
        }
    }
    if (!s1 || !s2)
        return false;

    s1 = s1.toAlias();
    s2 = s2.toAlias();

    if (auto fa1 = s1.isFuncAliasDeclaration())
        s1 = fa1.toAliasFunc();
    if (auto fa2 = s2.isFuncAliasDeclaration())
        s2 = fa2.toAliasFunc();

    // https://issues.dlang.org/show_bug.cgi?id=11259
    // compare import symbol to a package symbol
    static bool cmp(Dsymbol s1, Dsymbol s2)
    {
        auto imp = s1.isImport();
        return imp && imp.pkg && imp.pkg == s2.isPackage();
    }

    if (cmp(s1,s2) || cmp(s2,s1))
        return true;

    if (s1 == s2)
        return true;

    // https://issues.dlang.org/show_bug.cgi?id=18771
    // OverloadSets are equal if they contain the same functions
    auto overSet1 = s1.isOverloadSet();
    if (!overSet1)
        return false;

    auto overSet2 = s2.isOverloadSet();
    if (!overSet2)
        return false;

    if (overSet1.a.length != overSet2.a.length)
        return false;

    // OverloadSets contain array of Dsymbols => O(n*n)
    // to compare for equality as the order of overloads
    // might not be the same
Lnext:
    foreach(overload1; overSet1.a)
    {
        foreach(overload2; overSet2.a)
        {
            if (overload1 == overload2)
                continue Lnext;
        }
        return false;
    }
    return true;
}


/***********************************
 * A trait was not found. Give a decent error message
 * by trying a spell check.
 * Params:
 *      e = the offending trait
 */
private void traitNotFound(TraitsExp e)
{
    __gshared const StringTable!bool traitsStringTable;
    __gshared bool initialized;

    if (!initialized)
    {
        initialized = true;     // lazy initialization

        // All possible traits
        __gshared Identifier*[59] idents =
        [
            &Id.allMembers,
            &Id.child,
            &Id.classInstanceAlignment,
            &Id.classInstanceSize,
            &Id.compiles,
            &Id.derivedMembers,
            &Id.fullyQualifiedName,
            &Id.getAliasThis,
            &Id.getAttributes,
            &Id.getFunctionAttributes,
            &Id.getFunctionVariadicStyle,
            &Id.getLinkage,
            &Id.getLocation,
            &Id.getMember,
            &Id.getOverloads,
            &Id.getParameterStorageClasses,
            &Id.getPointerBitmap,
            &Id.getProtection,
            &Id.getTargetInfo,
            &Id.getUnitTests,
            &Id.getVirtualFunctions,
            &Id.getVirtualIndex,
            &Id.getVirtualMethods,
            &Id.getVisibility,
            &Id.hasCopyConstructor,
            &Id.hasMember,
            &Id.hasPostblit,
            &Id.identifier,
            &Id.isAbstractClass,
            &Id.isAbstractFunction,
            &Id.isArithmetic,
            &Id.isAssociativeArray,
            &Id.isCopyable,
            &Id.isDeprecated,
            &Id.isDisabled,
            &Id.isFinalClass,
            &Id.isFinalFunction,
            &Id.isFloating,
            &Id.isFuture,
            &Id.isIntegral,
            &Id.isLazy,
            &Id.isModule,
            &Id.isNested,
            &Id.isOut,
            &Id.isOverrideFunction,
            &Id.isPackage,
            &Id.isPOD,
            &Id.isRef,
            &Id.isReturnOnStack,
            &Id.isSame,
            &Id.isScalar,
            &Id.isStaticArray,
            &Id.isStaticFunction,
            &Id.isUnsigned,
            &Id.isVirtualFunction,
            &Id.isVirtualMethod,
            &Id.isZeroInit,
            &Id.parameters,
            &Id.parent,
        ];

        StringTable!(bool)* stringTable = cast(StringTable!(bool)*) &traitsStringTable;
        stringTable._init(idents.length);

        foreach (id; idents)
        {
            auto sv = stringTable.insert((*id).toString(), true);
            assert(sv);
        }
    }

    static const(char)[] trait_search_fp(const(char)[] seed, out int cost)
    {
        //printf("trait_search_fp('%s')\n", seed);
        if (!seed.length)
            return null;
        cost = 0;       // all the same cost
        const sv = traitsStringTable.lookup(seed);
        return sv ? sv.toString() : null;
    }

    if (auto sub = speller!trait_search_fp(e.ident.toString()))
        error(e.loc, "unrecognized trait `%s`, did you mean `%.*s`?", e.ident.toChars(), cast(int) sub.length, sub.ptr);
    else
        error(e.loc, "unrecognized trait `%s`", e.ident.toChars());
}
