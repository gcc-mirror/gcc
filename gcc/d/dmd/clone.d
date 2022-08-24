/**
 * Builds struct member functions if needed and not defined by the user.
 * Includes `opEquals`, `opAssign`, post blit, copy constructor and destructor.
 *
 * Copyright:   Copyright (C) 1999-2022 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/clone.d, _clone.d)
 * Documentation:  https://dlang.org/phobos/dmd_clone.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/clone.d
 */

module dmd.clone;

import core.stdc.stdio;
import dmd.aggregate;
import dmd.arraytypes;
import dmd.astenums;
import dmd.dclass;
import dmd.declaration;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.mtype;
import dmd.opover;
import dmd.semantic2;
import dmd.semantic3;
import dmd.statement;
import dmd.target;
import dmd.typesem;
import dmd.tokens;

/*******************************************
 * Merge function attributes pure, nothrow, @safe, @nogc, and @disable
 * from f into s1.
 * Params:
 *      s1 = storage class to merge into
 *      f = function
 * Returns:
 *      merged storage class
 */
StorageClass mergeFuncAttrs(StorageClass s1, const FuncDeclaration f) pure
{
    if (!f)
        return s1;
    StorageClass s2 = (f.storage_class & STC.disable);

    TypeFunction tf = cast(TypeFunction)f.type;
    if (tf.trust == TRUST.safe)
        s2 |= STC.safe;
    else if (tf.trust == TRUST.system)
        s2 |= STC.system;
    else if (tf.trust == TRUST.trusted)
        s2 |= STC.trusted;

    if (tf.purity != PURE.impure)
        s2 |= STC.pure_;
    if (tf.isnothrow)
        s2 |= STC.nothrow_;
    if (tf.isnogc)
        s2 |= STC.nogc;

    const sa = s1 & s2;
    const so = s1 | s2;

    StorageClass stc = (sa & (STC.pure_ | STC.nothrow_ | STC.nogc)) | (so & STC.disable);

    if (so & STC.system)
        stc |= STC.system;
    else if (sa & STC.trusted)
        stc |= STC.trusted;
    else if ((so & (STC.trusted | STC.safe)) == (STC.trusted | STC.safe))
        stc |= STC.trusted;
    else if (sa & STC.safe)
        stc |= STC.safe;

    return stc;
}

/*******************************************
 * Check given aggregate actually has an identity opAssign or not.
 * Params:
 *      ad = struct or class
 *      sc = current scope
 * Returns:
 *      if found, returns FuncDeclaration of opAssign, otherwise null
 */
FuncDeclaration hasIdentityOpAssign(AggregateDeclaration ad, Scope* sc)
{
    Dsymbol assign = search_function(ad, Id.assign);
    if (assign)
    {
        /* check identity opAssign exists
         */
        scope er = new NullExp(ad.loc, ad.type);    // dummy rvalue
        scope el = new IdentifierExp(ad.loc, Id.p); // dummy lvalue
        el.type = ad.type;
        Expressions a;
        a.setDim(1);
        const errors = global.startGagging(); // Do not report errors, even if the template opAssign fbody makes it.
        sc = sc.push();
        sc.tinst = null;
        sc.minst = null;

        a[0] = er;
        auto f = resolveFuncCall(ad.loc, sc, assign, null, ad.type, &a, FuncResolveFlag.quiet);
        if (!f)
        {
            a[0] = el;
            f = resolveFuncCall(ad.loc, sc, assign, null, ad.type, &a, FuncResolveFlag.quiet);
        }

        sc = sc.pop();
        global.endGagging(errors);
        if (f)
        {
            if (f.errors)
                return null;
            auto fparams = f.getParameterList();
            if (fparams.length)
            {
                auto fparam0 = fparams[0];
                if (fparam0.type.toDsymbol(null) != ad)
                    f = null;
            }
        }
        // BUGS: This detection mechanism cannot find some opAssign-s like follows:
        // struct S { void opAssign(ref immutable S) const; }
        return f;
    }
    return null;
}

/*******************************************
 * We need an opAssign for the struct if
 * it has a destructor or a postblit.
 * We need to generate one if a user-specified one does not exist.
 */
private bool needOpAssign(StructDeclaration sd)
{
    //printf("StructDeclaration::needOpAssign() %s\n", sd.toChars());

    static bool isNeeded()
    {
        //printf("\tneed\n");
        return true;
    }

    if (sd.isUnionDeclaration())
        return !isNeeded();

    if (sd.hasIdentityAssign || // because has identity==elaborate opAssign
        sd.dtor ||
        sd.postblit)
        return isNeeded();

    /* If any of the fields need an opAssign, then we
     * need it too.
     */
    foreach (v; sd.fields)
    {
        if (v.storage_class & STC.ref_)
            continue;
        if (v.overlapped)               // if field of a union
            continue;                   // user must handle it themselves
        Type tv = v.type.baseElemOf();
        if (tv.ty == Tstruct)
        {
            TypeStruct ts = cast(TypeStruct)tv;
            if (ts.sym.isUnionDeclaration())
                continue;
            if (needOpAssign(ts.sym))
                return isNeeded();
        }
    }
    return !isNeeded();
}

/******************************************
 * Build opAssign for a `struct`.
 *
 * The generated `opAssign` function has the following signature:
 *---
 *ref S opAssign(S s)    // S is the name of the `struct`
 *---
 *
 * The opAssign function will be built for a struct `S` if the
 * following constraints are met:
 *
 * 1. `S` does not have an identity `opAssign` defined.
 *
 * 2. `S` has at least one of the following members: a postblit (user-defined or
 * generated for fields that have a defined postblit), a destructor
 * (user-defined or generated for fields that have a defined destructor)
 * or at least one field that has a defined `opAssign`.
 *
 * 3. `S` does not have any non-mutable fields.
 *
 * If `S` has a disabled destructor or at least one field that has a disabled
 * `opAssign`, `S.opAssign` is going to be generated, but marked with `@disable`
 *
 * If `S` defines a destructor, the generated code for `opAssign` is:
 *
 *---
 *S __swap = void;
 *__swap = this;   // bit copy
 *this = s;        // bit copy
 *__swap.dtor();
 *---
 *
 * Otherwise, if `S` defines a postblit, the generated code for `opAssign` is:
 *
 *---
 *this = s;
 *---
 *
 * Note that the parameter to the generated `opAssign` is passed by value, which means
 * that the postblit is going to be called (if it is defined) in both  of the above
 * situations before entering the body of `opAssign`. The assignments in the above generated
 * function bodies are blit expressions, so they can be regarded as `memcpy`s
 * (`opAssign` is not called as this will result in an infinite recursion; the postblit
 * is not called because it has already been called when the parameter was passed by value).
 *
 * If `S` does not have a postblit or a destructor, but contains at least one field that defines
 * an `opAssign` function (which is not disabled), then the body will make member-wise
 * assignments:
 *
 *---
 *this.field1 = s.field1;
 *this.field2 = s.field2;
 *...;
 *---
 *
 * In this situation, the assignemnts are actual assign expressions (`opAssign` is used
 * if defined).
 *
 * References:
 *      https://dlang.org/spec/struct.html#assign-overload
 * Params:
 *      sd = struct to generate opAssign for
 *      sc = context
 * Returns:
 *      generated `opAssign` function
 */
FuncDeclaration buildOpAssign(StructDeclaration sd, Scope* sc)
{
    if (FuncDeclaration f = hasIdentityOpAssign(sd, sc))
    {
        sd.hasIdentityAssign = true;
        return f;
    }
    // Even if non-identity opAssign is defined, built-in identity opAssign
    // will be defined.
    if (!needOpAssign(sd))
        return null;

    //printf("StructDeclaration::buildOpAssign() %s\n", sd.toChars());
    StorageClass stc = STC.safe | STC.nothrow_ | STC.pure_ | STC.nogc;
    Loc declLoc = sd.loc;
    Loc loc; // internal code should have no loc to prevent coverage

    // One of our sub-field might have `@disable opAssign` so we need to
    // check for it.
    // In this event, it will be reflected by having `stc` (opAssign's
    // storage class) include `STC.disabled`.
    foreach (v; sd.fields)
    {
        if (v.storage_class & STC.ref_)
            continue;
        if (v.overlapped)
            continue;
        Type tv = v.type.baseElemOf();
        if (tv.ty != Tstruct)
            continue;
        StructDeclaration sdv = (cast(TypeStruct)tv).sym;
        stc = mergeFuncAttrs(stc, hasIdentityOpAssign(sdv, sc));
    }

    if (sd.dtor || sd.postblit)
    {
        // if the type is not assignable, we cannot generate opAssign
        if (!sd.type.isAssignable()) // https://issues.dlang.org/show_bug.cgi?id=13044
            return null;
        stc = mergeFuncAttrs(stc, sd.dtor);
        if (stc & STC.safe)
            stc = (stc & ~STC.safe) | STC.trusted;
    }

    auto fparams = new Parameters();
    fparams.push(new Parameter(STC.nodtor, sd.type, Id.p, null, null));
    auto tf = new TypeFunction(ParameterList(fparams), sd.handleType(), LINK.d, stc | STC.ref_);
    auto fop = new FuncDeclaration(declLoc, Loc.initial, Id.assign, stc, tf);
    fop.storage_class |= STC.inference;
    fop.flags  |= FUNCFLAG.generated;
    Expression e;
    if (stc & STC.disable)
    {
        e = null;
    }
    /* Do swap this and rhs.
     *    __swap = this; this = s; __swap.dtor();
     */
    else if (sd.dtor)
    {
        //printf("\tswap copy\n");
        TypeFunction tdtor = cast(TypeFunction)sd.dtor.type;
        assert(tdtor.ty == Tfunction);

        auto idswap = Identifier.generateId("__swap");
        auto swap = new VarDeclaration(loc, sd.type, idswap, new VoidInitializer(loc));
        swap.storage_class |= STC.nodtor | STC.temp | STC.ctfe;
        if (tdtor.isScopeQual)
            swap.storage_class |= STC.scope_;
        auto e1 = new DeclarationExp(loc, swap);

        auto e2 = new BlitExp(loc, new VarExp(loc, swap), new ThisExp(loc));
        auto e3 = new BlitExp(loc, new ThisExp(loc), new IdentifierExp(loc, Id.p));

        /* Instead of running the destructor on s, run it
         * on swap. This avoids needing to copy swap back in to s.
         */
        auto e4 = new CallExp(loc, new DotVarExp(loc, new VarExp(loc, swap), sd.dtor, false));

        e = Expression.combine(e1, e2, e3, e4);
    }
    /* postblit was called when the value was passed to opAssign, we just need to blit the result */
    else if (sd.postblit)
    {
        e = new BlitExp(loc, new ThisExp(loc), new IdentifierExp(loc, Id.p));
        sd.hasBlitAssign = true;
    }
    else
    {
        /* Do memberwise copy.
         *
         * If sd is a nested struct, its vthis field assignment is:
         * 1. If it's nested in a class, it's a rebind of class reference.
         * 2. If it's nested in a function or struct, it's an update of void*.
         * In both cases, it will change the parent context.
         */
        //printf("\tmemberwise copy\n");
        e = null;
        foreach (v; sd.fields)
        {
            // this.v = s.v;
            auto ec = new AssignExp(loc,
                new DotVarExp(loc, new ThisExp(loc), v),
                new DotVarExp(loc, new IdentifierExp(loc, Id.p), v));
            e = Expression.combine(e, ec);
        }
    }
    if (e)
    {
        Statement s1 = new ExpStatement(loc, e);
        /* Add:
         *   return this;
         */
        auto er = new ThisExp(loc);
        Statement s2 = new ReturnStatement(loc, er);
        fop.fbody = new CompoundStatement(loc, s1, s2);
        tf.isreturn = true;
    }
    sd.members.push(fop);
    fop.addMember(sc, sd);
    sd.hasIdentityAssign = true; // temporary mark identity assignable
    const errors = global.startGagging(); // Do not report errors, even if the template opAssign fbody makes it.
    Scope* sc2 = sc.push();
    sc2.stc = 0;
    sc2.linkage = LINK.d;
    fop.dsymbolSemantic(sc2);
    fop.semantic2(sc2);
    // https://issues.dlang.org/show_bug.cgi?id=15044
    //semantic3(fop, sc2); // isn't run here for lazy forward reference resolution.

    sc2.pop();
    if (global.endGagging(errors)) // if errors happened
    {
        // Disable generated opAssign, because some members forbid identity assignment.
        fop.storage_class |= STC.disable;
        fop.fbody = null; // remove fbody which contains the error
    }

    //printf("-StructDeclaration::buildOpAssign() %s, errors = %d\n", sd.toChars(), (fop.storage_class & STC.disable) != 0);
    //printf("fop.type: %s\n", fop.type.toPrettyChars());
    return fop;
}

/*******************************************
 * We need an opEquals for the struct if
 * any fields has an opEquals.
 * Generate one if a user-specified one does not exist.
 */
bool needOpEquals(StructDeclaration sd)
{
    //printf("StructDeclaration::needOpEquals() %s\n", sd.toChars());
    if (sd.isUnionDeclaration())
    {
        /* If a union has only one field, treat it like a struct
         */
        if (sd.fields.length != 1)
            goto Ldontneed;
    }
    if (sd.hasIdentityEquals)
        goto Lneed;
    /* If any of the fields has an opEquals, then we
     * need it too.
     */
    foreach (VarDeclaration v; sd.fields)
    {
        if (v.storage_class & STC.ref_)
            continue;
        if (v.overlapped)
            continue;
        Type tv = v.type.toBasetype();
        auto tvbase = tv.baseElemOf();
        if (tvbase.ty == Tstruct)
        {
            TypeStruct ts = cast(TypeStruct)tvbase;
            if (ts.sym.isUnionDeclaration() && ts.sym.fields.length != 1)
                continue;
            if (needOpEquals(ts.sym))
                goto Lneed;
        }
        if (tvbase.isfloating())
        {
            // This is necessray for:
            //  1. comparison of +0.0 and -0.0 should be true.
            //  2. comparison of NANs should be false always.
            goto Lneed;
        }
        if (tvbase.ty == Tarray)
            goto Lneed;
        if (tvbase.ty == Taarray)
            goto Lneed;
        if (tvbase.ty == Tclass)
            goto Lneed;
    }
Ldontneed:
    //printf("\tdontneed\n");
    return false;
Lneed:
    //printf("\tneed\n");
    return true;
}

/*******************************************
 * Check given aggregate actually has an identity opEquals or not.
 */
private FuncDeclaration hasIdentityOpEquals(AggregateDeclaration ad, Scope* sc)
{
    FuncDeclaration f;
    if (Dsymbol eq = search_function(ad, Id.eq))
    {
        /* check identity opEquals exists
         */
        scope er = new NullExp(ad.loc, null); // dummy rvalue
        scope el = new IdentifierExp(ad.loc, Id.p); // dummy lvalue
        Expressions a;
        a.setDim(1);

        bool hasIt(Type tthis)
        {
            const errors = global.startGagging(); // Do not report errors, even if the template opAssign fbody makes it
            sc = sc.push();
            sc.tinst = null;
            sc.minst = null;

            FuncDeclaration rfc(Expression e)
            {
                a[0] = e;
                a[0].type = tthis;
                return resolveFuncCall(ad.loc, sc, eq, null, tthis, &a, FuncResolveFlag.quiet);
            }

            f = rfc(er);
            if (!f)
                f = rfc(el);

            sc = sc.pop();
            global.endGagging(errors);

            return f !is null;
        }

        if (hasIt(ad.type)               ||
            hasIt(ad.type.constOf())     ||
            hasIt(ad.type.immutableOf()) ||
            hasIt(ad.type.sharedOf())    ||
            hasIt(ad.type.sharedConstOf()))
        {
            if (f.errors)
                return null;
        }
    }
    return f;
}

/******************************************
 * Build opEquals for struct.
 *      const bool opEquals(const S s) { ... }
 *
 * By fixing https://issues.dlang.org/show_bug.cgi?id=3789
 * opEquals is changed to be never implicitly generated.
 * Now, struct objects comparison s1 == s2 is translated to:
 *      s1.tupleof == s2.tupleof
 * to calculate structural equality. See EqualExp.op_overload.
 */
FuncDeclaration buildOpEquals(StructDeclaration sd, Scope* sc)
{
    if (hasIdentityOpEquals(sd, sc))
    {
        sd.hasIdentityEquals = true;
    }
    return null;
}

/******************************************
 * Build __xopEquals for TypeInfo_Struct
 *      bool __xopEquals(ref const S p) const
 *      {
 *          return this == p;
 *      }
 *
 * This is called by TypeInfo.equals(p1, p2). If the struct does not support
 * const objects comparison, it will throw "not implemented" Error in runtime.
 */
FuncDeclaration buildXopEquals(StructDeclaration sd, Scope* sc)
{
    if (!needOpEquals(sd))
        return null; // bitwise comparison would work

    //printf("StructDeclaration::buildXopEquals() %s\n", sd.toChars());
    if (Dsymbol eq = search_function(sd, Id.eq))
    {
        if (FuncDeclaration fd = eq.isFuncDeclaration())
        {
            TypeFunction tfeqptr;
            {
                Scope scx;
                /* const bool opEquals(ref const S s);
                 */
                auto parameters = new Parameters();
                parameters.push(new Parameter(STC.ref_ | STC.const_, sd.type, null, null, null));
                tfeqptr = new TypeFunction(ParameterList(parameters), Type.tbool, LINK.d);
                tfeqptr.mod = MODFlags.const_;
                tfeqptr = cast(TypeFunction)tfeqptr.typeSemantic(Loc.initial, &scx);
            }
            fd = fd.overloadExactMatch(tfeqptr);
            if (fd)
                return fd;
        }
    }
    if (!sd.xerreq)
    {
        // object._xopEquals
        Identifier id = Identifier.idPool("_xopEquals");
        Expression e = new IdentifierExp(sd.loc, Id.empty);
        e = new DotIdExp(sd.loc, e, Id.object);
        e = new DotIdExp(sd.loc, e, id);
        e = e.expressionSemantic(sc);
        if (!e.isErrorExp())
        {
            Dsymbol s = getDsymbol(e);
            assert(s);
            sd.xerreq = s.isFuncDeclaration();
        }
    }
    Loc declLoc; // loc is unnecessary so __xopEquals is never called directly
    Loc loc; // loc is unnecessary so errors are gagged
    auto parameters = new Parameters();
    parameters.push(new Parameter(STC.ref_ | STC.const_, sd.type, Id.p, null, null));
    auto tf = new TypeFunction(ParameterList(parameters), Type.tbool, LINK.d, STC.const_);
    tf = tf.addSTC(STC.const_).toTypeFunction();
    Identifier id = Id.xopEquals;
    auto fop = new FuncDeclaration(declLoc, Loc.initial, id, 0, tf);
    fop.flags |= FUNCFLAG.generated;
    fop.parent = sd;
    Expression e1 = new IdentifierExp(loc, Id.This);
    Expression e2 = new IdentifierExp(loc, Id.p);
    Expression e = new EqualExp(EXP.equal, loc, e1, e2);
    fop.fbody = new ReturnStatement(loc, e);
    uint errors = global.startGagging(); // Do not report errors
    Scope* sc2 = sc.push();
    sc2.stc = 0;
    sc2.linkage = LINK.d;
    fop.dsymbolSemantic(sc2);
    fop.semantic2(sc2);
    sc2.pop();
    if (global.endGagging(errors)) // if errors happened
        fop = sd.xerreq;
    return fop;
}

/******************************************
 * Build __xopCmp for TypeInfo_Struct
 *      int __xopCmp(ref const S p) const
 *      {
 *          return this.opCmp(p);
 *      }
 *
 * This is called by TypeInfo.compare(p1, p2). If the struct does not support
 * const objects comparison, it will throw "not implemented" Error in runtime.
 */
FuncDeclaration buildXopCmp(StructDeclaration sd, Scope* sc)
{
    //printf("StructDeclaration::buildXopCmp() %s\n", toChars());
    if (Dsymbol cmp = search_function(sd, Id.cmp))
    {
        if (FuncDeclaration fd = cmp.isFuncDeclaration())
        {
            TypeFunction tfcmpptr;
            {
                Scope scx;
                /* const int opCmp(ref const S s);
                 */
                auto parameters = new Parameters();
                parameters.push(new Parameter(STC.ref_ | STC.const_, sd.type, null, null, null));
                tfcmpptr = new TypeFunction(ParameterList(parameters), Type.tint32, LINK.d);
                tfcmpptr.mod = MODFlags.const_;
                tfcmpptr = cast(TypeFunction)tfcmpptr.typeSemantic(Loc.initial, &scx);
            }
            fd = fd.overloadExactMatch(tfcmpptr);
            if (fd)
                return fd;
        }
    }
    else
    {
        version (none) // FIXME: doesn't work for recursive alias this
        {
            /* Check opCmp member exists.
             * Consider 'alias this', but except opDispatch.
             */
            Expression e = new DsymbolExp(sd.loc, sd);
            e = new DotIdExp(sd.loc, e, Id.cmp);
            Scope* sc2 = sc.push();
            e = e.trySemantic(sc2);
            sc2.pop();
            if (e)
            {
                Dsymbol s = null;
                switch (e.op)
                {
                case EXP.overloadSet:
                    s = e.isOverExp().vars;
                    break;
                case EXP.scope_:
                    s = e.isScopeExp().sds;
                    break;
                case EXP.variable:
                    s = e.isVarExp().var;
                    break;
                default:
                    break;
                }
                if (!s || s.ident != Id.cmp)
                    e = null; // there's no valid member 'opCmp'
            }
            if (!e)
                return null; // bitwise comparison would work
            /* Essentially, a struct which does not define opCmp is not comparable.
             * At this time, typeid(S).compare might be correct that throwing "not implement" Error.
             * But implementing it would break existing code, such as:
             *
             * struct S { int value; }  // no opCmp
             * int[S] aa;   // Currently AA key uses bitwise comparison
             *              // (It's default behavior of TypeInfo_Strust.compare).
             *
             * Not sure we should fix this inconsistency, so just keep current behavior.
             */
        }
        else
        {
            return null;
        }
    }
    if (!sd.xerrcmp)
    {
        // object._xopCmp
        Identifier id = Identifier.idPool("_xopCmp");
        Expression e = new IdentifierExp(sd.loc, Id.empty);
        e = new DotIdExp(sd.loc, e, Id.object);
        e = new DotIdExp(sd.loc, e, id);
        e = e.expressionSemantic(sc);
        if (!e.isErrorExp())
        {
            Dsymbol s = getDsymbol(e);
            assert(s);
            sd.xerrcmp = s.isFuncDeclaration();
        }
    }
    Loc declLoc; // loc is unnecessary so __xopCmp is never called directly
    Loc loc; // loc is unnecessary so errors are gagged
    auto parameters = new Parameters();
    parameters.push(new Parameter(STC.ref_ | STC.const_, sd.type, Id.p, null, null));
    auto tf = new TypeFunction(ParameterList(parameters), Type.tint32, LINK.d, STC.const_);
    tf = tf.addSTC(STC.const_).toTypeFunction();
    Identifier id = Id.xopCmp;
    auto fop = new FuncDeclaration(declLoc, Loc.initial, id, 0, tf);
    fop.flags |= FUNCFLAG.generated;
    fop.parent = sd;
    Expression e1 = new IdentifierExp(loc, Id.This);
    Expression e2 = new IdentifierExp(loc, Id.p);
    Expression e = new CallExp(loc, new DotIdExp(loc, e1, Id.cmp), e2);
    fop.fbody = new ReturnStatement(loc, e);
    uint errors = global.startGagging(); // Do not report errors
    Scope* sc2 = sc.push();
    sc2.stc = 0;
    sc2.linkage = LINK.d;
    fop.dsymbolSemantic(sc2);
    fop.semantic2(sc2);
    sc2.pop();
    if (global.endGagging(errors)) // if errors happened
        fop = sd.xerrcmp;
    return fop;
}

/*******************************************
 * We need a toHash for the struct if
 * any fields has a toHash.
 * Generate one if a user-specified one does not exist.
 */
private bool needToHash(StructDeclaration sd)
{
    //printf("StructDeclaration::needToHash() %s\n", sd.toChars());
    if (sd.isUnionDeclaration())
        goto Ldontneed;
    if (sd.xhash)
        goto Lneed;

    /* If any of the fields has an toHash, then we
     * need it too.
     */
    foreach (VarDeclaration v; sd.fields)
    {
        if (v.storage_class & STC.ref_)
            continue;
        if (v.overlapped)
            continue;
        Type tv = v.type.toBasetype();
        auto tvbase = tv.baseElemOf();
        if (tvbase.ty == Tstruct)
        {
            TypeStruct ts = cast(TypeStruct)tvbase;
            if (ts.sym.isUnionDeclaration())
                continue;
            if (needToHash(ts.sym))
                goto Lneed;
        }
        if (tvbase.isfloating())
        {
            /* This is necessary because comparison of +0.0 and -0.0 should be true,
             * i.e. not a bit compare.
             */
            goto Lneed;
        }
        if (tvbase.ty == Tarray)
            goto Lneed;
        if (tvbase.ty == Taarray)
            goto Lneed;
        if (tvbase.ty == Tclass)
            goto Lneed;
    }
Ldontneed:
    //printf("\tdontneed\n");
    return false;
Lneed:
    //printf("\tneed\n");
    return true;
}

/******************************************
 * Build __xtoHash for non-bitwise hashing
 *      static hash_t xtoHash(ref const S p) nothrow @trusted;
 */
FuncDeclaration buildXtoHash(StructDeclaration sd, Scope* sc)
{
    if (Dsymbol s = search_function(sd, Id.tohash))
    {
        __gshared TypeFunction tftohash;
        if (!tftohash)
        {
            tftohash = new TypeFunction(ParameterList(), Type.thash_t, LINK.d);
            tftohash.mod = MODFlags.const_;
            tftohash = cast(TypeFunction)tftohash.merge();
        }
        if (FuncDeclaration fd = s.isFuncDeclaration())
        {
            fd = fd.overloadExactMatch(tftohash);
            if (fd)
                return fd;
        }
    }
    if (!needToHash(sd))
        return null;

    /* The trouble is that the following code relies on .tupleof, but .tupleof
     * is not allowed for C files. If we allow it for C files, then that turns on
     * the other D properties, too, such as .dup which will then conflict with allowed
     * field names.
     * One way to fix it is to replace the following foreach and .tupleof with C
     * statements and expressions.
     * But, it's debatable whether C structs should even need toHash().
     * Note that it would only be necessary if it has floating point fields.
     * For now, we'll just not generate a toHash() for C files.
     */
    if (sc.flags & SCOPE.Cfile)
        return null;

    //printf("StructDeclaration::buildXtoHash() %s\n", sd.toPrettyChars());
    Loc declLoc; // loc is unnecessary so __xtoHash is never called directly
    Loc loc; // internal code should have no loc to prevent coverage
    auto parameters = new Parameters();
    parameters.push(new Parameter(STC.ref_ | STC.const_, sd.type, Id.p, null, null));
    auto tf = new TypeFunction(ParameterList(parameters), Type.thash_t, LINK.d, STC.nothrow_ | STC.trusted);
    Identifier id = Id.xtoHash;
    auto fop = new FuncDeclaration(declLoc, Loc.initial, id, STC.static_, tf);
    fop.flags |= FUNCFLAG.generated;

    /* Do memberwise hashing.
     *
     * If sd is a nested struct, and if it's nested in a class, the calculated
     * hash value will also contain the result of parent class's toHash().
     */
    const(char)[] code =
        ".object.size_t h = 0;" ~
        "foreach (i, T; typeof(p.tupleof))" ~
        // workaround https://issues.dlang.org/show_bug.cgi?id=17968
        "    static if(is(T* : const(.object.Object)*)) " ~
        "        h = h * 33 + typeid(const(.object.Object)).getHash(cast(const void*)&p.tupleof[i]);" ~
        "    else " ~
        "        h = h * 33 + typeid(T).getHash(cast(const void*)&p.tupleof[i]);" ~
        "return h;";
    fop.fbody = new CompileStatement(loc, new StringExp(loc, code));
    Scope* sc2 = sc.push();
    sc2.stc = 0;
    sc2.linkage = LINK.d;
    fop.dsymbolSemantic(sc2);
    fop.semantic2(sc2);
    sc2.pop();

    //printf("%s fop = %s %s\n", sd.toChars(), fop.toChars(), fop.type.toChars());
    return fop;
}

/*****************************************
 * Create aggregate destructor for struct/class by aggregating
 * all the destructors in userDtors[] with the destructors for
 * all the members.
 * Sets ad's fieldDtor, aggrDtor, dtor and tidtor fields.
 * Params:
 *      ad = struct or class to build destructor for
 *      sc = context
 * Note:
 * Close similarity with StructDeclaration::buildPostBlit(),
 * and the ordering changes (runs backward instead of forwards).
 */
void buildDtors(AggregateDeclaration ad, Scope* sc)
{
    //printf("AggregateDeclaration::buildDtor() %s\n", ad.toChars());
    if (ad.isUnionDeclaration())
        return;                    // unions don't have destructors

    StorageClass stc = STC.safe | STC.nothrow_ | STC.pure_ | STC.nogc;
    Loc declLoc = ad.userDtors.dim ? ad.userDtors[0].loc : ad.loc;
    Loc loc; // internal code should have no loc to prevent coverage
    FuncDeclaration xdtor_fwd = null;

    // Build the field destructor (`ad.fieldDtor`), if needed.
    // If the user dtor is an extern(C++) prototype, then we expect it performs a full-destruction and skip building.
    const bool dtorIsCppPrototype = ad.userDtors.dim && ad.userDtors[0]._linkage == LINK.cpp && !ad.userDtors[0].fbody;
    if (!dtorIsCppPrototype)
    {
        Expression e = null;
        for (size_t i = 0; i < ad.fields.dim; i++)
        {
            auto v = ad.fields[i];
            if (v.storage_class & STC.ref_)
                continue;
            if (v.overlapped)
                continue;
            auto tv = v.type.baseElemOf();
            if (tv.ty != Tstruct)
                continue;
            auto sdv = (cast(TypeStruct)tv).sym;
            if (!sdv.dtor)
                continue;

            // fix: https://issues.dlang.org/show_bug.cgi?id=17257
            // braces for shrink wrapping scope of a
            {
                xdtor_fwd = sdv.dtor; // this dtor is temporary it could be anything
                auto a = new AliasDeclaration(Loc.initial, Id.__xdtor, xdtor_fwd);
                a.addMember(sc, ad); // temporarily add to symbol table
            }

            sdv.dtor.functionSemantic();

            stc = mergeFuncAttrs(stc, sdv.dtor);
            if (stc & STC.disable)
            {
                e = null;
                break;
            }

            Expression ex;
            tv = v.type.toBasetype();
            if (tv.ty == Tstruct)
            {
                // this.v.__xdtor()

                ex = new ThisExp(loc);
                ex = new DotVarExp(loc, ex, v);

                // This is a hack so we can call destructors on const/immutable objects.
                // Do it as a type 'paint', `cast()`
                ex = new CastExp(loc, ex, MODFlags.none);
                if (stc & STC.safe)
                    stc = (stc & ~STC.safe) | STC.trusted;

                ex = new DotVarExp(loc, ex, sdv.dtor, false);
                ex = new CallExp(loc, ex);
            }
            else
            {
                // __ArrayDtor((cast(S*)this.v.ptr)[0 .. n])

                const n = tv.numberOfElems(loc);
                if (n == 0)
                    continue;

                ex = new ThisExp(loc);
                ex = new DotVarExp(loc, ex, v);

                // This is a hack so we can call destructors on const/immutable objects.
                ex = new DotIdExp(loc, ex, Id.ptr);
                ex = new CastExp(loc, ex, sdv.type.pointerTo());
                if (stc & STC.safe)
                    stc = (stc & ~STC.safe) | STC.trusted;

                SliceExp se = new SliceExp(loc, ex, new IntegerExp(loc, 0, Type.tsize_t),
                                           new IntegerExp(loc, n, Type.tsize_t));
                // Prevent redundant bounds check
                se.upperIsInBounds = true;
                se.lowerIsLessThanUpper = true;

                ex = new CallExp(loc, new IdentifierExp(loc, Id.__ArrayDtor), se);
            }
            e = Expression.combine(ex, e); // combine in reverse order
        }

        if (e || (stc & STC.disable))
        {
            //printf("Building __fieldDtor(), %s\n", e.toChars());
            auto dd = new DtorDeclaration(declLoc, Loc.initial, stc, Id.__fieldDtor);
            dd.flags |= FUNCFLAG.generated;
            dd.storage_class |= STC.inference;
            dd.fbody = new ExpStatement(loc, e);
            ad.members.push(dd);
            dd.dsymbolSemantic(sc);
            ad.fieldDtor = dd;
        }
    }

    // Generate list of dtors to call in that order
    DtorDeclarations dtors;
    foreach_reverse (userDtor; ad.userDtors[])
        dtors.push(userDtor);
    if (ad.fieldDtor)
        dtors.push(ad.fieldDtor);
    if (!dtorIsCppPrototype)
    {
        // extern(C++) destructors call into super to destruct the full hierarchy
        ClassDeclaration cldec = ad.isClassDeclaration();
        if (cldec && cldec.classKind == ClassKind.cpp && cldec.baseClass && cldec.baseClass.aggrDtor)
            dtors.push(cldec.baseClass.aggrDtor);
    }

    // Set/build `ad.aggrDtor`
    switch (dtors.dim)
    {
    case 0:
        break;

    case 1:
        // Use the single existing dtor directly as aggregate dtor.
        // Note that this might be `cldec.baseClass.aggrDtor`.
        ad.aggrDtor = dtors[0];
        break;

    default:
        // Build the aggregate destructor, calling all dtors in order.
        assert(!dtorIsCppPrototype);
        Expression e = null;
        e = null;
        stc = STC.safe | STC.nothrow_ | STC.pure_ | STC.nogc;
        foreach (FuncDeclaration fd; dtors)
        {
            stc = mergeFuncAttrs(stc, fd);
            if (stc & STC.disable)
            {
                e = null;
                break;
            }
            Expression ex = new ThisExp(loc);
            ex = new DotVarExp(loc, ex, fd, false);
            CallExp ce = new CallExp(loc, ex);
            ce.directcall = true;
            e = Expression.combine(e, ce);
        }
        auto dd = new DtorDeclaration(declLoc, Loc.initial, stc, Id.__aggrDtor);
        dd.flags |= FUNCFLAG.generated;
        dd.storage_class |= STC.inference;
        dd.fbody = new ExpStatement(loc, e);
        ad.members.push(dd);
        dd.dsymbolSemantic(sc);
        ad.aggrDtor = dd;
        break;
    }

    // Set/build `ad.dtor`.
    // On Windows, the dtor in the vtable is a shim with different signature.
    ad.dtor = (ad.aggrDtor && ad.aggrDtor._linkage == LINK.cpp && !target.cpp.twoDtorInVtable)
        ? buildWindowsCppDtor(ad, ad.aggrDtor, sc)
        : ad.aggrDtor;

    // Add an __xdtor alias to make `ad.dtor` accessible
    if (ad.dtor)
    {
        auto _alias = new AliasDeclaration(Loc.initial, Id.__xdtor, ad.dtor);
        _alias.dsymbolSemantic(sc);
        ad.members.push(_alias);
        if (xdtor_fwd)
            ad.symtab.update(_alias); // update forward dtor to correct one
        else
            _alias.addMember(sc, ad); // add to symbol table
    }

    // Set/build `ad.tidtor`
    ad.tidtor = buildExternDDtor(ad, sc);
}

/**
 * build a shim function around the compound dtor that accepts an argument
 *  that is used to implement the deleting C++ destructor
 *
 * Params:
 *  ad = the aggregate that contains the destructor to wrap
 *  dtor = the destructor to wrap
 *  sc = the scope in which to analyze the new function
 *
 * Returns:
 *  the shim destructor, semantically analyzed and added to the class as a member
 */
private DtorDeclaration buildWindowsCppDtor(AggregateDeclaration ad, DtorDeclaration dtor, Scope* sc)
{
    auto cldec = ad.isClassDeclaration();
    if (!cldec || cldec.cppDtorVtblIndex == -1) // scalar deleting dtor not built for non-virtual dtors
        return dtor;

    // generate deleting C++ destructor corresponding to:
    // void* C::~C(int del)
    // {
    //   this->~C();
    //   // TODO: if (del) delete (char*)this;
    //   return (void*) this;
    // }
    Parameter delparam = new Parameter(STC.undefined_, Type.tuns32, Identifier.idPool("del"), new IntegerExp(dtor.loc, 0, Type.tuns32), null);
    Parameters* params = new Parameters;
    params.push(delparam);
    auto ftype = new TypeFunction(ParameterList(params), Type.tvoidptr, LINK.cpp, dtor.storage_class);
    auto func = new DtorDeclaration(dtor.loc, dtor.loc, dtor.storage_class, Id.cppdtor);
    func.type = ftype;

    // Always generate the function with body, because it is not exported from DLLs.
    const loc = dtor.loc;
    auto stmts = new Statements;
    auto call = new CallExp(loc, dtor, null);
    call.directcall = true;
    stmts.push(new ExpStatement(loc, call));
    stmts.push(new ReturnStatement(loc, new CastExp(loc, new ThisExp(loc), Type.tvoidptr)));
    func.fbody = new CompoundStatement(loc, stmts);
    func.flags |= FUNCFLAG.generated;

    auto sc2 = sc.push();
    sc2.stc &= ~STC.static_; // not a static destructor
    sc2.linkage = LINK.cpp;

    ad.members.push(func);
    func.addMember(sc2, ad);
    func.dsymbolSemantic(sc2);

    sc2.pop();
    return func;
}

/**
 * build a shim function around the aggregate dtor that translates
 *  a C++ destructor to a destructor with extern(D) calling convention
 *
 * Params:
 *  ad = the aggregate that contains the destructor to wrap
 *  sc = the scope in which to analyze the new function
 *
 * Returns:
 *  the shim destructor, semantically analyzed and added to the class as a member
 */
private DtorDeclaration buildExternDDtor(AggregateDeclaration ad, Scope* sc)
{
    auto dtor = ad.aggrDtor;
    if (!dtor)
        return null;

    // Don't try to call `@disable`d dtors
    if (dtor.storage_class & STC.disable)
        return null;

    // Generate shim only when ABI incompatible on target platform
    if (ad.classKind != ClassKind.cpp || !target.cpp.wrapDtorInExternD)
        return dtor;

    // generate member function that adjusts calling convention
    // (EAX used for 'this' instead of ECX on Windows/stack on others):
    // extern(D) void __ticppdtor()
    // {
    //     Class.__dtor();
    // }
    auto ftype = new TypeFunction(ParameterList(), Type.tvoid, LINK.d, dtor.storage_class);
    auto func = new DtorDeclaration(dtor.loc, dtor.loc, dtor.storage_class, Id.ticppdtor);
    func.type = ftype;

    auto call = new CallExp(dtor.loc, dtor, null);
    call.directcall = true;                   // non-virtual call Class.__dtor();
    func.fbody = new ExpStatement(dtor.loc, call);
    func.flags |= FUNCFLAG.generated;
    func.storage_class |= STC.inference;

    auto sc2 = sc.push();
    sc2.stc &= ~STC.static_; // not a static destructor
    sc2.linkage = LINK.d;

    ad.members.push(func);
    func.addMember(sc2, ad);
    func.dsymbolSemantic(sc2);
    func.functionSemantic(); // to infer attributes

    sc2.pop();
    return func;
}

/******************************************
 * Create inclusive invariant for struct/class by aggregating
 * all the invariants in invs[].
 * ---
 * void __invariant() const [pure nothrow @trusted]
 * {
 *     invs[0](), invs[1](), ...;
 * }
 * ---
 */
FuncDeclaration buildInv(AggregateDeclaration ad, Scope* sc)
{
    switch (ad.invs.dim)
    {
    case 0:
        return null;

    case 1:
        // Don't return invs[0] so it has uniquely generated name.
        goto default;

    default:
        Expression e = null;
        StorageClass stcx = 0;
        StorageClass stc = STC.safe | STC.nothrow_ | STC.pure_ | STC.nogc;
        foreach (i, inv; ad.invs)
        {
            stc = mergeFuncAttrs(stc, inv);
            if (stc & STC.disable)
            {
                // What should do?
            }
            const stcy = (inv.storage_class & STC.synchronized_) |
                         (inv.type.mod & MODFlags.shared_ ? STC.shared_ : 0);
            if (i == 0)
                stcx = stcy;
            else if (stcx ^ stcy)
            {
                version (all)
                {
                    // currently rejects
                    ad.error(inv.loc, "mixing invariants with different `shared`/`synchronized` qualifiers is not supported");
                    e = null;
                    break;
                }
            }
            e = Expression.combine(e, new CallExp(Loc.initial, new VarExp(Loc.initial, inv, false)));
        }
        auto inv = new InvariantDeclaration(ad.loc, Loc.initial, stc | stcx,
                Id.classInvariant, new ExpStatement(Loc.initial, e));
        ad.members.push(inv);
        inv.dsymbolSemantic(sc);
        return inv;
    }
}

/*****************************************
 * Create inclusive postblit for struct by aggregating
 * all the postblits in postblits[] with the postblits for
 * all the members.
 * Note the close similarity with AggregateDeclaration::buildDtor(),
 * and the ordering changes (runs forward instead of backwards).
 */
FuncDeclaration buildPostBlit(StructDeclaration sd, Scope* sc)
{
    //printf("buildPostBlit() %s\n", sd.toChars());
    if (sd.isUnionDeclaration())
        return null;

    const hasUserDefinedPosblit = sd.postblits.dim && !sd.postblits[0].isDisabled ? true : false;

    // by default, the storage class of the created postblit
    StorageClass stc = STC.safe | STC.nothrow_ | STC.pure_ | STC.nogc;
    Loc declLoc = sd.postblits.dim ? sd.postblits[0].loc : sd.loc;
    Loc loc; // internal code should have no loc to prevent coverage

    // if any of the postblits are disabled, then the generated postblit
    // will be disabled
    foreach (postblit; sd.postblits)
        stc |= postblit.storage_class & STC.disable;

    VarDeclaration[] fieldsToDestroy;
    auto postblitCalls = new Statements();
    // iterate through all the struct fields that are not disabled
    for (size_t i = 0; i < sd.fields.dim && !(stc & STC.disable); i++)
    {
        auto structField = sd.fields[i];
        if (structField.storage_class & STC.ref_)
            continue;
        if (structField.overlapped)
            continue;
        // if it's a struct declaration or an array of structs
        Type tv = structField.type.baseElemOf();
        if (tv.ty != Tstruct)
            continue;
        auto sdv = (cast(TypeStruct)tv).sym;
        // which has a postblit declaration
        if (!sdv.postblit)
            continue;
        assert(!sdv.isUnionDeclaration());

        // if this field's postblit is not `nothrow`, add a `scope(failure)`
        // block to destroy any prior successfully postblitted fields should
        // this field's postblit fail
        if (fieldsToDestroy.length > 0 && !(cast(TypeFunction)sdv.postblit.type).isnothrow)
        {
             // create a list of destructors that need to be called
            Expression[] dtorCalls;
            foreach(sf; fieldsToDestroy)
            {
                Expression ex;
                tv = sf.type.toBasetype();
                if (tv.ty == Tstruct)
                {
                    // this.v.__xdtor()

                    ex = new ThisExp(loc);
                    ex = new DotVarExp(loc, ex, sf);

                    // This is a hack so we can call destructors on const/immutable objects.
                    ex = new AddrExp(loc, ex);
                    ex = new CastExp(loc, ex, sf.type.mutableOf().pointerTo());
                    ex = new PtrExp(loc, ex);
                    if (stc & STC.safe)
                        stc = (stc & ~STC.safe) | STC.trusted;

                    auto sfv = (cast(TypeStruct)sf.type.baseElemOf()).sym;

                    ex = new DotVarExp(loc, ex, sfv.dtor, false);
                    ex = new CallExp(loc, ex);

                    dtorCalls ~= ex;
                }
                else
                {
                    // _ArrayDtor((cast(S*)this.v.ptr)[0 .. n])

                    const length = tv.numberOfElems(loc);

                    ex = new ThisExp(loc);
                    ex = new DotVarExp(loc, ex, sf);

                    // This is a hack so we can call destructors on const/immutable objects.
                    ex = new DotIdExp(loc, ex, Id.ptr);
                    ex = new CastExp(loc, ex, sdv.type.pointerTo());
                    if (stc & STC.safe)
                        stc = (stc & ~STC.safe) | STC.trusted;

                    auto se = new SliceExp(loc, ex, new IntegerExp(loc, 0, Type.tsize_t),
                                                    new IntegerExp(loc, length, Type.tsize_t));
                    // Prevent redundant bounds check
                    se.upperIsInBounds = true;
                    se.lowerIsLessThanUpper = true;

                    ex = new CallExp(loc, new IdentifierExp(loc, Id.__ArrayDtor), se);

                    dtorCalls ~= ex;
                }
            }
            fieldsToDestroy = [];

            // aggregate the destructor calls
            auto dtors = new Statements();
            foreach_reverse(dc; dtorCalls)
            {
                dtors.push(new ExpStatement(loc, dc));
            }

            // put destructor calls in a `scope(failure)` block
            postblitCalls.push(new ScopeGuardStatement(loc, TOK.onScopeFailure, new CompoundStatement(loc, dtors)));
        }

        // perform semantic on the member postblit in order to
        // be able to aggregate it later on with the rest of the
        // postblits
        sdv.postblit.functionSemantic();

        stc = mergeFuncAttrs(stc, sdv.postblit);
        stc = mergeFuncAttrs(stc, sdv.dtor);

        // if any of the struct member fields has disabled
        // its postblit, then `sd` is not copyable, so no
        // postblit is generated
        if (stc & STC.disable)
        {
            postblitCalls.setDim(0);
            break;
        }

        Expression ex;
        tv = structField.type.toBasetype();
        if (tv.ty == Tstruct)
        {
            // this.v.__xpostblit()

            ex = new ThisExp(loc);
            ex = new DotVarExp(loc, ex, structField);

            // This is a hack so we can call postblits on const/immutable objects.
            ex = new AddrExp(loc, ex);
            ex = new CastExp(loc, ex, structField.type.mutableOf().pointerTo());
            ex = new PtrExp(loc, ex);
            if (stc & STC.safe)
                stc = (stc & ~STC.safe) | STC.trusted;

            ex = new DotVarExp(loc, ex, sdv.postblit, false);
            ex = new CallExp(loc, ex);
        }
        else
        {
            // _ArrayPostblit((cast(S*)this.v.ptr)[0 .. n])

            const length = tv.numberOfElems(loc);
            if (length == 0)
                continue;

            ex = new ThisExp(loc);
            ex = new DotVarExp(loc, ex, structField);

            // This is a hack so we can call postblits on const/immutable objects.
            ex = new DotIdExp(loc, ex, Id.ptr);
            ex = new CastExp(loc, ex, sdv.type.pointerTo());
            if (stc & STC.safe)
                stc = (stc & ~STC.safe) | STC.trusted;

            auto se = new SliceExp(loc, ex, new IntegerExp(loc, 0, Type.tsize_t),
                                            new IntegerExp(loc, length, Type.tsize_t));
            // Prevent redundant bounds check
            se.upperIsInBounds = true;
            se.lowerIsLessThanUpper = true;
            ex = new CallExp(loc, new IdentifierExp(loc, Id.__ArrayPostblit), se);
        }
        postblitCalls.push(new ExpStatement(loc, ex)); // combine in forward order

        /* https://issues.dlang.org/show_bug.cgi?id=10972
         * When subsequent field postblit calls fail,
         * this field should be destructed for Exception Safety.
         */
        if (sdv.dtor)
        {
            sdv.dtor.functionSemantic();

            // keep a list of fields that need to be destroyed in case
            // of a future postblit failure
            fieldsToDestroy ~= structField;
        }
    }

    void checkShared()
    {
        if (sd.type.isShared())
            stc |= STC.shared_;
    }

    // Build our own "postblit" which executes a, but only if needed.
    if (postblitCalls.dim || (stc & STC.disable))
    {
        //printf("Building __fieldPostBlit()\n");
        checkShared();
        auto dd = new PostBlitDeclaration(declLoc, Loc.initial, stc, Id.__fieldPostblit);
        dd.flags |= FUNCFLAG.generated;
        dd.storage_class |= STC.inference | STC.scope_;
        dd.fbody = (stc & STC.disable) ? null : new CompoundStatement(loc, postblitCalls);
        sd.postblits.shift(dd);
        sd.members.push(dd);
        dd.dsymbolSemantic(sc);
    }

    // create __xpostblit, which is the generated postblit
    FuncDeclaration xpostblit = null;
    switch (sd.postblits.dim)
    {
    case 0:
        break;

    case 1:
        xpostblit = sd.postblits[0];
        break;

    default:
        Expression e = null;
        stc = STC.safe | STC.nothrow_ | STC.pure_ | STC.nogc;
        foreach (fd; sd.postblits)
        {
            stc = mergeFuncAttrs(stc, fd);
            if (stc & STC.disable)
            {
                e = null;
                break;
            }
            Expression ex = new ThisExp(loc);
            ex = new DotVarExp(loc, ex, fd, false);
            ex = new CallExp(loc, ex);
            e = Expression.combine(e, ex);
        }

        checkShared();
        auto dd = new PostBlitDeclaration(declLoc, Loc.initial, stc, Id.__aggrPostblit);
        dd.flags |= FUNCFLAG.generated;
        dd.storage_class |= STC.inference;
        dd.fbody = new ExpStatement(loc, e);
        sd.members.push(dd);
        dd.dsymbolSemantic(sc);
        xpostblit = dd;
        break;
    }

    // Add an __xpostblit alias to make the inclusive postblit accessible
    if (xpostblit)
    {
        auto _alias = new AliasDeclaration(Loc.initial, Id.__xpostblit, xpostblit);
        _alias.dsymbolSemantic(sc);
        sd.members.push(_alias);
        _alias.addMember(sc, sd); // add to symbol table
    }

    if (sd.hasCopyCtor)
    {
        // we have user defined postblit, so we prioritize it
        if (hasUserDefinedPosblit)
        {
            sd.hasCopyCtor = false;
            return xpostblit;
        }
        // we have fields with postblits, so print deprecations
        if (xpostblit && !xpostblit.isDisabled())
        {
            deprecation(sd.loc, "`struct %s` implicitly-generated postblit hides copy constructor.", sd.toChars);
            deprecationSupplemental(sd.loc, "The field postblit will have priority over the copy constructor.");
            deprecationSupplemental(sd.loc, "To change this, the postblit should be disabled for `struct %s`", sd.toChars());
            sd.hasCopyCtor = false;
        }
        else
            xpostblit = null;
    }

    return xpostblit;
}

/**
 * Generates a copy constructor declaration with the specified storage
 * class for the parameter and the function.
 *
 * Params:
 *  sd = the `struct` that contains the copy constructor
 *  paramStc = the storage class of the copy constructor parameter
 *  funcStc = the storage class for the copy constructor declaration
 *
 * Returns:
 *  The copy constructor declaration for struct `sd`.
 */
private CtorDeclaration generateCopyCtorDeclaration(StructDeclaration sd, const StorageClass paramStc, const StorageClass funcStc)
{
    auto fparams = new Parameters();
    auto structType = sd.type;
    fparams.push(new Parameter(paramStc | STC.ref_ | STC.return_ | STC.scope_, structType, Id.p, null, null));
    ParameterList pList = ParameterList(fparams);
    auto tf = new TypeFunction(pList, structType, LINK.d, STC.ref_);
    auto ccd = new CtorDeclaration(sd.loc, Loc.initial, STC.ref_, tf, true);
    ccd.storage_class |= funcStc;
    ccd.storage_class |= STC.inference;
    ccd.flags |= FUNCFLAG.generated;
    return ccd;
}

/**
 * Generates a trivial copy constructor body that simply does memberwise
 * initialization:
 *
 *    this.field1 = rhs.field1;
 *    this.field2 = rhs.field2;
 *    ...
 *
 * Params:
 *  sd = the `struct` declaration that contains the copy constructor
 *
 * Returns:
 *  A `CompoundStatement` containing the body of the copy constructor.
 */
private Statement generateCopyCtorBody(StructDeclaration sd)
{
    Loc loc;
    Expression e;
    foreach (v; sd.fields)
    {
        auto ec = new AssignExp(loc,
            new DotVarExp(loc, new ThisExp(loc), v),
            new DotVarExp(loc, new IdentifierExp(loc, Id.p), v));
        e = Expression.combine(e, ec);
        //printf("e.toChars = %s\n", e.toChars());
    }
    Statement s1 = new ExpStatement(loc, e);
    return new CompoundStatement(loc, s1);
}

/**
 * Determine if a copy constructor is needed for struct sd,
 * if the following conditions are met:
 *
 * 1. sd does not define a copy constructor
 * 2. at least one field of sd defines a copy constructor
 *
 * Params:
 *  sd = the `struct` for which the copy constructor is generated
 *  hasCpCtor = set to true if a copy constructor is already present
 *
 * Returns:
 *  `true` if one needs to be generated
 *  `false` otherwise
 */
private bool needCopyCtor(StructDeclaration sd, out bool hasCpCtor)
{
    if (global.errors)
        return false;

    auto ctor = sd.search(sd.loc, Id.ctor);
    if (ctor)
    {
        if (ctor.isOverloadSet())
            return false;
        if (auto td = ctor.isTemplateDeclaration())
            ctor = td.funcroot;
    }

    CtorDeclaration cpCtor;
    CtorDeclaration rvalueCtor;

    if (!ctor)
        goto LcheckFields;

    overloadApply(ctor, (Dsymbol s)
    {
        if (s.isTemplateDeclaration())
            return 0;
        auto ctorDecl = s.isCtorDeclaration();
        assert(ctorDecl);
        if (ctorDecl.isCpCtor)
        {
            if (!cpCtor)
                cpCtor = ctorDecl;
            return 0;
        }

        auto tf = ctorDecl.type.toTypeFunction();
        const dim = tf.parameterList.length;
        if (dim == 1 || (dim > 1 && tf.parameterList[1].defaultArg))
        {
            auto param = tf.parameterList[0];
            if (param.type.mutableOf().unSharedOf() == sd.type.mutableOf().unSharedOf())
            {
                rvalueCtor = ctorDecl;
            }
        }
        return 0;
    });

    if (cpCtor)
    {
        if (rvalueCtor)
        {
            .error(sd.loc, "`struct %s` may not define both a rvalue constructor and a copy constructor", sd.toChars());
            errorSupplemental(rvalueCtor.loc,"rvalue constructor defined here");
            errorSupplemental(cpCtor.loc, "copy constructor defined here");
        }
        hasCpCtor = true;
        return false;
    }

LcheckFields:
    VarDeclaration fieldWithCpCtor;
    // see if any struct members define a copy constructor
    foreach (v; sd.fields)
    {
        if (v.storage_class & STC.ref_)
            continue;
        if (v.overlapped)
            continue;

        auto ts = v.type.baseElemOf().isTypeStruct();
        if (!ts)
            continue;
        if (ts.sym.hasCopyCtor)
        {
            fieldWithCpCtor = v;
            break;
        }
    }

    if (fieldWithCpCtor && rvalueCtor)
    {
        .error(sd.loc, "`struct %s` may not define a rvalue constructor and have fields with copy constructors", sd.toChars());
        errorSupplemental(rvalueCtor.loc,"rvalue constructor defined here");
        errorSupplemental(fieldWithCpCtor.loc, "field with copy constructor defined here");
        return false;
    }
    else if (!fieldWithCpCtor)
        return false;
    return true;
}

/**
 * Generates a copy constructor if needCopyCtor() returns true.
 * The generated copy constructor will be of the form:
 *   this(ref return scope inout(S) rhs) inout
 *   {
 *      this.field1 = rhs.field1;
 *      this.field2 = rhs.field2;
 *      ...
 *   }
 *
 * Params:
 *  sd = the `struct` for which the copy constructor is generated
 *  sc = the scope where the copy constructor is generated
 *
 * Returns:
 *  `true` if `struct` sd defines a copy constructor (explicitly or generated),
 *  `false` otherwise.
 */
bool buildCopyCtor(StructDeclaration sd, Scope* sc)
{
    bool hasCpCtor;
    if (!needCopyCtor(sd, hasCpCtor))
        return hasCpCtor;

    //printf("generating copy constructor for %s\n", sd.toChars());
    const MOD paramMod = MODFlags.wild;
    const MOD funcMod = MODFlags.wild;
    auto ccd = generateCopyCtorDeclaration(sd, ModToStc(paramMod), ModToStc(funcMod));
    auto copyCtorBody = generateCopyCtorBody(sd);
    ccd.fbody = copyCtorBody;
    sd.members.push(ccd);
    ccd.addMember(sc, sd);
    const errors = global.startGagging();
    Scope* sc2 = sc.push();
    sc2.stc = 0;
    sc2.linkage = LINK.d;
    ccd.dsymbolSemantic(sc2);
    ccd.semantic2(sc2);
    ccd.semantic3(sc2);
    //printf("ccd semantic: %s\n", ccd.type.toChars());
    sc2.pop();
    if (global.endGagging(errors) || sd.isUnionDeclaration())
    {
        ccd.storage_class |= STC.disable;
        ccd.fbody = null;
    }
    return true;
}
