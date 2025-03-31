/**
 * Handles operator overloading.
 *
 * Specification: $(LINK2 https://dlang.org/spec/operatoroverloading.html, Operator Overloading)
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/opover.d, _opover.d)
 * Documentation:  https://dlang.org/phobos/dmd_opover.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/opover.d
 */

module dmd.opover;

import core.stdc.stdio;
import dmd.aggregate;
import dmd.aliasthis;
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
import dmd.funcsem;
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.optimize;
import dmd.statement;
import dmd.templatesem;
import dmd.tokens;
import dmd.typesem;
import dmd.visitor;

/***********************************
 * Determine if operands of binary op can be reversed
 * to fit operator overload.
 */
bool isCommutative(EXP op) @safe
{
    switch (op)
    {
    case EXP.add:
    case EXP.mul:
    case EXP.and:
    case EXP.or:
    case EXP.xor:
    // EqualExp
    case EXP.equal:
    case EXP.notEqual:
    // CmpExp
    case EXP.lessThan:
    case EXP.lessOrEqual:
    case EXP.greaterThan:
    case EXP.greaterOrEqual:
        return true;
    default:
        break;
    }
    return false;
}

/// Returns: whether `op` can be overloaded with `opBinary`
private bool hasOpBinary(EXP op) pure @safe
{
    switch (op)
    {
        case EXP.add: return true;
        case EXP.min: return true;
        case EXP.mul: return true;
        case EXP.div: return true;
        case EXP.mod: return true;
        case EXP.and: return true;
        case EXP.or: return true;
        case EXP.xor: return true;
        case EXP.leftShift: return true;
        case EXP.rightShift: return true;
        case EXP.unsignedRightShift: return true;
        case EXP.concatenate: return true;
        case EXP.pow: return true;
        case EXP.in_: return true;
        default: return false;
    }
}

/**
 * Remove the = from op=, e.g. += becomes +
 *
 * Params:
 *     op = tag for a binary assign operator
 * Returns: the corresponding binary operator, or `op` if it wasn't an assign operator
*/
private EXP stripAssignOp(EXP op)
{
    switch (op)
    {
    case EXP.addAssign: return EXP.add;
    case EXP.minAssign: return EXP.min;
    case EXP.mulAssign: return EXP.mul;
    case EXP.divAssign: return EXP.div;
    case EXP.modAssign: return EXP.mod;
    case EXP.andAssign: return EXP.and;
    case EXP.orAssign: return EXP.or;
    case EXP.xorAssign: return EXP.xor;
    case EXP.leftShiftAssign: return EXP.leftShift;
    case EXP.rightShiftAssign: return EXP.rightShift;
    case EXP.unsignedRightShiftAssign: return EXP.unsignedRightShift;
    case EXP.concatenateAssign: return EXP.concatenate;
    case EXP.powAssign: return EXP.pow;
    default: return op;
    }
}

/*******************************************
 * Helper function to turn operator into template argument list
 */
Objects* opToArg(Scope* sc, EXP op)
{
    Expression e = new StringExp(Loc.initial, EXPtoString(stripAssignOp(op)));
    e = e.expressionSemantic(sc);
    auto tiargs = new Objects();
    tiargs.push(e);
    return tiargs;
}

// Try alias this on first operand
Expression checkAliasThisForLhs(AggregateDeclaration ad, Scope* sc, BinExp e, Type[2] aliasThisStop)
{
    if (!ad || !ad.aliasthis)
        return null;

    /* Rewrite (e1 op e2) as:
     *      (e1.aliasthis op e2)
     */
    if (isRecursiveAliasThis(aliasThisStop[0], e.e1.type))
        return null;
    //printf("att %s e1 = %s\n", Token.toChars(e.op), e.e1.type.toChars());
    BinExp be = cast(BinExp)e.copy();
    // Resolve 'alias this' but in case of assigment don't resolve properties yet
    // because 'e1 = e2' could mean 'e1(e2)' or 'e1() = e2'
    bool findOnly = e.isAssignExp() !is null;
    be.e1 = resolveAliasThis(sc, e.e1, true, findOnly);
    if (!be.e1)
        return null;

    return be.trySemanticAliasThis(sc, aliasThisStop);
}

// Try alias this on second operand
Expression checkAliasThisForRhs(AggregateDeclaration ad, Scope* sc, BinExp e, Type[2] aliasThisStop)
{
    if (!ad || !ad.aliasthis)
        return null;
    /* Rewrite (e1 op e2) as:
     *      (e1 op e2.aliasthis)
     */
    if (isRecursiveAliasThis(aliasThisStop[1], e.e2.type))
        return null;
    //printf("att %s e2 = %s\n", Token.toChars(e.op), e.e2.type.toChars());
    BinExp be = cast(BinExp)e.copy();
    be.e2 = resolveAliasThis(sc, e.e2, true);
    if (!be.e2)
        return null;

    return be.trySemanticAliasThis(sc, aliasThisStop);
}

Expression opOverloadUnary(UnaExp e, Scope* sc)
{
    if (auto ae = e.e1.isArrayExp())
    {
        ae.e1 = ae.e1.expressionSemantic(sc);
        ae.e1 = resolveProperties(sc, ae.e1);
        Expression ae1old = ae.e1;
        const(bool) maybeSlice = (ae.arguments.length == 0 || ae.arguments.length == 1 && (*ae.arguments)[0].isIntervalExp());
        IntervalExp ie = null;
        if (maybeSlice && ae.arguments.length)
        {
            ie = (*ae.arguments)[0].isIntervalExp();
        }
        Type att = null; // first cyclic `alias this` type
        while (true)
        {
            if (ae.e1.isErrorExp())
            {
                return ae.e1;
            }
            Expression ae1save = ae.e1;
            ae.lengthVar = null;

            AggregateDeclaration ad = isAggregate(ae.e1.type);
            if (!ad)
                break;

            if (search_function(ad, Id.opIndexUnary))
            {
                Expression e0;
                // Deal with $
                Expression ae2 = resolveOpDollar(sc, ae, e0);
                if (!ae2) // op(a[i..j]) might be: a.opSliceUnary!(op)(i, j)
                    goto Lfallback;
                if (ae2.isErrorExp())
                    return ae2;
                /* Rewrite op(a[arguments]) as:
                 *      a.opIndexUnary!(op)(arguments)
                 */
                Expression result = dotTemplateCall(ae.e1, Id.opIndexUnary, opToArg(sc, e.op), (*ae.arguments)[]);
                if (maybeSlice) // op(a[]) might be: a.opSliceUnary!(op)()
                    result = result.trySemantic(sc);
                else
                    result = result.expressionSemantic(sc);

                if (result)
                    return Expression.combine(e0, result);
            }
        Lfallback:
            if (maybeSlice && search_function(ad, Id.opSliceUnary))
            {
                // Deal with $
                Expression e0;
                auto ae2 = resolveOpDollar(sc, ae, ie, e0);
                if (ae2.isErrorExp())
                    return ae2;
                /* Rewrite op(a[i..j]) as:
                 *      a.opSliceUnary!(op)(i, j)
                 */
                Expression result = ie ?
                    dotTemplateCall(ae.e1, Id.opSliceUnary, opToArg(sc, e.op), ie.lwr, ie.upr) :
                    dotTemplateCall(ae.e1, Id.opSliceUnary, opToArg(sc, e.op));

                return Expression.combine(e0, result.expressionSemantic(sc));
            }
            // Didn't find it. Forward to aliasthis
            if (ad.aliasthis && !isRecursiveAliasThis(att, ae.e1.type))
            {
                /* Rewrite op(a[arguments]) as:
                 *      op(a.aliasthis[arguments])
                 */
                ae.e1 = resolveAliasThis(sc, ae1save, true);
                if (ae.e1)
                    continue;
            }
            break;
        }
        ae.e1 = ae1old; // recovery
        ae.lengthVar = null;
    }
    e.e1 = e.e1.expressionSemantic(sc);
    e.e1 = resolveProperties(sc, e.e1);
    Type att = null; // first cyclic `alias this` type
    while (1)
    {
        if (e.e1.isErrorExp())
        {
            return e.e1;
        }

        AggregateDeclaration ad = isAggregate(e.e1.type);
        if (!ad)
            break;

        /* Rewrite as:
         *      e1.opUnary!(op)()
         */
        if (Dsymbol fd = search_function(ad, Id.opUnary))
            return dotTemplateCall(e.e1, Id.opUnary, opToArg(sc, e.op)).expressionSemantic(sc);

        // Didn't find it. Forward to aliasthis
        if (ad.aliasthis && !isRecursiveAliasThis(att, e.e1.type))
        {
            /* Rewrite op(e1) as:
             *      op(e1.aliasthis)
             */
            //printf("att una %s e1 = %s\n", EXPtoString(op).ptr, this.e1.type.toChars());
            if (auto e1 = resolveAliasThis(sc, e.e1, true))
            {
                e.e1 = e1;
                continue;
            }
            break;
        }

        // For ++ and --, rewrites to += and -= are also tried, so don't error yet
        if (!e.isPreExp())
        {
            error(e.loc, "operator `%s` is not defined for `%s`", EXPtoString(e.op).ptr, ad.toChars());
            errorSupplemental(ad.loc, "perhaps overload the operator with `auto opUnary(string op : \"%s\")() {}`",
                EXPtoString(e.op).ptr);
            return ErrorExp.get();
        }

        break;
    }
    return null;
}

Expression opOverloadArray(ArrayExp ae, Scope* sc)
{
    ae.e1 = ae.e1.expressionSemantic(sc);
    ae.e1 = resolveProperties(sc, ae.e1);
    Expression ae1old = ae.e1;
    const(bool) maybeSlice = (ae.arguments.length == 0 || ae.arguments.length == 1 && (*ae.arguments)[0].isIntervalExp());
    IntervalExp ie = null;
    if (maybeSlice && ae.arguments.length)
    {
        ie = (*ae.arguments)[0].isIntervalExp();
    }
    Type att = null; // first cyclic `alias this` type
    while (true)
    {
        if (ae.e1.isErrorExp())
        {
            return ae.e1;
        }
        Expression e0 = null;
        Expression ae1save = ae.e1;
        ae.lengthVar = null;
        Type t1b = ae.e1.type.toBasetype();
        AggregateDeclaration ad = isAggregate(t1b);
        if (!ad)
        {
            // If the non-aggregate expression ae.e1 is indexable or sliceable,
            // convert it to the corresponding concrete expression.
            if (isIndexableNonAggregate(t1b) || ae.e1.isTypeExp())
            {
                // Convert to SliceExp
                if (maybeSlice)
                    return new SliceExp(ae.loc, ae.e1, ie).expressionSemantic(sc);

                // Convert to IndexExp
                if (ae.arguments.length == 1)
                    return new IndexExp(ae.loc, ae.e1, (*ae.arguments)[0]).expressionSemantic(sc);
            }
            break;
        }
        if (search_function(ad, Id.opIndex))
        {
            // Deal with $
            auto ae2 = resolveOpDollar(sc, ae, e0);
            if (!ae2) // a[i..j] might be: a.opSlice(i, j)
                goto Lfallback;
            if (ae2.isErrorExp())
                return ae2;
            /* Rewrite e1[arguments] as:
             *      e1.opIndex(arguments)
             */
            Expressions* a = ae.arguments.copy();
            Expression result = new DotIdExp(ae.loc, ae.e1, Id.opIndex);
            result = new CallExp(ae.loc, result, a);
            if (maybeSlice) // a[] might be: a.opSlice()
                result = result.trySemantic(sc);
            else
                result = result.expressionSemantic(sc);

            if (result)
                return Expression.combine(e0, result);
        }
    Lfallback:
        if (maybeSlice && ae.e1.isTypeExp())
        {
            Expression result = new SliceExp(ae.loc, ae.e1, ie);
            result = result.expressionSemantic(sc);
            return Expression.combine(e0, result);
        }
        if (maybeSlice && search_function(ad, Id.opSlice))
        {
            // Deal with $
            auto ae2 = resolveOpDollar(sc, ae, ie, e0);

            if (ae2.isErrorExp())
            {
                if (!e0 && !search_function(ad, Id.dollar))
                    ad.loc.errorSupplemental("perhaps define `opDollar` for `%s`", ad.toChars());

                return ae2;
            }
            /* Rewrite a[i..j] as:
             *      a.opSlice(i, j)
             */
            auto a = new Expressions();
            if (ie)
            {
                a.push(ie.lwr);
                a.push(ie.upr);
            }
            Expression result = new DotIdExp(ae.loc, ae.e1, Id.opSlice);
            result = new CallExp(ae.loc, result, a);
            result = result.expressionSemantic(sc);
            return Expression.combine(e0, result);
        }
        // Didn't find it. Forward to aliasthis
        if (ad.aliasthis && !isRecursiveAliasThis(att, ae.e1.type))
        {
            //printf("att arr e1 = %s\n", this.e1.type.toChars());
            /* Rewrite op(a[arguments]) as:
             *      op(a.aliasthis[arguments])
             */
            ae.e1 = resolveAliasThis(sc, ae1save, true);
            if (ae.e1)
                continue;
        }
        break;
    }
    ae.e1 = ae1old; // recovery
    ae.lengthVar = null;
    return null;
}

/***********************************************
 * This is mostly the same as opOverloadUnary but has
 * a different rewrite.
 */
Expression opOverloadCast(CastExp e, Scope* sc, Type att = null)
{
    AggregateDeclaration ad = isAggregate(e.e1.type);
    if (!ad)
        return null;

    // Rewrite as: e1.opCast!(T)()
    if (Dsymbol fd = search_function(ad, Id.opCast))
    {
        version (all)
        {
            // Backwards compatibility with D1 if opCast is a function, not a template
            if (fd.isFuncDeclaration())
            {
                // Rewrite as:  e1.opCast()
                return build_overload(e.loc, sc, e.e1, null, fd);
            }
        }
        auto tiargs = new Objects();
        tiargs.push(e.to);
        return dotTemplateCall(e.e1, Id.opCast, tiargs).expressionSemantic(sc);
    }
    // Didn't find it. Forward to aliasthis
    if (ad.aliasthis && !isRecursiveAliasThis(att, e.e1.type))
    {
        // Rewrite `e1.opCast()` as `e1.aliasthis.opCast()`
        if (auto e1 = resolveAliasThis(sc, e.e1, true))
        {
            CastExp result = e.copy().isCastExp();
            result.e1 = e1;
            return result.opOverloadCast(sc, att);
        }
    }
    return null;
}

// When no operator overload functions are found for `e`, recursively try with `alias this`
// Returns: `null` when still no overload found, otherwise resolved lowering
Expression binAliasThis(BinExp e, Scope* sc, Type[2] aliasThisStop)
{
    AggregateDeclaration ad1 = isAggregate(e.e1.type);
    AggregateDeclaration ad2 = isAggregate(e.e2.type);
    Expression rewrittenLhs;
    if (!(e.isAssignExp && ad2 && ad1 == ad2)) // https://issues.dlang.org/show_bug.cgi?id=2943
    {
        if (Expression result = checkAliasThisForLhs(ad1, sc, e, aliasThisStop))
        {
            /* https://issues.dlang.org/show_bug.cgi?id=19441
             *
             * alias this may not be used for partial assignment.
             * If a struct has a single member which is aliased this
             * directly or aliased to a ref getter function that returns
             * the mentioned member, then alias this may be
             * used since the object will be fully initialised.
             * If the struct is nested, the context pointer is considered
             * one of the members, hence the `ad1.fields.length == 2 && ad1.vthis`
             * condition.
             */
            auto ae = result.isAssignExp();
            if (!ae)
                return result;     // i.e: Rewrote `e1 = e2` -> `e1(e2)`

            auto dve = ae.e1.isDotVarExp();
            if (!dve)
                return result;     // i.e: Rewrote `e1 = e2` -> `e1() = e2`

            if (auto ad = dve.var.isMember2())
            {
                // i.e: Rewrote `e1 = e2` -> `e1.some.var = e2`
                // Ensure that `var` is the only field member in `ad`
                if (ad.fields.length == 1 || (ad.fields.length == 2 && ad.vthis))
                {
                    if (dve.var == ad.aliasthis.sym)
                        return result;
                }
            }
            rewrittenLhs = ae.e1;
        }
    }
    if (!(e.isAssignExp && ad1 && ad1 == ad2)) // https://issues.dlang.org/show_bug.cgi?id=2943
    {
        if (Expression result = checkAliasThisForRhs(ad2, sc, e, aliasThisStop))
            return result;
    }
    if (rewrittenLhs)
    {
        error(e.loc, "cannot use `alias this` to partially initialize variable `%s` of type `%s`. Use `%s`",
                e.e1.toChars(), ad1.toChars(), rewrittenLhs.toChars());
        return ErrorExp.get();
    }
    return null;
}

Expression opOverloadAssign(AssignExp e, Scope* sc, Type[2] aliasThisStop)
{
    AggregateDeclaration ad1 = isAggregate(e.e1.type);
    AggregateDeclaration ad2 = isAggregate(e.e2.type);
    if (ad1 == ad2)
    {
        StructDeclaration sd = ad1.isStructDeclaration();
        if (sd &&
            (!sd.hasIdentityAssign ||
                /* Do a blit if we can and the rvalue is something like .init,
                 * where a postblit is not necessary.
                 */
                (sd.hasBlitAssign && !e.e2.isLvalue())))
        {
            /* This is bitwise struct assignment. */
            return null;
        }
    }
    Dsymbol s = search_function(ad1, Id.opAssign);

    bool choseReverse;
    if (auto result = pickBestBinaryOverload(sc, null, s, null, e, choseReverse))
        return result;

    return binAliasThis(e, sc, aliasThisStop);
}

Expression opOverloadBinary(BinExp e, Scope* sc, Type[2] aliasThisStop)
{
    if (Expression err = binSemanticProp(e, sc))
        return err;

    AggregateDeclaration ad1 = isAggregate(e.e1.type);
    AggregateDeclaration ad2 = isAggregate(e.e2.type);

    // Try opBinary and opBinaryRight
    Dsymbol s = search_function(ad1, Id.opBinary);
    if (s && !s.isTemplateDeclaration())
    {
        error(e.e1.loc, "`%s.opBinary` isn't a template", e.e1.toChars());
        return ErrorExp.get();
    }

    Dsymbol s_r = search_function(ad2, Id.opBinaryRight);
    if (s_r && !s_r.isTemplateDeclaration())
    {
        error(e.e2.loc, "`%s.opBinaryRight` isn't a template", e.e2.toChars());
        return ErrorExp.get();
    }
    if (s_r && s_r == s) // https://issues.dlang.org/show_bug.cgi?id=12778
        s_r = null;

    bool choseReverse;
    if (auto result = pickBestBinaryOverload(sc, opToArg(sc, e.op), s, s_r, e, choseReverse))
        return result;

    return binAliasThis(e, sc, aliasThisStop);
}

/**
 * If applicable, print an error relating to implementing / fixing `opBinary` functions.
 * Params:
 *   e = binary operation
 *   sc = scope to try `opBinary!""` semantic in for error messages
 * Returns: `true` when an error related to `opBinary` was printed
 */
bool suggestBinaryOverloads(BinExp e, Scope* sc)
{
    if (!e.op.hasOpBinary)
        return false;

    AggregateDeclaration ad1 = isAggregate(e.e1.type);
    AggregateDeclaration ad2 = isAggregate(e.e2.type);

    if (ad1)
    {
        if (Dsymbol s = search_function(ad1, Id.opBinary))
        {
            // This expressionSemantic will fail, otherwise operator overloading would have succeeded before
            dotTemplateCall(e.e1, Id.opBinary, opToArg(sc, e.op), e.e2).expressionSemantic(sc);
            errorSupplemental(s.loc, "`opBinary` defined here");
            return true;
        }
        error(e.loc, "operator `%s` is not defined for type `%s`", EXPtoString(e.op).ptr, e.e1.type.toChars);
        errorSupplemental(ad1.loc, "perhaps overload the operator with `auto opBinary(string op : \"%s\")(%s rhs) {}`", EXPtoString(e.op).ptr, e.e2.type.toChars);
        return true;
    }
    else if (ad2)
    {
        if (Dsymbol s_r = search_function(ad1, Id.opBinaryRight))
        {
            dotTemplateCall(e.e2, Id.opBinaryRight, opToArg(sc, e.op), e.e1).expressionSemantic(sc);
            errorSupplemental(s_r.loc, "`opBinaryRight` defined here");
            return true;
        }
        error(e.loc, "operator `%s` is not defined for type `%s`", EXPtoString(e.op).ptr, e.e2.type.toChars);
        errorSupplemental(ad2.loc, "perhaps overload the operator with `auto opBinaryRight(string op : \"%s\")(%s rhs) {}`", EXPtoString(e.op).ptr, e.e1.type.toChars);
        return true;
    }
    return false;
}

/**
 * If applicable, print an error relating to implementing / fixing `opOpAssign` or `opUnary` functions.
 * Params:
 *   exp = binary operation
 *   sc = scope to try `opOpAssign!""` semantic in for error messages
 *   parent = if `exp` was lowered from this `PreExp` or `PostExp`, mention `opUnary` as well
 * Returns: `true` when an error related to `opOpAssign` was printed
 */
bool suggestOpOpAssign(BinAssignExp exp, Scope* sc, Expression parent)
{
    auto ad = isAggregate(exp.e1.type);
    if (!ad)
        return false;

    if (parent && (parent.isPreExp() || parent.isPostExp()))
    {
        error(exp.loc, "operator `%s` not supported for `%s` of type `%s`", EXPtoString(parent.op).ptr, exp.e1.toChars(), ad.toChars());
        errorSupplemental(ad.loc,
            "perhaps implement `auto opUnary(string op : \"%s\")() {}`"~
            " or `auto opOpAssign(string op : \"%s\")(int) {}`",
            EXPtoString(stripAssignOp(parent.op)).ptr,
            EXPtoString(stripAssignOp(exp.op)).ptr
            );
        return true;
    }

    if (const s = search_function(ad, Id.opOpAssign))
    {
        // This expressionSemantic will fail, otherwise operator overloading would have succeeded before
        dotTemplateCall(exp.e1, Id.opOpAssign, opToArg(sc, exp.op), exp.e2).expressionSemantic(sc);
    }
    else
    {
        error(exp.loc, "operator `%s` not supported for `%s` of type `%s`", EXPtoString(exp.op).ptr, exp.e1.toChars(), ad.toChars());
        errorSupplemental(ad.loc, "perhaps implement `auto opOpAssign(string op : \"%s\")(%s) {}`",
            EXPtoString(stripAssignOp(exp.op)).ptr, exp.e2.type.toChars());
    }
    return true;
}

// Helper to construct e.id!tiargs(args), e.g. `lhs.opBinary!"+"(rhs)`
private Expression dotTemplateCall(Expression e, Identifier id, Objects* tiargs, Expression[] args...)
{
    auto ti = new DotTemplateInstanceExp(e.loc, e, id, tiargs);
    auto expressions = new Expressions();
    expressions.pushSlice(args);
    return new CallExp(e.loc, ti, expressions);
}

Expression opOverloadEqual(EqualExp e, Scope* sc, Type[2] aliasThisStop)
{
    Type t1 = e.e1.type.toBasetype();
    Type t2 = e.e2.type.toBasetype();

    /* Array equality is handled by expressionSemantic() potentially
     * lowering to object.__equals(), which takes care of overloaded
     * operators for the element types.
     */
    if (t1.isStaticOrDynamicArray() && t2.isStaticOrDynamicArray())
    {
        return null;
    }

    /* Check for class equality with null literal or typeof(null).
     */
    if (t1.isTypeClass() && e.e2.isNullExp() ||
        t2.isTypeClass() && e.e1.isNullExp())
    {
        error(e.loc, "use `%s` instead of `%s` when comparing with `null`",
            EXPtoString(e.op == EXP.equal ? EXP.identity : EXP.notIdentity).ptr,
            EXPtoString(e.op).ptr);
        return ErrorExp.get();
    }
    if (t1.isTypeClass() && t2.isTypeNull() ||
        t1.isTypeNull() && t2.isTypeClass())
    {
        // Comparing a class with typeof(null) should not call opEquals
        return null;
    }

    /* Check for class equality.
     */
    if (t1.isTypeClass() && t2.isTypeClass())
    {
        ClassDeclaration cd1 = t1.isClassHandle();
        ClassDeclaration cd2 = t2.isClassHandle();
        if (!(cd1.classKind == ClassKind.cpp || cd2.classKind == ClassKind.cpp))
        {
            /* Rewrite as:
             *      .object.opEquals(e1, e2)
             */
            if (!ClassDeclaration.object)
            {
                error(e.loc, "cannot compare classes for equality because `object.Object` was not declared");
                return null;
            }

            Expression e1x = e.e1;
            Expression e2x = e.e2;

            /* The explicit cast is necessary for interfaces
             * https://issues.dlang.org/show_bug.cgi?id=4088
             */
            Type to = ClassDeclaration.object.getType();
            if (cd1.isInterfaceDeclaration())
                e1x = new CastExp(e.loc, e.e1, t1.isMutable() ? to : to.constOf());
            if (cd2.isInterfaceDeclaration())
                e2x = new CastExp(e.loc, e.e2, t2.isMutable() ? to : to.constOf());

            Expression result = new IdentifierExp(e.loc, Id.empty);
            result = new DotIdExp(e.loc, result, Id.object);
            result = new DotIdExp(e.loc, result, Id.opEquals);
            result = new CallExp(e.loc, result, e1x, e2x);
            if (e.op == EXP.notEqual)
                result = new NotExp(e.loc, result);
            result = result.expressionSemantic(sc);
            return result;
        }
    }

    EXP cmpOp;
    if (Expression result = compare_overload(e, sc, Id.opEquals, cmpOp, aliasThisStop))
    {
        if (lastComma(result).isCallExp() && e.op == EXP.notEqual)
        {
            result = new NotExp(result.loc, result);
            result = result.expressionSemantic(sc);
        }
        return result;
    }

    /* Check for pointer equality.
     */
    if (t1.isTypePointer() || t2.isTypePointer())
    {
        /* Rewrite:
         *      ptr1 == ptr2
         * as:
         *      ptr1 is ptr2
         *
         * This is just a rewriting for deterministic AST representation
         * as the backend input.
         */
        auto op2 = e.op == EXP.equal ? EXP.identity : EXP.notIdentity;
        Expression r = new IdentityExp(op2, e.loc, e.e1, e.e2);
        return r.expressionSemantic(sc);
    }

    /* Check for struct equality without opEquals.
     */
    if (t1.isTypeStruct() && t2.isTypeStruct())
    {
        auto sd = t1.isTypeStruct().sym;
        if (sd != t2.isTypeStruct().sym)
            return null;

        import dmd.clone : needOpEquals;
        if (!sc.previews.fieldwise && !needOpEquals(sd))
        {
            // Use bitwise equality.
            auto op2 = e.op == EXP.equal ? EXP.identity : EXP.notIdentity;
            Expression r = new IdentityExp(op2, e.loc, e.e1, e.e2);
            return r.expressionSemantic(sc);
        }

        /* Do memberwise equality.
         * https://dlang.org/spec/expression.html#equality_expressions
         * Rewrite:
         *      e1 == e2
         * as:
         *      e1.tupleof == e2.tupleof
         *
         * If sd is a nested struct, and if it's nested in a class, it will
         * also compare the parent class's equality. Otherwise, compares
         * the identity of parent context through void*.
         */
        e = e.copy().isEqualExp();
        e.e1 = new DotIdExp(e.loc, e.e1, Id._tupleof);
        e.e2 = new DotIdExp(e.loc, e.e2, Id._tupleof);

        auto sc2 = sc.push();
        sc2.noAccessCheck = true;
        Expression r = e.expressionSemantic(sc2);
        sc2.pop();
        return r;
    }

    /* Check for tuple equality.
     */
    auto tup1 = e.e1.isTupleExp();
    auto tup2 = e.e2.isTupleExp();
    if (tup1 && tup2)
    {
        size_t dim = tup1.exps.length;
        if (dim != tup2.exps.length)
        {
            error(e.loc, "mismatched sequence lengths, `%d` and `%d`",
                cast(int)dim, cast(int)tup2.exps.length);
            return ErrorExp.get();
        }

        Expression result;
        if (dim == 0)
        {
            // zero-length tuple comparison should always return true or false.
            result = IntegerExp.createBool(e.op == EXP.equal);
        }
        else
        {
            for (size_t i = 0; i < dim; i++)
            {
                auto ex1 = (*tup1.exps)[i];
                auto ex2 = (*tup2.exps)[i];
                auto eeq = new EqualExp(e.op, e.loc, ex1, ex2);

                if (!result)
                    result = eeq;
                else if (e.op == EXP.equal)
                    result = new LogicalExp(e.loc, EXP.andAnd, result, eeq);
                else
                    result = new LogicalExp(e.loc, EXP.orOr, result, eeq);
            }
            assert(result);
        }
        result = Expression.combine(tup1.e0, tup2.e0, result);
        result = result.expressionSemantic(sc);

        return result;
    }
    return null;
}

Expression opOverloadCmp(CmpExp exp, Scope* sc, Type[2] aliasThisStop)
{
    //printf("CmpExp:: () (%s)\n", e.toChars());
    EXP cmpOp = exp.op;
    auto e = compare_overload(exp, sc, Id.opCmp, cmpOp, aliasThisStop);
    if (!e)
        return null;

    if (!e.type.isScalar() && e.type.equals(exp.e1.type))
    {
        error(e.loc, "recursive `opCmp` expansion");
        return ErrorExp.get();
    }
    if (!e.isCallExp())
        return e;

    Type t1 = exp.e1.type.toBasetype();
    Type t2 = exp.e2.type.toBasetype();
    if (!t1.isTypeClass() || !t2.isTypeClass())
    {
        return new CmpExp(cmpOp, exp.loc, e, IntegerExp.literal!0).expressionSemantic(sc);
    }

    // Lower to object.__cmp(e1, e2)
    Expression cl = new IdentifierExp(exp.loc, Id.empty);
    cl = new DotIdExp(exp.loc, cl, Id.object);
    cl = new DotIdExp(exp.loc, cl, Id.__cmp);
    cl = cl.expressionSemantic(sc);

    auto arguments = new Expressions();
    // Check if op_overload found a better match by calling e2.opCmp(e1)
    // If the operands were swapped, then the result must be reversed
    // e1.opCmp(e2) == -e2.opCmp(e1)
    // cmpop takes care of this
    if (exp.op == cmpOp)
    {
        arguments.push(exp.e1);
        arguments.push(exp.e2);
    }
    else
    {
        // Use better match found by op_overload
        arguments.push(exp.e2);
        arguments.push(exp.e1);
    }

    cl = new CallExp(e.loc, cl, arguments);
    cl = new CmpExp(cmpOp, exp.loc, cl, new IntegerExp(0));
    return cl.expressionSemantic(sc);
}

/*********************************
 * Operator overloading for op=
 */
Expression opOverloadBinaryAssign(BinAssignExp e, Scope* sc, Type[2] aliasThisStop)
{
    if (auto ae = e.e1.isArrayExp())
    {
        ae.e1 = ae.e1.expressionSemantic(sc);
        ae.e1 = resolveProperties(sc, ae.e1);
        Expression ae1old = ae.e1;
        const(bool) maybeSlice = (ae.arguments.length == 0 || ae.arguments.length == 1 && (*ae.arguments)[0].isIntervalExp());
        IntervalExp ie = null;
        if (maybeSlice && ae.arguments.length)
        {
            ie = (*ae.arguments)[0].isIntervalExp();
        }
        Type att = null; // first cyclic `alias this` type
        while (true)
        {
            if (ae.e1.isErrorExp())
                return ae.e1;

            Expression e0 = null;
            Expression ae1save = ae.e1;
            ae.lengthVar = null;
            AggregateDeclaration ad = isAggregate(ae.e1.type);
            if (!ad)
                break;
            if (search_function(ad, Id.opIndexOpAssign))
            {
                // Deal with $
                Expression ae2 = resolveOpDollar(sc, ae, e0);
                if (!ae2) // (a[i..j] op= e2) might be: a.opSliceOpAssign!(op)(e2, i, j)
                    goto Lfallback;
                if (ae2.isErrorExp())
                    return ae2;
                e.e2 = e.e2.expressionSemantic(sc);
                if (e.e2.isErrorExp())
                    return e.e2;

                /* Rewrite a[arguments] op= e2 as:
                 *      a.opIndexOpAssign!(op)(e2, arguments)
                 */
                Expressions* a = ae.arguments.copy();
                a.insert(0, e.e2);
                Expression result = dotTemplateCall(ae.e1, Id.opIndexOpAssign, opToArg(sc, e.op), (*a)[]);
                if (maybeSlice) // (a[] op= e2) might be: a.opSliceOpAssign!(op)(e2)
                    result = result.trySemantic(sc);
                else
                    result = result.expressionSemantic(sc);

                if (result)
                    return Expression.combine(e0, result);
            }
        Lfallback:
            if (maybeSlice && search_function(ad, Id.opSliceOpAssign))
            {
                // Deal with $
                Expression ae2 = resolveOpDollar(sc, ae, ie, e0);
                if (ae2.isErrorExp())
                    return ae2;

                e.e2 = e.e2.expressionSemantic(sc);
                if (e.e2.isErrorExp())
                    return e.e2;

                /* Rewrite (a[i..j] op= e2) as:
                 *      a.opSliceOpAssign!(op)(e2, i, j)
                 */
                auto result = ie ?
                    dotTemplateCall(ae.e1, Id.opSliceOpAssign, opToArg(sc, e.op), e.e2, ie.lwr, ie.upr) :
                    dotTemplateCall(ae.e1, Id.opSliceOpAssign, opToArg(sc, e.op), e.e2);

                return Expression.combine(e0, result.expressionSemantic(sc));
            }
            // Didn't find it. Forward to aliasthis
            if (ad.aliasthis && !isRecursiveAliasThis(att, ae.e1.type))
            {
                /* Rewrite (a[arguments] op= e2) as:
                 *      a.aliasthis[arguments] op= e2
                 */
                ae.e1 = resolveAliasThis(sc, ae1save, true);
                if (ae.e1)
                    continue;
            }
            break;
        }
        ae.e1 = ae1old; // recovery
        ae.lengthVar = null;
    }

    if (Expression result = e.binSemanticProp(sc))
        return result;

    // Don't attempt 'alias this' if an error occurred
    if (e.e1.type.isTypeError() || e.e2.type.isTypeError())
        return ErrorExp.get();

    AggregateDeclaration ad1 = isAggregate(e.e1.type);
    Dsymbol s = search_function(ad1, Id.opOpAssign);
    if (s && !s.isTemplateDeclaration())
    {
        error(e.loc, "`%s.opOpAssign` isn't a template", e.e1.toChars());
        return ErrorExp.get();
    }

    bool choseReverse;
    if (auto res = pickBestBinaryOverload(sc, opToArg(sc, e.op), s, null, e, choseReverse))
        return res;

    Expression result = checkAliasThisForLhs(ad1, sc, e, aliasThisStop);
    if (result || !s) // no point in trying Rhs alias-this if there's no overload of any kind in lhs
        return result;

    return checkAliasThisForRhs(isAggregate(e.e2.type), sc, e, aliasThisStop);
}

/**
Given symbols `s` and `s_r`, try to instantiate `e.e1.s!tiargs(e.e2)` and `e.e2.s_r!tiargs(e.e1)`,
and return the one with the best match level.

Params:
    sc = scope
    tiargs = (optional) template arguments to instantiate symbols with
    s = (optional) symbol of straightforward template (e.g. opBinary)
    s_r = (optional) symbol of reversed template (e.g. opBinaryRight)
    e = binary expression being overloaded, supplying arguments to the function calls
    choseReverse = set to true when `s_r` was chosen instead of `s`
Returns:
    Resulting operator overload function call, or `null` if neither symbol worked
*/
private Expression pickBestBinaryOverload(Scope* sc, Objects* tiargs, Dsymbol s, Dsymbol s_r, BinExp e, out bool choseReverse)
{
    if (!s && !s_r)
        return null;

    Expressions* args1 = new Expressions(1);
    (*args1)[0] = e.e1;
    expandTuples(args1);
    Expressions* args2 = new Expressions(1);
    (*args2)[0] = e.e2;
    expandTuples(args2);
    MatchAccumulator m;

    if (s)
    {
        functionResolve(m, s, e.loc, sc, tiargs, e.e1.type, ArgumentList(args2), null);
        if (m.lastf && (m.lastf.errors || m.lastf.hasSemantic3Errors()))
            return ErrorExp.get();
    }
    FuncDeclaration lastf = m.lastf;
    int count = m.count;
    if (s_r)
    {
        functionResolve(m, s_r, e.loc, sc, tiargs, e.e2.type, ArgumentList(args1), null);
        if (m.lastf && (m.lastf.errors || m.lastf.hasSemantic3Errors()))
            return ErrorExp.get();
    }
    if (m.count > 1)
    {
        /* The following if says "not ambiguous" if there's one match
         * from s and one from s_r, in which case we pick s.
         * This doesn't follow the spec, but is a workaround for the case
         * where opEquals was generated from templates and we cannot figure
         * out if both s and s_r came from the same declaration or not.
         * The test case is:
         *   import std.typecons;
         *   void main() {
         *    assert(tuple("has a", 2u) == tuple("has a", 1));
         *   }
         */
        if (!(m.lastf == lastf && m.count == 2 && count == 1))
        {
            // Error, ambiguous
            error(e.loc, "overloads `%s` and `%s` both match argument list for `%s`", m.lastf.type.toChars(), m.nextf.type.toChars(), m.lastf.toChars());
        }
    }
    else if (m.last == MATCH.nomatch)
    {
        if (tiargs)
            return null;
        m.lastf = null;
    }

    if (lastf && m.lastf == lastf || !s_r && m.last == MATCH.nomatch)
    {
        choseReverse = false;
        // Rewrite (e1 op e2) as e1.opfunc(e2)
        return build_overload(e.loc, sc, e.e1, e.e2, m.lastf ? m.lastf : s);
    }
    else
    {
        choseReverse = true;
        // Rewrite (e1 op e2) as e2.opfunc_r(e1)
        return build_overload(e.loc, sc, e.e2, e.e1, m.lastf ? m.lastf : s_r);
    }
}

/******************************************
 * Common code for overloading of EqualExp and CmpExp
 */
private Expression compare_overload(BinExp e, Scope* sc, Identifier id, ref EXP cmpOp, Type[2] aliasThisStop)
{
    //printf("BinExp::compare_overload(id = %s) %s\n", id.toChars(), e.toChars());
    AggregateDeclaration ad1 = isAggregate(e.e1.type);
    AggregateDeclaration ad2 = isAggregate(e.e2.type);
    Dsymbol s = search_function(ad1, id);
    Dsymbol s_r = search_function(ad2, id);

    if (s == s_r)
        s_r = null;

    bool choseReverse;
    if (auto res = pickBestBinaryOverload(sc, null, s, s_r, e, choseReverse))
    {
        if (choseReverse)
            cmpOp = reverseRelation(e.op);
        return res;
    }

    /*
     * https://issues.dlang.org/show_bug.cgi?id=16657
     * at this point, no matching opEquals was found for structs,
     * so we should not follow the alias this comparison code.
     */
    if (e.isEqualExp() && ad1 == ad2)
        return null;
    Expression result = checkAliasThisForLhs(ad1, sc, e, aliasThisStop);
    if (result)
        return result;

    result = checkAliasThisForRhs(isAggregate(e.e2.type), sc, e, aliasThisStop);
    if (result)
        return result;

    if (s || s_r)
        return null;

    Expression suggestOverloading(Expression other, AggregateDeclaration ad)
    {
        error(e.loc, "no operator `%s` for type `%s`", EXPtoString(e.op).ptr, ad.toChars);
        string op = e.isEqualExp() ? "bool" : "int";
        errorSupplemental(ad.loc, "perhaps overload it with `%.*s %s(%s other) const {}`", op.fTuple.expand, id.toChars, other.type.toChars);
        return ErrorExp.get();
    }

    // Classes have opCmp and opEquals defined in `Object` to fall back on already
    if (ad1 && ad1.isStructDeclaration)
        return suggestOverloading(e.e2, ad1);
    if (ad2 && ad2.isStructDeclaration)
        return suggestOverloading(e.e1, ad2);

    return null;
}

/***********************************
 * Utility to build a function call out of this reference and argument.
 */
Expression build_overload(Loc loc, Scope* sc, Expression ethis, Expression earg, Dsymbol d)
{
    assert(d);
    Expression e;
    if (Declaration decl = d.isDeclaration())
        e = new DotVarExp(loc, ethis, decl, false);
    else
        e = new DotIdExp(loc, ethis, d.ident);
    e = new CallExp(loc, e, earg);
    e = e.expressionSemantic(sc);
    return e;
}

/***************************************
 * Search for function funcid in aggregate ad.
 */
Dsymbol search_function(ScopeDsymbol ad, Identifier funcid)
{
    if (!ad)
        return null;
    if (Dsymbol s = ad.search(Loc.initial, funcid))
    {
        //printf("search_function: s = '%s'\n", s.kind());
        Dsymbol s2 = s.toAlias();
        //printf("search_function: s2 = '%s'\n", s2.kind());
        FuncDeclaration fd = s2.isFuncDeclaration();
        if (fd && fd.type.isTypeFunction())
            return fd;
        if (TemplateDeclaration td = s2.isTemplateDeclaration())
            return td;
    }
    return null;
}

/**************************************
 * Figure out what is being foreach'd over by looking at the ForeachAggregate.
 * Params:
 *      sc = context
 *      isForeach = true for foreach, false for foreach_reverse
 *      feaggr = ForeachAggregate
 *      sapply = set to function opApply/opApplyReverse, or delegate, or null.
 *               Overload resolution is not done.
 * Returns:
 *      true if successfully figured it out; feaggr updated with semantic analysis.
 *      false for failed, which is an error.
 */
bool inferForeachAggregate(Scope* sc, bool isForeach, ref Expression feaggr, out Dsymbol sapply)
{
    //printf("inferForeachAggregate(%s)\n", feaggr.toChars());
    bool sliced;
    Type att = null;
    auto aggr = feaggr;
    while (1)
    {
        aggr = aggr.expressionSemantic(sc);
        aggr = resolveProperties(sc, aggr);
        aggr = aggr.optimize(WANTvalue);
        if (!aggr.type || aggr.isErrorExp())
            return false;
        Type tab = aggr.type.toBasetype();
        switch (tab.ty)
        {
        case Tarray:            // https://dlang.org/spec/statement.html#foreach_over_arrays
        case Tsarray:           // https://dlang.org/spec/statement.html#foreach_over_arrays
        case Ttuple:            // https://dlang.org/spec/statement.html#foreach_over_tuples
        case Taarray:           // https://dlang.org/spec/statement.html#foreach_over_associative_arrays
            break;

        case Tclass:
        case Tstruct:
        {
            AggregateDeclaration ad = isAggregate(tab);
            if (!sliced)
            {
                sapply = search_function(ad, isForeach ? Id.apply : Id.applyReverse);
                if (sapply)
                {
                    // https://dlang.org/spec/statement.html#foreach_over_struct_and_classes
                    // opApply aggregate
                    break;
                }
                if (feaggr.op != EXP.type)
                {
                    /* See if rewriting `aggr` to `aggr[]` will work
                     */
                    Expression rinit = new ArrayExp(aggr.loc, feaggr);
                    rinit = rinit.trySemantic(sc);
                    if (rinit) // if it worked
                    {
                        aggr = rinit;
                        sliced = true;  // only try it once
                        continue;
                    }
                }
            }
            if (ad.search(Loc.initial, isForeach ? Id.Ffront : Id.Fback))
            {
                // https://dlang.org/spec/statement.html#foreach-with-ranges
                // range aggregate
                break;
            }
            if (ad.aliasthis)
            {
                if (isRecursiveAliasThis(att, tab))     // error, circular alias this
                    return false;
                aggr = resolveAliasThis(sc, aggr);
                continue;
            }
            return false;
        }

        case Tdelegate:        // https://dlang.org/spec/statement.html#foreach_over_delegates
            if (auto de = aggr.isDelegateExp())
            {
                sapply = de.func;
            }
            break;

        case Terror:
            break;

        default:
            return false;
        }
        feaggr = aggr;
        return true;
    }
    assert(0);
}

/*****************************************
 * Given array of foreach parameters and an aggregate type,
 * find best opApply overload,
 * if any of the parameter types are missing, attempt to infer
 * them from the aggregate type.
 * Params:
 *      fes = the foreach statement
 *      sc = context
 *      sapply = null or opApply or delegate, overload resolution has not been done.
 *               Do overload resolution on sapply.
 * Returns:
 *      false for errors
 */
bool inferApplyArgTypes(ForeachStatement fes, Scope* sc, ref Dsymbol sapply)
{
    if (!fes.parameters || !fes.parameters.length)
        return false;
    if (sapply) // prefer opApply
    {
        foreach (Parameter p; *fes.parameters)
        {
            if (p.type)
            {
                p.type = p.type.typeSemantic(fes.loc, sc);
                p.type = p.type.addStorageClass(p.storageClass);
            }
        }

        // Determine ethis for sapply
        Expression ethis;
        Type tab = fes.aggr.type.toBasetype();
        if (tab.isTypeClass() || tab.isTypeStruct())
            ethis = fes.aggr;
        else
        {
            assert(tab.isTypeDelegate() && fes.aggr.isDelegateExp());
            ethis = fes.aggr.isDelegateExp().e1;
        }

        /* Look for like an
         *  int opApply(int delegate(ref Type [, ...]) dg);
         * overload
         */
        if (FuncDeclaration fd = sapply.isFuncDeclaration())
        {
            if (auto fdapply = findBestOpApplyMatch(ethis, fd, fes.parameters))
            {
                // Fill in any missing types on foreach parameters[]
                matchParamsToOpApply(fdapply.type.isTypeFunction(), fes.parameters, true);
                sapply = fdapply;
                return true;
            }
            return false;
        }
        return true;   // shouldn't this be false?
    }

    Parameter p = (*fes.parameters)[0];
    Type taggr = fes.aggr.type;
    assert(taggr);
    Type tab = taggr.toBasetype();
    switch (tab.ty)
    {
    case Tarray:
    case Tsarray:
    case Ttuple:
        if (fes.parameters.length == 2)
        {
            if (!p.type)
            {
                p.type = Type.tsize_t; // key type
                p.type = p.type.addStorageClass(p.storageClass);
            }
            p = (*fes.parameters)[1];
        }
        if (!p.type && !tab.isTypeTuple())
        {
            p.type = tab.nextOf(); // value type
            p.type = p.type.addStorageClass(p.storageClass);
        }
        break;

    case Taarray:
        {
            TypeAArray taa = tab.isTypeAArray();
            if (fes.parameters.length == 2)
            {
                if (!p.type)
                {
                    p.type = taa.index; // key type
                    p.type = p.type.addStorageClass(p.storageClass);
                    if (p.storageClass & STC.ref_) // key must not be mutated via ref
                        p.type = p.type.addMod(MODFlags.const_);
                }
                p = (*fes.parameters)[1];
            }
            if (!p.type)
            {
                p.type = taa.next; // value type
                p.type = p.type.addStorageClass(p.storageClass);
            }
            break;
        }

    case Tclass:
    case Tstruct:
    {
        AggregateDeclaration ad = isAggregate(tab);
        if (fes.parameters.length == 1)
        {
            if (!p.type)
            {
                /* Look for a front() or back() overload
                 */
                Identifier id = (fes.op == TOK.foreach_) ? Id.Ffront : Id.Fback;
                Dsymbol s = ad.search(Loc.initial, id);
                FuncDeclaration fd = s ? s.isFuncDeclaration() : null;
                if (fd)
                {
                    // Resolve inout qualifier of front type
                    p.type = fd.type.nextOf();
                    if (p.type)
                    {
                        p.type = p.type.substWildTo(tab.mod);
                        p.type = p.type.addStorageClass(p.storageClass);
                    }
                }
                else if (s && s.isTemplateDeclaration())
                {
                }
                else if (s && s.isDeclaration())
                    p.type = s.isDeclaration().type;
                else
                    break;
            }
            break;
        }
        break;
    }

    case Tdelegate:
    {
        auto td = tab.isTypeDelegate();
        if (!matchParamsToOpApply(td.next.isTypeFunction(), fes.parameters, true))
            return false;
        break;
    }

    default:
        break; // ignore error, caught later
    }
    return true;
}

/*********************************************
 * Find best overload match on fstart given ethis and parameters[].
 * Params:
 *      ethis = expression to use for `this`
 *      fstart = opApply or foreach delegate
 *      parameters = ForeachTypeList (i.e. foreach parameters)
 * Returns:
 *      best match if there is one, null if error
 */
private FuncDeclaration findBestOpApplyMatch(Expression ethis, FuncDeclaration fstart, Parameters* parameters)
{
    MOD mod = ethis.type.mod;
    MATCH match = MATCH.nomatch;
    FuncDeclaration fd_best;
    FuncDeclaration fd_ambig;

    overloadApply(fstart, (Dsymbol s)
    {
        auto f = s.isFuncDeclaration();
        if (!f)
            return 0;           // continue
        auto tf = f.type.isTypeFunction();
        MATCH m = MATCH.exact;
        if (f.isThis())
        {
            if (!MODimplicitConv(mod, tf.mod))
                m = MATCH.nomatch;
            else if (mod != tf.mod)
                m = MATCH.constant;
        }
        if (!matchParamsToOpApply(tf, parameters, false))
            m = MATCH.nomatch;
        if (m > match)
        {
            fd_best = f;
            fd_ambig = null;
            match = m;
        }
        else if (m == match && m > MATCH.nomatch)
        {
            assert(fd_best);
            auto bestTf = fd_best.type.isTypeFunction();
            assert(bestTf);

            // Found another overload with different attributes?
            // e.g. @system vs. @safe opApply
            // @@@DEPRECATED_2.112@@@
            // See semantic2.d Semantic2Visitor.visit(FuncDeclaration):
            // Remove `false` after deprecation period is over.
            bool ambig = tf.attributesEqual(bestTf, false);

            // opApplies with identical attributes could still accept
            // different function bodies as delegate
            // => different parameters or attributes
            if (ambig)
            {
                // Fetch the delegates that receive the function body
                auto tfBody = tf.parameterList[0].type.isTypeDelegate().next;
                assert(tfBody);

                auto bestBody = bestTf.parameterList[0].type.isTypeDelegate().next;
                assert(bestBody);

                // Ignore covariant matches, as later on it can be redone
                // after the opApply delegate has its attributes inferred.
                ambig = !(tfBody.covariant(bestBody) == Covariant.yes || bestBody.covariant(tfBody) == Covariant.yes);
            }

            if (ambig)
                fd_ambig = f;                           // not covariant, so ambiguous
        }
        return 0;               // continue
    });

    if (fd_ambig)
    {
        .error(ethis.loc, "`%s.%s` matches more than one declaration:",
            ethis.toChars(), fstart.ident.toChars());
        .errorSupplemental(fd_best.loc, "`%s`\nand:", fd_best.type.toChars());
        .errorSupplemental(fd_ambig.loc, "`%s`", fd_ambig.type.toChars());
        return null;
    }

    return fd_best;
}

/******************************
 * Determine if foreach parameters match opApply parameters.
 * Infer missing foreach parameter types from type of opApply delegate.
 * Params:
 *      tf = type of opApply or delegate
 *      parameters = foreach parameters
 *      infer = infer missing parameter types
 * Returns:
 *      true for match for this function
 *      false for no match for this function
 */
private bool matchParamsToOpApply(TypeFunction tf, Parameters* parameters, bool infer)
{
    enum nomatch = false;

    /* opApply/delegate has exactly one parameter, and that parameter
     * is a delegate that looks like:
     *     int opApply(int delegate(ref Type [, ...]) dg);
     */
    if (tf.parameterList.length != 1)
        return nomatch;

    /* Get the type of opApply's dg parameter
     */
    Parameter p0 = tf.parameterList[0];
    auto de = p0.type.isTypeDelegate();
    if (!de)
        return nomatch;
    TypeFunction tdg = de.next.isTypeFunction();

    /* We now have tdg, the type of the delegate.
     * tdg's parameters must match that of the foreach arglist (i.e. parameters).
     * Fill in missing types in parameters.
     */
    const nparams = tdg.parameterList.length;
    if (nparams == 0 || nparams != parameters.length || tdg.parameterList.varargs != VarArg.none)
        return nomatch; // parameter mismatch

    foreach (u, p; *parameters)
    {
        Parameter param = tdg.parameterList[u];
        if (p.type)
        {
            if (!p.type.equals(param.type))
                return nomatch;
        }
        else if (infer)
        {
            p.type = param.type;
            p.type = p.type.addStorageClass(p.storageClass);
        }
    }
    return true;
}

/**
 * Reverse relational operator, eg >= becomes <=
 * Note this is not negation.
 * Params:
 *      op = comparison operator to reverse
 * Returns:
 *      reverse of op
 */
private EXP reverseRelation(EXP op) pure @safe
{
    switch (op)
    {
        case EXP.greaterOrEqual:  op = EXP.lessOrEqual;    break;
        case EXP.greaterThan:     op = EXP.lessThan;       break;
        case EXP.lessOrEqual:     op = EXP.greaterOrEqual; break;
        case EXP.lessThan:        op = EXP.greaterThan;    break;
        default:                  break;
    }
    return op;
}
