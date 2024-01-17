/**
 * Handles operator overloading.
 *
 * Specification: $(LINK2 https://dlang.org/spec/operatoroverloading.html, Operator Overloading)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/opover.d, _opover.d)
 * Documentation:  https://dlang.org/phobos/dmd_opover.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/opover.d
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
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.optimize;
import dmd.statement;
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

/***********************************
 * Get Identifier for operator overload.
 */
private Identifier opId(Expression e)
{
    switch (e.op)
    {
    case EXP.uadd:                      return Id.uadd;
    case EXP.negate:                    return Id.neg;
    case EXP.tilde:                     return Id.com;
    case EXP.cast_:                     return Id._cast;
    case EXP.in_:                       return Id.opIn;
    case EXP.plusPlus:                  return Id.postinc;
    case EXP.minusMinus:                return Id.postdec;
    case EXP.add:                       return Id.add;
    case EXP.min:                       return Id.sub;
    case EXP.mul:                       return Id.mul;
    case EXP.div:                       return Id.div;
    case EXP.mod:                       return Id.mod;
    case EXP.pow:                       return Id.pow;
    case EXP.leftShift:                 return Id.shl;
    case EXP.rightShift:                return Id.shr;
    case EXP.unsignedRightShift:        return Id.ushr;
    case EXP.and:                       return Id.iand;
    case EXP.or:                        return Id.ior;
    case EXP.xor:                       return Id.ixor;
    case EXP.concatenate:               return Id.cat;
    case EXP.assign:                    return Id.assign;
    case EXP.addAssign:                 return Id.addass;
    case EXP.minAssign:                 return Id.subass;
    case EXP.mulAssign:                 return Id.mulass;
    case EXP.divAssign:                 return Id.divass;
    case EXP.modAssign:                 return Id.modass;
    case EXP.powAssign:                 return Id.powass;
    case EXP.leftShiftAssign:           return Id.shlass;
    case EXP.rightShiftAssign:          return Id.shrass;
    case EXP.unsignedRightShiftAssign:  return Id.ushrass;
    case EXP.andAssign:                 return Id.andass;
    case EXP.orAssign:                  return Id.orass;
    case EXP.xorAssign:                 return Id.xorass;
    case EXP.concatenateAssign:         return Id.catass;
    case EXP.equal:                     return Id.eq;
    case EXP.lessThan:
    case EXP.lessOrEqual:
    case EXP.greaterThan:
    case EXP.greaterOrEqual:            return Id.cmp;
    case EXP.array:                     return Id.index;
    case EXP.star:                      return Id.opStar;
    default:                            assert(0);
    }
}

/***********************************
 * Get Identifier for reverse operator overload,
 * `null` if not supported for this operator.
 */
private Identifier opId_r(Expression e)
{
    switch (e.op)
    {
    case EXP.in_:               return Id.opIn_r;
    case EXP.add:               return Id.add_r;
    case EXP.min:               return Id.sub_r;
    case EXP.mul:               return Id.mul_r;
    case EXP.div:               return Id.div_r;
    case EXP.mod:               return Id.mod_r;
    case EXP.pow:               return Id.pow_r;
    case EXP.leftShift:         return Id.shl_r;
    case EXP.rightShift:        return Id.shr_r;
    case EXP.unsignedRightShift:return Id.ushr_r;
    case EXP.and:               return Id.iand_r;
    case EXP.or:                return Id.ior_r;
    case EXP.xor:               return Id.ixor_r;
    case EXP.concatenate:       return Id.cat_r;
    default:                    return null;
    }
}

/*******************************************
 * Helper function to turn operator into template argument list
 */
Objects* opToArg(Scope* sc, EXP op)
{
    /* Remove the = from op=
     */
    switch (op)
    {
    case EXP.addAssign:
        op = EXP.add;
        break;
    case EXP.minAssign:
        op = EXP.min;
        break;
    case EXP.mulAssign:
        op = EXP.mul;
        break;
    case EXP.divAssign:
        op = EXP.div;
        break;
    case EXP.modAssign:
        op = EXP.mod;
        break;
    case EXP.andAssign:
        op = EXP.and;
        break;
    case EXP.orAssign:
        op = EXP.or;
        break;
    case EXP.xorAssign:
        op = EXP.xor;
        break;
    case EXP.leftShiftAssign:
        op = EXP.leftShift;
        break;
    case EXP.rightShiftAssign:
        op = EXP.rightShift;
        break;
    case EXP.unsignedRightShiftAssign:
        op = EXP.unsignedRightShift;
        break;
    case EXP.concatenateAssign:
        op = EXP.concatenate;
        break;
    case EXP.powAssign:
        op = EXP.pow;
        break;
    default:
        break;
    }
    Expression e = new StringExp(Loc.initial, EXPtoString(op));
    e = e.expressionSemantic(sc);
    auto tiargs = new Objects();
    tiargs.push(e);
    return tiargs;
}

// Try alias this on first operand
private Expression checkAliasThisForLhs(AggregateDeclaration ad, Scope* sc, BinExp e)
{
    if (!ad || !ad.aliasthis)
        return null;

    /* Rewrite (e1 op e2) as:
     *      (e1.aliasthis op e2)
     */
    if (isRecursiveAliasThis(e.att1, e.e1.type))
        return null;
    //printf("att %s e1 = %s\n", Token.toChars(e.op), e.e1.type.toChars());
    BinExp be = cast(BinExp)e.copy();
    // Resolve 'alias this' but in case of assigment don't resolve properties yet
    // because 'e1 = e2' could mean 'e1(e2)' or 'e1() = e2'
    bool findOnly = (e.op == EXP.assign);
    be.e1 = resolveAliasThis(sc, e.e1, true, findOnly);
    if (!be.e1)
        return null;

    Expression result;
    if (be.op == EXP.concatenateAssign)
        result = be.op_overload(sc);
    else
        result = be.trySemantic(sc);

    return result;
}

// Try alias this on second operand
private Expression checkAliasThisForRhs(AggregateDeclaration ad, Scope* sc, BinExp e)
{
    if (!ad || !ad.aliasthis)
        return null;
    /* Rewrite (e1 op e2) as:
     *      (e1 op e2.aliasthis)
     */
    if (isRecursiveAliasThis(e.att2, e.e2.type))
        return null;
    //printf("att %s e2 = %s\n", Token.toChars(e.op), e.e2.type.toChars());
    BinExp be = cast(BinExp)e.copy();
    be.e2 = resolveAliasThis(sc, e.e2, true);
    if (!be.e2)
        return null;

    Expression result;
    if (be.op == EXP.concatenateAssign)
        result = be.op_overload(sc);
    else
        result = be.trySemantic(sc);

    return result;
}

/************************************
 * Operator overload.
 * Check for operator overload, if so, replace
 * with function call.
 * Params:
 *      e = expression with operator
 *      sc = context
 *      pop = if not null, is set to the operator that was actually overloaded,
 *            which may not be `e.op`. Happens when operands are reversed to
 *            match an overload
 * Returns:
 *      `null` if not an operator overload,
 *      otherwise the lowered expression
 */
Expression op_overload(Expression e, Scope* sc, EXP* pop = null)
{
        Expression visit(Expression e)
        {
            assert(0);
        }

        Expression visitUna(UnaExp e)
        {
            //printf("UnaExp::op_overload() (%s)\n", e.toChars());
            Expression result;
            if (auto ae = e.e1.isArrayExp())
            {
                ae.e1 = ae.e1.expressionSemantic(sc);
                ae.e1 = resolveProperties(sc, ae.e1);
                Expression ae1old = ae.e1;
                const(bool) maybeSlice = (ae.arguments.length == 0 || ae.arguments.length == 1 && (*ae.arguments)[0].op == EXP.interval);
                IntervalExp ie = null;
                if (maybeSlice && ae.arguments.length)
                {
                    ie = (*ae.arguments)[0].isIntervalExp();
                }
                Type att = null; // first cyclic `alias this` type
                while (true)
                {
                    if (ae.e1.op == EXP.error)
                    {
                        return ae.e1;
                    }
                    Expression e0 = null;
                    Expression ae1save = ae.e1;
                    ae.lengthVar = null;
                    Type t1b = ae.e1.type.toBasetype();
                    AggregateDeclaration ad = isAggregate(t1b);
                    if (!ad)
                        break;
                    if (search_function(ad, Id.opIndexUnary))
                    {
                        // Deal with $
                        result = resolveOpDollar(sc, ae, &e0);
                        if (!result) // op(a[i..j]) might be: a.opSliceUnary!(op)(i, j)
                            goto Lfallback;
                        if (result.op == EXP.error)
                            return result;
                        /* Rewrite op(a[arguments]) as:
                         *      a.opIndexUnary!(op)(arguments)
                         */
                        Expressions* a = ae.arguments.copy();
                        Objects* tiargs = opToArg(sc, e.op);
                        result = new DotTemplateInstanceExp(e.loc, ae.e1, Id.opIndexUnary, tiargs);
                        result = new CallExp(e.loc, result, a);
                        if (maybeSlice) // op(a[]) might be: a.opSliceUnary!(op)()
                            result = result.trySemantic(sc);
                        else
                            result = result.expressionSemantic(sc);
                        if (result)
                        {
                            return Expression.combine(e0, result);
                        }
                    }
                Lfallback:
                    if (maybeSlice && search_function(ad, Id.opSliceUnary))
                    {
                        // Deal with $
                        result = resolveOpDollar(sc, ae, ie, &e0);
                        if (result.op == EXP.error)
                            return result;
                        /* Rewrite op(a[i..j]) as:
                         *      a.opSliceUnary!(op)(i, j)
                         */
                        auto a = new Expressions();
                        if (ie)
                        {
                            a.push(ie.lwr);
                            a.push(ie.upr);
                        }
                        Objects* tiargs = opToArg(sc, e.op);
                        result = new DotTemplateInstanceExp(e.loc, ae.e1, Id.opSliceUnary, tiargs);
                        result = new CallExp(e.loc, result, a);
                        result = result.expressionSemantic(sc);
                        result = Expression.combine(e0, result);
                        return result;
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
                if (e.e1.op == EXP.error)
                {
                    return e.e1;
                }

                AggregateDeclaration ad = isAggregate(e.e1.type);
                if (!ad)
                    break;

                Dsymbol fd = null;
                /* Rewrite as:
                 *      e1.opUnary!(op)()
                 */
                fd = search_function(ad, Id.opUnary);
                if (fd)
                {
                    Objects* tiargs = opToArg(sc, e.op);
                    result = new DotTemplateInstanceExp(e.loc, e.e1, fd.ident, tiargs);
                    result = new CallExp(e.loc, result);
                    result = result.expressionSemantic(sc);
                    return result;
                }
                // D1-style operator overloads, deprecated
                if (e.op != EXP.prePlusPlus && e.op != EXP.preMinusMinus)
                {
                    auto id = opId(e);
                    fd = search_function(ad, id);
                    if (fd)
                    {
                        // @@@DEPRECATED_2.110@@@.
                        // Deprecated in 2.088, made an error in 2.100
                        error(e.loc, "`%s` is obsolete.  Use `opUnary(string op)() if (op == \"%s\")` instead.", id.toChars(), EXPtoString(e.op).ptr);
                        return ErrorExp.get();
                    }
                }
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
                break;
            }
            return result;
        }

        Expression visitArray(ArrayExp ae)
        {
            //printf("ArrayExp::op_overload() (%s)\n", ae.toChars());
            ae.e1 = ae.e1.expressionSemantic(sc);
            ae.e1 = resolveProperties(sc, ae.e1);
            Expression ae1old = ae.e1;
            const(bool) maybeSlice = (ae.arguments.length == 0 || ae.arguments.length == 1 && (*ae.arguments)[0].op == EXP.interval);
            IntervalExp ie = null;
            if (maybeSlice && ae.arguments.length)
            {
                ie = (*ae.arguments)[0].isIntervalExp();
            }
            Expression result;
            Type att = null; // first cyclic `alias this` type
            while (true)
            {
                if (ae.e1.op == EXP.error)
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
                    if (isIndexableNonAggregate(t1b) || ae.e1.op == EXP.type)
                    {
                        // Convert to SliceExp
                        if (maybeSlice)
                        {
                            result = new SliceExp(ae.loc, ae.e1, ie);
                            result = result.expressionSemantic(sc);
                            return result;
                        }
                        // Convert to IndexExp
                        if (ae.arguments.length == 1)
                        {
                            result = new IndexExp(ae.loc, ae.e1, (*ae.arguments)[0]);
                            result = result.expressionSemantic(sc);
                            return result;
                        }
                    }
                    break;
                }
                if (search_function(ad, Id.index))
                {
                    // Deal with $
                    result = resolveOpDollar(sc, ae, &e0);
                    if (!result) // a[i..j] might be: a.opSlice(i, j)
                        goto Lfallback;
                    if (result.op == EXP.error)
                        return result;
                    /* Rewrite e1[arguments] as:
                     *      e1.opIndex(arguments)
                     */
                    Expressions* a = ae.arguments.copy();
                    result = new DotIdExp(ae.loc, ae.e1, Id.index);
                    result = new CallExp(ae.loc, result, a);
                    if (maybeSlice) // a[] might be: a.opSlice()
                        result = result.trySemantic(sc);
                    else
                        result = result.expressionSemantic(sc);
                    if (result)
                    {
                        return Expression.combine(e0, result);
                    }
                }
            Lfallback:
                if (maybeSlice && ae.e1.op == EXP.type)
                {
                    result = new SliceExp(ae.loc, ae.e1, ie);
                    result = result.expressionSemantic(sc);
                    result = Expression.combine(e0, result);
                    return result;
                }
                if (maybeSlice && search_function(ad, Id.slice))
                {
                    // Deal with $
                    result = resolveOpDollar(sc, ae, ie, &e0);

                    if (result.op == EXP.error)
                    {
                        if (!e0 && !search_function(ad, Id.dollar)) {
                            ae.loc.errorSupplemental("Aggregate declaration '%s' does not define 'opDollar'", ae.e1.toChars());
                        }
                        return result;
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
                    result = new DotIdExp(ae.loc, ae.e1, Id.slice);
                    result = new CallExp(ae.loc, result, a);
                    result = result.expressionSemantic(sc);
                    result = Expression.combine(e0, result);
                    return result;
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
            return result;
        }

        /***********************************************
         * This is mostly the same as UnaryExp::op_overload(), but has
         * a different rewrite.
         */
        Expression visitCast(CastExp e, Type att = null)
        {
            //printf("CastExp::op_overload() (%s)\n", e.toChars());
            Expression result;
            AggregateDeclaration ad = isAggregate(e.e1.type);
            if (ad)
            {
                Dsymbol fd = null;
                /* Rewrite as:
                 *      e1.opCast!(T)()
                 */
                fd = search_function(ad, Id._cast);
                if (fd)
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
                    result = new DotTemplateInstanceExp(e.loc, e.e1, fd.ident, tiargs);
                    result = new CallExp(e.loc, result);
                    result = result.expressionSemantic(sc);
                    return result;
                }
                // Didn't find it. Forward to aliasthis
                if (ad.aliasthis && !isRecursiveAliasThis(att, e.e1.type))
                {
                    /* Rewrite op(e1) as:
                     *      op(e1.aliasthis)
                     */
                    if (auto e1 = resolveAliasThis(sc, e.e1, true))
                    {
                        result = e.copy();
                        (cast(UnaExp)result).e1 = e1;
                        result = visitCast(result.isCastExp(), att);
                        return result;
                    }
                }
            }
            return result;
        }

        Expression visitBin(BinExp e)
        {
            //printf("BinExp::op_overload() (%s)\n", e.toChars());
            Identifier id = opId(e);
            Identifier id_r = opId_r(e);
            int argsset = 0;
            AggregateDeclaration ad1 = isAggregate(e.e1.type);
            AggregateDeclaration ad2 = isAggregate(e.e2.type);
            if (e.op == EXP.assign && ad1 == ad2)
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
            Dsymbol s = null;
            Dsymbol s_r = null;
            Objects* tiargs = null;
            if (e.op == EXP.plusPlus || e.op == EXP.minusMinus)
            {
                // Bug4099 fix
                if (ad1 && search_function(ad1, Id.opUnary))
                    return null;
            }
            if (e.op != EXP.equal && e.op != EXP.notEqual && e.op != EXP.assign && e.op != EXP.plusPlus && e.op != EXP.minusMinus)
            {
                /* Try opBinary and opBinaryRight
                 */
                if (ad1)
                {
                    s = search_function(ad1, Id.opBinary);
                    if (s && !s.isTemplateDeclaration())
                    {
                        error(e.e1.loc, "`%s.opBinary` isn't a template", e.e1.toChars());
                        return ErrorExp.get();
                    }
                }
                if (ad2)
                {
                    s_r = search_function(ad2, Id.opBinaryRight);
                    if (s_r && !s_r.isTemplateDeclaration())
                    {
                        error(e.e2.loc, "`%s.opBinaryRight` isn't a template", e.e2.toChars());
                        return ErrorExp.get();
                    }
                    if (s_r && s_r == s) // https://issues.dlang.org/show_bug.cgi?id=12778
                        s_r = null;
                }
                // Set tiargs, the template argument list, which will be the operator string
                if (s || s_r)
                {
                    id = Id.opBinary;
                    id_r = Id.opBinaryRight;
                    tiargs = opToArg(sc, e.op);
                }
            }
            if (!s && !s_r)
            {
                // Try the D1-style operators, deprecated
                if (ad1 && id)
                {
                    s = search_function(ad1, id);
                    if (s && id != Id.assign)
                    {
                        // @@@DEPRECATED_2.110@@@.
                        // Deprecated in 2.088, made an error in 2.100
                        if (id == Id.postinc || id == Id.postdec)
                            error(e.loc, "`%s` is obsolete.  Use `opUnary(string op)() if (op == \"%s\")` instead.", id.toChars(), EXPtoString(e.op).ptr);
                        else
                            error(e.loc, "`%s` is obsolete.  Use `opBinary(string op)(...) if (op == \"%s\")` instead.", id.toChars(), EXPtoString(e.op).ptr);
                        return ErrorExp.get();
                    }
                }
                if (ad2 && id_r)
                {
                    s_r = search_function(ad2, id_r);
                    // https://issues.dlang.org/show_bug.cgi?id=12778
                    // If both x.opBinary(y) and y.opBinaryRight(x) found,
                    // and they are exactly same symbol, x.opBinary(y) should be preferred.
                    if (s_r && s_r == s)
                        s_r = null;
                    if (s_r)
                    {
                        // @@@DEPRECATED_2.110@@@.
                        // Deprecated in 2.088, made an error in 2.100
                        error(e.loc, "`%s` is obsolete.  Use `opBinaryRight(string op)(...) if (op == \"%s\")` instead.", id_r.toChars(), EXPtoString(e.op).ptr);
                        return ErrorExp.get();
                    }
                }
            }
            Expressions* args1 = new Expressions();
            Expressions* args2 = new Expressions();
            if (s || s_r)
            {
                /* Try:
                 *      a.opfunc(b)
                 *      b.opfunc_r(a)
                 * and see which is better.
                 */
                args1.setDim(1);
                (*args1)[0] = e.e1;
                expandTuples(args1);
                args2.setDim(1);
                (*args2)[0] = e.e2;
                expandTuples(args2);
                argsset = 1;
                MatchAccumulator m;
                if (s)
                {
                    functionResolve(m, s, e.loc, sc, tiargs, e.e1.type, ArgumentList(args2));
                    if (m.lastf && (m.lastf.errors || m.lastf.hasSemantic3Errors()))
                    {
                        return ErrorExp.get();
                    }
                }
                FuncDeclaration lastf = m.lastf;
                if (s_r)
                {
                    functionResolve(m, s_r, e.loc, sc, tiargs, e.e2.type, ArgumentList(args1));
                    if (m.lastf && (m.lastf.errors || m.lastf.hasSemantic3Errors()))
                    {
                        return ErrorExp.get();
                    }
                }
                if (m.count > 1)
                {
                    // Error, ambiguous
                    error(e.loc, "overloads `%s` and `%s` both match argument list for `%s`", m.lastf.type.toChars(), m.nextf.type.toChars(), m.lastf.toChars());
                }
                else if (m.last == MATCH.nomatch)
                {
                    if (tiargs)
                        goto L1;
                    m.lastf = null;
                }
                if (e.op == EXP.plusPlus || e.op == EXP.minusMinus)
                {
                    // Kludge because operator overloading regards e++ and e--
                    // as unary, but it's implemented as a binary.
                    // Rewrite (e1 ++ e2) as e1.postinc()
                    // Rewrite (e1 -- e2) as e1.postdec()
                    return build_overload(e.loc, sc, e.e1, null, m.lastf ? m.lastf : s);
                }
                else if (lastf && m.lastf == lastf || !s_r && m.last == MATCH.nomatch)
                {
                    // Rewrite (e1 op e2) as e1.opfunc(e2)
                    return build_overload(e.loc, sc, e.e1, e.e2, m.lastf ? m.lastf : s);
                }
                else
                {
                    // Rewrite (e1 op e2) as e2.opfunc_r(e1)
                    return build_overload(e.loc, sc, e.e2, e.e1, m.lastf ? m.lastf : s_r);
                }
            }
        L1:
            version (all)
            {
                // Retained for D1 compatibility
                if (isCommutative(e.op) && !tiargs)
                {
                    s = null;
                    s_r = null;
                    if (ad1 && id_r)
                    {
                        s_r = search_function(ad1, id_r);
                    }
                    if (ad2 && id)
                    {
                        s = search_function(ad2, id);
                        if (s && s == s_r) // https://issues.dlang.org/show_bug.cgi?id=12778
                            s = null;
                    }
                    if (s || s_r)
                    {
                        /* Try:
                         *  a.opfunc_r(b)
                         *  b.opfunc(a)
                         * and see which is better.
                         */
                        if (!argsset)
                        {
                            args1.setDim(1);
                            (*args1)[0] = e.e1;
                            expandTuples(args1);
                            args2.setDim(1);
                            (*args2)[0] = e.e2;
                            expandTuples(args2);
                        }
                        MatchAccumulator m;
                        if (s_r)
                        {
                            functionResolve(m, s_r, e.loc, sc, tiargs, e.e1.type, ArgumentList(args2));
                            if (m.lastf && (m.lastf.errors || m.lastf.hasSemantic3Errors()))
                            {
                                return ErrorExp.get();
                            }
                        }
                        FuncDeclaration lastf = m.lastf;
                        if (s)
                        {
                            functionResolve(m, s, e.loc, sc, tiargs, e.e2.type, ArgumentList(args1));
                            if (m.lastf && (m.lastf.errors || m.lastf.hasSemantic3Errors()))
                            {
                                return ErrorExp.get();
                            }
                        }
                        if (m.count > 1)
                        {
                            // Error, ambiguous
                            error(e.loc, "overloads `%s` and `%s` both match argument list for `%s`", m.lastf.type.toChars(), m.nextf.type.toChars(), m.lastf.toChars());
                        }
                        else if (m.last == MATCH.nomatch)
                        {
                            m.lastf = null;
                        }

                        if (lastf && m.lastf == lastf || !s && m.last == MATCH.nomatch)
                        {
                            // Rewrite (e1 op e2) as e1.opfunc_r(e2)
                            return build_overload(e.loc, sc, e.e1, e.e2, m.lastf ? m.lastf : s_r);
                        }
                        else
                        {
                            // Rewrite (e1 op e2) as e2.opfunc(e1)
                            Expression result = build_overload(e.loc, sc, e.e2, e.e1, m.lastf ? m.lastf : s);
                            // When reversing operands of comparison operators,
                            // need to reverse the sense of the op
                            if (pop)
                                *pop = reverseRelation(e.op);
                            return result;
                        }
                    }
                }
            }

            Expression rewrittenLhs;
            if (!(e.op == EXP.assign && ad2 && ad1 == ad2)) // https://issues.dlang.org/show_bug.cgi?id=2943
            {
                if (Expression result = checkAliasThisForLhs(ad1, sc, e))
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
                    if (result.op != EXP.assign)
                        return result;     // i.e: Rewrote `e1 = e2` -> `e1(e2)`

                    auto ae = result.isAssignExp();
                    if (ae.e1.op != EXP.dotVariable)
                        return result;     // i.e: Rewrote `e1 = e2` -> `e1() = e2`

                    auto dve = ae.e1.isDotVarExp();
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
            if (!(e.op == EXP.assign && ad1 && ad1 == ad2)) // https://issues.dlang.org/show_bug.cgi?id=2943
            {
                if (Expression result = checkAliasThisForRhs(ad2, sc, e))
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

        Expression visitEqual(EqualExp e)
        {
            //printf("EqualExp::op_overload() (%s)\n", e.toChars());
            Type t1 = e.e1.type.toBasetype();
            Type t2 = e.e2.type.toBasetype();

            /* Array equality is handled by expressionSemantic() potentially
             * lowering to object.__equals(), which takes care of overloaded
             * operators for the element types.
             */
            if ((t1.ty == Tarray || t1.ty == Tsarray) &&
                (t2.ty == Tarray || t2.ty == Tsarray))
            {
                return null;
            }

            /* Check for class equality with null literal or typeof(null).
             */
            if (t1.ty == Tclass && e.e2.op == EXP.null_ ||
                t2.ty == Tclass && e.e1.op == EXP.null_)
            {
                error(e.loc, "use `%s` instead of `%s` when comparing with `null`",
                    EXPtoString(e.op == EXP.equal ? EXP.identity : EXP.notIdentity).ptr,
                    EXPtoString(e.op).ptr);
                return ErrorExp.get();
            }
            if (t1.ty == Tclass && t2.ty == Tnull ||
                t1.ty == Tnull && t2.ty == Tclass)
            {
                // Comparing a class with typeof(null) should not call opEquals
                return null;
            }

            /* Check for class equality.
             */
            if (t1.ty == Tclass && t2.ty == Tclass)
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
                    result = new DotIdExp(e.loc, result, Id.eq);
                    result = new CallExp(e.loc, result, e1x, e2x);
                    if (e.op == EXP.notEqual)
                        result = new NotExp(e.loc, result);
                    result = result.expressionSemantic(sc);
                    return result;
                }
            }

            if (Expression result = compare_overload(e, sc, Id.eq, null))
            {
                if (lastComma(result).op == EXP.call && e.op == EXP.notEqual)
                {
                    result = new NotExp(result.loc, result);
                    result = result.expressionSemantic(sc);
                }
                return result;
            }

            /* Check for pointer equality.
             */
            if (t1.ty == Tpointer || t2.ty == Tpointer)
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
            if (t1.ty == Tstruct && t2.ty == Tstruct)
            {
                auto sd = t1.isTypeStruct().sym;
                if (sd != t2.isTypeStruct().sym)
                    return null;

                import dmd.clone : needOpEquals;
                if (global.params.fieldwise != FeatureState.enabled && !needOpEquals(sd))
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
                sc2.flags |= SCOPE.noaccesscheck;
                Expression r = e.expressionSemantic(sc2);
                sc2.pop();
                return r;
            }

            /* Check for tuple equality.
             */
            if (e.e1.op == EXP.tuple && e.e2.op == EXP.tuple)
            {
                auto tup1 = e.e1.isTupleExp();
                auto tup2 = e.e2.isTupleExp();
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

        Expression visitCmp(CmpExp e)
        {
            //printf("CmpExp:: () (%s)\n", e.toChars());
            return compare_overload(e, sc, Id.cmp, pop);
        }

        /*********************************
         * Operator overloading for op=
         */
        Expression visitBinAssign(BinAssignExp e)
        {
            //printf("BinAssignExp::op_overload() (%s)\n", e.toChars());
            if (auto ae = e.e1.isArrayExp())
            {
                ae.e1 = ae.e1.expressionSemantic(sc);
                ae.e1 = resolveProperties(sc, ae.e1);
                Expression ae1old = ae.e1;
                const(bool) maybeSlice = (ae.arguments.length == 0 || ae.arguments.length == 1 && (*ae.arguments)[0].op == EXP.interval);
                IntervalExp ie = null;
                if (maybeSlice && ae.arguments.length)
                {
                    ie = (*ae.arguments)[0].isIntervalExp();
                }
                Type att = null; // first cyclic `alias this` type
                while (true)
                {
                    if (ae.e1.op == EXP.error)
                    {
                        return ae.e1;
                    }
                    Expression e0 = null;
                    Expression ae1save = ae.e1;
                    ae.lengthVar = null;
                    Type t1b = ae.e1.type.toBasetype();
                    AggregateDeclaration ad = isAggregate(t1b);
                    if (!ad)
                        break;
                    if (search_function(ad, Id.opIndexOpAssign))
                    {
                        // Deal with $
                        Expression result = resolveOpDollar(sc, ae, &e0);
                        if (!result) // (a[i..j] op= e2) might be: a.opSliceOpAssign!(op)(e2, i, j)
                            goto Lfallback;
                        if (result.op == EXP.error)
                            return result;
                        result = e.e2.expressionSemantic(sc);
                        if (result.op == EXP.error)
                            return result;
                        e.e2 = result;
                        /* Rewrite a[arguments] op= e2 as:
                         *      a.opIndexOpAssign!(op)(e2, arguments)
                         */
                        Expressions* a = ae.arguments.copy();
                        a.insert(0, e.e2);
                        Objects* tiargs = opToArg(sc, e.op);
                        result = new DotTemplateInstanceExp(e.loc, ae.e1, Id.opIndexOpAssign, tiargs);
                        result = new CallExp(e.loc, result, a);
                        if (maybeSlice) // (a[] op= e2) might be: a.opSliceOpAssign!(op)(e2)
                            result = result.trySemantic(sc);
                        else
                            result = result.expressionSemantic(sc);
                        if (result)
                        {
                            return Expression.combine(e0, result);
                        }
                    }
                Lfallback:
                    if (maybeSlice && search_function(ad, Id.opSliceOpAssign))
                    {
                        // Deal with $
                        Expression result = resolveOpDollar(sc, ae, ie, &e0);
                        if (result.op == EXP.error)
                            return result;
                        result = e.e2.expressionSemantic(sc);
                        if (result.op == EXP.error)
                            return result;
                        e.e2 = result;
                        /* Rewrite (a[i..j] op= e2) as:
                         *      a.opSliceOpAssign!(op)(e2, i, j)
                         */
                        auto a = new Expressions();
                        a.push(e.e2);
                        if (ie)
                        {
                            a.push(ie.lwr);
                            a.push(ie.upr);
                        }
                        Objects* tiargs = opToArg(sc, e.op);
                        result = new DotTemplateInstanceExp(e.loc, ae.e1, Id.opSliceOpAssign, tiargs);
                        result = new CallExp(e.loc, result, a);
                        result = result.expressionSemantic(sc);
                        result = Expression.combine(e0, result);
                        return result;
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
            Expression result = e.binSemanticProp(sc);
            if (result)
                return result;
            // Don't attempt 'alias this' if an error occurred
            if (e.e1.type.ty == Terror || e.e2.type.ty == Terror)
            {
                return ErrorExp.get();
            }
            Identifier id = opId(e);
            Expressions* args2 = new Expressions();
            AggregateDeclaration ad1 = isAggregate(e.e1.type);
            Dsymbol s = null;
            Objects* tiargs = null;
            /* Try opOpAssign
             */
            if (ad1)
            {
                s = search_function(ad1, Id.opOpAssign);
                if (s && !s.isTemplateDeclaration())
                {
                    error(e.loc, "`%s.opOpAssign` isn't a template", e.e1.toChars());
                    return ErrorExp.get();
                }
            }
            // Set tiargs, the template argument list, which will be the operator string
            if (s)
            {
                id = Id.opOpAssign;
                tiargs = opToArg(sc, e.op);
            }

            // Try D1-style operator overload, deprecated
            if (!s && ad1 && id)
            {
                s = search_function(ad1, id);
                if (s)
                {
                    // @@@DEPRECATED_2.110@@@.
                    // Deprecated in 2.088, made an error in 2.100
                    scope char[] op = EXPtoString(e.op).dup;
                    op[$-1] = '\0'; // remove trailing `=`
                    error(e.loc, "`%s` is obsolete.  Use `opOpAssign(string op)(...) if (op == \"%s\")` instead.", id.toChars(), op.ptr);
                    return ErrorExp.get();
                }
            }

            if (s)
            {
                /* Try:
                 *      a.opOpAssign(b)
                 */
                args2.setDim(1);
                (*args2)[0] = e.e2;
                expandTuples(args2);
                MatchAccumulator m;
                functionResolve(m, s, e.loc, sc, tiargs, e.e1.type, ArgumentList(args2));
                if (m.lastf && (m.lastf.errors || m.lastf.hasSemantic3Errors()))
                {
                    return ErrorExp.get();
                }
                if (m.count > 1)
                {
                    // Error, ambiguous
                    error(e.loc, "overloads `%s` and `%s` both match argument list for `%s`", m.lastf.type.toChars(), m.nextf.type.toChars(), m.lastf.toChars());
                }
                else if (m.last == MATCH.nomatch)
                {
                    if (tiargs)
                        goto L1;
                    m.lastf = null;
                }
                // Rewrite (e1 op e2) as e1.opOpAssign(e2)
                return build_overload(e.loc, sc, e.e1, e.e2, m.lastf ? m.lastf : s);
            }
        L1:
            result = checkAliasThisForLhs(ad1, sc, e);
            if (result || !s) // no point in trying Rhs alias-this if there's no overload of any kind in lhs
                return result;

            return checkAliasThisForRhs(isAggregate(e.e2.type), sc, e);
        }

    if (pop)
        *pop = e.op;

    switch (e.op)
    {
        case EXP.cast_         : return visitCast(e.isCastExp());
        case EXP.array         : return visitArray(e.isArrayExp());

        case EXP.notEqual      :
        case EXP.equal         : return visitEqual(e.isEqualExp());

        case EXP.lessOrEqual   :
        case EXP.greaterThan   :
        case EXP.greaterOrEqual:
        case EXP.lessThan      : return visitCmp(cast(CmpExp)e);

        default:
            if (auto ex = e.isBinAssignExp()) return visitBinAssign(ex);
            if (auto ex = e.isBinExp())       return visitBin(ex);
            if (auto ex = e.isUnaExp())       return visitUna(ex);
            return visit(e);
    }
}

/******************************************
 * Common code for overloading of EqualExp and CmpExp
 */
private Expression compare_overload(BinExp e, Scope* sc, Identifier id, EXP* pop)
{
    //printf("BinExp::compare_overload(id = %s) %s\n", id.toChars(), e.toChars());
    AggregateDeclaration ad1 = isAggregate(e.e1.type);
    AggregateDeclaration ad2 = isAggregate(e.e2.type);
    Dsymbol s = null;
    Dsymbol s_r = null;
    if (ad1)
    {
        s = search_function(ad1, id);
    }
    if (ad2)
    {
        s_r = search_function(ad2, id);
        if (s == s_r)
            s_r = null;
    }
    Objects* tiargs = null;
    if (s || s_r)
    {
        /* Try:
         *      a.opEquals(b)
         *      b.opEquals(a)
         * and see which is better.
         */
        Expressions* args1 = new Expressions(1);
        (*args1)[0] = e.e1;
        expandTuples(args1);
        Expressions* args2 = new Expressions(1);
        (*args2)[0] = e.e2;
        expandTuples(args2);
        MatchAccumulator m;
        if (0 && s && s_r)
        {
            printf("s  : %s\n", s.toPrettyChars());
            printf("s_r: %s\n", s_r.toPrettyChars());
        }
        if (s)
        {
            functionResolve(m, s, e.loc, sc, tiargs, e.e1.type, ArgumentList(args2));
            if (m.lastf && (m.lastf.errors || m.lastf.hasSemantic3Errors()))
                return ErrorExp.get();
        }
        FuncDeclaration lastf = m.lastf;
        int count = m.count;
        if (s_r)
        {
            functionResolve(m, s_r, e.loc, sc, tiargs, e.e2.type, ArgumentList(args1));
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
            m.lastf = null;
        }
        Expression result;
        if (lastf && m.lastf == lastf || !s_r && m.last == MATCH.nomatch)
        {
            // Rewrite (e1 op e2) as e1.opfunc(e2)
            result = build_overload(e.loc, sc, e.e1, e.e2, m.lastf ? m.lastf : s);
        }
        else
        {
            // Rewrite (e1 op e2) as e2.opfunc_r(e1)
            result = build_overload(e.loc, sc, e.e2, e.e1, m.lastf ? m.lastf : s_r);
            // When reversing operands of comparison operators,
            // need to reverse the sense of the op
            if (pop)
                *pop = reverseRelation(e.op);
        }
        return result;
    }
    /*
     * https://issues.dlang.org/show_bug.cgi?id=16657
     * at this point, no matching opEquals was found for structs,
     * so we should not follow the alias this comparison code.
     */
    if ((e.op == EXP.equal || e.op == EXP.notEqual) && ad1 == ad2)
        return null;
    Expression result = checkAliasThisForLhs(ad1, sc, e);
    return result ? result : checkAliasThisForRhs(isAggregate(e.e2.type), sc, e);
}

/***********************************
 * Utility to build a function call out of this reference and argument.
 */
Expression build_overload(const ref Loc loc, Scope* sc, Expression ethis, Expression earg, Dsymbol d)
{
    assert(d);
    Expression e;
    Declaration decl = d.isDeclaration();
    if (decl)
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
    Dsymbol s = ad.search(Loc.initial, funcid);
    if (s)
    {
        //printf("search_function: s = '%s'\n", s.kind());
        Dsymbol s2 = s.toAlias();
        //printf("search_function: s2 = '%s'\n", s2.kind());
        FuncDeclaration fd = s2.isFuncDeclaration();
        if (fd && fd.type.ty == Tfunction)
            return fd;
        TemplateDeclaration td = s2.isTemplateDeclaration();
        if (td)
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
        if (!aggr.type || aggr.op == EXP.error)
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
            AggregateDeclaration ad = (tab.ty == Tclass) ? tab.isTypeClass().sym
                                                         : tab.isTypeStruct().sym;
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
        if (tab.ty == Tclass || tab.ty == Tstruct)
            ethis = fes.aggr;
        else
        {
            assert(tab.ty == Tdelegate && fes.aggr.op == EXP.delegate_);
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
        if (!p.type && tab.ty != Ttuple)
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
        AggregateDeclaration ad = (tab.ty == Tclass) ? tab.isTypeClass().sym
                                                     : tab.isTypeStruct().sym;
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
