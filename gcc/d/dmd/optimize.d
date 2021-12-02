/**
 * Perform constant folding.
 *
 * Copyright:   Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 http://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/optimize.d, _optimize.d)
 * Documentation:  https://dlang.org/phobos/dmd_optimize.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/optimize.d
 */

module dmd.optimize;

import core.stdc.stdio;

import dmd.astenums;
import dmd.constfold;
import dmd.ctfeexpr;
import dmd.dclass;
import dmd.declaration;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.errors;
import dmd.expression;
import dmd.expressionsem;
import dmd.globals;
import dmd.init;
import dmd.mtype;
import dmd.root.ctfloat;
import dmd.sideeffect;
import dmd.tokens;
import dmd.visitor;

/*************************************
 * If variable has a const initializer,
 * return that initializer.
 * Returns:
 *      initializer if there is one,
 *      null if not,
 *      ErrorExp if error
 */
Expression expandVar(int result, VarDeclaration v)
{
    //printf("expandVar(result = %d, v = %p, %s)\n", result, v, v ? v.toChars() : "null");

    /********
     * Params:
     *  e = initializer expression
     */
    Expression initializerReturn(Expression e)
    {
        if (e.type != v.type)
        {
            e = e.castTo(null, v.type);
        }
        v.inuse++;
        e = e.optimize(result);
        v.inuse--;
        //if (e) printf("\te = %p, %s, e.type = %d, %s\n", e, e.toChars(), e.type.ty, e.type.toChars());
        return e;
    }

    static Expression nullReturn()
    {
        return null;
    }

    static Expression errorReturn()
    {
        return ErrorExp.get();
    }

    if (!v)
        return nullReturn();
    if (!v.originalType && v.semanticRun < PASS.semanticdone) // semantic() not yet run
        v.dsymbolSemantic(null);
    if (v.type &&
        (v.isConst() || v.isImmutable() || v.storage_class & STC.manifest))
    {
        Type tb = v.type.toBasetype();
        if (v.storage_class & STC.manifest ||
            tb.isscalar() ||
            ((result & WANTexpand) && (tb.ty != Tsarray && tb.ty != Tstruct)))
        {
            if (v._init)
            {
                if (v.inuse)
                {
                    if (v.storage_class & STC.manifest)
                    {
                        v.error("recursive initialization of constant");
                        return errorReturn();
                    }
                    return nullReturn();
                }
                Expression ei = v.getConstInitializer();
                if (!ei)
                {
                    if (v.storage_class & STC.manifest)
                    {
                        v.error("enum cannot be initialized with `%s`", v._init.toChars());
                        return errorReturn();
                    }
                    return nullReturn();
                }
                if (ei.op == TOK.construct || ei.op == TOK.blit)
                {
                    AssignExp ae = cast(AssignExp)ei;
                    ei = ae.e2;
                    if (ei.isConst() == 1)
                    {
                    }
                    else if (ei.op == TOK.string_)
                    {
                        // https://issues.dlang.org/show_bug.cgi?id=14459
                        // Do not constfold the string literal
                        // if it's typed as a C string, because the value expansion
                        // will drop the pointer identity.
                        if (!(result & WANTexpand) && ei.type.toBasetype().ty == Tpointer)
                            return nullReturn();
                    }
                    else
                        return nullReturn();
                    if (ei.type == v.type)
                    {
                        // const variable initialized with const expression
                    }
                    else if (ei.implicitConvTo(v.type) >= MATCH.constant)
                    {
                        // const var initialized with non-const expression
                        ei = ei.implicitCastTo(null, v.type);
                        ei = ei.expressionSemantic(null);
                    }
                    else
                        return nullReturn();
                }
                else if (!(v.storage_class & STC.manifest) &&
                         ei.isConst() != 1 &&
                         ei.op != TOK.string_ &&
                         ei.op != TOK.address)
                {
                    return nullReturn();
                }

                if (!ei.type)
                {
                    return nullReturn();
                }
                else
                {
                    // Should remove the copy() operation by
                    // making all mods to expressions copy-on-write
                    return initializerReturn(ei.copy());
                }
            }
            else
            {
                // v does not have an initializer
                version (all)
                {
                    return nullReturn();
                }
                else
                {
                    // BUG: what if const is initialized in constructor?
                    auto e = v.type.defaultInit();
                    e.loc = e1.loc;
                    return initializerReturn(e);
                }
            }
            assert(0);
        }
    }
    return nullReturn();
}

private Expression fromConstInitializer(int result, Expression e1)
{
    //printf("fromConstInitializer(result = %x, %s)\n", result, e1.toChars());
    //static int xx; if (xx++ == 10) assert(0);
    Expression e = e1;
    if (e1.op == TOK.variable)
    {
        VarExp ve = cast(VarExp)e1;
        VarDeclaration v = ve.var.isVarDeclaration();
        e = expandVar(result, v);
        if (e)
        {
            // If it is a comma expression involving a declaration, we mustn't
            // perform a copy -- we'd get two declarations of the same variable.
            // See bugzilla 4465.
            if (e.op == TOK.comma && (cast(CommaExp)e).e1.op == TOK.declaration)
                e = e1;
            else if (e.type != e1.type && e1.type && e1.type.ty != Tident)
            {
                // Type 'paint' operation
                e = e.copy();
                e.type = e1.type;
            }
            e.loc = e1.loc;
        }
        else
        {
            e = e1;
        }
    }
    return e;
}

/* It is possible for constant folding to change an array expression of
 * unknown length, into one where the length is known.
 * If the expression 'arr' is a literal, set lengthVar to be its length.
 */
package void setLengthVarIfKnown(VarDeclaration lengthVar, Expression arr)
{
    if (!lengthVar)
        return;
    if (lengthVar._init && !lengthVar._init.isVoidInitializer())
        return; // we have previously calculated the length
    size_t len;
    if (arr.op == TOK.string_)
        len = (cast(StringExp)arr).len;
    else if (arr.op == TOK.arrayLiteral)
        len = (cast(ArrayLiteralExp)arr).elements.dim;
    else
    {
        Type t = arr.type.toBasetype();
        if (t.ty == Tsarray)
            len = cast(size_t)(cast(TypeSArray)t).dim.toInteger();
        else
            return; // we don't know the length yet
    }
    Expression dollar = new IntegerExp(Loc.initial, len, Type.tsize_t);
    lengthVar._init = new ExpInitializer(Loc.initial, dollar);
    lengthVar.storage_class |= STC.static_ | STC.const_;
}

/* Same as above, but determines the length from 'type'. */
package void setLengthVarIfKnown(VarDeclaration lengthVar, Type type)
{
    if (!lengthVar)
        return;
    if (lengthVar._init && !lengthVar._init.isVoidInitializer())
        return; // we have previously calculated the length
    size_t len;
    Type t = type.toBasetype();
    if (t.ty == Tsarray)
        len = cast(size_t)(cast(TypeSArray)t).dim.toInteger();
    else
        return; // we don't know the length yet
    Expression dollar = new IntegerExp(Loc.initial, len, Type.tsize_t);
    lengthVar._init = new ExpInitializer(Loc.initial, dollar);
    lengthVar.storage_class |= STC.static_ | STC.const_;
}

/*********************************
 * Constant fold an Expression.
 * Params:
 *      e = expression to const fold; this may get modified in-place
 *      result = WANTvalue, WANTexpand, or both
 *      keepLvalue = `e` is an lvalue, and keep it as an lvalue since it is
 *                   an argument to a `ref` or `out` parameter, or the operand of `&` operator
 * Returns:
 *      Constant folded version of `e`
 */
Expression Expression_optimize(Expression e, int result, bool keepLvalue)
{
    extern (C++) final class OptimizeVisitor : Visitor
    {
        alias visit = Visitor.visit;

        Expression ret;
        private const int result;
        private const bool keepLvalue;

        extern (D) this(Expression e, int result, bool keepLvalue)
        {
            this.ret = e;               // default result is original expression
            this.result = result;
            this.keepLvalue = keepLvalue;
        }

        void error()
        {
            ret = ErrorExp.get();
        }

        bool expOptimize(ref Expression e, int flags, bool keepLvalue = false)
        {
            if (!e)
                return false;
            Expression ex = Expression_optimize(e, flags, keepLvalue);
            if (ex.op == TOK.error)
            {
                ret = ex; // store error result
                return true;
            }
            else
            {
                e = ex; // modify original
                return false;
            }
        }

        bool unaOptimize(UnaExp e, int flags)
        {
            return expOptimize(e.e1, flags);
        }

        bool binOptimize(BinExp e, int flags, bool keepLhsLvalue = false)
        {
            expOptimize(e.e1, flags, keepLhsLvalue);
            expOptimize(e.e2, flags);
            return ret.op == TOK.error;
        }

        override void visit(Expression e)
        {
            //printf("Expression::optimize(result = x%x) %s\n", result, e.toChars());
        }

        override void visit(VarExp e)
        {
            VarDeclaration v = e.var.isVarDeclaration();

            if (!(keepLvalue && v && !(v.storage_class & STC.manifest)))
                ret = fromConstInitializer(result, e);

            // if unoptimized, try to optimize the dtor expression
            // (e.g., might be a LogicalExp with constant lhs)
            if (ret == e && v && v.edtor)
            {
                // prevent infinite recursion (`<var>.~this()`)
                if (!v.inuse)
                {
                    v.inuse++;
                    expOptimize(v.edtor, WANTvalue);
                    v.inuse--;
                }
            }
        }

        override void visit(TupleExp e)
        {
            expOptimize(e.e0, WANTvalue);
            for (size_t i = 0; i < e.exps.dim; i++)
            {
                expOptimize((*e.exps)[i], WANTvalue);
            }
        }

        override void visit(ArrayLiteralExp e)
        {
            if (e.elements)
            {
                expOptimize(e.basis, result & WANTexpand);
                for (size_t i = 0; i < e.elements.dim; i++)
                {
                    expOptimize((*e.elements)[i], result & WANTexpand);
                }
            }
        }

        override void visit(AssocArrayLiteralExp e)
        {
            assert(e.keys.dim == e.values.dim);
            for (size_t i = 0; i < e.keys.dim; i++)
            {
                expOptimize((*e.keys)[i], result & WANTexpand);
                expOptimize((*e.values)[i], result & WANTexpand);
            }
        }

        override void visit(StructLiteralExp e)
        {
            if (e.stageflags & stageOptimize)
                return;
            int old = e.stageflags;
            e.stageflags |= stageOptimize;
            if (e.elements)
            {
                for (size_t i = 0; i < e.elements.dim; i++)
                {
                    expOptimize((*e.elements)[i], result & WANTexpand);
                }
            }
            e.stageflags = old;
        }

        override void visit(UnaExp e)
        {
            //printf("UnaExp::optimize() %s\n", e.toChars());
            if (unaOptimize(e, result))
                return;
        }

        override void visit(NegExp e)
        {
            if (unaOptimize(e, result))
                return;
            if (e.e1.isConst() == 1)
            {
                ret = Neg(e.type, e.e1).copy();
            }
        }

        override void visit(ComExp e)
        {
            if (unaOptimize(e, result))
                return;
            if (e.e1.isConst() == 1)
            {
                ret = Com(e.type, e.e1).copy();
            }
        }

        override void visit(NotExp e)
        {
            if (unaOptimize(e, result))
                return;
            if (e.e1.isConst() == 1)
            {
                ret = Not(e.type, e.e1).copy();
            }
        }

        override void visit(SymOffExp e)
        {
            assert(e.var);
        }

        override void visit(AddrExp e)
        {
            //printf("AddrExp::optimize(result = %d) %s\n", result, e.toChars());
            /* Rewrite &(a,b) as (a,&b)
             */
            if (e.e1.op == TOK.comma)
            {
                CommaExp ce = cast(CommaExp)e.e1;
                auto ae = new AddrExp(e.loc, ce.e2, e.type);
                ret = new CommaExp(ce.loc, ce.e1, ae);
                ret.type = e.type;
                return;
            }
            // Keep lvalue-ness
            if (expOptimize(e.e1, result, true))
                return;
            // Convert &*ex to ex
            if (e.e1.op == TOK.star)
            {
                Expression ex = (cast(PtrExp)e.e1).e1;
                if (e.type.equals(ex.type))
                    ret = ex;
                else if (e.type.toBasetype().equivalent(ex.type.toBasetype()))
                {
                    ret = ex.copy();
                    ret.type = e.type;
                }
                return;
            }
            if (e.e1.op == TOK.variable)
            {
                VarExp ve = cast(VarExp)e.e1;
                if (!ve.var.isReference() && !ve.var.isImportedSymbol())
                {
                    ret = new SymOffExp(e.loc, ve.var, 0, ve.hasOverloads);
                    ret.type = e.type;
                    return;
                }
            }
            if (e.e1.op == TOK.index)
            {
                // Convert &array[n] to &array+n
                IndexExp ae = cast(IndexExp)e.e1;
                if (ae.e2.op == TOK.int64 && ae.e1.op == TOK.variable)
                {
                    sinteger_t index = ae.e2.toInteger();
                    VarExp ve = cast(VarExp)ae.e1;
                    if (ve.type.ty == Tsarray && !ve.var.isImportedSymbol())
                    {
                        TypeSArray ts = cast(TypeSArray)ve.type;
                        sinteger_t dim = ts.dim.toInteger();
                        if (index < 0 || index >= dim)
                        {
                            e.error("array index %lld is out of bounds `[0..%lld]`", index, dim);
                            return error();
                        }

                        import core.checkedint : mulu;
                        bool overflow;
                        const offset = mulu(index, ts.nextOf().size(e.loc), overflow);
                        if (overflow)
                        {
                            e.error("array offset overflow");
                            return error();
                        }

                        ret = new SymOffExp(e.loc, ve.var, offset);
                        ret.type = e.type;
                        return;
                    }
                }
            }
        }

        override void visit(PtrExp e)
        {
            //printf("PtrExp::optimize(result = x%x) %s\n", result, e.toChars());
            if (expOptimize(e.e1, result))
                return;
            // Convert *&ex to ex
            // But only if there is no type punning involved
            if (e.e1.op == TOK.address)
            {
                Expression ex = (cast(AddrExp)e.e1).e1;
                if (e.type.equals(ex.type))
                    ret = ex;
                else if (e.type.toBasetype().equivalent(ex.type.toBasetype()))
                {
                    ret = ex.copy();
                    ret.type = e.type;
                }
            }
            if (keepLvalue)
                return;
            // Constant fold *(&structliteral + offset)
            if (e.e1.op == TOK.add)
            {
                Expression ex = Ptr(e.type, e.e1).copy();
                if (!CTFEExp.isCantExp(ex))
                {
                    ret = ex;
                    return;
                }
            }
            if (e.e1.op == TOK.symbolOffset)
            {
                SymOffExp se = cast(SymOffExp)e.e1;
                VarDeclaration v = se.var.isVarDeclaration();
                Expression ex = expandVar(result, v);
                if (ex && ex.op == TOK.structLiteral)
                {
                    StructLiteralExp sle = cast(StructLiteralExp)ex;
                    ex = sle.getField(e.type, cast(uint)se.offset);
                    if (ex && !CTFEExp.isCantExp(ex))
                    {
                        ret = ex;
                        return;
                    }
                }
            }
        }

        override void visit(DotVarExp e)
        {
            //printf("DotVarExp::optimize(result = x%x) %s\n", result, e.toChars());
            if (expOptimize(e.e1, result))
                return;
            if (keepLvalue)
                return;
            Expression ex = e.e1;
            if (ex.op == TOK.variable)
            {
                VarExp ve = cast(VarExp)ex;
                VarDeclaration v = ve.var.isVarDeclaration();
                ex = expandVar(result, v);
            }
            if (ex && ex.op == TOK.structLiteral)
            {
                StructLiteralExp sle = cast(StructLiteralExp)ex;
                VarDeclaration vf = e.var.isVarDeclaration();
                if (vf && !vf.overlapped)
                {
                    /* https://issues.dlang.org/show_bug.cgi?id=13021
                     * Prevent optimization if vf has overlapped fields.
                     */
                    ex = sle.getField(e.type, vf.offset);
                    if (ex && !CTFEExp.isCantExp(ex))
                    {
                        ret = ex;
                        return;
                    }
                }
            }
        }

        override void visit(NewExp e)
        {
            expOptimize(e.thisexp, WANTvalue);
            // Optimize parameters
            if (e.newargs)
            {
                for (size_t i = 0; i < e.newargs.dim; i++)
                {
                    expOptimize((*e.newargs)[i], WANTvalue);
                }
            }
            if (e.arguments)
            {
                for (size_t i = 0; i < e.arguments.dim; i++)
                {
                    expOptimize((*e.arguments)[i], WANTvalue);
                }
            }
        }

        override void visit(CallExp e)
        {
            //printf("CallExp::optimize(result = %d) %s\n", result, e.toChars());
            // Optimize parameters with keeping lvalue-ness
            if (expOptimize(e.e1, result))
                return;
            if (e.arguments)
            {
                Type t1 = e.e1.type.toBasetype();
                if (t1.ty == Tdelegate)
                    t1 = t1.nextOf();
                // t1 can apparently be void for __ArrayDtor(T) calls
                if (auto tf = t1.isTypeFunction())
                {
                    for (size_t i = 0; i < e.arguments.dim; i++)
                    {
                        Parameter p = tf.parameterList[i];
                        bool keep = p && p.isReference();
                        expOptimize((*e.arguments)[i], WANTvalue, keep);
                    }
                }
            }
        }

        override void visit(CastExp e)
        {
            //printf("CastExp::optimize(result = %d) %s\n", result, e.toChars());
            //printf("from %s to %s\n", e.type.toChars(), e.to.toChars());
            //printf("from %s\n", e.type.toChars());
            //printf("e1.type %s\n", e.e1.type.toChars());
            //printf("type = %p\n", e.type);
            assert(e.type);
            TOK op1 = e.e1.op;
            Expression e1old = e.e1;
            if (expOptimize(e.e1, result, keepLvalue))
                return;
            if (!keepLvalue)
                e.e1 = fromConstInitializer(result, e.e1);
            if (e.e1 == e1old && e.e1.op == TOK.arrayLiteral && e.type.toBasetype().ty == Tpointer && e.e1.type.toBasetype().ty != Tsarray)
            {
                // Casting this will result in the same expression, and
                // infinite loop because of Expression::implicitCastTo()
                return; // no change
            }
            if ((e.e1.op == TOK.string_ || e.e1.op == TOK.arrayLiteral) &&
                (e.type.ty == Tpointer || e.type.ty == Tarray))
            {
                const esz  = e.type.nextOf().size(e.loc);
                const e1sz = e.e1.type.toBasetype().nextOf().size(e.e1.loc);
                if (esz == SIZE_INVALID || e1sz == SIZE_INVALID)
                    return error();

                if (e1sz == esz)
                {
                    // https://issues.dlang.org/show_bug.cgi?id=12937
                    // If target type is void array, trying to paint
                    // e.e1 with that type will cause infinite recursive optimization.
                    if (e.type.nextOf().ty == Tvoid)
                        return;
                    ret = e.e1.castTo(null, e.type);
                    //printf(" returning1 %s\n", ret.toChars());
                    return;
                }
            }

            if (e.e1.op == TOK.structLiteral && e.e1.type.implicitConvTo(e.type) >= MATCH.constant)
            {
                //printf(" returning2 %s\n", e.e1.toChars());
            L1:
                // Returning e1 with changing its type
                ret = (e1old == e.e1 ? e.e1.copy() : e.e1);
                ret.type = e.type;
                return;
            }
            /* The first test here is to prevent infinite loops
             */
            if (op1 != TOK.arrayLiteral && e.e1.op == TOK.arrayLiteral)
            {
                ret = e.e1.castTo(null, e.to);
                return;
            }
            if (e.e1.op == TOK.null_ && (e.type.ty == Tpointer || e.type.ty == Tclass || e.type.ty == Tarray))
            {
                //printf(" returning3 %s\n", e.e1.toChars());
                goto L1;
            }
            if (e.type.ty == Tclass && e.e1.type.ty == Tclass)
            {
                import dmd.astenums : Sizeok;

                // See if we can remove an unnecessary cast
                ClassDeclaration cdfrom = e.e1.type.isClassHandle();
                ClassDeclaration cdto = e.type.isClassHandle();
                if (cdto == ClassDeclaration.object && !cdfrom.isInterfaceDeclaration())
                    goto L1;    // can always convert a class to Object
                // Need to determine correct offset before optimizing away the cast.
                // https://issues.dlang.org/show_bug.cgi?id=16980
                cdfrom.size(e.loc);
                assert(cdfrom.sizeok == Sizeok.done);
                assert(cdto.sizeok == Sizeok.done || !cdto.isBaseOf(cdfrom, null));
                int offset;
                if (cdto.isBaseOf(cdfrom, &offset) && offset == 0)
                {
                    //printf(" returning4 %s\n", e.e1.toChars());
                    goto L1;
                }
            }
            if (e.e1.type.mutableOf().unSharedOf().equals(e.to.mutableOf().unSharedOf()))
            {
                //printf(" returning5 %s\n", e.e1.toChars());
                goto L1;
            }
            if (e.e1.isConst())
            {
                if (e.e1.op == TOK.symbolOffset)
                {
                    if (e.type.toBasetype().ty != Tsarray)
                    {
                        const esz = e.type.size(e.loc);
                        const e1sz = e.e1.type.size(e.e1.loc);
                        if (esz == SIZE_INVALID ||
                            e1sz == SIZE_INVALID)
                            return error();

                        if (esz == e1sz)
                            goto L1;
                    }
                    return;
                }
                if (e.to.toBasetype().ty != Tvoid)
                {
                    if (e.e1.type.equals(e.type) && e.type.equals(e.to))
                        ret = e.e1;
                    else
                        ret = Cast(e.loc, e.type, e.to, e.e1).copy();
                }
            }
            //printf(" returning6 %s\n", ret.toChars());
        }

        override void visit(BinAssignExp e)
        {
            //printf("BinAssignExp::optimize(result = %d) %s\n", result, e.toChars());
            if (binOptimize(e, result, /*keepLhsLvalue*/ true))
                return;
            if (e.op == TOK.leftShiftAssign || e.op == TOK.rightShiftAssign || e.op == TOK.unsignedRightShiftAssign)
            {
                if (e.e2.isConst() == 1)
                {
                    sinteger_t i2 = e.e2.toInteger();
                    d_uns64 sz = e.e1.type.size(e.e1.loc);
                    assert(sz != SIZE_INVALID);
                    sz *= 8;
                    if (i2 < 0 || i2 >= sz)
                    {
                        e.error("shift assign by %lld is outside the range `0..%llu`", i2, cast(ulong)sz - 1);
                        return error();
                    }
                }
            }
        }

        override void visit(BinExp e)
        {
            //printf("BinExp::optimize(result = %d) %s\n", result, e.toChars());
            const keepLhsLvalue = e.op == TOK.construct || e.op == TOK.blit || e.op == TOK.assign
                || e.op == TOK.plusPlus || e.op == TOK.minusMinus
                || e.op == TOK.prePlusPlus || e.op == TOK.preMinusMinus;
            binOptimize(e, result, keepLhsLvalue);
        }

        override void visit(AddExp e)
        {
            //printf("AddExp::optimize(%s)\n", e.toChars());
            if (binOptimize(e, result))
                return;
            if (e.e1.isConst() && e.e2.isConst())
            {
                if (e.e1.op == TOK.symbolOffset && e.e2.op == TOK.symbolOffset)
                    return;
                ret = Add(e.loc, e.type, e.e1, e.e2).copy();
            }
        }

        override void visit(MinExp e)
        {
            if (binOptimize(e, result))
                return;
            if (e.e1.isConst() && e.e2.isConst())
            {
                if (e.e2.op == TOK.symbolOffset)
                    return;
                ret = Min(e.loc, e.type, e.e1, e.e2).copy();
            }
        }

        override void visit(MulExp e)
        {
            //printf("MulExp::optimize(result = %d) %s\n", result, e.toChars());
            if (binOptimize(e, result))
                return;
            if (e.e1.isConst() == 1 && e.e2.isConst() == 1)
            {
                ret = Mul(e.loc, e.type, e.e1, e.e2).copy();
            }
        }

        override void visit(DivExp e)
        {
            //printf("DivExp::optimize(%s)\n", e.toChars());
            if (binOptimize(e, result))
                return;
            if (e.e1.isConst() == 1 && e.e2.isConst() == 1)
            {
                ret = Div(e.loc, e.type, e.e1, e.e2).copy();
            }
        }

        override void visit(ModExp e)
        {
            if (binOptimize(e, result))
                return;
            if (e.e1.isConst() == 1 && e.e2.isConst() == 1)
            {
                ret = Mod(e.loc, e.type, e.e1, e.e2).copy();
            }
        }

        extern (D) void shift_optimize(BinExp e, UnionExp function(const ref Loc, Type, Expression, Expression) shift)
        {
            if (binOptimize(e, result))
                return;
            if (e.e2.isConst() == 1)
            {
                sinteger_t i2 = e.e2.toInteger();
                d_uns64 sz = e.e1.type.size(e.e1.loc);
                assert(sz != SIZE_INVALID);
                sz *= 8;
                if (i2 < 0 || i2 >= sz)
                {
                    e.error("shift by %lld is outside the range `0..%llu`", i2, cast(ulong)sz - 1);
                    return error();
                }
                if (e.e1.isConst() == 1)
                    ret = (*shift)(e.loc, e.type, e.e1, e.e2).copy();
            }
        }

        override void visit(ShlExp e)
        {
            //printf("ShlExp::optimize(result = %d) %s\n", result, e.toChars());
            shift_optimize(e, &Shl);
        }

        override void visit(ShrExp e)
        {
            //printf("ShrExp::optimize(result = %d) %s\n", result, e.toChars());
            shift_optimize(e, &Shr);
        }

        override void visit(UshrExp e)
        {
            //printf("UshrExp::optimize(result = %d) %s\n", result, toChars());
            shift_optimize(e, &Ushr);
        }

        override void visit(AndExp e)
        {
            if (binOptimize(e, result))
                return;
            if (e.e1.isConst() == 1 && e.e2.isConst() == 1)
                ret = And(e.loc, e.type, e.e1, e.e2).copy();
        }

        override void visit(OrExp e)
        {
            if (binOptimize(e, result))
                return;
            if (e.e1.isConst() == 1 && e.e2.isConst() == 1)
                ret = Or(e.loc, e.type, e.e1, e.e2).copy();
        }

        override void visit(XorExp e)
        {
            if (binOptimize(e, result))
                return;
            if (e.e1.isConst() == 1 && e.e2.isConst() == 1)
                ret = Xor(e.loc, e.type, e.e1, e.e2).copy();
        }

        override void visit(PowExp e)
        {
            if (binOptimize(e, result))
                return;
            // All negative integral powers are illegal.
            if (e.e1.type.isintegral() && (e.e2.op == TOK.int64) && cast(sinteger_t)e.e2.toInteger() < 0)
            {
                e.error("cannot raise `%s` to a negative integer power. Did you mean `(cast(real)%s)^^%s` ?", e.e1.type.toBasetype().toChars(), e.e1.toChars(), e.e2.toChars());
                return error();
            }
            // If e2 *could* have been an integer, make it one.
            if (e.e2.op == TOK.float64 && e.e2.toReal() == real_t(cast(sinteger_t)e.e2.toReal()))
            {
                // This only applies to floating point, or positive integral powers.
                if (e.e1.type.isfloating() || cast(sinteger_t)e.e2.toInteger() >= 0)
                    e.e2 = new IntegerExp(e.loc, e.e2.toInteger(), Type.tint64);
            }
            if (e.e1.isConst() == 1 && e.e2.isConst() == 1)
            {
                Expression ex = Pow(e.loc, e.type, e.e1, e.e2).copy();
                if (!CTFEExp.isCantExp(ex))
                {
                    ret = ex;
                    return;
                }
            }
        }

        override void visit(CommaExp e)
        {
            //printf("CommaExp::optimize(result = %d) %s\n", result, e.toChars());
            // Comma needs special treatment, because it may
            // contain compiler-generated declarations. We can interpret them, but
            // otherwise we must NOT attempt to constant-fold them.
            // In particular, if the comma returns a temporary variable, it needs
            // to be an lvalue (this is particularly important for struct constructors)
            expOptimize(e.e1, WANTvalue);
            expOptimize(e.e2, result, keepLvalue);
            if (ret.op == TOK.error)
                return;
            if (!e.e1 || e.e1.op == TOK.int64 || e.e1.op == TOK.float64 || !hasSideEffect(e.e1))
            {
                ret = e.e2;
                if (ret)
                    ret.type = e.type;
            }
            //printf("-CommaExp::optimize(result = %d) %s\n", result, e.e.toChars());
        }

        override void visit(ArrayLengthExp e)
        {
            //printf("ArrayLengthExp::optimize(result = %d) %s\n", result, e.toChars());
            if (unaOptimize(e, WANTexpand))
                return;
            // CTFE interpret static immutable arrays (to get better diagnostics)
            if (e.e1.op == TOK.variable)
            {
                VarDeclaration v = (cast(VarExp)e.e1).var.isVarDeclaration();
                if (v && (v.storage_class & STC.static_) && (v.storage_class & STC.immutable_) && v._init)
                {
                    if (Expression ci = v.getConstInitializer())
                        e.e1 = ci;
                }
            }
            if (e.e1.op == TOK.string_ || e.e1.op == TOK.arrayLiteral || e.e1.op == TOK.assocArrayLiteral || e.e1.type.toBasetype().ty == Tsarray)
            {
                ret = ArrayLength(e.type, e.e1).copy();
            }
        }

        override void visit(EqualExp e)
        {
            //printf("EqualExp::optimize(result = %x) %s\n", result, e.toChars());
            if (binOptimize(e, WANTvalue))
                return;
            Expression e1 = fromConstInitializer(result, e.e1);
            Expression e2 = fromConstInitializer(result, e.e2);
            if (e1.op == TOK.error)
            {
                ret = e1;
                return;
            }
            if (e2.op == TOK.error)
            {
                ret = e2;
                return;
            }
            ret = Equal(e.op, e.loc, e.type, e1, e2).copy();
            if (CTFEExp.isCantExp(ret))
                ret = e;
        }

        override void visit(IdentityExp e)
        {
            //printf("IdentityExp::optimize(result = %d) %s\n", result, e.toChars());
            if (binOptimize(e, WANTvalue))
                return;
            if ((e.e1.isConst() && e.e2.isConst()) || (e.e1.op == TOK.null_ && e.e2.op == TOK.null_))
            {
                ret = Identity(e.op, e.loc, e.type, e.e1, e.e2).copy();
                if (CTFEExp.isCantExp(ret))
                    ret = e;
            }
        }

        override void visit(IndexExp e)
        {
            //printf("IndexExp::optimize(result = %d) %s\n", result, e.toChars());
            if (expOptimize(e.e1, result & WANTexpand))
                return;
            Expression ex = fromConstInitializer(result, e.e1);
            // We might know $ now
            setLengthVarIfKnown(e.lengthVar, ex);
            if (expOptimize(e.e2, WANTvalue))
                return;
            // Don't optimize to an array literal element directly in case an lvalue is requested
            if (keepLvalue && ex.op == TOK.arrayLiteral)
                return;
            ret = Index(e.type, ex, e.e2).copy();
            if (CTFEExp.isCantExp(ret) || (!ret.isErrorExp() && keepLvalue && !ret.isLvalue()))
                ret = e;
        }

        override void visit(SliceExp e)
        {
            //printf("SliceExp::optimize(result = %d) %s\n", result, e.toChars());
            if (expOptimize(e.e1, result & WANTexpand))
                return;
            if (!e.lwr)
            {
                if (e.e1.op == TOK.string_)
                {
                    // Convert slice of string literal into dynamic array
                    Type t = e.e1.type.toBasetype();
                    if (Type tn = t.nextOf())
                        ret = e.e1.castTo(null, tn.arrayOf());
                }
            }
            else
            {
                e.e1 = fromConstInitializer(result, e.e1);
                // We might know $ now
                setLengthVarIfKnown(e.lengthVar, e.e1);
                expOptimize(e.lwr, WANTvalue);
                expOptimize(e.upr, WANTvalue);
                if (ret.op == TOK.error)
                    return;
                ret = Slice(e.type, e.e1, e.lwr, e.upr).copy();
                if (CTFEExp.isCantExp(ret))
                    ret = e;
            }
            // https://issues.dlang.org/show_bug.cgi?id=14649
            // Leave the slice form so it might be
            // a part of array operation.
            // Assume that the backend codegen will handle the form `e[]`
            // as an equal to `e` itself.
            if (ret.op == TOK.string_)
            {
                e.e1 = ret;
                e.lwr = null;
                e.upr = null;
                ret = e;
            }
            //printf("-SliceExp::optimize() %s\n", ret.toChars());
        }

        override void visit(LogicalExp e)
        {
            //printf("LogicalExp::optimize(%d) %s\n", result, e.toChars());
            if (expOptimize(e.e1, WANTvalue))
                return;
            const oror = e.op == TOK.orOr;
            if (e.e1.isBool(oror))
            {
                // Replace with (e1, oror)
                ret = IntegerExp.createBool(oror);
                ret = Expression.combine(e.e1, ret);
                if (e.type.toBasetype().ty == Tvoid)
                {
                    ret = new CastExp(e.loc, ret, Type.tvoid);
                    ret.type = e.type;
                }
                ret = Expression_optimize(ret, result, false);
                return;
            }
            expOptimize(e.e2, WANTvalue);
            if (e.e1.isConst())
            {
                if (e.e2.isConst())
                {
                    bool n1 = e.e1.isBool(true);
                    bool n2 = e.e2.isBool(true);
                    ret = new IntegerExp(e.loc, oror ? (n1 || n2) : (n1 && n2), e.type);
                }
                else if (e.e1.isBool(!oror))
                {
                    if (e.type.toBasetype().ty == Tvoid)
                        ret = e.e2;
                    else
                    {
                        ret = new CastExp(e.loc, e.e2, e.type);
                        ret.type = e.type;
                    }
                }
            }
        }

        override void visit(CmpExp e)
        {
            //printf("CmpExp::optimize() %s\n", e.toChars());
            if (binOptimize(e, WANTvalue))
                return;
            Expression e1 = fromConstInitializer(result, e.e1);
            Expression e2 = fromConstInitializer(result, e.e2);
            ret = Cmp(e.op, e.loc, e.type, e1, e2).copy();
            if (CTFEExp.isCantExp(ret))
                ret = e;
        }

        override void visit(CatExp e)
        {
            //printf("CatExp::optimize(%d) %s\n", result, e.toChars());
            if (binOptimize(e, result))
                return;
            if (e.e1.op == TOK.concatenate)
            {
                // https://issues.dlang.org/show_bug.cgi?id=12798
                // optimize ((expr ~ str1) ~ str2)
                CatExp ce1 = cast(CatExp)e.e1;
                scope CatExp cex = new CatExp(e.loc, ce1.e2, e.e2);
                cex.type = e.type;
                Expression ex = Expression_optimize(cex, result, false);
                if (ex != cex)
                {
                    e.e1 = ce1.e1;
                    e.e2 = ex;
                }
            }
            // optimize "str"[] -> "str"
            if (e.e1.op == TOK.slice)
            {
                SliceExp se1 = cast(SliceExp)e.e1;
                if (se1.e1.op == TOK.string_ && !se1.lwr)
                    e.e1 = se1.e1;
            }
            if (e.e2.op == TOK.slice)
            {
                SliceExp se2 = cast(SliceExp)e.e2;
                if (se2.e1.op == TOK.string_ && !se2.lwr)
                    e.e2 = se2.e1;
            }
            ret = Cat(e.loc, e.type, e.e1, e.e2).copy();
            if (CTFEExp.isCantExp(ret))
                ret = e;
        }

        override void visit(CondExp e)
        {
            if (expOptimize(e.econd, WANTvalue))
                return;
            if (e.econd.isBool(true))
                ret = Expression_optimize(e.e1, result, keepLvalue);
            else if (e.econd.isBool(false))
                ret = Expression_optimize(e.e2, result, keepLvalue);
            else
            {
                expOptimize(e.e1, result, keepLvalue);
                expOptimize(e.e2, result, keepLvalue);
            }
        }
    }

    scope OptimizeVisitor v = new OptimizeVisitor(e, result, keepLvalue);

    // Optimize the expression until it can no longer be simplified.
    size_t b;
    while (1)
    {
        if (b++ == global.recursionLimit)
        {
            e.error("infinite loop while optimizing expression");
            fatal();
        }
        auto ex = v.ret;
        ex.accept(v);
        if (ex == v.ret)
            break;
    }
    return v.ret;
}
