/**
 * Perform constant folding.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
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
import dmd.location;
import dmd.mtype;
import dmd.printast;
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
                if (ei.op == EXP.construct || ei.op == EXP.blit)
                {
                    AssignExp ae = cast(AssignExp)ei;
                    ei = ae.e2;
                    if (ei.isConst() == 1)
                    {
                    }
                    else if (ei.op == EXP.string_)
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
                         ei.op != EXP.string_ &&
                         ei.op != EXP.address)
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
    if (auto ve = e1.isVarExp())
    {
        VarDeclaration v = ve.var.isVarDeclaration();
        e = expandVar(result, v);
        if (e)
        {
            // If it is a comma expression involving a declaration, we mustn't
            // perform a copy -- we'd get two declarations of the same variable.
            // See bugzilla 4465.
            if (e.op == EXP.comma && e.isCommaExp().e1.isDeclarationExp())
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

/***
 * It is possible for constant folding to change an array expression of
 * unknown length, into one where the length is known.
 * If the expression 'arr' is a literal, set lengthVar to be its length.
 * Params:
 *    lengthVar = variable declaration for the `.length` property
 *    arr = String, ArrayLiteral, or of TypeSArray
 */
package void setLengthVarIfKnown(VarDeclaration lengthVar, Expression arr)
{
    if (!lengthVar)
        return;
    if (lengthVar._init && !lengthVar._init.isVoidInitializer())
        return; // we have previously calculated the length
    dinteger_t len;
    if (auto se = arr.isStringExp())
        len = se.len;
    else if (auto ale = arr.isArrayLiteralExp())
        len = ale.elements.length;
    else
    {
        auto tsa = arr.type.toBasetype().isTypeSArray();
        if (!tsa)
            return; // we don't know the length yet
        len = tsa.dim.toInteger();
    }
    Expression dollar = new IntegerExp(Loc.initial, len, Type.tsize_t);
    lengthVar._init = new ExpInitializer(Loc.initial, dollar);
    lengthVar.storage_class |= STC.static_ | STC.const_;
}

/***
 * Same as above, but determines the length from 'type'.
 * Params:
 *    lengthVar = variable declaration for the `.length` property
 *    type = TypeSArray
 */
package void setLengthVarIfKnown(VarDeclaration lengthVar, Type type)
{
    if (!lengthVar)
        return;
    if (lengthVar._init && !lengthVar._init.isVoidInitializer())
        return; // we have previously calculated the length
    auto tsa = type.toBasetype().isTypeSArray();
    if (!tsa)
        return; // we don't know the length yet
    const len = tsa.dim.toInteger();
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
    //printf("Expression_optimize() e: %s result: %d keepLvalue %d\n", e.toChars(), result, keepLvalue);
    Expression ret = e;

    void error()
    {
        ret = ErrorExp.get();
    }

    /* Returns: true if error
     */
    bool expOptimize(ref Expression e, int flags, bool keepLvalue = false)
    {
        if (!e)
            return false;
        Expression ex = Expression_optimize(e, flags, keepLvalue);
        if (ex.op == EXP.error)
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
        return expOptimize(e.e1, flags, keepLhsLvalue) |
               expOptimize(e.e2, flags);
    }

    void visitExp(Expression e)
    {
        //printf("Expression::optimize(result = x%x) %s\n", result, e.toChars());
    }

    void visitVar(VarExp e)
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

    void visitTuple(TupleExp e)
    {
        expOptimize(e.e0, WANTvalue);
        foreach (ref ex; (*e.exps)[])
        {
            expOptimize(ex, WANTvalue);
        }
    }

    void visitArrayLiteral(ArrayLiteralExp e)
    {
        if (e.elements)
        {
            expOptimize(e.basis, result & WANTexpand);
            foreach (ref ex; (*e.elements)[])
            {
                expOptimize(ex, result & WANTexpand);
            }
        }
    }

    void visitAssocArrayLiteral(AssocArrayLiteralExp e)
    {
        assert(e.keys.length == e.values.length);
        foreach (i, ref ekey; (*e.keys)[])
        {
            expOptimize(ekey, result & WANTexpand);
            expOptimize((*e.values)[i], result & WANTexpand);
        }
    }

    void visitStructLiteral(StructLiteralExp e)
    {
        if (e.stageflags & stageOptimize)
            return;
        int old = e.stageflags;
        e.stageflags |= stageOptimize;
        if (e.elements)
        {
            foreach (ref ex; (*e.elements)[])
            {
                expOptimize(ex, result & WANTexpand);
            }
        }
        e.stageflags = old;
    }

    void visitUna(UnaExp e)
    {
        //printf("UnaExp::optimize() %s\n", e.toChars());
        if (unaOptimize(e, result))
            return;
    }

    void visitNeg(NegExp e)
    {
        if (unaOptimize(e, result))
            return;
        if (e.e1.isConst() == 1)
        {
            ret = Neg(e.type, e.e1).copy();
        }
    }

    void visitCom(ComExp e)
    {
        if (unaOptimize(e, result))
            return;
        if (e.e1.isConst() == 1)
        {
            ret = Com(e.type, e.e1).copy();
        }
    }

    void visitNop(NotExp e)
    {
        if (unaOptimize(e, result))
            return;
        if (e.e1.isConst() == 1)
        {
            ret = Not(e.type, e.e1).copy();
        }
    }

    void visitSymOff(SymOffExp e)
    {
        assert(e.var);
    }

    void visitAddr(AddrExp e)
    {
        //printf("AddrExp::optimize(result = %d, keepLvalue = %d) %s\n", result, keepLvalue, e.toChars());
        /* Rewrite &(a,b) as (a,&b)
         */
        if (auto ce = e.e1.isCommaExp())
        {
            auto ae = new AddrExp(e.loc, ce.e2, e.type);
            ret = new CommaExp(ce.loc, ce.e1, ae);
            ret.type = e.type;
            return;
        }
        // Keep lvalue-ness
        if (expOptimize(e.e1, result, true))
            return;                     // error return

        // Convert &*ex to ex
        if (auto pe = e.e1.isPtrExp())
        {
            Expression ex = pe.e1;
            if (e.type.equals(ex.type))
                ret = ex;
            else if (e.type.toBasetype().equivalent(ex.type.toBasetype()))
            {
                ret = ex.copy();
                ret.type = e.type;
            }
            return;
        }
        if (auto ve = e.e1.isVarExp())
        {
            if (!ve.var.isReference() && !ve.var.isImportedSymbol())
            {
                ret = new SymOffExp(e.loc, ve.var, 0, ve.hasOverloads);
                ret.type = e.type;
                return;
            }
        }
        if (e.e1.isDotVarExp())
        {
            /******************************
             * Run down the left side of the a.b.c expression to determine the
             * leftmost variable being addressed (`a`), and accumulate the offsets of the `.b` and `.c`.
             * Params:
             *      e = the DotVarExp or VarExp
             *      var = set to the VarExp at the end, or null if doesn't end in VarExp
             *      eint = set to the IntegerExp at the end, or null if doesn't end in IntegerExp
             *      offset = accumulation of all the .var offsets encountered
             * Returns: true on error
             */
            static bool getVarAndOffset(Expression e, out VarDeclaration var, out IntegerExp eint, ref uint offset)
            {
                if (e.type.size() == SIZE_INVALID)  // trigger computation of v.offset
                    return true;

                if (auto dve = e.isDotVarExp())
                {
                    auto v = dve.var.isVarDeclaration();
                    if (!v || !v.isField() || v.isBitFieldDeclaration())
                        return false;

                    if (getVarAndOffset(dve.e1, var, eint, offset))
                        return true;
                    offset += v.offset;
                }
                else if (auto ve = e.isVarExp())
                {
                    if (!ve.var.isReference() &&
                        !ve.var.isImportedSymbol() &&
                        ve.var.isDataseg() &&
                        ve.var.isCsymbol())
                    {
                        var = ve.var.isVarDeclaration();
                    }
                }
                else if (auto ep = e.isPtrExp())
                {
                    if (auto ei = ep.e1.isIntegerExp())
                    {
                        eint = ei;
                    }
                    else if (auto se = ep.e1.isSymOffExp())
                    {
                        if (!se.var.isReference() &&
                            !se.var.isImportedSymbol() &&
                            se.var.isDataseg())
                        {
                            var = se.var.isVarDeclaration();
                            offset += se.offset;
                        }
                    }
                }
                else if (auto ei = e.isIndexExp())
                {
                    if (auto ve = ei.e1.isVarExp())
                    {
                        if (!ve.var.isReference() &&
                            !ve.var.isImportedSymbol() &&
                            ve.var.isDataseg() &&
                            ve.var.isCsymbol())
                        {
                            if (auto ie = ei.e2.isIntegerExp())
                            {
                                var = ve.var.isVarDeclaration();
                                offset += ie.toInteger() * ve.type.toBasetype().nextOf().size();
                            }
                        }
                    }
                }
                return false;
            }

            uint offset;
            VarDeclaration var;
            IntegerExp eint;
            if (getVarAndOffset(e.e1, var, eint, offset))
            {
                ret = ErrorExp.get();
                return;
            }
            if (var)
            {
                ret = new SymOffExp(e.loc, var, offset, false);
                ret.type = e.type;
                return;
            }
            if (eint)
            {
                ret = new IntegerExp(e.loc, eint.toInteger() + offset, e.type);
                return;
            }
        }
        else if (auto ae = e.e1.isIndexExp())
        {
            if (ae.e2.isIntegerExp() && ae.e1.isIndexExp())
            {
                /* Rewrite `(a[i])[index]` to `(&a[i]) + index*size`
                 */
                sinteger_t index = ae.e2.toInteger();
                auto ae1 = ae.e1.isIndexExp();          // ae1 is a[i]
                if (auto ts = ae1.type.isTypeSArray())
                {
                    sinteger_t dim = ts.dim.toInteger();

                    if (index < 0 || index > dim)
                    {
                        e.error("array index %lld is out of bounds `[0..%lld]`", index, dim);
                        return error();
                    }

                    import core.checkedint : mulu;
                    bool overflow;
                    const offset = mulu(index, ts.nextOf().size(e.loc), overflow); // offset = index*size
                    if (overflow)
                    {
                        e.error("array offset overflow");
                        return error();
                    }

                    Expression ex = new AddrExp(ae1.loc, ae1);  // &a[i]
                    ex.type = ae1.type.pointerTo();

                    Expression add = new AddExp(ae.loc, ex, new IntegerExp(ae.e2.loc, offset, ae.e2.type));
                    add.type = e.type;
                    ret = Expression_optimize(add, result, keepLvalue);
                    return;
                }
            }

            // Convert &array[n] to &array+n
            if (ae.e2.isIntegerExp() && ae.e1.isVarExp())
            {
                sinteger_t index = ae.e2.toInteger();
                VarExp ve = ae.e1.isVarExp();
                if (ve.type.isTypeSArray() && !ve.var.isImportedSymbol())
                {
                    TypeSArray ts = ve.type.isTypeSArray();
                    sinteger_t dim = ts.dim.toInteger();
                    if (index < 0 || index >= dim)
                    {
                        /* 0 for C static arrays means size is unknown, no need to check,
                         * and address one past the end is OK, too
                         */
                        if (!((dim == 0 || dim == index) && ve.var.isCsymbol()))
                        {
                            e.error("array index %lld is out of bounds `[0..%lld]`", index, dim);
                            return error();
                        }
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
            // Convert &((a.b)[index]) to (&a.b)+index*elementsize
            else if (ae.e2.isIntegerExp() && ae.e1.isDotVarExp())
            {
                sinteger_t index = ae.e2.toInteger();
                DotVarExp ve = ae.e1.isDotVarExp();
                if (ve.type.isTypeSArray() && ve.var.isField() && ve.e1.isPtrExp())
                {
                    TypeSArray ts = ve.type.isTypeSArray();
                    sinteger_t dim = ts.dim.toInteger();
                    if (index < 0 || index >= dim)
                    {
                        /* 0 for C static arrays means size is unknown, no need to check,
                         * and address one past the end is OK, too
                         */
                        if (!((dim == 0 || dim == index) && ve.var.isCsymbol()))
                        {
                            e.error("array index %lld is out of bounds `[0..%lld]`", index, dim);
                            return error();
                        }
                    }

                    import core.checkedint : mulu;
                    bool overflow;
                    const offset = mulu(index, ts.nextOf().size(e.loc), overflow); // index*elementsize
                    if (overflow)
                    {
                        e.error("array offset overflow");
                        return error();
                    }

                    auto pe = new AddrExp(e.loc, ve);
                    pe.type = e.type;
                    ret = new AddExp(e.loc, pe, new IntegerExp(e.loc, offset, Type.tsize_t));
                    ret.type = e.type;
                    return;
                }
            }
        }
    }

    void visitPtr(PtrExp e)
    {
        //printf("PtrExp::optimize(result = x%x) %s\n", result, e.toChars());
        if (expOptimize(e.e1, result))
            return;
        // Convert *&ex to ex
        // But only if there is no type punning involved
        if (auto ey = e.e1.isAddrExp())
        {
            Expression ex = ey.e1;
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
        if (e.e1.op == EXP.add)
        {
            Expression ex = Ptr(e.type, e.e1).copy();
            if (!CTFEExp.isCantExp(ex))
            {
                ret = ex;
                return;
            }
        }
        if (auto se = e.e1.isSymOffExp())
        {
            VarDeclaration v = se.var.isVarDeclaration();
            Expression ex = expandVar(result, v);
            if (ex && ex.isStructLiteralExp())
            {
                StructLiteralExp sle = ex.isStructLiteralExp();
                ex = sle.getField(e.type, cast(uint)se.offset);
                if (ex && !CTFEExp.isCantExp(ex))
                {
                    ret = ex;
                    return;
                }
            }
        }
    }

    void visitDotVar(DotVarExp e)
    {
        //printf("DotVarExp::optimize(result = x%x) %s\n", result, e.toChars());
        if (expOptimize(e.e1, result))
            return;
        if (keepLvalue)
            return;
        Expression ex = e.e1;
        if (auto ve = ex.isVarExp())
        {
            VarDeclaration v = ve.var.isVarDeclaration();
            ex = expandVar(result, v);
        }
        if (ex && ex.isStructLiteralExp())
        {
            StructLiteralExp sle = ex.isStructLiteralExp();
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

    void visitNew(NewExp e)
    {
        expOptimize(e.thisexp, WANTvalue);
        // Optimize parameters
        if (e.arguments)
        {
            foreach (ref arg; (*e.arguments)[])
            {
                expOptimize(arg, WANTvalue);
            }
        }
    }

    void visitCall(CallExp e)
    {
        //printf("CallExp::optimize(result = %d) %s\n", result, e.toChars());
        // Optimize parameters with keeping lvalue-ness
        if (expOptimize(e.e1, result))
            return;
        if (e.arguments)
        {
            Type t1 = e.e1.type.toBasetype();
            if (auto td = t1.isTypeDelegate())
                t1 = td.next;
            // t1 can apparently be void for __ArrayDtor(T) calls
            if (auto tf = t1.isTypeFunction())
            {
                foreach (i, ref arg; (*e.arguments)[])
                {
                    Parameter p = tf.parameterList[i];
                    bool keep = p && p.isReference();
                    expOptimize(arg, WANTvalue, keep);
                }
            }
        }
    }

    void visitCast(CastExp e)
    {
        //printf("CastExp::optimize(result = %d) %s\n", result, e.toChars());
        //printf("from %s to %s\n", e.type.toChars(), e.to.toChars());
        //printf("from %s\n", e.type.toChars());
        //printf("e1.type %s\n", e.e1.type.toChars());
        //printf("type = %p\n", e.type);
        assert(e.type);
        const op1 = e.e1.op;
        Expression e1old = e.e1;
        if (expOptimize(e.e1, result, keepLvalue))
            return;
        if (!keepLvalue)
            e.e1 = fromConstInitializer(result, e.e1);
        if (e.e1 == e1old && e.e1.op == EXP.arrayLiteral && e.type.toBasetype().ty == Tpointer && e.e1.type.toBasetype().ty != Tsarray)
        {
            // Casting this will result in the same expression, and
            // infinite loop because of Expression::implicitCastTo()
            return; // no change
        }
        if ((e.e1.op == EXP.string_ || e.e1.op == EXP.arrayLiteral) &&
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

        // Returning e.e1 with changing its type
        void returnE_e1()
        {
            ret = (e1old == e.e1 ? e.e1.copy() : e.e1);
            ret.type = e.type;
        }

        if (e.e1.op == EXP.structLiteral && e.e1.type.implicitConvTo(e.type) >= MATCH.constant)
        {
            //printf(" returning2 %s\n", e.e1.toChars());
            return returnE_e1();
        }
        /* The first test here is to prevent infinite loops
         */
        if (op1 != EXP.arrayLiteral && e.e1.op == EXP.arrayLiteral)
        {
            ret = e.e1.castTo(null, e.to);
            return;
        }
        if (e.e1.op == EXP.null_ && (e.type.ty == Tpointer || e.type.ty == Tclass || e.type.ty == Tarray))
        {
            //printf(" returning3 %s\n", e.e1.toChars());
            return returnE_e1();
        }
        if (e.type.ty == Tclass && e.e1.type.ty == Tclass)
        {
            import dmd.astenums : Sizeok;

            // See if we can remove an unnecessary cast
            ClassDeclaration cdfrom = e.e1.type.isClassHandle();
            ClassDeclaration cdto = e.type.isClassHandle();
            if (cdfrom.errors || cdto.errors)
                return error();
            if (cdto == ClassDeclaration.object && !cdfrom.isInterfaceDeclaration())
                return returnE_e1();    // can always convert a class to Object
            // Need to determine correct offset before optimizing away the cast.
            // https://issues.dlang.org/show_bug.cgi?id=16980
            if (cdfrom.size(e.loc) == SIZE_INVALID)
                return error();
            assert(cdfrom.sizeok == Sizeok.done);
            assert(cdto.sizeok == Sizeok.done || !cdto.isBaseOf(cdfrom, null));
            int offset;
            if (cdto.isBaseOf(cdfrom, &offset) && offset == 0)
            {
                //printf(" returning4 %s\n", e.e1.toChars());
                return returnE_e1();
            }
        }
        if (e.e1.type.mutableOf().unSharedOf().equals(e.to.mutableOf().unSharedOf()))
        {
            //printf(" returning5 %s\n", e.e1.toChars());
            return returnE_e1();
        }
        if (e.e1.isConst())
        {
            if (e.e1.op == EXP.symbolOffset)
            {
                if (e.type.toBasetype().ty != Tsarray)
                {
                    const esz = e.type.size(e.loc);
                    const e1sz = e.e1.type.size(e.e1.loc);
                    if (esz == SIZE_INVALID ||
                        e1sz == SIZE_INVALID)
                        return error();

                    if (esz == e1sz)
                        return returnE_e1();
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

    void visitBinAssign(BinAssignExp e)
    {
        //printf("BinAssignExp::optimize(result = %d) %s\n", result, e.toChars());
        if (binOptimize(e, result, /*keepLhsLvalue*/ true))
            return;
        if (e.op == EXP.leftShiftAssign || e.op == EXP.rightShiftAssign || e.op == EXP.unsignedRightShiftAssign)
        {
            if (e.e2.isConst() == 1)
            {
                sinteger_t i2 = e.e2.toInteger();
                uinteger_t sz = e.e1.type.size(e.e1.loc);
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

    void visitBin(BinExp e)
    {
        //printf("BinExp::optimize(result = %d) %s\n", result, e.toChars());
        const keepLhsLvalue = e.op == EXP.construct || e.op == EXP.blit || e.op == EXP.assign
            || e.op == EXP.plusPlus || e.op == EXP.minusMinus
            || e.op == EXP.prePlusPlus || e.op == EXP.preMinusMinus;
        binOptimize(e, result, keepLhsLvalue);
    }

    void visitAdd(AddExp e)
    {
        //printf("AddExp::optimize(%s)\n", e.toChars());
        if (binOptimize(e, result))
            return;
        if (e.e1.isConst() && e.e2.isConst())
        {
            if (e.e1.op == EXP.symbolOffset && e.e2.op == EXP.symbolOffset)
                return;
            ret = Add(e.loc, e.type, e.e1, e.e2).copy();
        }
    }

    void visitMin(MinExp e)
    {
        //printf("MinExp::optimize(%s)\n", e.toChars());
        if (binOptimize(e, result))
            return;
        if (e.e1.isConst() && e.e2.isConst())
        {
            if (e.e2.op == EXP.symbolOffset)
                return;
            ret = Min(e.loc, e.type, e.e1, e.e2).copy();
        }
    }

    void visitMul(MulExp e)
    {
        //printf("MulExp::optimize(result = %d) %s\n", result, e.toChars());
        if (binOptimize(e, result))
            return;
        if (e.e1.isConst() == 1 && e.e2.isConst() == 1)
        {
            ret = Mul(e.loc, e.type, e.e1, e.e2).copy();
        }
    }

    void visitDiv(DivExp e)
    {
        //printf("DivExp::optimize(%s)\n", e.toChars());
        if (binOptimize(e, result))
            return;
        if (e.e1.isConst() == 1 && e.e2.isConst() == 1)
        {
            ret = Div(e.loc, e.type, e.e1, e.e2).copy();
        }
    }

    void visitMod(ModExp e)
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
            uinteger_t sz = e.e1.type.size(e.e1.loc);
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

    void visitShl(ShlExp e)
    {
        //printf("ShlExp::optimize(result = %d) %s\n", result, e.toChars());
        shift_optimize(e, &Shl);
    }

    void visitShr(ShrExp e)
    {
        //printf("ShrExp::optimize(result = %d) %s\n", result, e.toChars());
        shift_optimize(e, &Shr);
    }

    void visitUshr(UshrExp e)
    {
        //printf("UshrExp::optimize(result = %d) %s\n", result, toChars());
        shift_optimize(e, &Ushr);
    }

    void visitAnd(AndExp e)
    {
        if (binOptimize(e, result))
            return;
        if (e.e1.isConst() == 1 && e.e2.isConst() == 1)
            ret = And(e.loc, e.type, e.e1, e.e2).copy();
    }

    void visitOr(OrExp e)
    {
        if (binOptimize(e, result))
            return;
        if (e.e1.isConst() == 1 && e.e2.isConst() == 1)
            ret = Or(e.loc, e.type, e.e1, e.e2).copy();
    }

    void visitXor(XorExp e)
    {
        if (binOptimize(e, result))
            return;
        if (e.e1.isConst() == 1 && e.e2.isConst() == 1)
            ret = Xor(e.loc, e.type, e.e1, e.e2).copy();
    }

    void visitPow(PowExp e)
    {
        if (binOptimize(e, result))
            return;
        // All negative integral powers are illegal.
        if (e.e1.type.isintegral() && (e.e2.op == EXP.int64) && cast(sinteger_t)e.e2.toInteger() < 0)
        {
            e.error("cannot raise `%s` to a negative integer power. Did you mean `(cast(real)%s)^^%s` ?", e.e1.type.toBasetype().toChars(), e.e1.toChars(), e.e2.toChars());
            return error();
        }
        // If e2 *could* have been an integer, make it one.
        if (e.e2.op == EXP.float64 && e.e2.toReal() == real_t(cast(sinteger_t)e.e2.toReal()))
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

    void visitComma(CommaExp e)
    {
        //printf("CommaExp::optimize(result = %d) %s\n", result, e.toChars());
        // Comma needs special treatment, because it may
        // contain compiler-generated declarations. We can interpret them, but
        // otherwise we must NOT attempt to constant-fold them.
        // In particular, if the comma returns a temporary variable, it needs
        // to be an lvalue (this is particularly important for struct constructors)
        expOptimize(e.e1, WANTvalue);
        expOptimize(e.e2, result, keepLvalue);
        if (ret.op == EXP.error)
            return;
        if (!e.e1 || e.e1.op == EXP.int64 || e.e1.op == EXP.float64 || !hasSideEffect(e.e1))
        {
            ret = e.e2;
            if (ret)
                ret.type = e.type;
        }
        //printf("-CommaExp::optimize(result = %d) %s\n", result, e.e.toChars());
    }

    void visitArrayLength(ArrayLengthExp e)
    {
        //printf("ArrayLengthExp::optimize(result = %d) %s\n", result, e.toChars());
        if (unaOptimize(e, WANTexpand))
            return;
        // CTFE interpret static immutable arrays (to get better diagnostics)
        if (auto ve = e.e1.isVarExp())
        {
            VarDeclaration v = ve.var.isVarDeclaration();
            if (v && (v.storage_class & STC.static_) && (v.storage_class & STC.immutable_) && v._init)
            {
                if (Expression ci = v.getConstInitializer())
                    e.e1 = ci;
            }
        }
        if (e.e1.op == EXP.string_ || e.e1.op == EXP.arrayLiteral || e.e1.op == EXP.assocArrayLiteral || e.e1.type.toBasetype().ty == Tsarray || e.e1.op == EXP.null_)
        {
            ret = ArrayLength(e.type, e.e1).copy();
        }
    }

    void visitEqual(EqualExp e)
    {
        //printf("EqualExp::optimize(result = %x) %s\n", result, e.toChars());
        if (binOptimize(e, WANTvalue))
            return;
        Expression e1 = fromConstInitializer(result, e.e1);
        Expression e2 = fromConstInitializer(result, e.e2);
        if (e1.op == EXP.error)
        {
            ret = e1;
            return;
        }
        if (e2.op == EXP.error)
        {
            ret = e2;
            return;
        }
        ret = Equal(e.op, e.loc, e.type, e1, e2).copy();
        if (CTFEExp.isCantExp(ret))
            ret = e;
    }

    void visitIdentity(IdentityExp e)
    {
        //printf("IdentityExp::optimize(result = %d) %s\n", result, e.toChars());
        if (binOptimize(e, WANTvalue))
            return;
        if ((e.e1.isConst() && e.e2.isConst()) || (e.e1.op == EXP.null_ && e.e2.op == EXP.null_))
        {
            ret = Identity(e.op, e.loc, e.type, e.e1, e.e2).copy();
            if (CTFEExp.isCantExp(ret))
                ret = e;
        }
    }

    void visitIndex(IndexExp e)
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
        if (keepLvalue && ex.op == EXP.arrayLiteral)
            return;
        ret = Index(e.type, ex, e.e2, e.indexIsInBounds).copy();
        if (CTFEExp.isCantExp(ret) || (!ret.isErrorExp() && keepLvalue && !ret.isLvalue()))
            ret = e;
    }

    void visitSlice(SliceExp e)
    {
        //printf("SliceExp::optimize(result = %d) %s\n", result, e.toChars());
        if (expOptimize(e.e1, result & WANTexpand))
            return;
        if (!e.lwr)
        {
            if (e.e1.op == EXP.string_)
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
            if (ret.op == EXP.error)
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
        if (ret.op == EXP.string_)
        {
            e.e1 = ret;
            e.lwr = null;
            e.upr = null;
            ret = e;
        }
        //printf("-SliceExp::optimize() %s\n", ret.toChars());
    }

    void visitLogical(LogicalExp e)
    {
        //printf("LogicalExp::optimize(%d) %s\n", result, e.toChars());
        if (expOptimize(e.e1, WANTvalue))
            return;
        const oror = e.op == EXP.orOr;
        if (e.e1.toBool().hasValue(oror))
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
            const e1Opt = e.e1.toBool();
            if (e.e2.isConst())
            {
                bool n1 = e1Opt.get();
                bool n2 = e.e2.toBool().get();
                ret = new IntegerExp(e.loc, oror ? (n1 || n2) : (n1 && n2), e.type);
            }
            else if (e1Opt.hasValue(!oror))
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

    void visitCmp(CmpExp e)
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

    void visitCat(CatExp e)
    {
        //printf("CatExp::optimize(%d) %s\n", result, e.toChars());
        if (binOptimize(e, result))
            return;
        if (auto ce1 = e.e1.isCatExp())
        {
            // https://issues.dlang.org/show_bug.cgi?id=12798
            // optimize ((expr ~ str1) ~ str2)
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
        if (auto se1 = e.e1.isSliceExp())
        {
            if (se1.e1.op == EXP.string_ && !se1.lwr)
                e.e1 = se1.e1;
        }
        if (auto se2 = e.e2.isSliceExp())
        {
            if (se2.e1.op == EXP.string_ && !se2.lwr)
                e.e2 = se2.e1;
        }
        ret = Cat(e.loc, e.type, e.e1, e.e2).copy();
        if (CTFEExp.isCantExp(ret))
            ret = e;
    }

    void visitCond(CondExp e)
    {
        if (expOptimize(e.econd, WANTvalue))
            return;
        const opt = e.econd.toBool();
        if (opt.hasValue(true))
            ret = Expression_optimize(e.e1, result, keepLvalue);
        else if (opt.hasValue(false))
            ret = Expression_optimize(e.e2, result, keepLvalue);
        else
        {
            expOptimize(e.e1, result, keepLvalue);
            expOptimize(e.e2, result, keepLvalue);
        }
    }

    // Optimize the expression until it can no longer be simplified.
    size_t b;
    while (1)
    {
        if (b++ == global.recursionLimit)
        {
            e.error("infinite loop while optimizing expression");
            fatal();
        }

        auto ex = ret;
        switch (ex.op)
        {
            case EXP.variable:          visitVar(ex.isVarExp()); break;
            case EXP.tuple:             visitTuple(ex.isTupleExp()); break;
            case EXP.arrayLiteral:      visitArrayLiteral(ex.isArrayLiteralExp()); break;
            case EXP.assocArrayLiteral: visitAssocArrayLiteral(ex.isAssocArrayLiteralExp()); break;
            case EXP.structLiteral:     visitStructLiteral(ex.isStructLiteralExp()); break;

            case EXP.import_:
            case EXP.assert_:
            case EXP.dotIdentifier:
            case EXP.dotTemplateDeclaration:
            case EXP.dotTemplateInstance:
            case EXP.delegate_:
            case EXP.dotType:
            case EXP.uadd:
            case EXP.delete_:
            case EXP.vector:
            case EXP.vectorArray:
            case EXP.array:
            case EXP.delegatePointer:
            case EXP.delegateFunctionPointer:
            case EXP.preMinusMinus:
            case EXP.prePlusPlus:       visitUna(cast(UnaExp)ex); break;

            case EXP.negate:            visitNeg(ex.isNegExp()); break;
            case EXP.tilde:             visitCom(ex.isComExp()); break;
            case EXP.not:               visitNop(ex.isNotExp()); break;
            case EXP.symbolOffset:      visitSymOff(ex.isSymOffExp()); break;
            case EXP.address:           visitAddr(ex.isAddrExp()); break;
            case EXP.star:              visitPtr(ex.isPtrExp()); break;
            case EXP.dotVariable:       visitDotVar(ex.isDotVarExp()); break;
            case EXP.new_:              visitNew(ex.isNewExp()); break;
            case EXP.call:              visitCall(ex.isCallExp()); break;
            case EXP.cast_:             visitCast(ex.isCastExp()); break;

            case EXP.addAssign:
            case EXP.minAssign:
            case EXP.mulAssign:
            case EXP.divAssign:
            case EXP.modAssign:
            case EXP.andAssign:
            case EXP.orAssign:
            case EXP.xorAssign:
            case EXP.powAssign:
            case EXP.leftShiftAssign:
            case EXP.rightShiftAssign:
            case EXP.unsignedRightShiftAssign:
            case EXP.concatenateElemAssign:
            case EXP.concatenateDcharAssign:
            case EXP.concatenateAssign: visitBinAssign(ex.isBinAssignExp()); break;

            case EXP.minusMinus:
            case EXP.plusPlus:
            case EXP.assign:
            case EXP.construct:
            case EXP.blit:
            case EXP.in_:
            case EXP.remove:
            case EXP.dot:                       visitBin(cast(BinExp)ex); break;

            case EXP.add:                       visitAdd(ex.isAddExp()); break;
            case EXP.min:                       visitMin(ex.isMinExp()); break;
            case EXP.mul:                       visitMul(ex.isMulExp()); break;
            case EXP.div:                       visitDiv(ex.isDivExp()); break;
            case EXP.mod:                       visitMod(ex.isModExp()); break;
            case EXP.leftShift:                 visitShl(ex.isShlExp()); break;
            case EXP.rightShift:                visitShr(ex.isShrExp()); break;
            case EXP.unsignedRightShift:        visitUshr(ex.isUshrExp()); break;
            case EXP.and:                       visitAnd(ex.isAndExp()); break;
            case EXP.or:                        visitOr(ex.isOrExp()); break;
            case EXP.xor:                       visitXor(ex.isXorExp()); break;
            case EXP.pow:                       visitPow(ex.isPowExp()); break;
            case EXP.comma:                     visitComma(ex.isCommaExp()); break;
            case EXP.arrayLength:               visitArrayLength(ex.isArrayLengthExp()); break;
            case EXP.notEqual:
            case EXP.equal:                     visitEqual(ex.isEqualExp()); break;
            case EXP.notIdentity:
            case EXP.identity:                  visitIdentity(ex.isIdentityExp()); break;
            case EXP.index:                     visitIndex(ex.isIndexExp()); break;
            case EXP.slice:                     visitSlice(ex.isSliceExp()); break;
            case EXP.andAnd:
            case EXP.orOr:                      visitLogical(ex.isLogicalExp()); break;
            case EXP.lessThan:
            case EXP.lessOrEqual:
            case EXP.greaterThan:
            case EXP.greaterOrEqual:            visitCmp(cast(CmpExp)ex); break;
            case EXP.concatenate:               visitCat(ex.isCatExp()); break;
            case EXP.question:                  visitCond(ex.isCondExp()); break;

            default:                            visitExp(ex); break;
        }

        if (ex == ret)
            break;
    }
    return ret;
}
