/**
 * Implement array operations, such as `a[] = b[] + c[]`.
 *
 * Specification: $(LINK2 https://dlang.org/spec/arrays.html#array-operations, Array Operations)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/arrayop.d, _arrayop.d)
 * Documentation:  https://dlang.org/phobos/dmd_arrayop.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/arrayop.d
 */

module dmd.arrayop;

import core.stdc.stdio;
import dmd.arraytypes;
import dmd.astenums;
import dmd.declaration;
import dmd.dscope;
import dmd.dsymbol;
import dmd.errors;
import dmd.expression;
import dmd.expressionsem;
import dmd.funcsem;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.common.outbuffer;
import dmd.tokens;
import dmd.visitor;

/**********************************************
 * Check that there are no uses of arrays without [].
 */
bool isArrayOpValid(Expression e)
{
    //printf("isArrayOpValid() %s\n", e.toChars());
    if (e.op == EXP.slice)
        return true;
    if (e.op == EXP.arrayLiteral)
    {
        Type t = e.type.toBasetype();
        while (t.ty == Tarray || t.ty == Tsarray)
            t = t.nextOf().toBasetype();
        return (t.ty != Tvoid);
    }
    Type tb = e.type.toBasetype();
    if (tb.ty == Tarray || tb.ty == Tsarray)
    {
        if (isUnaArrayOp(e.op))
        {
            return isArrayOpValid(e.isUnaExp().e1);
        }
        if (isBinArrayOp(e.op) || isBinAssignArrayOp(e.op) || e.op == EXP.assign)
        {
            BinExp be = e.isBinExp();
            return isArrayOpValid(be.e1) && isArrayOpValid(be.e2);
        }
        if (e.op == EXP.construct)
        {
            BinExp be = e.isBinExp();
            return be.e1.op == EXP.slice && isArrayOpValid(be.e2);
        }
        // if (e.op == EXP.call)
        // {
        // TODO: Decide if [] is required after arrayop calls.
        // }
        return false;
    }
    return true;
}

bool isNonAssignmentArrayOp(Expression e)
{
    if (e.op == EXP.slice)
        return isNonAssignmentArrayOp(e.isSliceExp().e1);

    Type tb = e.type.toBasetype();
    if (tb.ty == Tarray || tb.ty == Tsarray)
    {
        return (isUnaArrayOp(e.op) || isBinArrayOp(e.op));
    }
    return false;
}

bool checkNonAssignmentArrayOp(Expression e, bool suggestion = false)
{
    if (isNonAssignmentArrayOp(e))
    {
        const(char)* s = "";
        if (suggestion)
            s = " (possible missing [])";
        error(e.loc, "array operation `%s` without destination memory not allowed%s", e.toChars(), s);
        return true;
    }
    return false;
}

/***********************************
 * Construct the array operation expression, call object._arrayOp!(tiargs)(args).
 *
 * Encode operand types and operations into tiargs using reverse polish notation (RPN) to preserve precedence.
 * Unary operations are prefixed with "u" (e.g. "u~").
 * Pass operand values (slices or scalars) as args.
 *
 * Scalar expression sub-trees of `e` are evaluated before calling
 * into druntime to hoist them out of the loop. This is a valid
 * evaluation order as the actual array operations have no
 * side-effect.
 * References:
 * https://github.com/dlang/dmd/blob/cdfadf8a18f474e6a1b8352af2541efe3e3467cc/druntime/src/object.d#L4694
 * https://github.com/dlang/dmd/blob/master/druntime/src/core/internal/array/operations.d
 */
Expression arrayOp(BinExp e, Scope* sc)
{
    //printf("BinExp.arrayOp() %s\n", e.toChars());
    Type tb = e.type.toBasetype();
    assert(tb.ty == Tarray || tb.ty == Tsarray);
    Type tbn = tb.nextOf().toBasetype();
    if (tbn.ty == Tvoid)
    {
        error(e.loc, "cannot perform array operations on `void[]` arrays");
        return ErrorExp.get();
    }
    if (!isArrayOpValid(e))
        return arrayOpInvalidError(e);

    auto tiargs = new Objects();
    auto args = buildArrayOp(sc, e, tiargs);

    import dmd.dtemplate : TemplateDeclaration;
    __gshared TemplateDeclaration arrayOp;
    if (arrayOp is null)
    {
        // Create .object._arrayOp
        Identifier idArrayOp = Identifier.idPool("_arrayOp");
        Expression id = new IdentifierExp(e.loc, Id.empty);
        id = new DotIdExp(e.loc, id, Id.object);
        id = new DotIdExp(e.loc, id, idArrayOp);

        id = id.expressionSemantic(sc);
        if (auto te = id.isTemplateExp())
            arrayOp = te.td;
        else
        {
            ObjectNotFound(e.loc, idArrayOp);   // fatal error
            return ErrorExp.get();
        }
    }

    auto fd = resolveFuncCall(e.loc, sc, arrayOp, tiargs, null, ArgumentList(args), FuncResolveFlag.standard);
    if (!fd || fd.errors)
        return ErrorExp.get();
    return new CallExp(e.loc, new VarExp(e.loc, fd, false), args).expressionSemantic(sc);
}

/// ditto
Expression arrayOp(BinAssignExp e, Scope* sc)
{
    //printf("BinAssignExp.arrayOp() %s\n", e.toChars());

    /* Check that the elements of e1 can be assigned to
     */
    Type tn = e.e1.type.toBasetype().nextOf();

    if (tn && (!tn.isMutable() || !tn.isAssignable()))
    {
        error(e.loc, "slice `%s` is not mutable", e.e1.toChars());
        if (e.op == EXP.addAssign)
            checkPossibleAddCatError!(AddAssignExp, CatAssignExp)(e.isAddAssignExp);
        return ErrorExp.get();
    }
    if (e.e1.op == EXP.arrayLiteral)
    {
        return e.e1.modifiableLvalue(sc);
    }

    return arrayOp(e.isBinExp(), sc);
}

/******************************************
 * Convert the expression tree e to template and function arguments,
 * using reverse polish notation (RPN) to encode order of operations.
 * Encode operations as string arguments, using a "u" prefix for unary operations.
 */
private Expressions* buildArrayOp(Scope* sc, Expression e, Objects* tiargs)
{
    extern (C++) final class BuildArrayOpVisitor : Visitor
    {
        alias visit = Visitor.visit;
        Scope* sc;
        Objects* tiargs;
        Expressions* args;

    public:
        extern (D) this(Scope* sc, Objects* tiargs) scope @safe
        {
            this.sc = sc;
            this.tiargs = tiargs;
            this.args = new Expressions();
        }

        override void visit(Expression e)
        {
            tiargs.push(e.type);
            args.push(e);
        }

        override void visit(SliceExp e)
        {
            visit(cast(Expression) e);
        }

        override void visit(CastExp e)
        {
            visit(cast(Expression) e);
        }

        override void visit(UnaExp e)
        {
            Type tb = e.type.toBasetype();
            if (tb.ty != Tarray && tb.ty != Tsarray) // hoist scalar expressions
            {
                visit(cast(Expression) e);
            }
            else
            {
                // RPN, prefix unary ops with u
                OutBuffer buf;
                buf.writestring("u");
                buf.writestring(EXPtoString(e.op));
                e.e1.accept(this);
                tiargs.push(new StringExp(Loc.initial, buf.extractSlice()).expressionSemantic(sc));
            }
        }

        override void visit(BinExp e)
        {
            Type tb = e.type.toBasetype();
            if (tb.ty != Tarray && tb.ty != Tsarray) // hoist scalar expressions
            {
                visit(cast(Expression) e);
            }
            else
            {
                // RPN
                e.e1.accept(this);
                e.e2.accept(this);
                tiargs.push(new StringExp(Loc.initial, EXPtoString(e.op)).expressionSemantic(sc));
            }
        }
    }

    scope v = new BuildArrayOpVisitor(sc, tiargs);
    e.accept(v);
    return v.args;
}

/***********************************************
 * Some implicit casting can be performed by the _arrayOp template.
 * Params:
 *      tfrom = type converting from
 *      tto   = type converting to
 * Returns:
 *      true if can be performed by _arrayOp
 */
bool isArrayOpImplicitCast(TypeDArray tfrom, TypeDArray tto)
{
    const tyf = tfrom.nextOf().toBasetype().ty;
    const tyt = tto  .nextOf().toBasetype().ty;
    return tyf == tyt ||
           tyf == Tint32 && tyt == Tfloat64;
}

/***********************************************
 * Test if expression is a unary array op.
 */
bool isUnaArrayOp(EXP op) @safe
{
    switch (op)
    {
    case EXP.negate:
    case EXP.tilde:
        return true;
    default:
        break;
    }
    return false;
}

/***********************************************
 * Test if expression is a binary array op.
 */
bool isBinArrayOp(EXP op) @safe
{
    switch (op)
    {
    case EXP.add:
    case EXP.min:
    case EXP.mul:
    case EXP.div:
    case EXP.mod:
    case EXP.xor:
    case EXP.and:
    case EXP.or:
    case EXP.pow:
        return true;
    default:
        break;
    }
    return false;
}

/***********************************************
 * Test if expression is a binary assignment array op.
 */
bool isBinAssignArrayOp(EXP op) @safe
{
    switch (op)
    {
    case EXP.addAssign:
    case EXP.minAssign:
    case EXP.mulAssign:
    case EXP.divAssign:
    case EXP.modAssign:
    case EXP.xorAssign:
    case EXP.andAssign:
    case EXP.orAssign:
    case EXP.powAssign:
        return true;
    default:
        break;
    }
    return false;
}

/***********************************************
 * Test if operand is a valid array op operand.
 */
bool isArrayOpOperand(Expression e)
{
    //printf("Expression.isArrayOpOperand() %s\n", e.toChars());
    if (e.op == EXP.slice)
        return true;
    if (e.op == EXP.arrayLiteral)
    {
        Type t = e.type.toBasetype();
        while (t.ty == Tarray || t.ty == Tsarray)
            t = t.nextOf().toBasetype();
        return (t.ty != Tvoid);
    }
    Type tb = e.type.toBasetype();
    if (tb.ty == Tarray)
    {
        return (isUnaArrayOp(e.op) ||
                isBinArrayOp(e.op) ||
                isBinAssignArrayOp(e.op) ||
                e.op == EXP.assign);
    }
    return false;
}


/***************************************************
 * Print error message about invalid array operation.
 * Params:
 *      e = expression with the invalid array operation
 * Returns:
 *      instance of ErrorExp
 */

ErrorExp arrayOpInvalidError(Expression e)
{
    error(e.loc, "invalid array operation `%s` (possible missing [])", e.toChars());
    if (e.op == EXP.add)
        checkPossibleAddCatError!(AddExp, CatExp)(e.isAddExp());
    else if (e.op == EXP.addAssign)
        checkPossibleAddCatError!(AddAssignExp, CatAssignExp)(e.isAddAssignExp());
    return ErrorExp.get();
}

private void checkPossibleAddCatError(AddT, CatT)(AddT ae)
{
    if (!ae.e2.type || ae.e2.type.ty != Tarray || !ae.e2.type.implicitConvTo(ae.e1.type))
        return;
    CatT ce = new CatT(ae.loc, ae.e1, ae.e2);
    errorSupplemental(ae.loc, "did you mean to concatenate (`%s`) instead ?", ce.toChars());
}
