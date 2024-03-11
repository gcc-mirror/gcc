/**
 * A depth-first visitor for expressions.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/apply.d, _apply.d)
 * Documentation:  https://dlang.org/phobos/dmd_apply.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/apply.d
 */

module dmd.postordervisitor;

import dmd.arraytypes;
import dmd.dtemplate;
import dmd.expression;
import dmd.root.array;
import dmd.visitor;

bool walkPostorder(Expression e, StoppableVisitor v)
{
    scope PostorderExpressionVisitor pv = new PostorderExpressionVisitor(v);
    e.accept(pv);
    return v.stop;
}

/**************************************
 * An Expression tree walker that will visit each Expression e in the tree,
 * in depth-first evaluation order, and call fp(e,param) on it.
 * fp() signals whether the walking continues with its return value:
 * Returns:
 *      0       continue
 *      1       done
 * It's a bit slower than using virtual functions, but more encapsulated and less brittle.
 * Creating an iterator for this would be much more complex.
 */
private extern (C++) final class PostorderExpressionVisitor : StoppableVisitor
{
    alias visit = typeof(super).visit;
public:
    StoppableVisitor v;

    extern (D) this(StoppableVisitor v) scope @safe
    {
        this.v = v;
    }

    bool doCond(Expression e)
    {
        if (!stop && e)
            e.accept(this);
        return stop;
    }

    extern(D) bool doCond(Expression[] e)
    {
        for (size_t i = 0; i < e.length && !stop; i++)
            doCond(e[i]);
        return stop;
    }

    bool applyTo(Expression e)
    {
        e.accept(v);
        stop = v.stop;
        return true;
    }

    override void visit(Expression e)
    {
        applyTo(e);
    }

    override void visit(NewExp e)
    {
        //printf("NewExp::apply(): %s\n", toChars());
        doCond(e.thisexp) || doCond(e.arguments.peekSlice()) || applyTo(e);
    }

    override void visit(NewAnonClassExp e)
    {
        //printf("NewAnonClassExp::apply(): %s\n", toChars());
        doCond(e.thisexp) || doCond(e.arguments.peekSlice()) || applyTo(e);
    }

    override void visit(TypeidExp e)
    {
        doCond(isExpression(e.obj)) || applyTo(e);
    }

    override void visit(UnaExp e)
    {
        doCond(e.e1) || applyTo(e);
    }

    override void visit(BinExp e)
    {
        doCond(e.e1) || doCond(e.e2) || applyTo(e);
    }

    override void visit(AssertExp e)
    {
        //printf("CallExp::apply(apply_fp_t fp, void *param): %s\n", toChars());
        doCond(e.e1) || doCond(e.msg) || applyTo(e);
    }

    override void visit(CallExp e)
    {
        //printf("CallExp::apply(apply_fp_t fp, void *param): %s\n", toChars());
        doCond(e.e1) || doCond(e.arguments.peekSlice()) || applyTo(e);
    }

    override void visit(ArrayExp e)
    {
        //printf("ArrayExp::apply(apply_fp_t fp, void *param): %s\n", toChars());
        doCond(e.e1) || doCond(e.arguments.peekSlice()) || applyTo(e);
    }

    override void visit(SliceExp e)
    {
        doCond(e.e1) || doCond(e.lwr) || doCond(e.upr) || applyTo(e);
    }

    override void visit(ArrayLiteralExp e)
    {
        doCond(e.basis) || doCond(e.elements.peekSlice()) || applyTo(e);
    }

    override void visit(AssocArrayLiteralExp e)
    {
        doCond(e.keys.peekSlice()) || doCond(e.values.peekSlice()) || applyTo(e);
    }

    override void visit(StructLiteralExp e)
    {
        if (e.stageflags & stageApply)
            return;
        const old = e.stageflags;
        e.stageflags |= stageApply;
        doCond(e.elements.peekSlice()) || applyTo(e);
        e.stageflags = old;
    }

    override void visit(TupleExp e)
    {
        doCond(e.e0) || doCond(e.exps.peekSlice()) || applyTo(e);
    }

    override void visit(CondExp e)
    {
        doCond(e.econd) || doCond(e.e1) || doCond(e.e2) || applyTo(e);
    }
}
