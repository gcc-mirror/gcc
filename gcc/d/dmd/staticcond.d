/**
 * Lazily evaluate static conditions for `static if`, `static assert` and template constraints.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/staticcond.d, _staticcond.d)
 * Documentation:  https://dlang.org/phobos/dmd_staticcond.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/staticcond.d
 */

module dmd.staticcond;

import dmd.expression;
import dmd.root.array;
import dmd.common.outbuffer;
import dmd.tokens;




/********************************************
 * Format a static condition as a tree-like structure, marking failed and
 * bypassed expressions.
 * Params:
 *      original = original expression
 *      instantiated = instantiated expression
 *      negatives = array with negative clauses from `instantiated` expression
 *      full = controls whether it shows the full output or only failed parts
 *      itemCount = returns the number of written clauses
 * Returns:
 *      formatted string or `null` if the expressions were `null`, or if the
 *      instantiated expression is not based on the original one
 */
const(char)* visualizeStaticCondition(Expression original, Expression instantiated,
    const Expression[] negatives, bool full, ref uint itemCount)
{
    if (!original || !instantiated || original.loc !is instantiated.loc)
        return null;

    OutBuffer buf;

    if (full)
        itemCount = visualizeFull(original, instantiated, negatives, buf);
    else
        itemCount = visualizeShort(original, instantiated, negatives, buf);

    return buf.extractChars();
}

private uint visualizeFull(Expression original, Expression instantiated,
    const Expression[] negatives, ref OutBuffer buf)
{
    // tree-like structure; traverse and format simultaneously
    uint count;
    uint indent;

    static void printOr(uint indent, ref OutBuffer buf)
    {
        buf.reserve(indent * 4 + 8);
        foreach (i; 0 .. indent)
            buf.writestring("    ");
        buf.writestring("    or:\n");
    }

    // returns true if satisfied
    bool impl(Expression orig, Expression e, bool inverted, bool orOperand, bool unreached)
    {
        EXP op = orig.op;

        // lower all 'not' to the bottom
        // !(A && B) -> !A || !B
        // !(A || B) -> !A && !B
        if (inverted)
        {
            if (op == EXP.andAnd)
                op = EXP.orOr;
            else if (op == EXP.orOr)
                op = EXP.andAnd;
        }

        if (op == EXP.not)
        {
            NotExp no = cast(NotExp)orig;
            NotExp ne = cast(NotExp)e;
            assert(ne);
            return impl(no.e1, ne.e1, !inverted, orOperand, unreached);
        }
        else if (op == EXP.andAnd)
        {
            BinExp bo = cast(BinExp)orig;
            BinExp be = cast(BinExp)e;
            assert(be);
            const r1 = impl(bo.e1, be.e1, inverted, false, unreached);
            const r2 = impl(bo.e2, be.e2, inverted, false, unreached || !r1);
            return r1 && r2;
        }
        else if (op == EXP.orOr)
        {
            if (!orOperand) // do not indent A || B || C twice
                indent++;
            BinExp bo = cast(BinExp)orig;
            BinExp be = cast(BinExp)e;
            assert(be);
            const r1 = impl(bo.e1, be.e1, inverted, true, unreached);
            printOr(indent, buf);
            const r2 = impl(bo.e2, be.e2, inverted, true, unreached);
            if (!orOperand)
                indent--;
            return r1 || r2;
        }
        else if (op == EXP.question)
        {
            CondExp co = cast(CondExp)orig;
            CondExp ce = cast(CondExp)e;
            assert(ce);
            if (!inverted)
            {
                // rewrite (A ? B : C) as (A && B || !A && C)
                if (!orOperand)
                    indent++;
                const r1 = impl(co.econd, ce.econd, inverted, false, unreached);
                const r2 = impl(co.e1, ce.e1, inverted, false, unreached || !r1);
                printOr(indent, buf);
                const r3 = impl(co.econd, ce.econd, !inverted, false, unreached);
                const r4 = impl(co.e2, ce.e2, inverted, false, unreached || !r3);
                if (!orOperand)
                    indent--;
                return r1 && r2 || r3 && r4;
            }
            else
            {
                // rewrite !(A ? B : C) as (!A || !B) && (A || !C)
                if (!orOperand)
                    indent++;
                const r1 = impl(co.econd, ce.econd, inverted, false, unreached);
                printOr(indent, buf);
                const r2 = impl(co.e1, ce.e1, inverted, false, unreached);
                const r12 = r1 || r2;
                const r3 = impl(co.econd, ce.econd, !inverted, false, unreached || !r12);
                printOr(indent, buf);
                const r4 = impl(co.e2, ce.e2, inverted, false, unreached || !r12);
                if (!orOperand)
                    indent--;
                return (r1 || r2) && (r3 || r4);
            }
        }
        else // 'primitive' expression
        {
            buf.reserve(indent * 4 + 4);
            foreach (i; 0 .. indent)
                buf.writestring("    ");

            // find its value; it may be not computed, if there was a short circuit,
            // but we handle this case with `unreached` flag
            bool value = true;
            if (!unreached)
            {
                foreach (fe; negatives)
                {
                    if (fe is e)
                    {
                        value = false;
                        break;
                    }
                }
            }
            // write the marks first
            const satisfied = inverted ? !value : value;
            if (!satisfied && !unreached)
                buf.writestring("  > ");
            else if (unreached)
                buf.writestring("  - ");
            else
                buf.writestring("    ");
            // then the expression itself
            if (inverted)
                buf.writeByte('!');
            buf.writestring(orig.toChars);
            buf.writenl();
            count++;
            return satisfied;
        }
    }

    impl(original, instantiated, false, true, false);
    return count;
}

private uint visualizeShort(Expression original, Expression instantiated,
    const Expression[] negatives, ref OutBuffer buf)
{
    // simple list; somewhat similar to long version, so no comments
    // one difference is that it needs to hold items to display in a stack

    static struct Item
    {
        Expression orig;
        bool inverted;
    }

    Array!Item stack;

    bool impl(Expression orig, Expression e, bool inverted)
    {
        EXP op = orig.op;

        if (inverted)
        {
            if (op == EXP.andAnd)
                op = EXP.orOr;
            else if (op == EXP.orOr)
                op = EXP.andAnd;
        }

        if (op == EXP.not)
        {
            NotExp no = cast(NotExp)orig;
            NotExp ne = cast(NotExp)e;
            assert(ne);
            return impl(no.e1, ne.e1, !inverted);
        }
        else if (op == EXP.andAnd)
        {
            BinExp bo = cast(BinExp)orig;
            BinExp be = cast(BinExp)e;
            assert(be);
            bool r = impl(bo.e1, be.e1, inverted);
            r = r && impl(bo.e2, be.e2, inverted);
            return r;
        }
        else if (op == EXP.orOr)
        {
            BinExp bo = cast(BinExp)orig;
            BinExp be = cast(BinExp)e;
            assert(be);
            const lbefore = stack.length;
            bool r = impl(bo.e1, be.e1, inverted);
            r = r || impl(bo.e2, be.e2, inverted);
            if (r)
                stack.setDim(lbefore); // purge added positive items
            return r;
        }
        else if (op == EXP.question)
        {
            CondExp co = cast(CondExp)orig;
            CondExp ce = cast(CondExp)e;
            assert(ce);
            if (!inverted)
            {
                const lbefore = stack.length;
                bool a = impl(co.econd, ce.econd, inverted);
                a = a && impl(co.e1, ce.e1, inverted);
                bool b;
                if (!a)
                {
                    b = impl(co.econd, ce.econd, !inverted);
                    b = b && impl(co.e2, ce.e2, inverted);
                }
                const r = a || b;
                if (r)
                    stack.setDim(lbefore);
                return r;
            }
            else
            {
                bool a;
                {
                    const lbefore = stack.length;
                    a = impl(co.econd, ce.econd, inverted);
                    a = a || impl(co.e1, ce.e1, inverted);
                    if (a)
                        stack.setDim(lbefore);
                }
                bool b;
                if (a)
                {
                    const lbefore = stack.length;
                    b = impl(co.econd, ce.econd, !inverted);
                    b = b || impl(co.e2, ce.e2, inverted);
                    if (b)
                        stack.setDim(lbefore);
                }
                return a && b;
            }
        }
        else // 'primitive' expression
        {
            bool value = true;
            foreach (fe; negatives)
            {
                if (fe is e)
                {
                    value = false;
                    break;
                }
            }
            const satisfied = inverted ? !value : value;
            if (!satisfied)
                stack.push(Item(orig, inverted));
            return satisfied;
        }
    }

    impl(original, instantiated, false);

    foreach (i; 0 .. stack.length)
    {
        // write the expression only
        buf.writestring("       ");
        if (stack[i].inverted)
            buf.writeByte('!');
        buf.writestring(stack[i].orig.toChars);
        // here with no trailing newline
        if (i + 1 < stack.length)
            buf.writenl();
    }
    return cast(uint)stack.length;
}
