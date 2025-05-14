/**
 * Perform checks for `nothrow`.
 *
 * Specification: $(LINK2 https://dlang.org/spec/function.html#nothrow-functions, Nothrow Functions)
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/canthrow.d, _canthrow.d)
 * Documentation:  https://dlang.org/phobos/dmd_canthrow.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/canthrow.d
 */

module dmd.canthrow;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.attrib;
import dmd.astenums;
import dmd.blockexit : BE, checkThrow;
import dmd.declaration;
import dmd.dsymbol;
import dmd.dsymbolsem : include;
import dmd.errorsink;
import dmd.expression;
import dmd.expressionsem : errorSupplementalInferredAttr;
import dmd.func;
import dmd.globals;
import dmd.init;
import dmd.mtype;
import dmd.tokens;
import dmd.visitor;
import dmd.visitor.postorder;

/**
 * Status indicating what kind of throwable might be caused by an expression.
 *
 * This is a subset of `BE` restricted to the values actually used by `canThrow`.
 */
enum CT : BE
{
    /// Never throws an `Exception` or `Throwable`
    none = BE.none,

    /// Might throw an `Exception`
    exception = BE.throw_,

    // Might throw an `Error`
    error = BE.errthrow,
}

/********************************************
 * If `eSink` is not null, generate an error if `e` throws
 * Params:
 *      e = expression to check for throwing
 *      func = function
 *      eSink = if !null, then send error messages to eSink
 * Returns: `CT.exception` or `CT.error` if the expression may throw exceptions.
 */
CT canThrow(Expression e, FuncDeclaration func, ErrorSink eSink)
{
    //printf("Expression::canThrow(%d) %s\n", mustNotThrow, e.toChars());
    // stop walking if we determine this expression can throw
    extern (C++) final class CanThrow : StoppableVisitor
    {
        alias visit = typeof(super).visit;
        CT result;

    public:
        extern (D) this() scope @safe
        {
        }

        void checkFuncThrows(Expression e, FuncDeclaration f)
        {
            auto tf = f.type.toBasetype().isTypeFunction();
            if (tf && !tf.isNothrow)
            {
                if (eSink)
                {
                    eSink.error(e.loc, "%s `%s` is not `nothrow`", f.kind(), f.toPrettyChars());
                    if (!f.isDtorDeclaration())
                        errorSupplementalInferredAttr(f, 10, false, STC.nothrow_, eSink);

                    import dmd.expressionsem : checkOverriddenDtor;
                    f.checkOverriddenDtor(null, e.loc, dd => dd.type.toTypeFunction().isNothrow, "not nothrow");
                }
                else if (func)
                {
                    func.setThrowCall(e.loc, f);
                }
                result |= CT.exception;
            }
        }

        override void visit(Expression)
        {
        }

        override void visit(DeclarationExp de)
        {
            result |= Dsymbol_canThrow(de.declaration, func, eSink);
        }

        override void visit(CallExp ce)
        {
            if (ce.inDebugStatement)
                return;

            if (global.errors && !ce.e1.type)
                return; // error recovery

            if (ce.f && ce.arguments.length > 0)
            {
                Type tb = (*ce.arguments)[0].type.toBasetype();
                if (auto tbNext = tb.nextOf())
                {
                    if (auto ts = tbNext.baseElemOf().isTypeStruct())
                    {
                        auto sd = ts.sym;
                        const id = ce.f.ident;
                        if (sd.postblit && isArrayConstruction(id))
                        {
                            checkFuncThrows(ce, sd.postblit);
                            return;
                        }
                    }
                }
            }

            /* If calling a function or delegate that is typed as nothrow,
             * then this expression cannot throw.
             * Note that pure functions can throw.
             */
            if (ce.f && ce.f == func)
                return;
            const tf = ce.calledFunctionType();
            if (tf && tf.isNothrow)
                return;

            if (ce.f)
                checkFuncThrows(ce, ce.f);
            else if (eSink)
            {
                auto e1 = ce.e1;
                if (auto pe = e1.isPtrExp())   // print 'fp' if e1 is (*fp)
                    e1 = pe.e1;
                eSink.error(ce.loc, "`%s` is not `nothrow`", e1.toChars());
            }
            result |= CT.exception;
        }

        override void visit(NewExp ne)
        {
            if (ne.member)
            {
                // See if constructor call can throw
                checkFuncThrows(ne, ne.member);
            }
            // regard storage allocation failures as not recoverable
        }

        override void visit(DeleteExp de)
        {
            Type tb = de.e1.type.toBasetype();
            AggregateDeclaration ad = null;
            switch (tb.ty)
            {
            case Tclass:
                ad = tb.isTypeClass().sym;
                break;

            default:
                assert(0);  // error should have been detected by semantic()
            }

            if (ad.dtor)
                checkFuncThrows(de, ad.dtor);
        }

        override void visit(AssignExp ae)
        {
            // blit-init cannot throw
            if (ae.op == EXP.blit)
                return;
            /* Element-wise assignment could invoke postblits.
             */
            Type t;
            if (ae.type.toBasetype().ty == Tsarray)
            {
                if (!ae.e2.isLvalue())
                    return;
                t = ae.type;
            }
            else if (auto se = ae.e1.isSliceExp())
                t = se.e1.type;
            else
                return;

            if (auto ts = t.baseElemOf().isTypeStruct())
                if (auto postblit = ts.sym.postblit)
                    checkFuncThrows(ae, postblit);
        }

        override void visit(ThrowExp te)
        {
            const res = checkThrow(te.loc, te.e1, func, eSink);
            assert((res & ~(CT.exception | CT.error)) == 0);
            result |= res;
        }

        override void visit(NewAnonClassExp)
        {
            assert(0); // should have been lowered by semantic()
        }
    }

    scope CanThrow ct = new CanThrow();
    walkPostorder(e, ct);
    return ct.result;
}

/**************************************
 * Does symbol `s`, when initialized, throw?
 * Mirrors logic in Dsymbol_toElem().
 */
private CT Dsymbol_canThrow(Dsymbol s, FuncDeclaration func, ErrorSink eSink)
{
    CT result;

    int symbolDg(Dsymbol s)
    {
        result |= Dsymbol_canThrow(s, func, eSink);
        return 0;
    }

    //printf("Dsymbol_toElem() %s\n", s.toChars());
    if (auto vd = s.isVarDeclaration())
    {
        s = s.toAlias();
        if (s != vd)
            return Dsymbol_canThrow(s, func, eSink);
        if (vd.storage_class & STC.manifest)
        {
        }
        else if (vd.isStatic() || vd.storage_class & (STC.extern_ | STC.gshared))
        {
        }
        else
        {
            if (vd._init)
            {
                if (auto ie = vd._init.isExpInitializer())
                    result |= canThrow(ie.exp, func, eSink);
            }
            if (vd.needsScopeDtor())
                result |= canThrow(vd.edtor, func, eSink);
        }
    }
    else if (auto ad = s.isAttribDeclaration())
    {
        ad.include(null).foreachDsymbol(&symbolDg);
    }
    else if (auto tm = s.isTemplateMixin())
    {
        tm.members.foreachDsymbol(&symbolDg);
    }
    else if (auto td = s.isTupleDeclaration())
    {
        td.foreachVar(&symbolDg);
    }
    return result;
}
