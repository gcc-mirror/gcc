/**
 * Perform checks for `nothrow`.
 *
 * Specification: $(LINK2 https://dlang.org/spec/function.html#nothrow-functions, Nothrow Functions)
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/canthrow.d, _canthrow.d)
 * Documentation:  https://dlang.org/phobos/dmd_canthrow.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/canthrow.d
 */

module dmd.canthrow;

import dmd.aggregate;
import dmd.arraytypes;
import dmd.attrib;
import dmd.astenums;
import dmd.blockexit : BE, checkThrow;
import dmd.declaration;
import dmd.dsymbol;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.init;
import dmd.mtype;
import dmd.postordervisitor;
import dmd.root.rootobject;
import dmd.tokens;
import dmd.visitor;

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
 * Returns true if the expression may throw exceptions.
 * If 'mustNotThrow' is true, generate an error if it throws
 */
extern (C++) /* CT */ BE canThrow(Expression e, FuncDeclaration func, bool mustNotThrow)
{
    //printf("Expression::canThrow(%d) %s\n", mustNotThrow, e.toChars());
    // stop walking if we determine this expression can throw
    extern (C++) final class CanThrow : StoppableVisitor
    {
        alias visit = typeof(super).visit;
        FuncDeclaration func;
        bool mustNotThrow;
        CT result;

    public:
        extern (D) this(FuncDeclaration func, bool mustNotThrow) scope
        {
            this.func = func;
            this.mustNotThrow = mustNotThrow;
        }

        void checkFuncThrows(Expression e, FuncDeclaration f)
        {
            auto tf = f.type.toBasetype().isTypeFunction();
            if (tf && !tf.isnothrow)
            {
                if (mustNotThrow)
                {
                    e.error("%s `%s` is not `nothrow`", f.kind(), f.toPrettyChars());
                    if (!f.isDtorDeclaration())
                        errorSupplementalInferredAttr(f, 10, false, STC.nothrow_);

                    e.checkOverriddenDtor(null, f, dd => dd.type.toTypeFunction().isnothrow, "not nothrow");
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
            result |= Dsymbol_canThrow(de.declaration, func, mustNotThrow);
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
                auto tbNext = tb.nextOf();
                if (tbNext)
                {
                    auto ts = tbNext.baseElemOf().isTypeStruct();
                    if (ts)
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
            if (tf && tf.isnothrow)
                return;

            if (ce.f)
                checkFuncThrows(ce, ce.f);
            else if (mustNotThrow)
            {
                auto e1 = ce.e1;
                if (auto pe = e1.isPtrExp())   // print 'fp' if e1 is (*fp)
                    e1 = pe.e1;
                ce.error("`%s` is not `nothrow`", e1.toChars());
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
            const res = checkThrow(te.loc, te.e1, mustNotThrow, func);
            assert((res & ~(CT.exception | CT.error)) == 0);
            result |= res;
        }

        override void visit(NewAnonClassExp)
        {
            assert(0); // should have been lowered by semantic()
        }
    }

    scope CanThrow ct = new CanThrow(func, mustNotThrow);
    walkPostorder(e, ct);
    return ct.result;
}

/**************************************
 * Does symbol, when initialized, throw?
 * Mirrors logic in Dsymbol_toElem().
 */
private CT Dsymbol_canThrow(Dsymbol s, FuncDeclaration func, bool mustNotThrow)
{
    CT result;

    int symbolDg(Dsymbol s)
    {
        result |= Dsymbol_canThrow(s, func, mustNotThrow);
        return 0;
    }

    //printf("Dsymbol_toElem() %s\n", s.toChars());
    if (auto vd = s.isVarDeclaration())
    {
        s = s.toAlias();
        if (s != vd)
            return Dsymbol_canThrow(s, func, mustNotThrow);
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
                    result |= canThrow(ie.exp, func, mustNotThrow);
            }
            if (vd.needsScopeDtor())
                result |= canThrow(vd.edtor, func, mustNotThrow);
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
