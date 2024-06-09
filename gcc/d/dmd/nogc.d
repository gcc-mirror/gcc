/**
 * Checks that a function marked `@nogc` does not invoke the Garbage Collector.
 *
 * Specification: $(LINK2 https://dlang.org/spec/function.html#nogc-functions, No-GC Functions)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/nogc.d, _nogc.d)
 * Documentation:  https://dlang.org/phobos/dmd_nogc.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/nogc.d
 */

module dmd.nogc;

import core.stdc.stdio;

import dmd.aggregate;
import dmd.astenums;
import dmd.declaration;
import dmd.dscope;
import dmd.dtemplate : isDsymbol;
import dmd.errors;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.init;
import dmd.mtype;
import dmd.postordervisitor;
import dmd.tokens;
import dmd.visitor;

/**************************************
 * Look for GC-allocations
 */
extern (C++) final class NOGCVisitor : StoppableVisitor
{
    alias visit = typeof(super).visit;
public:
    FuncDeclaration f;
    bool checkOnly;     // don't print errors
    bool err;

    extern (D) this(FuncDeclaration f) scope @safe
    {
        this.f = f;
    }

    void doCond(Expression exp)
    {
        if (exp)
            walkPostorder(exp, this);
    }

    override void visit(Expression e)
    {
    }

    override void visit(DeclarationExp e)
    {
        // Note that, walkPostorder does not support DeclarationExp today.
        VarDeclaration v = e.declaration.isVarDeclaration();
        if (v && !(v.storage_class & STC.manifest) && !v.isDataseg() && v._init)
        {
            if (ExpInitializer ei = v._init.isExpInitializer())
            {
                doCond(ei.exp);
            }
        }
    }

    /**
     * Register that expression `e` requires the GC
     * Params:
     *   e = expression that uses GC
     *   format = error message when `e` is used in a `@nogc` function.
     *            Must contain format strings "`@nogc` %s `%s`" referring to the function.
     * Returns: `true` if `err` was set, `false` if it's not in a `@nogc` and not checkonly (-betterC)
     */
    private bool setGC(Expression e, const(char)* format)
    {
        if (checkOnly)
        {
            err = true;
            return true;
        }
        if (f.setGC(e.loc, format))
        {
            error(e.loc, format, f.kind(), f.toPrettyChars());
            err = true;
            return true;
        }
        return false;
    }

    override void visit(CallExp e)
    {
        import dmd.id : Id;
        import core.stdc.stdio : printf;
        if (!e.f)
            return;

        // Treat lowered hook calls as their original expressions.
        auto fd = stripHookTraceImpl(e.f);
        if (fd.ident == Id._d_arraysetlengthT)
        {
            if (setGC(e, "setting `length` in `@nogc` %s `%s` may cause a GC allocation"))
                return;
            f.printGCUsage(e.loc, "setting `length` may cause a GC allocation");
        }
    }

    override void visit(ArrayLiteralExp e)
    {
        if (e.type.ty != Tarray || !e.elements || !e.elements.length || e.onstack)
            return;
        if (setGC(e, "array literal in `@nogc` %s `%s` may cause a GC allocation"))
            return;
        f.printGCUsage(e.loc, "array literal may cause a GC allocation");
    }

    override void visit(AssocArrayLiteralExp e)
    {
        if (!e.keys.length)
            return;
        if (setGC(e, "associative array literal in `@nogc` %s `%s` may cause a GC allocation"))
            return;
        f.printGCUsage(e.loc, "associative array literal may cause a GC allocation");
    }

    override void visit(NewExp e)
    {
        if (e.member && !e.member.isNogc() && f.setGC(e.loc, null))
        {
            // @nogc-ness is already checked in NewExp::semantic
            return;
        }
        if (e.onstack)
            return;
        if (global.params.ehnogc && e.thrownew)
            return;                     // separate allocator is called for this, not the GC

        if (setGC(e, "cannot use `new` in `@nogc` %s `%s`"))
            return;
        f.printGCUsage(e.loc, "`new` causes a GC allocation");
    }

    override void visit(DeleteExp e)
    {
        if (VarExp ve = e.e1.isVarExp())
        {
            VarDeclaration v = ve.var.isVarDeclaration();
            if (v && v.onstack)
                return; // delete for scope allocated class object
        }

        // Semantic should have already handled this case.
        assert(0);
    }

    override void visit(IndexExp e)
    {
        Type t1b = e.e1.type.toBasetype();
        if (e.modifiable && t1b.ty == Taarray)
        {
            if (setGC(e, "assigning an associative array element in `@nogc` %s `%s` may cause a GC allocation"))
                return;
            f.printGCUsage(e.loc, "assigning an associative array element may cause a GC allocation");
        }
    }

    override void visit(AssignExp e)
    {
        if (e.e1.op == EXP.arrayLength)
        {
            if (setGC(e, "setting `length` in `@nogc` %s `%s` may cause a GC allocation"))
                return;
            f.printGCUsage(e.loc, "setting `length` may cause a GC allocation");
        }
    }

    override void visit(CatAssignExp e)
    {
        if (checkOnly)
        {
            err = true;
            return;
        }
        if (setGC(e, "cannot use operator `~=` in `@nogc` %s `%s`"))
            return;
        f.printGCUsage(e.loc, "operator `~=` may cause a GC allocation");
    }

    override void visit(CatExp e)
    {
        if (setGC(e, "cannot use operator `~` in `@nogc` %s `%s`"))
            return;
        f.printGCUsage(e.loc, "operator `~` may cause a GC allocation");
    }
}

Expression checkGC(Scope* sc, Expression e)
{
    if (sc.flags & SCOPE.ctfeBlock)     // ignore GC in ctfe blocks
        return e;

    /* If betterC, allow GC to happen in non-CTFE code.
     * Just don't generate code for it.
     * Detect non-CTFE use of the GC in betterC code.
     */
    const betterC = !global.params.useGC;
    FuncDeclaration f = sc.func;
    if (e && e.op != EXP.error && f && sc.intypeof != 1 &&
           (!(sc.flags & SCOPE.ctfe) || betterC) &&
           (f.type.ty == Tfunction &&
            (cast(TypeFunction)f.type).isnogc || f.nogcInprocess || global.params.v.gc) &&
           !(sc.flags & SCOPE.debug_))
    {
        scope NOGCVisitor gcv = new NOGCVisitor(f);
        gcv.checkOnly = betterC;
        walkPostorder(e, gcv);
        if (gcv.err)
        {
            if (betterC)
            {
                /* Allow ctfe to use the gc code, but don't let it into the runtime
                 */
                f.skipCodegen = true;
            }
            else
                return ErrorExp.get();
        }
    }
    return e;
}

/**
 * Removes `_d_HookTraceImpl` if found from `fd`.
 * This is needed to be able to find hooks that are called though the hook's `*Trace` wrapper.
 * Parameters:
 *  fd = The function declaration to remove `_d_HookTraceImpl` from
 */
private FuncDeclaration stripHookTraceImpl(FuncDeclaration fd)
{
    import dmd.id : Id;
    import dmd.dsymbol : Dsymbol;
    import dmd.rootobject : RootObject, DYNCAST;

    if (fd.ident != Id._d_HookTraceImpl)
        return fd;

    // Get the Hook from the second template parameter
    auto templateInstance = fd.parent.isTemplateInstance;
    RootObject hook = (*templateInstance.tiargs)[1];
    Dsymbol s = hook.isDsymbol();
    assert(s, "Expected _d_HookTraceImpl's second template parameter to be an alias to the hook!");
    return s.isFuncDeclaration;
}
