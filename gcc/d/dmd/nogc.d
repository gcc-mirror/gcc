/**
 * Checks that a function marked `@nogc` does not invoke the Garbage Collector.
 *
 * Specification: $(LINK2 https://dlang.org/spec/function.html#nogc-functions, No-GC Functions)
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/nogc.d, _nogc.d)
 * Documentation:  https://dlang.org/phobos/dmd_nogc.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/nogc.d
 */

module dmd.nogc;

import core.stdc.stdio;

import dmd.aggregate;
import dmd.astenums;
import dmd.declaration;
import dmd.common.outbuffer;
import dmd.dmodule;
import dmd.dscope;
import dmd.dtemplate : isDsymbol;
import dmd.dsymbol : PASS;
import dmd.errors;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.init;
import dmd.location;
import dmd.mtype;
import dmd.rootobject : RootObject, DYNCAST;
import dmd.semantic2;
import dmd.semantic3;
import dmd.tokens;
import dmd.visitor;
import dmd.visitor.postorder;

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
    bool nogcExceptions; // -preview=dip1008 enabled

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
     *   msg = error message when `e` is used in a `@nogc` function.
     * Returns: `true` if `err` was set, `false` if it's not in a `@nogc` and not checkonly (-betterC)
     */
    private bool setGC(Expression e, const(char)* msg)
    {
        if (checkOnly)
        {
            err = true;
            return true;
        }
        if (f.setGC(e.loc, msg))
        {
            error(e.loc, "%s causes a GC allocation in `@nogc` %s `%s`", msg, f.kind(), f.toChars());
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
            if (setGC(e, "setting this array's `length`"))
                return;
            f.printGCUsage(e.loc, "setting `length` may cause a GC allocation");
        }
    }

    override void visit(ArrayLiteralExp e)
    {
        if (e.type.ty != Tarray || !e.elements || !e.elements.length || e.onstack)
            return;
        if (setGC(e, "this array literal"))
            return;
        f.printGCUsage(e.loc, "array literal may cause a GC allocation");
    }

    override void visit(AssocArrayLiteralExp e)
    {
        if (!e.keys.length)
            return;
        if (setGC(e, "this associative array literal"))
            return;
        f.printGCUsage(e.loc, "associative array literal may cause a GC allocation");
    }

    override void visit(NewExp e)
    {
        if (e.placement)
            return;     // placement new doesn't use the GC
        if (e.member && !e.member.isNogc() && f.setGC(e.loc, null))
        {
            // @nogc-ness is already checked in NewExp::semantic
            return;
        }
        if (e.onstack)
            return;
        if (nogcExceptions && e.thrownew)
            return;                     // separate allocator is called for this, not the GC

        if (setGC(e, "allocating with `new`"))
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
            if (setGC(e, "assigning this associative array element"))
                return;
            f.printGCUsage(e.loc, "assigning an associative array element may cause a GC allocation");
        }
    }

    override void visit(AssignExp e)
    {
        if (e.e1.op == EXP.arrayLength)
        {
            if (setGC(e, "setting this array's `length`"))
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
        if (setGC(e, "appending to this array with operator `~=`"))
            return;
        f.printGCUsage(e.loc, "operator `~=` may cause a GC allocation");
    }

    override void visit(CatExp e)
    {
        if (setGC(e, "concatenating with operator `~`"))
            return;
        f.printGCUsage(e.loc, "operator `~` may cause a GC allocation");
    }
}

Expression checkGC(Scope* sc, Expression e)
{
    if (sc.ctfeBlock)     // ignore GC in ctfe blocks
        return e;

    /* If betterC, allow GC to happen in non-CTFE code.
     * Just don't generate code for it.
     * Detect non-CTFE use of the GC in betterC code.
     */
    const betterC = !global.params.useGC;
    FuncDeclaration f = sc.func;
    if (e && e.op != EXP.error && f && sc.intypeof != 1 &&
           (!sc.ctfe || betterC) &&
           (f.type.ty == Tfunction &&
            (cast(TypeFunction)f.type).isNogc || f.nogcInprocess || global.params.v.gc) &&
           !sc.debug_)
    {
        scope NOGCVisitor gcv = new NOGCVisitor(f);
        gcv.checkOnly = betterC;
        gcv.nogcExceptions = sc.previews.dip1008;
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

extern (D) void printGCUsage(FuncDeclaration fd, Loc loc, const(char)* warn)
{
    if (!global.params.v.gc)
        return;

    Module m = fd.getModule();
    if (m && m.isRoot() && !fd.inUnittest())
    {
        message(loc, "vgc: %s", warn);
    }
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

    if (fd.ident != Id._d_HookTraceImpl)
        return fd;

    // Get the Hook from the second template parameter
    auto templateInstance = fd.parent.isTemplateInstance;
    RootObject hook = (*templateInstance.tiargs)[1];
    Dsymbol s = hook.isDsymbol();
    assert(s, "Expected _d_HookTraceImpl's second template parameter to be an alias to the hook!");
    return s.isFuncDeclaration;
}

/**************************************
 * The function is doing something that may allocate with the GC,
 * so mark it as not nogc (not no-how).
 *
 * Params:
 *     fd = function
 *     loc = location of GC action
 *     fmt = format string for error message. Must include "%s `%s`" for the function kind and name.
 *     args = arguments to format string
 *
 * Returns:
 *      true if function is marked as @nogc, meaning a user error occurred
 */
extern (D) bool setGC(FuncDeclaration fd, Loc loc, const(char)* fmt, RootObject[] args...)
{
    //printf("setGC() %s\n", toChars());
    if (fd.nogcInprocess && fd.semanticRun < PASS.semantic3 && fd._scope)
    {
        fd.semantic2(fd._scope);
        fd.semantic3(fd._scope);
    }

    if (fd.nogcInprocess)
    {
        fd.nogcInprocess = false;
        if (fmt)
            fd.nogcViolation = new AttributeViolation(loc, fmt, args); // action that requires GC
        else if (args.length > 0)
        {
            if (auto sa = args[0].isDsymbol())
            {
                if (FuncDeclaration fd2 = sa.isFuncDeclaration())
                {
                    fd.nogcViolation = new AttributeViolation(loc, fd2); // call to non-@nogc function
                }
            }
        }

        fd.type.toTypeFunction().isNogc = false;
        if (fd.fes)
            fd.fes.func.setGC(Loc.init, null, null);
    }
    else if (fd.isNogc())
        return true;
    return false;
}

/**************************************
 * The function calls non-`@nogc` function f, mark it as not nogc.
 * Params:
 *     fd = function doin the call
 *     f = function being called
 * Returns:
 *      true if function is marked as @nogc, meaning a user error occurred
 */
extern (D) bool setGCCall(FuncDeclaration fd, FuncDeclaration f)
{
    return fd.setGC(fd.loc, null, f);
}

 bool isNogc(FuncDeclaration fd)
{
    //printf("isNogc() %s, inprocess: %d\n", toChars(), !!(flags & FUNCFLAG.nogcInprocess));
    if (fd.nogcInprocess)
        fd.setGC(fd.loc, null);
    return fd.type.toTypeFunction().isNogc;
}

extern (D) bool isNogcBypassingInference(FuncDeclaration fd)
{
    return !fd.nogcInprocess && fd.isNogc();
}
