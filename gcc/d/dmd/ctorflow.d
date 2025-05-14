/**
 * Manage flow analysis for constructors.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/ctorflow.d, _ctorflow.d)
 * Documentation:  https://dlang.org/phobos/dmd_ctorflow.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/ctorflow.d
 */

module dmd.ctorflow;

import core.stdc.stdio;

import dmd.root.rmem;
import dmd.location;

enum CSX : ushort
{
    none            = 0,
    this_ctor       = 0x01,     /// called this()
    super_ctor      = 0x02,     /// called super()
    label           = 0x04,     /// seen a label
    return_         = 0x08,     /// seen a return statement
    any_ctor        = 0x10,     /// either this() or super() was called
    halt            = 0x20,     /// assert(0)
}

/// Individual field in the Ctor with information about its callees and location.
struct FieldInit
{
    CSX csx; /// information about the field's callees
    Loc loc; /// location of the field initialization
}

/***********
 * Primitive flow analysis for constructors
 */
struct CtorFlow
{
    CSX callSuper;      /// state of calling other constructors

    FieldInit[] fieldinit;    /// state of field initializations

    void allocFieldinit(size_t dim)
    {
        fieldinit = (cast(FieldInit*)mem.xcalloc(FieldInit.sizeof, dim))[0 .. dim];
    }

    void freeFieldinit()
    {
        if (fieldinit.ptr)
            mem.xfree(fieldinit.ptr);

        fieldinit = null;
    }

    /***********************
     * Create a deep copy of `this`
     * Returns:
     *  a copy
     */
    CtorFlow clone()
    {
        return CtorFlow(callSuper, fieldinit.arraydup);
    }

    /**********************************
     * Set CSX bits in flow analysis state
     * Params:
     *  csx = bits to set
     */
    void orCSX(CSX csx) nothrow pure @safe
    {
        callSuper |= csx;
        foreach (ref u; fieldinit)
            u.csx |= csx;
    }

    /******************************
     * OR CSX bits to `this`
     * Params:
     *  ctorflow = bits to OR in
     */
    void OR(const ref CtorFlow ctorflow) pure nothrow @safe
    {
        callSuper |= ctorflow.callSuper;
        if (fieldinit.length && ctorflow.fieldinit.length)
        {
            assert(fieldinit.length == ctorflow.fieldinit.length);
            foreach (i, u; ctorflow.fieldinit)
            {
                auto fi = &fieldinit[i];
                fi.csx |= u.csx;
                if (fi.loc is Loc.init)
                    fi.loc = u.loc;
            }
        }
    }
}


/****************************************
 * Merge `b` flow analysis results into `a`.
 * Params:
 *      a = the path to merge `b` into
 *      b = the other path
 * Returns:
 *      false means one of the paths skips construction
 */
bool mergeCallSuper(ref CSX a, const CSX b) pure nothrow @safe
{
    // This does a primitive flow analysis to support the restrictions
    // regarding when and how constructors can appear.
    // It merges the results of two paths.
    // The two paths are `a` and `b`; the result is merged into `a`.
    if (b == a)
        return true;

    // Have ALL branches called a constructor?
    const aAll = (a & (CSX.this_ctor | CSX.super_ctor)) != 0;
    const bAll = (b & (CSX.this_ctor | CSX.super_ctor)) != 0;
    // Have ANY branches called a constructor?
    const aAny = (a & CSX.any_ctor) != 0;
    const bAny = (b & CSX.any_ctor) != 0;
    // Have any branches returned?
    const aRet = (a & CSX.return_) != 0;
    const bRet = (b & CSX.return_) != 0;
    // Have any branches halted?
    const aHalt = (a & CSX.halt) != 0;
    const bHalt = (b & CSX.halt) != 0;
    if (aHalt && bHalt)
    {
        a = CSX.halt;
    }
    else if ((!bHalt && bRet && !bAny && aAny) || (!aHalt && aRet && !aAny && bAny))
    {
        // If one has returned without a constructor call, there must not
        // be ctor calls in the other.
        return false;
    }
    else if (bHalt || bRet && bAll)
    {
        // If one branch has called a ctor and then exited, anything the
        // other branch has done is OK (except returning without a
        // ctor call, but we already checked that).
        a |= b & (CSX.any_ctor | CSX.label);
    }
    else if (aHalt || aRet && aAll)
    {
        a = cast(CSX)(b | (a & (CSX.any_ctor | CSX.label)));
    }
    else if (aAll != bAll) // both branches must have called ctors, or both not
        return false;
    else
    {
        // If one returned without a ctor, remember that
        if (bRet && !bAny)
            a |= CSX.return_;
        a |= b & (CSX.any_ctor | CSX.label);
    }
    return true;
}


/****************************************
 * Merge `b` flow analysis results into `a`.
 * Params:
 *      a = the path to merge `b` into
 *      b = the other path
 * Returns:
 *      false means either `a` or `b` skips initialization
 */
bool mergeFieldInit(ref CSX a, const CSX b) pure nothrow @safe
{
    if (b == a)
        return true;

    // Have any branches returned?
    const aRet = (a & CSX.return_) != 0;
    const bRet = (b & CSX.return_) != 0;
    // Have any branches halted?
    const aHalt = (a & CSX.halt) != 0;
    const bHalt = (b & CSX.halt) != 0;

    if (aHalt && bHalt)
    {
        a = CSX.halt;
        return true;
    }

    // The logic here is to prefer the branch that neither halts nor returns.
    bool ok;
    if (!bHalt && bRet)
    {
        // Branch b returns, no merging required.
        ok = (b & CSX.this_ctor);
    }
    else if (!aHalt && aRet)
    {
        // Branch a returns, but b doesn't, b takes precedence.
        ok = (a & CSX.this_ctor);
        a = b;
    }
    else if (bHalt)
    {
        // Branch b halts, no merging required.
        ok = (a & CSX.this_ctor);
    }
    else if (aHalt)
    {
        // Branch a halts, but b doesn't, b takes precedence.
        ok = (b & CSX.this_ctor);
        a = b;
    }
    else
    {
        // Neither branch returns nor halts, merge flags.
        ok = !((a ^ b) & CSX.this_ctor);
        a |= b;
    }
    return ok;
}
