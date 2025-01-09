/**
 * Checks whether member access or array casting is allowed in `@safe` code.
 *
 * Specification: $(LINK2 https://dlang.org/spec/function.html#function-safety, Function Safety)
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/safe.d, _safe.d)
 * Documentation:  https://dlang.org/phobos/dmd_safe.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/safe.d
 */

module dmd.safe;

import core.stdc.stdio;

import dmd.aggregate;
import dmd.astenums;
import dmd.common.outbuffer;
import dmd.dcast : implicitConvTo;
import dmd.dclass;
import dmd.declaration;
import dmd.dscope;
import dmd.dsymbol;
import dmd.dsymbolsem : determineSize;
import dmd.errors;
import dmd.expression;
import dmd.func;
import dmd.funcsem : isRootTraitsCompilesScope;
import dmd.globals : FeatureState, global;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.rootobject;
import dmd.target;
import dmd.tokens;
import dmd.typesem : hasPointers, arrayOf, size;

/*************************************************************
 * Check for unsafe access in @safe code:
 * 1. read overlapped pointers
 * 2. write misaligned pointers
 * 3. write overlapped storage classes
 * Print error if unsafe.
 * Params:
 *      sc = scope
 *      e = expression to check
 *      readonly = if access is read-only
 *      printmsg = print error message if true
 * Returns:
 *      true if error
 */

bool checkUnsafeAccess(Scope* sc, Expression e, bool readonly, bool printmsg)
{
    //printf("checkUnsafeAccess(e: '%s', readonly: %d, printmsg: %d)\n", e.toChars(), readonly, printmsg);
    if (e.op != EXP.dotVariable)
        return false;
    auto dve = cast(DotVarExp)e;
    VarDeclaration v = dve.var.isVarDeclaration();
    if (!v)
        return false;
    if (!sc.func)
        return false;
    auto ad = v.isMember2();
    if (!ad)
        return false;

    if (v.isSystem())
    {
        if (sc.setUnsafePreview(sc.previews.systemVariables, !printmsg, e.loc,
            "accessing `@system` field `%s.%s`", ad, v))
            return true;
    }

    // This branch shouldn't be here, but unfortunately calling `ad.determineSize`
    // breaks code with circular reference errors. Specifically, test23589.d fails
    if (ad.sizeok != Sizeok.done && !sc.func.isSafeBypassingInference())
        return false;

    // needed to set v.overlapped and v.overlapUnsafe
    if (ad.sizeok != Sizeok.done)
        ad.determineSize(ad.loc);

    import dmd.globals : FeatureState;
    const hasPointers = v.type.hasPointers();
    if (hasPointers)
    {
        if (v.overlapped)
        {
            if (sc.func.isSafeBypassingInference() && sc.setUnsafe(!printmsg, e.loc,
                "accessing overlapped field `%s.%s` with pointers", ad, v))
            {
                return true;
            }
            else
            {
                // @@@DEPRECATED_2.116@@@
                // https://issues.dlang.org/show_bug.cgi?id=20655
                // Inferring `@system` because of union access breaks code,
                // so make it a deprecation safety violation as of 2.106
                // To turn into an error, remove `isSafeBypassingInference` check in the
                // above if statement and remove the else branch
                sc.setUnsafePreview(FeatureState.default_, !printmsg, e.loc,
                    "accessing overlapped field `%s.%s` with pointers", ad, v);
            }
        }
    }

    if (v.type.hasInvariant())
    {
        if (v.overlapped)
        {
            if (sc.setUnsafe(!printmsg, e.loc,
                "accessing overlapped field `%s.%s` with a structs invariant",
                ad, v))
                return true;
        }
    }

    // @@@DEPRECATED_2.119@@@
    // https://issues.dlang.org/show_bug.cgi?id=24477
    // Should probably be turned into an error in a new edition
    if (v.type.hasUnsafeBitpatterns() && v.overlapped && sc.setUnsafePreview(
        FeatureState.default_, !printmsg, e.loc,
        "accessing overlapped field `%s.%s` with unsafe bit patterns", ad, v)
    )
    {
        return true;
    }

    if (readonly || !e.type.isMutable())
        return false;

    if (hasPointers && v.type.toBasetype().ty != Tstruct)
    {
        if ((!ad.type.alignment.isDefault() && ad.type.alignment.get() < target.ptrsize) ||
             (v.offset & (target.ptrsize - 1)))
        {
            if (sc.setUnsafe(!printmsg, e.loc,
                "modifying misaligned pointers through field `%s.%s`", ad, v))
                return true;
        }
    }

    if (v.overlapUnsafe)
    {
        if (sc.setUnsafe(!printmsg, e.loc,
            "modifying field `%s.%s` which overlaps with fields with other storage classes",
            ad, v))
        {
            return true;
        }
    }

    return false;
}


/**********************************************
 * Determine if it is @safe to cast e from tfrom to tto.
 * Params:
 *      e = expression to be cast
 *      tfrom = type of e
 *      tto = type to cast e to
 *      msg = reason why cast is unsafe or deprecated
 * Returns:
 *      true if @safe or deprecated
 */
bool isSafeCast(Expression e, Type tfrom, Type tto, ref string msg)
{
    // Implicit conversions are always safe
    if (tfrom.implicitConvTo(tto))
        return true;

    if (!tto.hasPointers())
        return true;

    auto tfromb = tfrom.toBasetype();
    auto ttob = tto.toBasetype();

    // Casting to void* is always safe, https://github.com/dlang/dmd/issues/20514
    if (ttob.isTypePointer() && ttob.nextOf().toBasetype().ty == Tvoid)
        return true;

    if (ttob.ty == Tclass && tfromb.ty == Tclass)
    {
        ClassDeclaration cdfrom = tfromb.isClassHandle();
        ClassDeclaration cdto = ttob.isClassHandle();
        int offset;

        if (cdfrom == cdto)
            goto Lsame;

        if (!cdfrom.isBaseOf(cdto, &offset) &&
            !((cdfrom.isInterfaceDeclaration() || cdto.isInterfaceDeclaration())
                && cdfrom.classKind == ClassKind.d && cdto.classKind == ClassKind.d))
        {
            msg = "Source object type is incompatible with target type";
            return false;
        }

        // no RTTI
        if (cdfrom.isCPPinterface() || cdto.isCPPinterface())
        {
            msg = "No dynamic type information for extern(C++) classes";
            return false;
        }
        if (cdfrom.classKind == ClassKind.cpp || cdto.classKind == ClassKind.cpp)
            msg = "No dynamic type information for extern(C++) classes";

    Lsame:
        if (!MODimplicitConv(tfromb.mod, ttob.mod))
        {
            msg = "Incompatible type qualifier";
            return false;
        }
        return true;
    }

    if (ttob.ty == Tarray && tfromb.ty == Tsarray) // https://issues.dlang.org/show_bug.cgi?id=12502
        tfromb = tfromb.nextOf().arrayOf();

    if (ttob.ty == Tarray   && tfromb.ty == Tarray ||
        ttob.ty == Tpointer && tfromb.ty == Tpointer)
    {
        Type ttobn = ttob.nextOf().toBasetype();
        Type tfromn = tfromb.nextOf().toBasetype();

        /* From void[] to anything mutable is unsafe because:
         *  int*[] api;
         *  void[] av = api;
         *  int[] ai = cast(int[]) av;
         *  ai[0] = 7;
         *  *api[0] crash!
         */
        if (tfromn.ty == Tvoid && ttobn.isMutable())
        {
            if (ttob.ty == Tarray && e.op == EXP.arrayLiteral)
                return true;
            msg = "`void` data may contain pointers and target element type is mutable";
            return false;
        }

        // If the struct is opaque we don't know about the struct members then the cast becomes unsafe
        if (ttobn.ty == Tstruct && !(cast(TypeStruct)ttobn).sym.members)
        {
            msg = "Target element type is opaque";
            return false;
        }
        if (tfromn.ty == Tstruct && !(cast(TypeStruct)tfromn).sym.members)
        {
            msg = "Source element type is opaque";
            return false;
        }

        if (e.op != EXP.arrayLiteral)
        {
            // For bool, only 0 and 1 are safe values
            // Runtime array cast reinterprets data
            if (ttobn.ty == Tbool && tfromn.ty != Tbool)
                msg = "Source element may have bytes which are not 0 or 1";
            else if (ttobn.hasUnsafeBitpatterns())
                msg = "Target element type has unsafe bit patterns";

            // Can't alias a bool pointer with a non-bool pointer
            if (ttobn.ty != Tbool && tfromn.ty == Tbool && ttobn.isMutable())
                msg = "Target element could be assigned a byte which is not 0 or 1";
            else if (tfromn.hasUnsafeBitpatterns() && ttobn.isMutable())
                msg = "Source element type has unsafe bit patterns and target element type is mutable";
        }

        const frompointers = tfromn.hasPointers();
        const topointers = ttobn.hasPointers();

        if (frompointers && !topointers && ttobn.isMutable())
        {
            msg = "Target element type is mutable and source element type contains a pointer";
            return false;
        }

        if (!frompointers && topointers)
        {
            msg = "Target element type contains a pointer";
            return false;
        }

        if (!topointers &&
            ttobn.ty != Tfunction && tfromn.ty != Tfunction &&
            (ttob.ty == Tarray || ttobn.size() <= tfromn.size()) &&
            MODimplicitConv(tfromn.mod, ttobn.mod))
        {
            return true;
        }
    }
    msg = "Source type is incompatible with target type containing a pointer";
    return false;
}

/*************************************************
 * Check for unsafe use of `.ptr` or `.funcptr`
 * Params:
 *      sc = context
 *      e = expression for error messages
 *      id = `ptr` or `funcptr`
 *      flag = DotExpFlag
 * Returns:
 *      true if error
 */
bool checkUnsafeDotExp(Scope* sc, Expression e, Identifier id, int flag)
{
    if (!(flag & DotExpFlag.noDeref)) // this use is attempting a dereference
    {
        if (id == Id.ptr)
            return sc.setUnsafe(false, e.loc, "using `%s.ptr` (instead of `&%s[0])`", e, e);
        else
            return sc.setUnsafe(false, e.loc, "using `%s.%s`", e, id);
    }
    return false;
}

/**************************************
 * Safer D adds safety checks to functions with the default
 * trust setting.
 */
bool isSaferD(FuncDeclaration fd)
{
    return fd.type.toTypeFunction().trust == TRUST.default_ && fd.saferD;
}

bool isSafe(FuncDeclaration fd)
{
    if (fd.safetyInprocess)
        setFunctionToUnsafe(fd);
    return fd.type.toTypeFunction().trust == TRUST.safe;
}

extern (D) bool isSafeBypassingInference(FuncDeclaration fd)
{
    return !(fd.safetyInprocess) && fd.isSafe();
}

bool isTrusted(FuncDeclaration fd)
{
    if (fd.safetyInprocess)
        setFunctionToUnsafe(fd);
    return fd.type.toTypeFunction().trust == TRUST.trusted;
}

/*****************************************************
 * Report safety violation for function `fd`, or squirrel away
 * error message in fd.safetyViolation if needed later.
 * Call when `fd` was just inferred to be @system OR
 * `fd` was @safe and an tried something unsafe.
 * Params:
 *   fd    = function we're gonna rat on
 *   gag   = suppress error message (used in escape.d)
 *   loc   = location of error
 *   format = printf-style format string
 *   arg0  = (optional) argument for first %s format specifier
 *   arg1  = (optional) argument for second %s format specifier
 *   arg2  = (optional) argument for third %s format specifier
 */
extern (D) void reportSafeError(FuncDeclaration fd, bool gag, Loc loc,
    const(char)* format = null, RootObject arg0 = null, RootObject arg1 = null, RootObject arg2 = null)
{
    if (fd.type.toTypeFunction().trust == TRUST.system) // function was just inferred to be @system
    {
        if (format)
            fd.safetyViolation = new AttributeViolation(loc, format, arg0, arg1, arg2);
        else if (arg0)
        {
            if (FuncDeclaration fd2 = (cast(Dsymbol) arg0).isFuncDeclaration())
            {
                fd.safetyViolation = new AttributeViolation(loc, fd2); // call to non-@nogc function
            }
        }
    }
    else if (fd.isSafe() || fd.isSaferD())
    {
        if (!gag && format)
        {
            OutBuffer buf;
            buf.printf(format, arg0 ? arg0.toChars() : "", arg1 ? arg1.toChars() : "", arg2 ? arg2.toChars() : "");
            if (fd.isSafe())
                buf.writestring(" is not allowed in a `@safe` function");
            else
                buf.writestring(" is not allowed in a function with default safety with `-preview=safer`");
            .error(loc, buf.extractChars());
        }
    }
}


/**********************************************
 * Function is doing something unsafe. If inference
 * is in process, commit the function to be @system.
 * Params:
 *      fd = the naughty function
 * Returns:
 *      true if this is a safe function and so an error OR is inferred to be @system,
 *      false otherwise.
 */
extern (D) bool setFunctionToUnsafe(FuncDeclaration fd)
{
    if (fd.safetyInprocess)
    {
        fd.safetyInprocess = false;
        fd.type.toTypeFunction().trust = TRUST.system;

        if (fd.fes)
            setFunctionToUnsafe(fd.fes.func);
        return true;
    }
    else if (fd.isSafe() || fd.isSaferD())
        return true;
    return false;
}


/**************************************
 * The function is calling `@system` function `f`, so mark it as unsafe.
 *
 * Params:
 *   fd = caller
 *   f = function being called (needed for diagnostic of inferred functions)
 * Returns: whether there's a safe error
 */
extern (D) bool setUnsafeCall(FuncDeclaration fd, FuncDeclaration f)
{
    if (setFunctionToUnsafe(fd))
    {
        reportSafeError(fd, false, f.loc, null, f, null);
        return fd.isSafe();
    }
    return false;
}

/**************************************
 * A statement / expression in this scope is not `@safe`,
 * so mark the enclosing function as `@system`
 *
 * Params:
 *   sc = scope that the unsafe statement / expression is in
 *   gag = surpress error message (used in escape.d)
 *   loc = location of error
 *   format = printf-style format string
 *   arg0  = (optional) argument for first %s format specifier
 *   arg1  = (optional) argument for second %s format specifier
 *   arg2  = (optional) argument for third %s format specifier
 * Returns: whether there is a safe error
 */
bool setUnsafe(Scope* sc,
    bool gag = false, Loc loc = Loc.init, const(char)* format = null,
    RootObject arg0 = null, RootObject arg1 = null, RootObject arg2 = null)
{
    if (sc.intypeof)
        return false; // typeof(cast(int*)0) is safe

    if (sc.debug_) // debug {} scopes are permissive
        return false;

    if (!sc.func)
    {
        if (sc.varDecl)
        {
            if (sc.varDecl.storage_class & STC.safe)
            {
                OutBuffer buf;
                buf.printf(format, arg0 ? arg0.toChars() : "", arg1 ? arg1.toChars() : "", arg2 ? arg2.toChars() : "");
                buf.printf(" can't initialize `@safe` variable `%s`", sc.varDecl.toChars());
                .error(loc, buf.extractChars());

                return true;
            }
            else if (!(sc.varDecl.storage_class & STC.trusted))
            {
                sc.varDecl.storage_class |= STC.system;
                sc.varDecl.systemInferred = true;
            }
        }
        return false;
    }


    if (isRootTraitsCompilesScope(sc)) // __traits(compiles, x)
    {
        if (sc.func.isSafeBypassingInference())
        {
            // Message wil be gagged, but still call error() to update global.errors and for
            // -verrors=spec
            OutBuffer buf;
            buf.printf(format, arg0 ? arg0.toChars() : "", arg1 ? arg1.toChars() : "", arg2 ? arg2.toChars() : "");
            buf.writestring(" is not allowed in a `@safe` function");
            .error(loc, buf.extractChars());
            return true;
        }
        return false;
    }

    if (setFunctionToUnsafe(sc.func))
    {
        if (format || arg0)
        {
            reportSafeError(sc.func, gag, loc, format, arg0, arg1, arg2);
        }
        return sc.func.isSafe(); // it is only an error if in an @safe function
    }
    return false;
}

/***************************************
 * Like `setUnsafe`, but for safety errors still behind preview switches
 *
 * Given a `FeatureState fs`, for example dip1000 / dip25 / systemVariables,
 * the behavior changes based on the setting:
 *
 * - In case of `-revert=fs`, it does nothing.
 * - In case of `-preview=fs`, it's the same as `setUnsafe`
 * - By default, print a deprecation in `@safe` functions, or store an attribute violation in inferred functions.
 *
 * Params:
 *   sc = used to find affected function/variable, and for checking whether we are in a deprecated / speculative scope
 *   fs = feature state from the preview flag
 *   gag = surpress error message
 *   loc = location of error
 *   format = printf-style format string
 *   arg0  = (optional) argument for first %s format specifier
 *   arg1  = (optional) argument for second %s format specifier
 *   arg2  = (optional) argument for third %s format specifier
 * Returns: whether an actual safe error (not deprecation) occured
 */
bool setUnsafePreview(Scope* sc, FeatureState fs, bool gag, Loc loc, const(char)* format,
    RootObject arg0 = null, RootObject arg1 = null, RootObject arg2 = null)
{
    //printf("setUnsafePreview() fs:%d %s\n", fs, fmt);
    assert(format);
    with (FeatureState) final switch (fs)
    {
      case disabled:
        return false;

      case enabled:
        return sc.setUnsafe(gag, loc, format, arg0, arg1, arg2);

      case default_:
        if (!sc.func)
            return false;
        if (sc.func.isSafeBypassingInference())
        {
            if (!gag && !sc.isDeprecated())
            {
                OutBuffer buf;
                buf.printf(format, arg0 ? arg0.toChars() : "", arg1 ? arg1.toChars() : "", arg2 ? arg2.toChars() : "");
                buf.writestring(" will become `@system` in a future release");
                deprecation(loc, buf.extractChars());
            }
        }
        else if (!sc.func.safetyViolation)
        {
            import dmd.func : AttributeViolation;
            sc.func.safetyViolation = new AttributeViolation(loc, format, arg0, arg1, arg2);
        }
        return false;
    }
}
