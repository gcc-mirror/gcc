/**
 * Evaluate compile-time conditionals, such as `static if` `version` and `debug`.
 *
 * Specification: $(LINK2 https://dlang.org/spec/version.html, Conditional Compilation)
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/cond.d, _cond.d)
 * Documentation:  https://dlang.org/phobos/dmd_cond.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/cond.d
 */

module dmd.cond;

import core.stdc.string;
import dmd.arraytypes;
import dmd.astenums;
import dmd.ast_node;
import dmd.dcast;
import dmd.dinterpret;
import dmd.dmodule;
import dmd.dscope;
import dmd.dsymbol;
import dmd.errors;
import dmd.expression;
import dmd.expressionsem : evalStaticCondition;
import dmd.globals;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.optimize;
import dmd.typesem;
import dmd.common.outbuffer;
import dmd.rootobject;
import dmd.root.string;
import dmd.tokens;
import dmd.utils;
import dmd.visitor;
import dmd.id;
import dmd.statement;
import dmd.declaration;
import dmd.dstruct;
import dmd.func;

/***********************************************************
 */

enum Include : ubyte
{
    notComputed,        /// not computed yet
    yes,                /// include the conditional code
    no,                 /// do not include the conditional code
}

extern (C++) abstract class Condition : ASTNode
{
    Loc loc;

    Include inc;

    override final DYNCAST dyncast() const
    {
        return DYNCAST.condition;
    }

    extern (D) this(Loc loc) @safe
    {
        this.loc = loc;
    }

    abstract Condition syntaxCopy();

    abstract int include(Scope* sc);

    inout(DebugCondition) isDebugCondition() inout
    {
        return null;
    }

    inout(VersionCondition) isVersionCondition() inout
    {
        return null;
    }

    inout(StaticIfCondition) isStaticIfCondition() inout
    {
        return null;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * Implements common functionality for StaticForeachDeclaration and
 * StaticForeachStatement This performs the necessary lowerings before
 * dmd.statementsem.makeTupleForeach can be used to expand the
 * corresponding `static foreach` declaration or statement.
 */

extern (C++) final class StaticForeach : RootObject
{
    extern(D) static immutable tupleFieldName = "tuple"; // used in lowering

    Loc loc;

    /***************
     * Not `null` iff the `static foreach` is over an aggregate. In
     * this case, it contains the corresponding ForeachStatement. For
     * StaticForeachDeclaration, the body is `null`.
    */
    ForeachStatement aggrfe;
    /***************
     * Not `null` iff the `static foreach` is over a range. Exactly
     * one of the `aggrefe` and `rangefe` fields is not null. See
     * `aggrfe` field for more details.
     */
    ForeachRangeStatement rangefe;

    /***************
     * true if it is necessary to expand a tuple into multiple
     * variables (see lowerNonArrayAggregate).
     */
    bool needExpansion = false;

    extern (D) this(Loc loc, ForeachStatement aggrfe, ForeachRangeStatement rangefe) @safe
    {
        assert(!!aggrfe ^ !!rangefe);

        this.loc = loc;
        this.aggrfe = aggrfe;
        this.rangefe = rangefe;
    }

    extern (D) StaticForeach syntaxCopy()
    {
        return new StaticForeach(
            loc,
            aggrfe ? aggrfe.syntaxCopy() : null,
            rangefe ? rangefe.syntaxCopy() : null
        );
    }

    /*****************************************
     * Wrap a statement into a function literal and call it.
     *
     * Params:
     *     loc = The source location.
     *     s  = The statement.
     * Returns:
     *     AST of the expression `(){ s; }()` with location loc.
     */
    extern(D) Expression wrapAndCall(Loc loc, Statement s)
    {
        auto tf = new TypeFunction(ParameterList(), null, LINK.default_, STC.none);
        auto fd = new FuncLiteralDeclaration(loc, loc, tf, TOK.reserved, null);
        fd.fbody = s;
        auto fe = new FuncExp(loc, fd);
        auto ce = new CallExp(loc, fe, new Expressions());
        return ce;
    }

    /*****************************************
     * Create a `foreach` statement from `aggrefe/rangefe` with given
     * `foreach` variables and body `s`.
     *
     * Params:
     *     loc = The source location.
     *     parameters = The foreach variables.
     *     s = The `foreach` body.
     * Returns:
     *     `foreach (parameters; aggregate) s;` or
     *     `foreach (parameters; lower .. upper) s;`
     *     Where aggregate/lower, upper are as for the current StaticForeach.
     */
    extern(D) Statement createForeach(Loc loc, Parameters* parameters, Statement s)
    {
        if (aggrfe)
        {
            return new ForeachStatement(loc, aggrfe.op, parameters, aggrfe.aggr, s, loc);
        }
        else
        {
            assert(rangefe && parameters.length == 1);
            return new ForeachRangeStatement(loc, rangefe.op, (*parameters)[0], rangefe.lwr, rangefe.upr, s, loc);
        }
    }

    /*****************************************
     * For a `static foreach` with multiple loop variables, the
     * aggregate is lowered to an array of tuples. As D does not have
     * built-in tuples, we need a suitable tuple type. This generates
     * a `struct` that serves as the tuple type. This type is only
     * used during CTFE and hence its typeinfo will not go to the
     * object file.
     *
     * Params:
     *     loc = The source location.
     *     e = The expressions we wish to store in the tuple.
     *     sc  = The current scope.
     * Returns:
     *     A struct type of the form
     *         struct Tuple
     *         {
     *             typeof(AliasSeq!(e)) tuple;
     *         }
     */

    extern(D) TypeStruct createTupleType(Loc loc, Expressions* e, Scope* sc)
    {   // TODO: move to druntime?
        auto sid = Identifier.generateId("Tuple");
        auto sdecl = new StructDeclaration(loc, sid, false);
        sdecl.storage_class |= STC.static_;
        sdecl.members = new Dsymbols();
        auto fid = Identifier.idPool(tupleFieldName);
        auto ty = new TypeTypeof(loc, new TupleExp(loc, e));
        sdecl.members.push(new VarDeclaration(loc, ty, fid, null, STC.none));
        auto r = cast(TypeStruct)sdecl.type;
        if (global.params.useTypeInfo && Type.dtypeinfo)
            r.vtinfo = TypeInfoStructDeclaration.create(r); // prevent typeinfo from going to object file
        return r;
    }

    /*****************************************
     * Create the AST for an instantiation of a suitable tuple type.
     *
     * Params:
     *     loc = The source location.
     *     type = A Tuple type, created with createTupleType.
     *     e = The expressions we wish to store in the tuple.
     * Returns:
     *     An AST for the expression `Tuple(e)`.
     */

    extern(D) Expression createTuple(Loc loc, TypeStruct type, Expressions* e) @safe
    {   // TODO: move to druntime?
        return new CallExp(loc, new TypeExp(loc, type), e);
    }

    /*****************************************
     * Returns:
     *     `true` iff ready to call `dmd.statementsem.makeTupleForeach`.
     */
    extern(D) bool ready()
    {
        return aggrfe && aggrfe.aggr && aggrfe.aggr.type && aggrfe.aggr.type.toBasetype().ty == Ttuple;
    }
}

/***********************************************************
 */
extern (C++) class DVCondition : Condition
{
    Identifier ident;
    Module mod;

    extern (D) this(Loc loc, Module mod, Identifier ident) @safe
    {
        super(loc);
        this.mod = mod;
        this.ident = ident;
    }

    override final DVCondition syntaxCopy()
    {
        return this; // don't need to copy
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class DebugCondition : DVCondition
{
    /**
     * Add an user-supplied identifier to the list of global debug identifiers
     *
     * Can be called from either the driver or a `debug = Ident;` statement.
     * Unlike version identifier, there isn't any reserved debug identifier
     * so no validation takes place.
     *
     * Params:
     *   ident = identifier to add
     */
    deprecated("Kept for C++ compat - Use the string overload instead")
    static void addGlobalIdent(const(char)* ident)
    {
        addGlobalIdent(ident[0 .. ident.strlen]);
    }

    /// Ditto
    extern(D) static void addGlobalIdent(string ident)
    {
        // Overload necessary for string literals
        addGlobalIdent(cast(const(char)[])ident);
    }


    /// Ditto
    extern(D) static void addGlobalIdent(const(char)[] ident)
    {
        global.debugids.push(Identifier.idPool(ident));
    }


    /**
     * Instantiate a new `DebugCondition`
     *
     * Params:
     *   mod = Module this node belongs to
     *   ident = Identifier required for this condition to pass.
     *           If `null`, this conditiion will use an integer level.
     *  loc = Location in the source file
     */
    extern (D) this(Loc loc, Module mod, Identifier ident) @safe
    {
        super(loc, mod, ident);
    }

    override int include(Scope* sc)
    {
        //printf("DebugCondition::include() level = %d, debuglevel = %d\n", level, global.params.debuglevel);
        if (inc != Include.notComputed)
        {
            return inc == Include.yes;
        }
        inc = Include.no;
        bool definedInModule = false;
        if (ident)
        {
            if (mod.debugids && findCondition(*mod.debugids, ident))
            {
                inc = Include.yes;
                definedInModule = true;
            }
            else if (findCondition(global.debugids, ident))
                inc = Include.yes;
            else
            {
                if (!mod.debugidsNot)
                    mod.debugidsNot = new Identifiers();
                mod.debugidsNot.push(ident);
            }
        }
        else if (global.params.debugEnabled)
            inc = Include.yes;

        if (!definedInModule)
            printDepsConditional(sc, this, "depsDebug ");
        return (inc == Include.yes);
    }

    override inout(DebugCondition) isDebugCondition() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/**
 * Node to represent a version condition
 *
 * A version condition is of the form:
 * ---
 * version (Identifier)
 * ---
 * In user code.
 * This class also provides means to add version identifier
 * to the list of global (cross module) identifiers.
 */
extern (C++) final class VersionCondition : DVCondition
{
    /**
     * Check if a given version identifier is reserved.
     *
     * Params:
     *   ident = identifier being checked
     *
     * Returns:
     *   `true` if it is reserved, `false` otherwise
     */
    extern(D) private static bool isReserved(const(char)[] ident) @safe
    {
        // This list doesn't include "D_*" versions, see the last return
        switch (ident)
        {
            case "AArch64":
            case "AIX":
            case "all":
            case "Alpha":
            case "Alpha_HardFloat":
            case "Alpha_SoftFloat":
            case "Android":
            case "ARM":
            case "ARM_HardFloat":
            case "ARM_SoftFloat":
            case "ARM_SoftFP":
            case "ARM_Thumb":
            case "AsmJS":
            case "assert":
            case "AVR":
            case "BigEndian":
            case "BSD":
            case "CppRuntime_LLVM":
            case "CppRuntime_DigitalMars":
            case "CppRuntime_GNU":
            case "CppRuntime_Microsoft":
            case "CppRuntime_Sun":
            case "CRuntime_Bionic":
            case "CRuntime_DigitalMars":
            case "CRuntime_Glibc":
            case "CRuntime_Microsoft":
            case "CRuntime_Musl":
            case "CRuntime_Newlib":
            case "CRuntime_UClibc":
            case "CRuntime_WASI":
            case "Cygwin":
            case "DigitalMars":
            case "DragonFlyBSD":
            case "Emscripten":
            case "ELFv1":
            case "ELFv2":
            case "Epiphany":
            case "FreeBSD":
            case "FreeStanding":
            case "GNU":
            case "Haiku":
            case "HPPA":
            case "HPPA64":
            case "Hurd":
            case "IA64":
            case "iOS":
            case "LDC":
            case "linux":
            case "LittleEndian":
            case "LoongArch32":
            case "LoongArch64":
            case "LoongArch_HardFloat":
            case "LoongArch_SoftFloat":
            case "MinGW":
            case "MIPS32":
            case "MIPS64":
            case "MIPS_EABI":
            case "MIPS_HardFloat":
            case "MIPS_N32":
            case "MIPS_N64":
            case "MIPS_O32":
            case "MIPS_O64":
            case "MIPS_SoftFloat":
            case "MSP430":
            case "NetBSD":
            case "none":
            case "NVPTX":
            case "NVPTX64":
            case "OpenBSD":
            case "OSX":
            case "PlayStation":
            case "PlayStation4":
            case "Posix":
            case "PPC":
            case "PPC64":
            case "PPC_HardFloat":
            case "PPC_SoftFloat":
            case "RISCV32":
            case "RISCV64":
            case "S390":
            case "S390X":
            case "SDC":
            case "SH":
            case "SkyOS":
            case "Solaris":
            case "SPARC":
            case "SPARC64":
            case "SPARC_HardFloat":
            case "SPARC_SoftFloat":
            case "SPARC_V8Plus":
            case "SystemZ":
            case "SysV3":
            case "SysV4":
            case "TVOS":
            case "unittest":
            case "VisionOS":
            case "WASI":
            case "WatchOS":
            case "WebAssembly":
            case "Win32":
            case "Win64":
            case "Windows":
            case "Xtensa":
            case "X86":
            case "X86_64":
                return true;

            default:
                // Anything that starts with "D_" is reserved
                return (ident.length >= 2 && ident[0 .. 2] == "D_");
        }
    }

    /**
     * Raises an error if a version identifier is reserved.
     *
     * Called when setting a version identifier, e.g. `-version=identifier`
     * parameter to the compiler or `version = Foo` in user code.
     *
     * Params:
     *   loc = Where the identifier is set
     *   ident = identifier being checked (ident[$] must be '\0')
     */
    extern(D) static void checkReserved(Loc loc, const(char)[] ident)
    {
        if (isReserved(ident))
            error(loc, "version identifier `%s` is reserved and cannot be set",
                  ident.ptr);
    }

    /**
     * Add an user-supplied global identifier to the list
     *
     * Only called from the driver for `-version=Ident` parameters.
     * Will raise an error if the identifier is reserved.
     *
     * Params:
     *   ident = identifier to add
     */
    deprecated("Kept for C++ compat - Use the string overload instead")
    static void addGlobalIdent(const(char)* ident)
    {
        addGlobalIdent(ident[0 .. ident.strlen]);
    }

    /// Ditto
    extern(D) static void addGlobalIdent(string ident)
    {
        // Overload necessary for string literals
        addGlobalIdent(cast(const(char)[])ident);
    }


    /// Ditto
    extern(D) static void addGlobalIdent(const(char)[] ident)
    {
        checkReserved(Loc.initial, ident);
        addPredefinedGlobalIdent(ident);
    }

    /**
     * Add any global identifier to the list, without checking
     * if it's predefined
     *
     * Only called from the driver after platform detection,
     * and internally.
     *
     * Params:
     *   ident = identifier to add (ident[$] must be '\0')
     */
    deprecated("Kept for C++ compat - Use the string overload instead")
    static void addPredefinedGlobalIdent(const(char)* ident)
    {
        addPredefinedGlobalIdent(ident.toDString());
    }

    /// Ditto
    extern(D) static void addPredefinedGlobalIdent(string ident)
    {
        // Forward: Overload necessary for string literal
        addPredefinedGlobalIdent(cast(const(char)[])ident);
    }


    /// Ditto
    extern(D) static void addPredefinedGlobalIdent(const(char)[] ident)
    {
        global.versionids.push(Identifier.idPool(ident));
    }

    /**
     * Instantiate a new `VersionCondition`
     *
     * Params:
     *   mod = Module this node belongs to
     *   ident = Identifier required for this condition to pass.
     *           If `null`, this conditiion will use an integer level.
     *  loc = Location in the source file
     */
    extern (D) this(Loc loc, Module mod, Identifier ident) @safe
    {
        super(loc, mod, ident);
    }

    override int include(Scope* sc)
    {
        //printf("VersionCondition::include() level = %d, versionlevel = %d\n", level, global.params.versionlevel);
        //if (ident) printf("\tident = '%s'\n", ident.toChars());
        if (inc != Include.notComputed)
        {
            return inc == Include.yes;
        }

        inc = Include.no;
        bool definedInModule = false;
        if (ident)
        {
            if (mod.versionids && findCondition(*mod.versionids, ident))
            {
                inc = Include.yes;
                definedInModule = true;
            }
            else if (findCondition(global.versionids, ident))
                inc = Include.yes;
            else
            {
                if (!mod.versionidsNot)
                    mod.versionidsNot = new Identifiers();
                mod.versionidsNot.push(ident);
            }
        }
        if (!definedInModule &&
            (!ident || (!isReserved(ident.toString()) && ident != Id._unittest && ident != Id._assert)))
        {
            printDepsConditional(sc, this, "depsVersion ");
        }
        return (inc == Include.yes);
    }

    override inout(VersionCondition) isVersionCondition() inout
    {
        return this;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 */
extern (C++) final class StaticIfCondition : Condition
{
    Expression exp;

    extern (D) this(Loc loc, Expression exp) @safe
    {
        super(loc);
        this.exp = exp;
    }

    override StaticIfCondition syntaxCopy()
    {
        return new StaticIfCondition(loc, exp.syntaxCopy());
    }

    override int include(Scope* sc)
    {
        // printf("StaticIfCondition::include(sc = %p) this=%p inc = %d\n", sc, this, inc);

        int errorReturn()
        {
            if (!global.gag)
                inc = Include.no; // so we don't see the error message again
            return 0;
        }

        if (inc != Include.notComputed)
        {
            return inc == Include.yes;
        }

        if (!sc)
        {
            error(loc, "`static if` conditional cannot be at global scope");
            inc = Include.no;
            return 0;
        }

        import dmd.staticcond;
        bool errors;

        bool result = evalStaticCondition(sc, exp, exp, errors);

        // Prevent repeated condition evaluation.
        // See: fail_compilation/fail7815.d
        if (inc != Include.notComputed)
            return (inc == Include.yes);
        if (errors)
            return errorReturn();
        if (result)
            inc = Include.yes;
        else
            inc = Include.no;
        return (inc == Include.yes);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }

    override inout(StaticIfCondition) isStaticIfCondition() inout
    {
        return this;
    }
}


/****************************************
 * Find `ident` in an array of identifiers.
 * Params:
 *      ids = array of identifiers
 *      ident = identifier to search for
 * Returns:
 *      true if found
 */
bool findCondition(ref Identifiers ids, Identifier ident) @safe nothrow pure
{
    foreach (id; ids)
    {
        if (id == ident)
            return true;
    }
    return false;
}

// Helper for printing dependency information
private void printDepsConditional(Scope* sc, DVCondition condition, const(char)[] depType)
{
    if (!global.params.moduleDeps.buffer || global.params.moduleDeps.name)
        return;
    OutBuffer* ob = global.params.moduleDeps.buffer;
    Module imod = sc ? sc._module : condition.mod;
    if (!imod)
        return;
    ob.writestring(depType);
    ob.writestring(imod.toPrettyChars());
    ob.writestring(" (");
    escapePath(ob, imod.srcfile.toChars());
    ob.writestring(") : ");
    if (condition.ident)
        ob.writestring(condition.ident.toString());
    ob.writeByte('\n');
}
