/**
 * Evaluate compile-time conditionals, such as `static if` `version` and `debug`.
 *
 * Specification: $(LINK2 https://dlang.org/spec/version.html, Conditional Compilation)
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/cond.d, _cond.d)
 * Documentation:  https://dlang.org/phobos/dmd_cond.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/cond.d
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
import dmd.expressionsem;
import dmd.globals;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
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

    extern (D) this(const ref Loc loc) @safe
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

    extern (D) this(const ref Loc loc, ForeachStatement aggrfe, ForeachRangeStatement rangefe) @safe
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
     * Turn an aggregate which is an array into an expression tuple
     * of its elements. I.e., lower
     *     static foreach (x; [1, 2, 3, 4]) { ... }
     * to
     *     static foreach (x; AliasSeq!(1, 2, 3, 4)) { ... }
     */
    private extern(D) void lowerArrayAggregate(Scope* sc)
    {
        auto aggr = aggrfe.aggr;
        Expression el = new ArrayLengthExp(aggr.loc, aggr);
        sc = sc.startCTFE();
        el = el.expressionSemantic(sc);
        sc = sc.endCTFE();
        el = el.optimize(WANTvalue);
        el = el.ctfeInterpret();
        if (el.op == EXP.int64)
        {
            Expressions *es = void;
            if (auto ale = aggr.isArrayLiteralExp())
            {
                // Directly use the elements of the array for the TupleExp creation
                es = ale.elements;
            }
            else
            {
                const length = cast(size_t)el.toInteger();
                es = new Expressions(length);
                foreach (i; 0 .. length)
                {
                    auto index = new IntegerExp(loc, i, Type.tsize_t);
                    auto value = new IndexExp(aggr.loc, aggr, index);
                    (*es)[i] = value;
                }
            }
            aggrfe.aggr = new TupleExp(aggr.loc, es);
            aggrfe.aggr = aggrfe.aggr.expressionSemantic(sc);
            aggrfe.aggr = aggrfe.aggr.optimize(WANTvalue);
            aggrfe.aggr = aggrfe.aggr.ctfeInterpret();
        }
        else
        {
            aggrfe.aggr = ErrorExp.get();
        }
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
    private extern(D) Expression wrapAndCall(const ref Loc loc, Statement s)
    {
        auto tf = new TypeFunction(ParameterList(), null, LINK.default_, 0);
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
    private extern(D) Statement createForeach(const ref Loc loc, Parameters* parameters, Statement s)
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

    private extern(D) TypeStruct createTupleType(const ref Loc loc, Expressions* e, Scope* sc)
    {   // TODO: move to druntime?
        auto sid = Identifier.generateId("Tuple");
        auto sdecl = new StructDeclaration(loc, sid, false);
        sdecl.storage_class |= STC.static_;
        sdecl.members = new Dsymbols();
        auto fid = Identifier.idPool(tupleFieldName);
        auto ty = new TypeTypeof(loc, new TupleExp(loc, e));
        sdecl.members.push(new VarDeclaration(loc, ty, fid, null, 0));
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

    private extern(D) Expression createTuple(const ref Loc loc, TypeStruct type, Expressions* e) @safe
    {   // TODO: move to druntime?
        return new CallExp(loc, new TypeExp(loc, type), e);
    }


    /*****************************************
     * Lower any aggregate that is not an array to an array using a
     * regular foreach loop within CTFE.  If there are multiple
     * `static foreach` loop variables, an array of tuples is
     * generated. In thise case, the field `needExpansion` is set to
     * true to indicate that the static foreach loop expansion will
     * need to expand the tuples into multiple variables.
     *
     * For example, `static foreach (x; range) { ... }` is lowered to:
     *
     *     static foreach (x; {
     *         typeof({
     *             foreach (x; range) return x;
     *         }())[] __res;
     *         foreach (x; range) __res ~= x;
     *         return __res;
     *     }()) { ... }
     *
     * Finally, call `lowerArrayAggregate` to turn the produced
     * array into an expression tuple.
     *
     * Params:
     *     sc = The current scope.
     */

    private void lowerNonArrayAggregate(Scope* sc)
    {
        auto nvars = aggrfe ? aggrfe.parameters.length : 1;
        auto aloc = aggrfe ? aggrfe.aggr.loc : rangefe.lwr.loc;
        // We need three sets of foreach loop variables because the
        // lowering contains three foreach loops.
        Parameters*[3] pparams = [new Parameters(), new Parameters(), new Parameters()];
        foreach (i; 0 .. nvars)
        {
            foreach (params; pparams)
            {
                auto p = aggrfe ? (*aggrfe.parameters)[i] : rangefe.prm;
                params.push(new Parameter(aloc, p.storageClass, p.type, p.ident, null, null));
            }
        }
        Expression[2] res;
        TypeStruct tplty = null;
        if (nvars == 1) // only one `static foreach` variable, generate identifiers.
        {
            foreach (i; 0 .. 2)
            {
                res[i] = new IdentifierExp(aloc, (*pparams[i])[0].ident);
            }
        }
        else // multiple `static foreach` variables, generate tuples.
        {
            foreach (i; 0 .. 2)
            {
                auto e = new Expressions(pparams[0].length);
                foreach (j, ref elem; *e)
                {
                    auto p = (*pparams[i])[j];
                    elem = new IdentifierExp(aloc, p.ident);
                }
                if (!tplty)
                {
                    tplty = createTupleType(aloc, e, sc);
                }
                res[i] = createTuple(aloc, tplty, e);
            }
            needExpansion = true; // need to expand the tuples later
        }
        // generate remaining code for the new aggregate which is an
        // array (see documentation comment).
        if (rangefe)
        {
            sc = sc.startCTFE();
            rangefe.lwr = rangefe.lwr.expressionSemantic(sc);
            rangefe.lwr = resolveProperties(sc, rangefe.lwr);
            rangefe.upr = rangefe.upr.expressionSemantic(sc);
            rangefe.upr = resolveProperties(sc, rangefe.upr);
            sc = sc.endCTFE();
            rangefe.lwr = rangefe.lwr.optimize(WANTvalue);
            rangefe.lwr = rangefe.lwr.ctfeInterpret();
            rangefe.upr = rangefe.upr.optimize(WANTvalue);
            rangefe.upr = rangefe.upr.ctfeInterpret();
        }
        auto s1 = new Statements();
        auto sfe = new Statements();
        if (tplty) sfe.push(new ExpStatement(loc, tplty.sym));
        sfe.push(new ReturnStatement(aloc, res[0]));
        s1.push(createForeach(aloc, pparams[0], new CompoundStatement(aloc, sfe)));
        s1.push(new ExpStatement(aloc, new AssertExp(aloc, IntegerExp.literal!0)));
        Type ety = new TypeTypeof(aloc, wrapAndCall(aloc, new CompoundStatement(aloc, s1)));
        auto aty = ety.arrayOf();
        auto idres = Identifier.generateId("__res");
        auto vard = new VarDeclaration(aloc, aty, idres, null, STC.temp);
        auto s2 = new Statements();

        // Run 'typeof' gagged to avoid duplicate errors and if it fails just create
        // an empty foreach to expose them.
        uint olderrors = global.startGagging();
        ety = ety.typeSemantic(aloc, sc);
        if (global.endGagging(olderrors))
            s2.push(createForeach(aloc, pparams[1], null));
        else
        {
            s2.push(new ExpStatement(aloc, vard));
            auto catass = new CatAssignExp(aloc, new IdentifierExp(aloc, idres), res[1]);
            s2.push(createForeach(aloc, pparams[1], new ExpStatement(aloc, catass)));
            s2.push(new ReturnStatement(aloc, new IdentifierExp(aloc, idres)));
        }

        Expression aggr = void;
        Type indexty = void;

        if (rangefe && (indexty = ety).isintegral())
        {
            rangefe.lwr.type = indexty;
            rangefe.upr.type = indexty;
            auto lwrRange = getIntRange(rangefe.lwr);
            auto uprRange = getIntRange(rangefe.upr);

            const lwr = rangefe.lwr.toInteger();
            auto  upr = rangefe.upr.toInteger();
            size_t length = 0;

            if (lwrRange.imin <= uprRange.imax)
                    length = cast(size_t) (upr - lwr);

            auto exps = new Expressions(length);

            if (rangefe.op == TOK.foreach_)
            {
                foreach (i; 0 .. length)
                    (*exps)[i] = new IntegerExp(aloc, lwr + i, indexty);
            }
            else
            {
                --upr;
                foreach (i; 0 .. length)
                    (*exps)[i] = new IntegerExp(aloc, upr - i, indexty);
            }
            aggr = new ArrayLiteralExp(aloc, indexty.arrayOf(), exps);
        }
        else
        {
            aggr = wrapAndCall(aloc, new CompoundStatement(aloc, s2));
            sc = sc.startCTFE();
            aggr = aggr.expressionSemantic(sc);
            aggr = resolveProperties(sc, aggr);
            sc = sc.endCTFE();
            aggr = aggr.optimize(WANTvalue);
            aggr = aggr.ctfeInterpret();
        }

        assert(!!aggrfe ^ !!rangefe);
        aggrfe = new ForeachStatement(loc, TOK.foreach_, pparams[2], aggr,
                                      aggrfe ? aggrfe._body : rangefe._body,
                                      aggrfe ? aggrfe.endloc : rangefe.endloc);
        rangefe = null;
        lowerArrayAggregate(sc); // finally, turn generated array into expression tuple
    }

    /*****************************************
     * Perform `static foreach` lowerings that are necessary in order
     * to finally expand the `static foreach` using
     * `dmd.statementsem.makeTupleForeach`.
     */
    extern(D) void prepare(Scope* sc)
    {
        assert(sc);

        if (aggrfe)
        {
            sc = sc.startCTFE();
            aggrfe.aggr = aggrfe.aggr.expressionSemantic(sc);
            sc = sc.endCTFE();
        }

        if (aggrfe && aggrfe.aggr.type.toBasetype().ty == Terror)
        {
            return;
        }

        if (!ready())
        {
            if (aggrfe && aggrfe.aggr.type.toBasetype().ty == Tarray)
            {
                lowerArrayAggregate(sc);
            }
            else
            {
                lowerNonArrayAggregate(sc);
            }
        }
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
    uint level;
    Identifier ident;
    Module mod;

    extern (D) this(const ref Loc loc, Module mod, uint level, Identifier ident) @safe
    {
        super(loc);
        this.mod = mod;
        this.level = level;
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
        if (!global.debugids)
            global.debugids = new Identifiers();
        global.debugids.push(Identifier.idPool(ident));
    }


    /**
     * Instantiate a new `DebugCondition`
     *
     * Params:
     *   mod = Module this node belongs to
     *   level = Minimum global level this condition needs to pass.
     *           Only used if `ident` is `null`.
     *   ident = Identifier required for this condition to pass.
     *           If `null`, this conditiion will use an integer level.
     *  loc = Location in the source file
     */
    extern (D) this(const ref Loc loc, Module mod, uint level, Identifier ident) @safe
    {
        super(loc, mod, level, ident);
    }

    override int include(Scope* sc)
    {
        //printf("DebugCondition::include() level = %d, debuglevel = %d\n", level, global.params.debuglevel);
        if (inc == Include.notComputed)
        {
            inc = Include.no;
            bool definedInModule = false;
            if (ident)
            {
                if (findCondition(mod.debugids, ident))
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
            else if (level <= global.params.debuglevel || level <= mod.debuglevel)
                inc = Include.yes;
            if (!definedInModule)
                printDepsConditional(sc, this, "depsDebug ");
        }
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

    override const(char)* toChars() const
    {
        return ident ? ident.toChars() : "debug".ptr;
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
            case "CppRuntime_Clang":
            case "CppRuntime_DigitalMars":
            case "CppRuntime_Gcc":
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
    extern(D) static void checkReserved(const ref Loc loc, const(char)[] ident)
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
        if (!global.versionids)
            global.versionids = new Identifiers();
        global.versionids.push(Identifier.idPool(ident));
    }

    /**
     * Instantiate a new `VersionCondition`
     *
     * Params:
     *   mod = Module this node belongs to
     *   level = Minimum global level this condition needs to pass.
     *           Only used if `ident` is `null`.
     *   ident = Identifier required for this condition to pass.
     *           If `null`, this conditiion will use an integer level.
     *  loc = Location in the source file
     */
    extern (D) this(const ref Loc loc, Module mod, uint level, Identifier ident) @safe
    {
        super(loc, mod, level, ident);
    }

    override int include(Scope* sc)
    {
        //printf("VersionCondition::include() level = %d, versionlevel = %d\n", level, global.params.versionlevel);
        //if (ident) printf("\tident = '%s'\n", ident.toChars());
        if (inc == Include.notComputed)
        {
            inc = Include.no;
            bool definedInModule = false;
            if (ident)
            {
                if (findCondition(mod.versionids, ident))
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
            else if (level <= global.params.versionlevel || level <= mod.versionlevel)
                inc = Include.yes;
            if (!definedInModule &&
                (!ident || (!isReserved(ident.toString()) && ident != Id._unittest && ident != Id._assert)))
            {
                printDepsConditional(sc, this, "depsVersion ");
            }
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

    override const(char)* toChars() const
    {
        return ident ? ident.toChars() : "version".ptr;
    }
}

/***********************************************************
 */
extern (C++) final class StaticIfCondition : Condition
{
    Expression exp;

    extern (D) this(const ref Loc loc, Expression exp) @safe
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

        if (inc == Include.notComputed)
        {
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
        }
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

    override const(char)* toChars() const
    {
        return exp ? exp.toChars() : "static if".ptr;
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
bool findCondition(Identifiers* ids, Identifier ident) @safe nothrow pure
{
    if (ids)
    {
        foreach (id; *ids)
        {
            if (id == ident)
                return true;
        }
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
    else
        ob.print(condition.level);
    ob.writeByte('\n');
}
