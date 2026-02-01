/**
 * Defines `TemplateDeclaration`, `TemplateInstance` and a few utilities
 *
 * This modules holds the two main template types:
 * `TemplateDeclaration`, which is the user-provided declaration of a template,
 * and `TemplateInstance`, which is an instance of a `TemplateDeclaration`
 * with specific arguments.
 *
 * Template_Parameter:
 * Additionally, the classes for template parameters are defined in this module.
 * The base class, `TemplateParameter`, is inherited by:
 * - `TemplateTypeParameter`
 * - `TemplateThisParameter`
 * - `TemplateValueParameter`
 * - `TemplateAliasParameter`
 * - `TemplateTupleParameter`
 *
 * Templates_semantic:
 * The start of the template instantiation process looks like this:
 * - A `TypeInstance` or `TypeIdentifier` is encountered.
 *   `TypeInstance` have a bang (e.g. `Foo!(arg)`) while `TypeIdentifier` don't.
 * - A `TemplateInstance` is instantiated
 * - Semantic is run on the `TemplateInstance` (see `dmd.dsymbolsem`)
 * - The `TemplateInstance` search for its `TemplateDeclaration`,
 *   runs semantic on the template arguments and deduce the best match
 *   among the possible overloads.
 * - The `TemplateInstance` search for existing instances with the same
 *   arguments, and uses it if found.
 * - Otherwise, the rest of semantic is run on the `TemplateInstance`.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/dtemplate.d, _dtemplate.d)
 * Documentation:  https://dlang.org/phobos/dmd_dtemplate.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/dtemplate.d
 */

module dmd.dtemplate;

import core.stdc.stdio;
import core.stdc.string;
import dmd.arraytypes;
import dmd.astenums;
import dmd.ast_node;
import dmd.declaration;
import dmd.dmodule;
import dmd.dscope;
import dmd.dsymbol;
import dmd.errors;
import dmd.errorsink;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.hdrgen;
import dmd.identifier;
import dmd.location;
import dmd.mangle;
import dmd.mtype;
import dmd.root.array;
import dmd.common.outbuffer;
import dmd.rootobject;
import dmd.tokens;
import dmd.visitor;

//debug = FindExistingInstance; // print debug stats of findExistingInstance
private enum LOG = false;

enum IDX_NOTFOUND = 0x12345678;

pure nothrow @nogc @trusted
{

/********************************************
 * These functions substitute for dynamic_cast. dynamic_cast does not work
 * on earlier versions of gcc.
 */
inout(Expression) isExpression(inout RootObject o)
{
    //return dynamic_cast<Expression *>(o);
    if (!o || o.dyncast() != DYNCAST.expression)
        return null;
    return cast(inout(Expression))o;
}

inout(Dsymbol) isDsymbol(inout RootObject o)
{
    //return dynamic_cast<Dsymbol *>(o);
    if (!o || o.dyncast() != DYNCAST.dsymbol)
        return null;
    return cast(inout(Dsymbol))o;
}

inout(Type) isType(inout RootObject o)
{
    //return dynamic_cast<Type *>(o);
    if (!o || o.dyncast() != DYNCAST.type)
        return null;
    return cast(inout(Type))o;
}

inout(Tuple) isTuple(inout RootObject o)
{
    //return dynamic_cast<Tuple *>(o);
    if (!o || o.dyncast() != DYNCAST.tuple)
        return null;
    return cast(inout(Tuple))o;
}

inout(Parameter) isParameter(inout RootObject o)
{
    //return dynamic_cast<Parameter *>(o);
    if (!o || o.dyncast() != DYNCAST.parameter)
        return null;
    return cast(inout(Parameter))o;
}

inout(Identifier) isIdentifier(inout RootObject o)
{
    if (!o || o.dyncast() != DYNCAST.identifier)
        return null;
    return cast(inout(Identifier))o;
}

inout(TemplateParameter) isTemplateParameter(inout RootObject o)
{
    if (!o || o.dyncast() != DYNCAST.templateparameter)
        return null;
    return cast(inout(TemplateParameter))o;
}

} // end @trusted casts

pure nothrow @nogc @safe
{

/**************************************
 * Is this Object an error?
 */
bool isError(const RootObject o)
{
    if (const t = isType(o))
        return (t.ty == Terror);
    if (const e = isExpression(o))
        return (e.op == EXP.error || !e.type || e.type.ty == Terror);
    if (const v = isTuple(o))
        return arrayObjectIsError(v.objects);
    const s = isDsymbol(o);
    assert(s);
    if (s.errors)
        return true;
    return s.parent ? isError(s.parent) : false;
}

/**************************************
 * Are any of the Objects an error?
 */
bool arrayObjectIsError(const ref Objects args)
{
    foreach (const o; args)
    {
        if (isError(o))
            return true;
    }
    return false;
}

/***********************
 * Try to get arg as a type.
 */
inout(Type) getType(inout RootObject o)
{
    inout t = isType(o);
    if (!t)
    {
        if (inout e = isExpression(o))
            return e.type;
    }
    return t;
}

}

RootObject objectSyntaxCopy(RootObject o)
{
    if (!o)
        return null;
    if (Type t = isType(o))
        return t.syntaxCopy();
    if (Expression e = isExpression(o))
        return e.syntaxCopy();
    return o;
}

extern (C++) final class Tuple : RootObject
{
    Objects objects;

    extern (D) this() {}

    /**
    Params:
        numObjects = The initial number of objects.
    */
    extern (D) this(size_t numObjects)
    {
        objects.setDim(numObjects);
    }

    // kludge for template.isType()
    override DYNCAST dyncast() const
    {
        return DYNCAST.tuple;
    }

    override const(char)* toChars() const
    {
        return objects.toChars();
    }
}

struct TemplatePrevious
{
    TemplatePrevious* prev;
    Scope* sc;
    Objects* dedargs;
}

/***********************************************************
 * [mixin] template Identifier (parameters) [Constraint]
 * https://dlang.org/spec/template.html
 * https://dlang.org/spec/template-mixin.html
 */
extern (C++) final class TemplateDeclaration : ScopeDsymbol
{
    import dmd.root.array : Array;

    TemplateParameters* parameters;     // array of TemplateParameter's
    TemplateParameters* origParameters; // originals for Ddoc

    Expression constraint;

    // Hash table to look up TemplateInstance's of this TemplateDeclaration
    void* instances;

    TemplateDeclaration overnext;       // next overloaded TemplateDeclaration
    TemplateDeclaration overroot;       // first in overnext list
    FuncDeclaration funcroot;           // first function in unified overload list

    Dsymbol onemember;      // if !=null then one member of this template

    bool literal;           // this template declaration is a literal
    bool ismixin;           // this is a mixin template declaration
    bool isstatic;          // this is static template declaration
    bool isTrivialAliasSeq; /// matches pattern `template AliasSeq(T...) { alias AliasSeq = T; }`
    bool isTrivialAlias;    /// matches pattern `template Alias(T) { alias Alias = qualifiers(T); }`
    bool deprecated_;       /// this template declaration is deprecated
    bool isCmacro;          /// Whether this template is a translation of a C macro
    bool haveComputedOneMember; /// Whether computeOneMeber has been called
    Visibility visibility;

    // threaded list of previous instantiation attempts on stack
    TemplatePrevious* previous;

    Expression lastConstraint; /// the constraint after the last failed evaluation
    Array!Expression lastConstraintNegs; /// its negative parts
    Objects* lastConstraintTiargs; /// template instance arguments for `lastConstraint`

    extern (D) this(Loc loc, Identifier ident, TemplateParameters* parameters, Expression constraint, Dsymbols* decldefs, bool ismixin = false, bool literal = false)
    {
        super(loc, ident);
        this.dsym = DSYM.templateDeclaration;
        static if (LOG)
        {
            printf("TemplateDeclaration(this = %p, id = '%s')\n", this, ident.toChars());
        }
        version (none)
        {
            if (parameters)
                for (int i = 0; i < parameters.length; i++)
                {
                    TemplateParameter tp = (*parameters)[i];
                    //printf("\tparameter[%d] = %p\n", i, tp);
                    TemplateTypeParameter ttp = tp.isTemplateTypeParameter();
                    if (ttp)
                    {
                        printf("\tparameter[%d] = %s : %s\n", i, tp.ident.toChars(), ttp.specType ? ttp.specType.toChars() : "");
                    }
                }
        }
        this.parameters = parameters;
        this.origParameters = parameters;
        this.constraint = constraint;
        this.members = decldefs;
        this.literal = literal;
        this.ismixin = ismixin;
        this.isstatic = true;
        this.haveComputedOneMember = false;
        this.visibility = Visibility(Visibility.Kind.undefined);
    }

    extern(D) void computeIsTrivialAlias(Dsymbol s)
    {
        /* Set isTrivialAliasSeq if this fits the pattern:
         *   template AliasSeq(T...) { alias AliasSeq = T; }
         * or set isTrivialAlias if this fits the pattern:
         *   template Alias(T) { alias Alias = qualifiers(T); }
         */
        if (!(parameters && parameters.length == 1))
            return;

        auto ad = s.isAliasDeclaration();
        if (!ad || !ad.type)
            return;

        auto ti = ad.type.isTypeIdentifier();

        if (!ti || ti.idents.length != 0)
            return;

        if (auto ttp = (*parameters)[0].isTemplateTupleParameter())
        {
            if (ti.ident is ttp.ident &&
                ti.mod == 0)
            {
                //printf("found isTrivialAliasSeq %s %s\n", s.toChars(), ad.type.toChars());
                isTrivialAliasSeq = true;
            }
        }
        else if (auto ttp = (*parameters)[0].isTemplateTypeParameter())
        {
            if (ti.ident is ttp.ident)
            {
                //printf("found isTrivialAlias %s %s\n", s.toChars(), ad.type.toChars());
                isTrivialAlias = true;
            }
        }
    }

    override TemplateDeclaration syntaxCopy(Dsymbol)
    {
        //printf("TemplateDeclaration.syntaxCopy()\n");
        TemplateParameters* p = null;
        if (parameters)
        {
            p = new TemplateParameters(parameters.length);
            foreach (i, ref param; *p)
                param = (*parameters)[i].syntaxCopy();
        }
        return new TemplateDeclaration(loc, ident, p, constraint ? constraint.syntaxCopy() : null, Dsymbol.arraySyntaxCopy(members), ismixin, literal);
    }

    override const(char)* kind() const
    {
        return (onemember && onemember.isAggregateDeclaration()) ? onemember.kind() : "template";
    }

    /****************************
     * Similar to `toChars`, but does not print the template constraints
     */
    const(char)* toCharsNoConstraints() const
    {
        HdrGenState hgs = { skipConstraints: true };
        OutBuffer buf;
        toCharsMaybeConstraints(this, buf, hgs);
        return buf.extractChars();
    }

    override Visibility visible() pure nothrow @nogc @safe
    {
        return visibility;
    }

    /**
     * Check if the last template parameter is a tuple one,
     * and returns it if so, else returns `null`.
     *
     * Returns:
     *   The last template parameter if it's a `TemplateTupleParameter`
     */
    extern (D) TemplateTupleParameter isVariadic()
    {
        const dim = parameters.length;
        if (dim == 0)
            return null;
        return (*parameters)[dim - 1].isTemplateTupleParameter();
    }

    extern(C++) override bool isDeprecated() const
    {
        return this.deprecated_;
    }

    /***********************************
     * We can overload templates.
     */
    override bool isOverloadable() const
    {
        return true;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

extern (C++) final class TypeDeduced : Type
{
    Type tded;
    Expressions argexps; // corresponding expressions
    Types tparams; // tparams[i].mod

    extern (D) this(Type tt, Expression e, Type tparam)
    {
        super(Tnone);
        tded = tt;
        argexps.push(e);
        tparams.push(tparam);
    }

    void update(Expression e, Type tparam)
    {
        argexps.push(e);
        tparams.push(tparam);
    }

    void update(Type tt, Expression e, Type tparam)
    {
        tded = tt;
        argexps.push(e);
        tparams.push(tparam);
    }
}

/***********************************************************
 * https://dlang.org/spec/template.html#TemplateParameter
 */
extern (C++) class TemplateParameter : ASTNode
{
    Loc loc;
    Identifier ident;

    /* True if this is a part of precedent parameter specialization pattern.
     *
     *  template A(T : X!TL, alias X, TL...) {}
     *  // X and TL are dependent template parameter
     *
     * A dependent template parameter should return MATCH.exact in matchArg()
     * to respect the match level of the corresponding precedent parameter.
     */
    bool dependent;

    /* ======================== TemplateParameter =============================== */
    extern (D) this(Loc loc, Identifier ident) @safe
    {
        this.loc = loc;
        this.ident = ident;
    }

    TemplateTypeParameter isTemplateTypeParameter()
    {
        return null;
    }

    TemplateValueParameter isTemplateValueParameter()
    {
        return null;
    }

    TemplateAliasParameter isTemplateAliasParameter()
    {
        return null;
    }

    TemplateThisParameter isTemplateThisParameter()
    {
        return null;
    }

    TemplateTupleParameter isTemplateTupleParameter()
    {
        return null;
    }

    abstract TemplateParameter syntaxCopy();

    abstract void print(RootObject oarg, RootObject oded);

    abstract RootObject specialization();

    abstract bool hasDefaultArg();

    override const(char)* toChars() const
    {
        return this.ident.toChars();
    }

    override DYNCAST dyncast() const
    {
        return DYNCAST.templateparameter;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/template.html#TemplateTypeParameter
 * Syntax:
 *  ident : specType = defaultType
 */
extern (C++) class TemplateTypeParameter : TemplateParameter
{
    Type specType;      // if !=null, this is the type specialization
    Type defaultType;

    extern (D) __gshared Type tdummy = null;

    extern (D) this(Loc loc, Identifier ident, Type specType, Type defaultType) @safe
    {
        super(loc, ident);
        this.specType = specType;
        this.defaultType = defaultType;
    }

    override final TemplateTypeParameter isTemplateTypeParameter()
    {
        return this;
    }

    override TemplateTypeParameter syntaxCopy()
    {
        return new TemplateTypeParameter(loc, ident, specType ? specType.syntaxCopy() : null, defaultType ? defaultType.syntaxCopy() : null);
    }

    override final void print(RootObject oarg, RootObject oded)
    {
        printf(" %s\n", ident.toChars());

        Type t = isType(oarg);
        Type ta = isType(oded);
        assert(ta);

        if (specType)
            printf("\tSpecialization: %s\n", specType.toChars());
        if (defaultType)
            printf("\tDefault:        %s\n", defaultType.toChars());
        printf("\tParameter:       %s\n", t ? t.toChars() : "NULL");
        printf("\tDeduced Type:   %s\n", ta.toChars());
    }

    override final RootObject specialization()
    {
        return specType;
    }

    override final bool hasDefaultArg()
    {
        return defaultType !is null;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/template.html#TemplateThisParameter
 * Syntax:
 *  this ident : specType = defaultType
 */
extern (C++) final class TemplateThisParameter : TemplateTypeParameter
{
    extern (D) this(Loc loc, Identifier ident, Type specType, Type defaultType) @safe
    {
        super(loc, ident, specType, defaultType);
    }

    override TemplateThisParameter isTemplateThisParameter()
    {
        return this;
    }

    override TemplateThisParameter syntaxCopy()
    {
        return new TemplateThisParameter(loc, ident, specType ? specType.syntaxCopy() : null, defaultType ? defaultType.syntaxCopy() : null);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/template.html#TemplateValueParameter
 * Syntax:
 *  valType ident : specValue = defaultValue
 */
extern (C++) final class TemplateValueParameter : TemplateParameter
{
    Type valType;
    Expression specValue;
    Expression defaultValue;

    extern (D) __gshared Expression[void*] edummies;

    extern (D) this(Loc loc, Identifier ident, Type valType,
        Expression specValue, Expression defaultValue) @safe
    {
        super(loc, ident);
        this.valType = valType;
        this.specValue = specValue;
        this.defaultValue = defaultValue;
    }

    override TemplateValueParameter isTemplateValueParameter()
    {
        return this;
    }

    override TemplateValueParameter syntaxCopy()
    {
        return new TemplateValueParameter(loc, ident,
            valType.syntaxCopy(),
            specValue ? specValue.syntaxCopy() : null,
            defaultValue ? defaultValue.syntaxCopy() : null);
    }

    override void print(RootObject oarg, RootObject oded)
    {
        printf(" %s\n", ident.toChars());
        Expression ea = isExpression(oded);
        if (specValue)
            printf("\tSpecialization: %s\n", specValue.toChars());
        printf("\tParameter Value: %s\n", ea ? ea.toChars() : "NULL");
    }

    override RootObject specialization()
    {
        return specValue;
    }

    override bool hasDefaultArg()
    {
        return defaultValue !is null;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/template.html#TemplateAliasParameter
 * Syntax:
 *  specType ident : specAlias = defaultAlias
 */
extern (C++) final class TemplateAliasParameter : TemplateParameter
{
    Type specType;
    RootObject specAlias;
    RootObject defaultAlias;

    extern (D) __gshared Dsymbol sdummy = null;

    extern (D) this(Loc loc, Identifier ident, Type specType, RootObject specAlias, RootObject defaultAlias) @safe
    {
        super(loc, ident);
        this.specType = specType;
        this.specAlias = specAlias;
        this.defaultAlias = defaultAlias;
    }

    override TemplateAliasParameter isTemplateAliasParameter()
    {
        return this;
    }

    override TemplateAliasParameter syntaxCopy()
    {
        return new TemplateAliasParameter(loc, ident, specType ? specType.syntaxCopy() : null, objectSyntaxCopy(specAlias), objectSyntaxCopy(defaultAlias));
    }

    override void print(RootObject oarg, RootObject oded)
    {
        printf(" %s\n", ident.toChars());
        Dsymbol sa = isDsymbol(oded);
        assert(sa);
        printf("\tParameter alias: %s\n", sa.toChars());
    }

    override RootObject specialization()
    {
        return specAlias;
    }

    override bool hasDefaultArg()
    {
        return defaultAlias !is null;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/template.html#TemplateSequenceParameter
 * Syntax:
 *  ident ...
 */
extern (C++) final class TemplateTupleParameter : TemplateParameter
{
    extern (D) this(Loc loc, Identifier ident) @safe
    {
        super(loc, ident);
    }

    override TemplateTupleParameter isTemplateTupleParameter()
    {
        return this;
    }

    override TemplateTupleParameter syntaxCopy()
    {
        return new TemplateTupleParameter(loc, ident);
    }

    override void print(RootObject oarg, RootObject oded)
    {
        printf(" %s... [", ident.toChars());
        Tuple v = isTuple(oded);
        assert(v);

        //printf("|%d| ", v.objects.length);
        foreach (i, o; v.objects)
        {
            if (i)
                printf(", ");

            Dsymbol sa = isDsymbol(o);
            if (sa)
                printf("alias: %s", sa.toChars());
            Type ta = isType(o);
            if (ta)
                printf("type: %s", ta.toChars());
            Expression ea = isExpression(o);
            if (ea)
                printf("exp: %s", ea.toChars());

            assert(!isTuple(o)); // no nested Tuple arguments
        }
        printf("]\n");
    }

    override RootObject specialization()
    {
        return null;
    }

    override bool hasDefaultArg()
    {
        return false;
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************************
 * https://dlang.org/spec/template.html#explicit_tmp_instantiation
 * Given:
 *  foo!(args) =>
 *      name = foo
 *      tiargs = args
 */
extern (C++) class TemplateInstance : ScopeDsymbol
{
    Identifier name;

    // Array of Types/Expressions of template
    // instance arguments [int*, char, 10*10]
    Objects* tiargs;

    // Array of Types/Expressions corresponding
    // to TemplateDeclaration.parameters
    // [int, char, 100]
    Objects tdtypes;

    // Modules imported by this template instance
    Modules importedModules;

    Dsymbol tempdecl;           // referenced by foo.bar.abc
    Dsymbol enclosing;          // if referencing local symbols, this is the context
    Dsymbol aliasdecl;          // !=null if instance is an alias for its sole member

    /**
    If this is not null and it has a value that is not the current object,
     then this field points to an existing template instance
     and that object has been duplicated into us.

    If this object is a duplicate,
     the ``memberOf`` field will be set to a root module (passed on CLI).

    This information is useful to deduplicate analysis that may occur
     after semantic 3 has completed.

    See_Also: memberOf
    */
    TemplateInstance inst;

    ScopeDsymbol argsym;        // argument symbol table

    /// For function template, these are the function fnames(name and loc of it) and arguments
    /// Relevant because different resolutions of `auto ref` parameters
    /// create different template instances even with the same template arguments
    Expressions* fargs;
    ArgumentLabels* fnames;

    TemplateInstances* deferred;

    /**
    If this is not null then this template instance appears in a root module's members.

    Note:   This is not useful for determining duplication status of this template instance.
            Use the field ``inst`` for determining if a template instance has been duplicated into this object.

    See_Also: inst
    */
    Module memberOf;

    // Used to determine the instance needs code generation.
    // Note that these are inaccurate until semantic analysis phase completed.
    TemplateInstance tinst;     // enclosing template instance
    TemplateInstance tnext;     // non-first instantiated instances
    Module minst;               // the top module that instantiated this instance

    private ushort _nest;       // for recursive pretty printing detection, 3 MSBs reserved for flags (below)
    ubyte inuse;                // for recursive expansion detection

    private enum Flag : uint
    {
        semantictiargsdone = 1u << (_nest.sizeof * 8 - 1), // MSB of _nest
        havetempdecl = semantictiargsdone >> 1,
        gagged = semantictiargsdone >> 2,
        available = gagged - 1 // always last flag minus one, 1s for all available bits
    }

    extern(D) final @safe @property pure nothrow @nogc
    {
        ushort nest() const { return _nest & Flag.available; }
        void nestUp() { assert(nest() < Flag.available); ++_nest; }
        void nestDown() { assert(nest() > 0); --_nest; }
        /// has semanticTiargs() been done?
        bool semantictiargsdone() const { return (_nest & Flag.semantictiargsdone) != 0; }
        void semantictiargsdone(bool x)
        {
            if (x) _nest |= Flag.semantictiargsdone;
            else _nest &= ~Flag.semantictiargsdone;
        }
        /// if used second constructor
        bool havetempdecl() const { return (_nest & Flag.havetempdecl) != 0; }
        void havetempdecl(bool x)
        {
            if (x) _nest |= Flag.havetempdecl;
            else _nest &= ~Flag.havetempdecl;
        }
        /// if the instantiation is done with error gagging
        bool gagged() const { return (_nest & Flag.gagged) != 0; }
        void gagged(bool x)
        {
            if (x) _nest |= Flag.gagged;
            else _nest &= ~Flag.gagged;
        }
    }

    extern (D) this(Loc loc, Identifier ident, Objects* tiargs) scope
    {
        super(loc, null);
        static if (LOG)
        {
            printf("TemplateInstance(this = %p, ident = '%s')\n", this, ident ? ident.toChars() : "null");
        }
        this.dsym = DSYM.templateInstance;
        this.name = ident;
        this.tiargs = tiargs;
    }

    /*****************
     * This constructor is only called when we figured out which function
     * template to instantiate.
     */
    extern (D) this(Loc loc, TemplateDeclaration td, Objects* tiargs) scope
    {
        super(loc, null);
        static if (LOG)
        {
            printf("TemplateInstance(this = %p, tempdecl = '%s')\n", this, td.toChars());
        }
        this.dsym = DSYM.templateInstance;
        this.name = td.ident;
        this.tiargs = tiargs;
        this.tempdecl = td;
        this.semantictiargsdone = true;
        this.havetempdecl = true;
        assert(tempdecl._scope);
    }

    extern (D) static Objects* arraySyntaxCopy(Objects* objs)
    {
        Objects* a = null;
        if (objs)
        {
            a = new Objects(objs.length);
            foreach (i, o; *objs)
                (*a)[i] = objectSyntaxCopy(o);
        }
        return a;
    }

    override TemplateInstance syntaxCopy(Dsymbol s)
    {
        TemplateInstance ti = s ? cast(TemplateInstance)s : new TemplateInstance(loc, name, null);
        ti.tiargs = arraySyntaxCopy(tiargs);
        TemplateDeclaration td;
        if (inst && tempdecl && (td = tempdecl.isTemplateDeclaration()) !is null)
            td.ScopeDsymbol.syntaxCopy(ti);
        else
            ScopeDsymbol.syntaxCopy(ti);
        return ti;
    }

    override const(char)* kind() const
    {
        return "template instance";
    }

    override final const(char)* toPrettyCharsHelper()
    {
        OutBuffer buf;
        toCBufferInstance(this, buf, true);
        return buf.extractChars();
    }

    /**************************************
     * Given an error instantiating the TemplateInstance,
     * give the nested TemplateInstance instantiations that got
     * us here. Those are a list threaded into the nested scopes.
     * Params:
     *  cl = classification of this trace as printing either errors or deprecations
     *  max_shown = maximum number of trace elements printed (controlled with -v/-verror-limit)
     */
    extern(D) final void printInstantiationTrace(Classification cl = Classification.error,
                                                 const(uint) max_shown = global.params.v.errorSupplementCount())
    {
        if (global.gag)
            return;

        // Print full trace for verbose mode, otherwise only short traces
        const(char)* format = "instantiated from here: `%s`";

        // This returns a function pointer
        scope printFn = () {
            final switch (cl)
            {
                case Classification.error:
                    return &errorSupplemental;
                case Classification.deprecation:
                    return &deprecationSupplemental;
                case Classification.gagged, Classification.tip, Classification.warning:
                    assert(0);
            }
        }();

        // determine instantiation depth and number of recursive instantiations
        int n_instantiations = 1;
        int n_totalrecursions = 0;
        for (TemplateInstance cur = this; cur; cur = cur.tinst)
        {
            ++n_instantiations;
            // Set error here as we don't want it to depend on the number of
            // entries that are being printed.
            if (cl == Classification.error ||
                (cl == Classification.warning && global.params.useWarnings == DiagnosticReporting.error) ||
                (cl == Classification.deprecation && global.params.useDeprecated == DiagnosticReporting.error))
                cur.errors = true;

            // If two instantiations use the same declaration, they are recursive.
            // (this works even if they are instantiated from different places in the
            // same template).
            // In principle, we could also check for multiple-template recursion, but it's
            // probably not worthwhile.
            if (cur.tinst && cur.tempdecl && cur.tinst.tempdecl && cur.tempdecl.loc.equals(cur.tinst.tempdecl.loc))
                ++n_totalrecursions;
        }

        if (n_instantiations <= max_shown)
        {
            for (TemplateInstance cur = this; cur; cur = cur.tinst)
                printFn(cur.loc, format, cur.toErrMsg());
        }
        else if (n_instantiations - n_totalrecursions <= max_shown)
        {
            // By collapsing recursive instantiations into a single line,
            // we can stay under the limit.
            int recursionDepth = 0;
            for (TemplateInstance cur = this; cur; cur = cur.tinst)
            {
                if (cur.tinst && cur.tempdecl && cur.tinst.tempdecl && cur.tempdecl.loc.equals(cur.tinst.tempdecl.loc))
                {
                    ++recursionDepth;
                }
                else
                {
                    if (recursionDepth)
                        printFn(cur.loc, "%d recursive instantiations from here: `%s`", recursionDepth + 2, cur.toChars());
                    else
                        printFn(cur.loc, format, cur.toChars());
                    recursionDepth = 0;
                }
            }
        }
        else
        {
            // Even after collapsing the recursions, the depth is too deep.
            // Just display the first few and last few instantiations.
            uint i = 0;
            for (TemplateInstance cur = this; cur; cur = cur.tinst)
            {
                if (i == max_shown / 2)
                    printFn(cur.loc, "... (%d instantiations, -v to show) ...", n_instantiations - max_shown);

                if (i < max_shown / 2 || i >= n_instantiations - max_shown + max_shown / 2)
                    printFn(cur.loc, format, cur.toChars());
                ++i;
            }
        }
    }

    /*************************************
     * Lazily generate identifier for template instance.
     * This is because 75% of the ident's are never needed.
     */
    override final Identifier getIdent()
    {
        if (!ident && inst && !errors)
            ident = genIdent(tiargs); // need an identifier for name mangling purposes.
        return ident;
    }

    /****************************************
     * This instance needs an identifier for name mangling purposes.
     * Create one by taking the template declaration name and adding
     * the type signature for it.
     */
    extern (D) final Identifier genIdent(Objects* args)
    {
        //printf("TemplateInstance.genIdent('%s')\n", tempdecl.ident.toChars());
        assert(args is tiargs);
        OutBuffer buf;
        mangleToBuffer(this, buf);
        //printf("\tgenIdent = %s\n", buf.peekChars());
        return Identifier.idPool(buf[]);
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/**********************************
 * Return true if e could be valid only as a template value parameter.
 * Return false if it might be an alias or tuple.
 * (Note that even in this case, it could still turn out to be a value).
 */
bool definitelyValueParameter(Expression e) @safe
{
    // None of these can be value parameters
    if (e.op == EXP.tuple || e.op == EXP.scope_ ||
        e.op == EXP.type || e.op == EXP.dotType ||
        e.op == EXP.template_ || e.op == EXP.dotTemplateDeclaration ||
        e.op == EXP.function_ || e.op == EXP.error ||
        e.op == EXP.this_ || e.op == EXP.super_ ||
        e.op == EXP.dot)
        return false;

    if (e.op != EXP.dotVariable)
        return true;

    /* Template instantiations involving a DotVar expression are difficult.
     * In most cases, they should be treated as a value parameter, and interpreted.
     * But they might also just be a fully qualified name, which should be treated
     * as an alias.
     */

    // x.y.f cannot be a value
    FuncDeclaration f = e.isDotVarExp().var.isFuncDeclaration();
    if (f)
        return false;

    while (e.op == EXP.dotVariable)
    {
        e = e.isDotVarExp().e1;
    }
    // this.x.y and super.x.y couldn't possibly be valid values.
    if (e.op == EXP.this_ || e.op == EXP.super_)
        return false;

    // e.type.x could be an alias
    if (e.op == EXP.dotType)
        return false;

    // var.x.y is the only other possible form of alias
    if (e.op != EXP.variable)
        return true;

    VarDeclaration v = e.isVarExp().var.isVarDeclaration();
    // func.x.y is not an alias
    if (!v)
        return true;

    // https://issues.dlang.org/show_bug.cgi?id=16685
    // var.x.y where var is a constant available at compile time
    if (v.storage_class & STC.manifest)
        return true;

    // TODO: Should we force CTFE if it is a global constant?
    return false;
}

/***********************************************************
 * https://dlang.org/spec/template-mixin.html
 * Syntax:
 *    mixin MixinTemplateName [TemplateArguments] [Identifier];
 */
extern (C++) final class TemplateMixin : TemplateInstance
{
    TypeQualified tqual;

    extern (D) this(Loc loc, Identifier ident, TypeQualified tqual, Objects* tiargs)
    {
        super(loc,
              tqual.idents.length ? cast(Identifier)tqual.idents[tqual.idents.length - 1] : (cast(TypeIdentifier)tqual).ident,
              tiargs ? tiargs : new Objects());
        //printf("TemplateMixin(ident = '%s')\n", ident ? ident.toChars() : "");
        this.dsym = DSYM.templateMixin;
        this.ident = ident;
        this.tqual = tqual;
    }

    override TemplateInstance syntaxCopy(Dsymbol s)
    {
        auto tm = new TemplateMixin(loc, ident, tqual.syntaxCopy(), tiargs);
        return TemplateInstance.syntaxCopy(tm);
    }

    override const(char)* kind() const
    {
        return "mixin";
    }

    override void accept(Visitor v)
    {
        v.visit(this);
    }
}

/***********************************************
 * Collect and print statistics on template instantiations.
 */
struct TemplateStats
{
    __gshared TemplateStats[const void*] stats;

    uint numInstantiations;     // number of instantiations of the template
    uint uniqueInstantiations;  // number of unique instantiations of the template

    TemplateInstances* allInstances;

    /*******************************
     * Add this instance
     * Params:
     *  td = template declaration
     *  ti = instance of td
     *  listInstances = keep track of instances of templates
     */
    static void incInstance(const TemplateDeclaration td,
                            const TemplateInstance ti,
                            bool listInstances)
    {
        void log(ref TemplateStats ts)
        {
            if (ts.allInstances is null)
                ts.allInstances = new TemplateInstances();
            if (listInstances)
                ts.allInstances.push(cast() ti);
        }

        // message(ti.loc, "incInstance %p %p", td, ti);
        if (!td)
            return;
        assert(ti);
        if (auto ts = cast(const void*) td in stats)
        {
            log(*ts);
            ++ts.numInstantiations;
        }
        else
        {
            stats[cast(const void*) td] = TemplateStats(1, 0);
            log(stats[cast(const void*) td]);
        }
    }

    /*******************************
     * Add this unique instance
     */
    static void incUnique(const TemplateDeclaration td,
                          const TemplateInstance ti)
    {
        // message(ti.loc, "incUnique %p %p", td, ti);
        if (!td)
            return;
        assert(ti);
        if (auto ts = cast(const void*) td in stats)
            ++ts.uniqueInstantiations;
        else
            stats[cast(const void*) td] = TemplateStats(0, 1);
    }
}

/********************************
 * Print informational statistics on template instantiations.
 * Params:
 *      listInstances = list instances of templates
 *      eSink = where the print is sent
 */
void printTemplateStats(bool listInstances, ErrorSink eSink)
{
    static struct TemplateDeclarationStats
    {
        TemplateDeclaration td;
        TemplateStats ts;
        static int compare(scope const TemplateDeclarationStats* a,
                           scope const TemplateDeclarationStats* b) @safe nothrow @nogc pure
        {
            auto diff = b.ts.uniqueInstantiations - a.ts.uniqueInstantiations;
            if (diff)
                return diff;

            return b.ts.numInstantiations - a.ts.numInstantiations;
        }
    }

    const stats_length = TemplateStats.stats.length;
    if (!stats_length)
        return;         // nothing to report

    Array!(TemplateDeclarationStats) sortedStats;
    sortedStats.reserve(stats_length);
    foreach (td_, ref ts; TemplateStats.stats)
    {
        sortedStats.push(TemplateDeclarationStats(cast(TemplateDeclaration) td_, ts));
    }

    sortedStats.sort!(TemplateDeclarationStats.compare);

    OutBuffer buf;
    foreach (const ref ss; sortedStats[])
    {
        buf.reset();
        HdrGenState hgs;
        hgs.skipConstraints = true;
        toCharsMaybeConstraints(ss.td, buf, hgs);
        const tchars = buf.peekChars();
        if (listInstances && ss.ts.allInstances)
        {
            eSink.message(ss.td.loc,
                    "vtemplate: %u (%u distinct) instantiation(s) of template `%s` found, they are:",
                    ss.ts.numInstantiations,
                    ss.ts.uniqueInstantiations,
                    tchars);
            foreach (const ti; (*ss.ts.allInstances)[])
            {
                if (ti.tinst)   // if has enclosing instance
                    eSink.message(ti.loc, "vtemplate: implicit instance `%s`", ti.toChars());
                else
                    eSink.message(ti.loc, "vtemplate: explicit instance `%s`", ti.toChars());
            }
        }
        else
        {
            eSink.message(ss.td.loc,
                    "vtemplate: %u (%u distinct) instantiation(s) of template `%s` found",
                    ss.ts.numInstantiations,
                    ss.ts.uniqueInstantiations,
                    tchars);
        }
    }
}

void write(ref OutBuffer buf, RootObject obj)
{
    if (obj)
    {
        if (auto e = isExpression(obj))
            buf.writestring(e.toErrMsg());
        else
            buf.writestring(obj.toChars());
    }
}
