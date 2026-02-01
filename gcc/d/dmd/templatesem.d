/**
 * Template semantics.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/templatesem.d, _templatesem.d)
 * Documentation:  https://dlang.org/phobos/dmd_templatesem.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/templatesem.d
 */

module dmd.templatesem;

import core.stdc.stdio;
import core.stdc.string;
import dmd.aggregate;
import dmd.aliasthis;
import dmd.arraytypes;
import dmd.astenums;
import dmd.ast_node;
import dmd.attrib;
import dmd.dcast;
import dmd.dclass;
import dmd.declaration;
import dmd.dinterpret;
import dmd.dmodule;
import dmd.dscope;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.errorsink;
import dmd.expression;
import dmd.expressionsem;
import dmd.func;
import dmd.funcsem;
import dmd.globals;
import dmd.hdrgen;
import dmd.id;
import dmd.identifier;
import dmd.impcnvtab;
import dmd.init;
import dmd.initsem;
import dmd.location;
import dmd.mtype;
import dmd.opover;
import dmd.optimize;
import dmd.root.array;
import dmd.common.outbuffer;
import dmd.rootobject;
import dmd.semantic2;
import dmd.semantic3;
import dmd.templateparamsem;
import dmd.timetrace;
import dmd.tokens;
import dmd.typesem;
import dmd.visitor;

alias funcLeastAsSpecialized = dmd.funcsem.leastAsSpecialized;

enum LOG = false;

/***********************************************************
 * Check whether the type t representation relies on one or more the template parameters.
 * Params:
 *      t           = Tested type, if null, returns false.
 *      tparams     = Template parameters.
 *      iStart      = Start index of tparams to limit the tested parameters. If it's
 *                    nonzero, tparams[0..iStart] will be excluded from the test target.
 */
bool reliesOnTident(Type t, TemplateParameters* tparams, size_t iStart = 0)
{
    return reliesOnTemplateParameters(t, (*tparams)[0 .. tparams.length]);
}

/***********************************************************
 * Check whether the type t representation relies on one or more the template parameters.
 * Params:
 *      t           = Tested type, if null, returns false.
 *      tparams     = Template parameters.
 */
bool reliesOnTemplateParameters(Type t, TemplateParameter[] tparams)
{
    bool visitVector(TypeVector t)
    {
        return t.basetype.reliesOnTemplateParameters(tparams);
    }

    bool visitAArray(TypeAArray t)
    {
        return t.next.reliesOnTemplateParameters(tparams) ||
               t.index.reliesOnTemplateParameters(tparams);
    }

    bool visitFunction(TypeFunction t)
    {
        foreach (i, fparam; t.parameterList)
        {
            if (fparam.type.reliesOnTemplateParameters(tparams))
                return true;
        }
        return t.next.reliesOnTemplateParameters(tparams);
    }

    bool visitIdentifier(TypeIdentifier t)
    {
        foreach (tp; tparams)
        {
            if (tp.ident.equals(t.ident))
                return true;
        }
        return false;
    }

    bool visitInstance(TypeInstance t)
    {
        foreach (tp; tparams)
        {
            if (t.tempinst.name == tp.ident)
                return true;
        }

        if (t.tempinst.tiargs)
            foreach (arg; *t.tempinst.tiargs)
            {
                if (Type ta = isType(arg))
                {
                    if (ta.reliesOnTemplateParameters(tparams))
                        return true;
                }
            }

        return false;
    }

    bool visitTypeof(TypeTypeof t)
    {
        //printf("TypeTypeof.reliesOnTemplateParameters('%s')\n", t.toChars());
        return t.exp.reliesOnTemplateParameters(tparams);
    }

    bool visitTuple(TypeTuple t)
    {
        if (t.arguments)
            foreach (arg; *t.arguments)
            {
                if (arg.type.reliesOnTemplateParameters(tparams))
                    return true;
            }

        return false;
    }

    if (!t)
        return false;

    Type tb = t.toBasetype();
    switch (tb.ty)
    {
        case Tvector:   return visitVector(tb.isTypeVector());
        case Taarray:   return visitAArray(tb.isTypeAArray());
        case Tfunction: return visitFunction(tb.isTypeFunction());
        case Tident:    return visitIdentifier(tb.isTypeIdentifier());
        case Tinstance: return visitInstance(tb.isTypeInstance());
        case Ttypeof:   return visitTypeof(tb.isTypeTypeof());
        case Ttuple:    return visitTuple(tb.isTypeTuple());
        case Tenum:     return false;
        default:        return tb.nextOf().reliesOnTemplateParameters(tparams);
    }
}

/***********************************************************
 * Check whether the expression representation relies on one or more the template parameters.
 * Params:
 *      e           = expression to test
 *      tparams     = Template parameters.
 * Returns:
 *      true if it does
 */
private bool reliesOnTemplateParameters(Expression e, TemplateParameter[] tparams)
{
    extern (C++) final class ReliesOnTemplateParameters : Visitor
    {
        alias visit = Visitor.visit;
    public:
        TemplateParameter[] tparams;
        bool result;

        extern (D) this(TemplateParameter[] tparams) @safe
        {
            this.tparams = tparams;
        }

        override void visit(Expression e)
        {
            //printf("Expression.reliesOnTemplateParameters('%s')\n", e.toChars());
        }

        override void visit(IdentifierExp e)
        {
            //printf("IdentifierExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            foreach (tp; tparams)
            {
                if (e.ident == tp.ident)
                {
                    result = true;
                    return;
                }
            }
        }

        override void visit(TupleExp e)
        {
            //printf("TupleExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            if (e.exps)
            {
                foreach (ea; *e.exps)
                {
                    ea.accept(this);
                    if (result)
                        return;
                }
            }
        }

        override void visit(ArrayLiteralExp e)
        {
            //printf("ArrayLiteralExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            if (e.elements)
            {
                foreach (el; *e.elements)
                {
                    el.accept(this);
                    if (result)
                        return;
                }
            }
        }

        override void visit(AssocArrayLiteralExp e)
        {
            //printf("AssocArrayLiteralExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            foreach (ek; *e.keys)
            {
                ek.accept(this);
                if (result)
                    return;
            }
            foreach (ev; *e.values)
            {
                ev.accept(this);
                if (result)
                    return;
            }
        }

        override void visit(StructLiteralExp e)
        {
            //printf("StructLiteralExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            if (e.elements)
            {
                foreach (ea; *e.elements)
                {
                    ea.accept(this);
                    if (result)
                        return;
                }
            }
        }

        override void visit(TypeExp e)
        {
            //printf("TypeExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            result = e.type.reliesOnTemplateParameters(tparams);
        }

        override void visit(NewExp e)
        {
            //printf("NewExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            if (e.placement)
                e.placement.accept(this);
            if (e.thisexp)
                e.thisexp.accept(this);
            result = e.newtype.reliesOnTemplateParameters(tparams);
            if (!result && e.arguments)
            {
                foreach (ea; *e.arguments)
                {
                    ea.accept(this);
                    if (result)
                        return;
                }
            }
        }

        override void visit(NewAnonClassExp e)
        {
            //printf("NewAnonClassExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            result = true;
        }

        override void visit(FuncExp e)
        {
            //printf("FuncExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            result = true;
        }

        override void visit(TypeidExp e)
        {
            //printf("TypeidExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            if (auto ea = isExpression(e.obj))
                ea.accept(this);
            else if (auto ta = isType(e.obj))
                result = ta.reliesOnTemplateParameters(tparams);
        }

        override void visit(TraitsExp e)
        {
            //printf("TraitsExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            if (e.args)
            {
                foreach (oa; *e.args)
                {
                    if (auto ea = isExpression(oa))
                        ea.accept(this);
                    else if (auto ta = isType(oa))
                        result = ta.reliesOnTemplateParameters(tparams);
                    if (result)
                        return;
                }
            }
        }

        override void visit(IsExp e)
        {
            //printf("IsExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            result = e.targ.reliesOnTemplateParameters(tparams);
        }

        override void visit(UnaExp e)
        {
            //printf("UnaExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            e.e1.accept(this);
        }

        override void visit(DotTemplateInstanceExp e)
        {
            //printf("DotTemplateInstanceExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            visit(e.isUnaExp());
            if (!result && e.ti.tiargs)
            {
                foreach (oa; *e.ti.tiargs)
                {
                    if (auto ea = isExpression(oa))
                        ea.accept(this);
                    else if (auto ta = isType(oa))
                        result = ta.reliesOnTemplateParameters(tparams);
                    if (result)
                        return;
                }
            }
        }

        override void visit(CallExp e)
        {
            //printf("CallExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            visit(e.isUnaExp());
            if (!result && e.arguments)
            {
                foreach (ea; *e.arguments)
                {
                    ea.accept(this);
                    if (result)
                        return;
                }
            }
        }

        override void visit(CastExp e)
        {
            //printf("CallExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            visit(e.isUnaExp());
            // e.to can be null for cast() with no type
            if (!result && e.to)
                result = e.to.reliesOnTemplateParameters(tparams);
        }

        override void visit(SliceExp e)
        {
            //printf("SliceExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            visit(e.isUnaExp());
            if (!result && e.lwr)
                e.lwr.accept(this);
            if (!result && e.upr)
                e.upr.accept(this);
        }

        override void visit(IntervalExp e)
        {
            //printf("IntervalExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            e.lwr.accept(this);
            if (!result)
                e.upr.accept(this);
        }

        override void visit(ArrayExp e)
        {
            //printf("ArrayExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            visit(e.isUnaExp());
            if (!result && e.arguments)
            {
                foreach (ea; *e.arguments)
                    ea.accept(this);
            }
        }

        override void visit(BinExp e)
        {
            //printf("BinExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            e.e1.accept(this);
            if (!result)
                e.e2.accept(this);
        }

        override void visit(CondExp e)
        {
            //printf("BinExp.reliesOnTemplateParameters('%s')\n", e.toChars());
            e.econd.accept(this);
            if (!result)
                visit(e.isBinExp());
        }
    }

    scope ReliesOnTemplateParameters v = new ReliesOnTemplateParameters(tparams);
    e.accept(v);
    return v.result;
}

void computeOneMember(TemplateDeclaration td)
{
    if (td is null || td.haveComputedOneMember)
        return;

    if (td && td.members && td.ident)
    {
        Dsymbol s;
        if (oneMembers(td.members, s, td.ident) && s)
        {
            td.onemember = s;
            s.parent = td;
            td.computeIsTrivialAlias(s);
        }
        td.haveComputedOneMember = true;
    }
}

bool declareParameter(TemplateParameter _this, Scope* sc)
{
    static bool typeDeclareParameter(TemplateTypeParameter _this, Scope* sc)
    {
        //printf("TemplateTypeParameter.declareParameter('%s')\n", ident.toChars());
        auto ti = new TypeIdentifier(_this.loc, _this.ident);
        Declaration ad = new AliasDeclaration(_this.loc, _this.ident, ti);
        return sc.insert(ad) !is null;
    }

    static bool valueDeclareParameter(TemplateValueParameter _this, Scope* sc)
    {
        /*
            Do type semantic earlier.

            This means for certain erroneous value parameters
            their "type" can be known earlier and thus a better
            error message given.

            For example:
            `template test(x* x) {}`
            now yields "undefined identifier" rather than the opaque
            "variable `x` is used as a type".
         */
        if (_this.valType)
            _this.valType = _this.valType.typeSemantic(_this.loc, sc);
        auto v = new VarDeclaration(_this.loc, _this.valType, _this.ident, null);
        v.storage_class = STC.templateparameter;
        return sc.insert(v) !is null;
    }

    static bool aliasDeclareParameter(TemplateAliasParameter _this, Scope* sc)
    {
        auto ti = new TypeIdentifier(_this.loc, _this.ident);
        Declaration ad = new AliasDeclaration(_this.loc, _this.ident, ti);
        return sc.insert(ad) !is null;
    }

    static bool tupleDeclareParameter(TemplateTupleParameter _this, Scope* sc)
    {
        auto ti = new TypeIdentifier(_this.loc, _this.ident);
        Declaration ad = new AliasDeclaration(_this.loc, _this.ident, ti);
        return sc.insert(ad) !is null;
    }

    if (auto tp = _this.isTemplateTypeParameter())
        return typeDeclareParameter(tp, sc);
    else if (auto vp = _this.isTemplateValueParameter())
        return valueDeclareParameter(vp, sc);
    else if (auto ap = _this.isTemplateAliasParameter())
        return aliasDeclareParameter(ap, sc);
    else if (auto tup = _this.isTemplateTupleParameter())
        return tupleDeclareParameter(tup, sc);

    assert(0); // unreachable
}

/************************************
 * Return hash of Objects.
 */
private size_t arrayObjectHash(ref Objects oa1)
{
    import dmd.root.hash : mixHash;

    size_t hash = 0;
    foreach (o1; oa1)
    {
        /* Must follow the logic of match()
         */
        if (auto t1 = isType(o1))
            hash = mixHash(hash, cast(size_t)t1.deco);
        else if (auto e1 = getExpression(o1))
            hash = mixHash(hash, expressionHash(e1));
        else if (auto s1 = isDsymbol(o1))
        {
            if (auto fa1 = s1.isFuncAliasDeclaration())
                s1 = fa1.toAliasFunc();
            hash = mixHash(hash, mixHash(cast(size_t)cast(void*)s1.getIdent(), cast(size_t)cast(void*)s1.parent));
        }
        else if (auto u1 = isTuple(o1))
            hash = mixHash(hash, arrayObjectHash(u1.objects));
    }
    return hash;
}

/************************************
 * Computes hash of expression.
 * Handles all Expression classes and MUST match their equals method,
 * i.e. e1.equals(e2) implies expressionHash(e1) == expressionHash(e2).
 */
private size_t expressionHash(Expression e)
{
    import dmd.root.ctfloat : CTFloat;
    import dmd.root.hash : calcHash, mixHash;

    switch (e.op)
    {
    case EXP.int64:
        return cast(size_t) e.isIntegerExp().getInteger();

    case EXP.float64:
        return CTFloat.hash(e.isRealExp().value);

    case EXP.complex80:
        auto ce = e.isComplexExp();
        return mixHash(CTFloat.hash(ce.toReal), CTFloat.hash(ce.toImaginary));

    case EXP.identifier:
        return cast(size_t)cast(void*) e.isIdentifierExp().ident;

    case EXP.null_:
        return cast(size_t)cast(void*) e.isNullExp().type;

    case EXP.string_:
        return calcHash(e.isStringExp.peekData());

    case EXP.tuple:
    {
        auto te = e.isTupleExp();
        size_t hash = 0;
        hash += te.e0 ? expressionHash(te.e0) : 0;
        foreach (elem; *te.exps)
            hash = mixHash(hash, expressionHash(elem));
        return hash;
    }

    case EXP.arrayLiteral:
    {
        auto ae = e.isArrayLiteralExp();
        size_t hash;
        foreach (i; 0 .. ae.elements.length)
            hash = mixHash(hash, expressionHash(ae[i]));
        return hash;
    }

    case EXP.assocArrayLiteral:
    {
        auto ae = e.isAssocArrayLiteralExp();
        size_t hash;
        foreach (i; 0 .. ae.keys.length)
            // reduction needs associative op as keys are unsorted (use XOR)
            hash ^= mixHash(expressionHash((*ae.keys)[i]), expressionHash((*ae.values)[i]));
        return hash;
    }

    case EXP.structLiteral:
    {
        auto se = e.isStructLiteralExp();
        size_t hash;
        foreach (elem; *se.elements)
            hash = mixHash(hash, elem ? expressionHash(elem) : 0);
        return hash;
    }

    case EXP.variable:
        return cast(size_t)cast(void*) e.isVarExp().var;

    case EXP.function_:
        return cast(size_t)cast(void*) e.isFuncExp().fd;

    default:
        // no custom equals for this expression
        //assert((&e.equals).funcptr is &Expression.equals);
        // equals based on identity
        return cast(size_t)cast(void*) e;
    }
}

/************************************
 * This struct is needed for TemplateInstance to be the key in an associative array.
 * Fixing https://issues.dlang.org/show_bug.cgi?id=15813 would make it unnecessary.
 */
struct TemplateInstanceBox
{
    TemplateInstance ti;
    size_t hash; // cached result of toHash()

    this(TemplateInstance ti)
    {
        this.ti = ti;
        hash = cast(size_t)cast(void*)this.ti.enclosing;
        hash += arrayObjectHash(this.ti.tdtypes);
        hash += hash == 0;
    }

    size_t toHash() const @safe pure nothrow
    {
        assert(hash);
        return hash;
    }

    bool opEquals(ref const TemplateInstanceBox s) @trusted const
    {
        bool res = void;
        if (ti.inst && s.ti.inst)
        {
            /* This clause is only used when an instance with errors
             * is replaced with a correct instance.
             */
            res = ti is s.ti;
        }
        else
        {
            /* Used when a proposed instance is used to see if there's
             * an existing instance.
             */
            static if (__VERSION__ < 2099) // https://issues.dlang.org/show_bug.cgi?id=22717
                res = (cast()s.ti).equalsx(cast()ti);
            else
                res = (cast()ti).equalsx(cast()s.ti);
        }

        debug (FindExistingInstance) ++(res ? nHits : nCollisions);
        return res;
    }

    debug (FindExistingInstance)
    {
        __gshared uint nHits, nCollisions;

        shared static ~this()
        {
            printf("debug (FindExistingInstance) TemplateInstanceBox.equals hits: %u collisions: %u\n",
                   nHits, nCollisions);
        }
    }
}

/************************************
 * Perform semantic analysis on template.
 * Params:
 *      sc = context
 *      tempdecl = template declaration
 */
void templateDeclarationSemantic(Scope* sc, TemplateDeclaration tempdecl)
{
    enum log = false;
    static if (log)
    {
        printf("TemplateDeclaration.dsymbolSemantic(this = %p, id = '%s')\n", this, tempdecl.ident.toChars());
        printf("sc.stc = %llx\n", sc.stc);
        printf("sc.module = %s\n", sc._module.toChars());
    }
    if (tempdecl.semanticRun != PASS.initial)
        return; // semantic() already run

    if (tempdecl._scope)
    {
        sc = tempdecl._scope;
        tempdecl._scope = null;
    }
    if (!sc)
        return;

    import dmd.timetrace;
    timeTraceBeginEvent(TimeTraceEventType.sema1TemplateDecl);
    scope (exit) timeTraceEndEvent(TimeTraceEventType.sema1TemplateDecl, tempdecl);
    // Remember templates defined in module object that we need to know about
    if (sc._module && sc._module.ident == Id.object)
    {
        if (tempdecl.ident == Id.RTInfo)
            Type.rtinfo = tempdecl;
    }

    /* Remember Scope for later instantiations, but make
     * a copy since attributes can change.
     */
    if (!tempdecl._scope)
    {
        tempdecl._scope = sc.copy();
        tempdecl._scope.setNoFree();
    }

    tempdecl.semanticRun = PASS.semantic;

    tempdecl.parent = sc.parent;
    tempdecl.visibility = sc.visibility;
    tempdecl.userAttribDecl = sc.userAttribDecl;
    tempdecl.cppnamespace = sc.namespace;
    tempdecl.isstatic = tempdecl.toParent().isModule() || (tempdecl._scope.stc & STC.static_);
    tempdecl.deprecated_ = !!(sc.stc & STC.deprecated_);

    checkGNUABITag(tempdecl, sc.linkage);

    if (!tempdecl.isstatic)
    {
        if (auto ad = tempdecl.parent.pastMixin().isAggregateDeclaration())
            ad.makeNested();
    }

    // Set up scope for parameters
    auto paramsym = new ScopeDsymbol();
    paramsym.parent = tempdecl.parent;
    Scope* paramscope = sc.push(paramsym);
    paramscope.stc = STC.none;

    if (global.params.ddoc.doOutput)
    {
        tempdecl.origParameters = new TemplateParameters(tempdecl.parameters.length);
        for (size_t i = 0; i < tempdecl.parameters.length; i++)
        {
            TemplateParameter tp = (*tempdecl.parameters)[i];
            (*tempdecl.origParameters)[i] = tp.syntaxCopy();
        }
    }
    for (size_t i = 0; i < tempdecl.parameters.length; i++)
    {
        TemplateParameter tp = (*tempdecl.parameters)[i];
        if (!tp.declareParameter(paramscope))
        {
            error(tp.loc, "parameter `%s` multiply defined", tp.ident.toChars());
            tempdecl.errors = true;
        }
        if (!tp.tpsemantic(paramscope, tempdecl.parameters))
        {
            tempdecl.errors = true;
        }
        if (i + 1 != tempdecl.parameters.length && tp.isTemplateTupleParameter())
        {
            tempdecl.computeOneMember(); // for .kind
            .error(tempdecl.loc, "%s `%s` template sequence parameter must be the last one", tempdecl.kind, tempdecl.toPrettyChars);
            tempdecl.errors = true;
        }
    }

    /* Calculate TemplateParameter.dependent
     */
    auto tparams = TemplateParameters(1);
    for (size_t i = 0; i < tempdecl.parameters.length; i++)
    {
        TemplateParameter tp = (*tempdecl.parameters)[i];
        tparams[0] = tp;

        for (size_t j = 0; j < tempdecl.parameters.length; j++)
        {
            // Skip cases like: X(T : T)
            if (i == j)
                continue;

            if (TemplateTypeParameter ttp = (*tempdecl.parameters)[j].isTemplateTypeParameter())
            {
                if (reliesOnTident(ttp.specType, &tparams))
                    tp.dependent = true;
            }
            else if (TemplateAliasParameter tap = (*tempdecl.parameters)[j].isTemplateAliasParameter())
            {
                if (reliesOnTident(tap.specType, &tparams) ||
                    reliesOnTident(isType(tap.specAlias), &tparams))
                {
                    tp.dependent = true;
                }
            }
        }
    }

    paramscope.pop();

    // Compute again
    tempdecl.onemember = null;
    tempdecl.computeOneMember();
    /* BUG: should check:
     *  1. template functions must not introduce virtual functions, as they
     *     cannot be accomodated in the vtbl[]
     *  2. templates cannot introduce non-static data members (i.e. fields)
     *     as they would change the instance size of the aggregate.
     */

    tempdecl.semanticRun = PASS.semanticdone;
}

void templateInstanceSemantic(TemplateInstance tempinst, Scope* sc, ArgumentList argumentList)
{
    //printf("[%s] TemplateInstance.dsymbolSemantic('%s', this=%p, gag = %d, sc = %p)\n", tempinst.loc.toChars(), tempinst.toChars(), tempinst, global.gag, sc);
    version (none)
    {
        for (Dsymbol s = tempinst; s; s = s.parent)
        {
            printf("\t%s\n", s.toChars());
        }
        printf("Scope\n");
        for (Scope* scx = sc; scx; scx = scx.enclosing)
        {
            printf("\t%s parent %s\n", scx._module ? scx._module.toChars() : "null", scx.parent ? scx.parent.toChars() : "null");
        }
    }

    static if (LOG)
    {
        printf("\n+TemplateInstance.dsymbolSemantic('%s', this=%p)\n", tempinst.toChars(), tempinst);
    }
    if (tempinst.inst) // if semantic() was already run
    {
        static if (LOG)
        {
            printf("-TemplateInstance.dsymbolSemantic('%s', this=%p) already run\n",
                   tempinst.inst.toChars(), tempinst.inst);
        }
        return;
    }
    if (tempinst.semanticRun != PASS.initial)
    {
        static if (LOG)
        {
            printf("Recursive template expansion\n");
        }
        auto ungag = Ungag(global.gag);
        if (!tempinst.gagged)
            global.gag = 0;
        .error(tempinst.loc, "%s `%s` recursive template expansion", tempinst.kind, tempinst.toPrettyChars);
        if (tempinst.gagged)
            tempinst.semanticRun = PASS.initial;
        else
            tempinst.inst = tempinst;
        tempinst.errors = true;
        return;
    }

    timeTraceBeginEvent(TimeTraceEventType.sema1TemplateInstance);
    scope (exit) timeTraceEndEvent(TimeTraceEventType.sema1TemplateInstance, tempinst);

    // Get the enclosing template instance from the scope tinst
    tempinst.tinst = sc.tinst;

    // Get the instantiating module from the scope minst
    tempinst.minst = sc.minst;
    // https://issues.dlang.org/show_bug.cgi?id=10920
    // If the enclosing function is non-root symbol,
    // this instance should be speculative.
    if (!tempinst.tinst && sc.func && sc.func.inNonRoot())
    {
        tempinst.minst = null;
    }

    tempinst.gagged = (global.gag > 0);

    tempinst.semanticRun = PASS.semantic;

    static if (LOG)
    {
        printf("\tdo semantic\n");
    }
    /* Find template declaration first,
     * then run semantic on each argument (place results in tiargs[]),
     * last find most specialized template from overload list/set.
     */
    if (!tempinst.findTempDecl(sc, null) || !tempinst.semanticTiargs(sc) || !tempinst.findBestMatch(sc, argumentList))
    {
    Lerror:
        if (tempinst.gagged)
        {
            // https://issues.dlang.org/show_bug.cgi?id=13220
            // Roll back status for later semantic re-running
            tempinst.semanticRun = PASS.initial;
        }
        else
            tempinst.inst = tempinst;
        tempinst.errors = true;
        return;
    }
    TemplateDeclaration tempdecl = tempinst.tempdecl.isTemplateDeclaration();
    assert(tempdecl);

    if (global.params.v.templates)
        TemplateStats.incInstance(tempdecl, tempinst, global.params.v.templatesListInstances);

    tempdecl.checkDeprecated(tempinst.loc, sc);

    // If tempdecl is a mixin, disallow it
    if (tempdecl.ismixin)
    {
        .error(tempinst.loc, "%s `%s` mixin templates are not regular templates", tempinst.kind, tempinst.toPrettyChars);
        goto Lerror;
    }

    tempinst.hasNestedArgs(tempinst.tiargs, tempdecl.isstatic);
    if (tempinst.errors)
        goto Lerror;

    // Copy the tempdecl namespace (not the scope one)
    tempinst.cppnamespace = tempdecl.cppnamespace;
    if (tempinst.cppnamespace)
        tempinst.cppnamespace.dsymbolSemantic(sc);

    /* Greatly simplified semantic processing for AliasSeq templates
     */
    if (tempdecl.isTrivialAliasSeq)
    {
        tempinst.inst = tempinst;
        return aliasSeqInstanceSemantic(tempinst, sc, tempdecl);
    }

    /* Greatly simplified semantic processing for Alias templates
     */
    else if (tempdecl.isTrivialAlias)
    {
        tempinst.inst = tempinst;
        return aliasInstanceSemantic(tempinst, sc, tempdecl);
    }


    /* See if there is an existing TemplateInstantiation that already
     * implements the typeargs. If so, just refer to that one instead.
     */
    tempinst.inst = tempdecl.findExistingInstance(tempinst, argumentList);
    TemplateInstance errinst = null;
    if (!tempinst.inst)
    {
        // So, we need to implement 'this' instance.
    }
    else if (tempinst.inst.gagged && !tempinst.gagged && tempinst.inst.errors)
    {
        // If the first instantiation had failed, re-run semantic,
        // so that error messages are shown.
        errinst = tempinst.inst;
    }
    else
    {
        // It's a match
        tempinst.parent = tempinst.inst.parent;
        tempinst.errors = tempinst.inst.errors;

        // If both this and the previous instantiation were gagged,
        // use the number of errors that happened last time.
        global.errors += tempinst.errors;
        global.gaggedErrors += tempinst.errors;

        // If the first instantiation was gagged, but this is not:
        if (tempinst.inst.gagged)
        {
            // It had succeeded, mark it is a non-gagged instantiation,
            // and reuse it.
            tempinst.inst.gagged = tempinst.gagged;
        }

        tempinst.tnext = tempinst.inst.tnext;
        tempinst.inst.tnext = tempinst;

        /* A module can have explicit template instance and its alias
         * in module scope (e,g, `alias Base64 = Base64Impl!('+', '/');`).
         * If the first instantiation 'inst' had happened in non-root module,
         * compiler can assume that its instantiated code would be included
         * in the separately compiled obj/lib file (e.g. phobos.lib).
         *
         * However, if 'this' second instantiation happened in root module,
         * compiler might need to invoke its codegen
         * (https://issues.dlang.org/show_bug.cgi?id=2500 & https://issues.dlang.org/show_bug.cgi?id=2644).
         * But whole import graph is not determined until all semantic pass finished,
         * so 'inst' should conservatively finish the semantic3 pass for the codegen.
         */
        if (tempinst.minst && tempinst.minst.isRoot() && !(tempinst.inst.minst && tempinst.inst.minst.isRoot()))
        {
            /* Swap the position of 'inst' and 'this' in the instantiation graph.
             * Then, the primary instance `inst` will be changed to a root instance,
             * along with all members of `inst` having their scopes updated.
             *
             * Before:
             *  non-root -> A!() -> B!()[inst] -> C!() { members[non-root] }
             *                      |
             *  root     -> D!() -> B!()[this]
             *
             * After:
             *  non-root -> A!() -> B!()[this]
             *                      |
             *  root     -> D!() -> B!()[inst] -> C!() { members[root] }
             */
            Module mi = tempinst.minst;
            TemplateInstance ti = tempinst.tinst;
            tempinst.minst = tempinst.inst.minst;
            tempinst.tinst = tempinst.inst.tinst;
            tempinst.inst.minst = mi;
            tempinst.inst.tinst = ti;

            /* https://issues.dlang.org/show_bug.cgi?id=21299
               `minst` has been updated on the primary instance `inst` so it is
               now coming from a root module, however all Dsymbol `inst.members`
               of the instance still have their `_scope.minst` pointing at the
               original non-root module. We must now propagate `minst` to all
               members so that forward referenced dependencies that get
               instantiated will also be appended to the root module, otherwise
               there will be undefined references at link-time.  */
            extern (C++) final class InstMemberWalker : Visitor
            {
                alias visit = Visitor.visit;
                TemplateInstance inst;

                extern (D) this(TemplateInstance inst) scope @safe
                {
                    this.inst = inst;
                }

                override void visit(Dsymbol d)
                {
                    if (d._scope)
                        d._scope.minst = inst.minst;
                }

                override void visit(ScopeDsymbol sds)
                {
                    sds.members.foreachDsymbol( s => s.accept(this) );
                    visit(cast(Dsymbol)sds);
                }

                override void visit(StructDeclaration sd)
                {
                    // need to visit auto-generated methods as well
                    if (sd.xeq) visit(sd.xeq);
                    if (sd.xcmp) visit(sd.xcmp);
                    if (sd.xhash) visit(sd.xhash);
                    visit(cast(ScopeDsymbol)sd);
                }

                override void visit(AttribDeclaration ad)
                {
                    ad.include(null).foreachDsymbol( s => s.accept(this) );
                    visit(cast(Dsymbol)ad);
                }

                override void visit(ConditionalDeclaration cd)
                {
                    if (cd.condition.inc)
                        visit(cast(AttribDeclaration)cd);
                    else
                        visit(cast(Dsymbol)cd);
                }
            }
            scope v = new InstMemberWalker(tempinst.inst);
            tempinst.inst.accept(v);

            if (!global.params.allInst &&
                tempinst.minst) // if inst was not speculative...
            {
                assert(!tempinst.minst.isRoot()); // ... it was previously appended to a non-root module
                // Append again to the root module members[], so that the instance will
                // get codegen chances (depending on `tempinst.inst.needsCodegen()`).
                tempinst.inst.appendToModuleMember();
            }

            assert(tempinst.inst.memberOf && tempinst.inst.memberOf.isRoot(), "no codegen chances");
        }

        // modules imported by an existing instance should be added to the module
        // that instantiates the instance.
        if (tempinst.minst)
            foreach(imp; tempinst.inst.importedModules)
                if (!tempinst.minst.aimports.contains(imp))
                    tempinst.minst.aimports.push(imp);

        static if (LOG)
        {
            printf("\tit's a match with instance %p, %d\n", tempinst.inst, tempinst.inst.semanticRun);
        }
        return;
    }
    static if (LOG)
    {
        printf("\timplement template instance %s '%s'\n", tempdecl.parent.toChars(), tempinst.toChars());
        printf("\ttempdecl %s\n", tempdecl.toChars());
    }
    const errorsave = global.errors;

    tempinst.inst = tempinst;
    tempinst.parent = tempinst.enclosing ? tempinst.enclosing : tempdecl.parent;
    //printf("parent = '%s'\n", parent.kind());

    if (global.params.v.templates)
        TemplateStats.incUnique(tempdecl, tempinst);

    TemplateInstance tempdecl_instance_idx = tempdecl.addInstance(tempinst);

    //getIdent();

    // Store the place we added it to in target_symbol_list(_idx) so we can
    // remove it later if we encounter an error.
    Dsymbols* target_symbol_list = tempinst.appendToModuleMember();
    size_t target_symbol_list_idx = target_symbol_list ? target_symbol_list.length - 1 : 0;

    // Copy the syntax trees from the TemplateDeclaration
    tempinst.members = Dsymbol.arraySyntaxCopy(tempdecl.members);

    // resolve TemplateThisParameter
    for (size_t i = 0; i < tempdecl.parameters.length; i++)
    {
        if ((*tempdecl.parameters)[i].isTemplateThisParameter() is null)
            continue;
        Type t = isType((*tempinst.tiargs)[i]);
        assert(t);
        if (STC stc = ModToStc(t.mod))
        {
            //printf("t = %s, stc = x%llx\n", t.toChars(), stc);
            auto s = new Dsymbols(new StorageClassDeclaration(stc, tempinst.members));
            tempinst.members = s;
        }
        break;
    }

    // Create our own scope for the template parameters
    Scope* _scope = tempdecl._scope;
    if (tempdecl.semanticRun == PASS.initial)
    {
        .error(tempinst.loc, "%s `%s` template instantiation `%s` forward references template declaration `%s`",
           tempinst.kind, tempinst.toPrettyChars, tempinst.toChars(), tempdecl.toChars());
        return;
    }

    static if (LOG)
    {
        printf("\tcreate scope for template parameters '%s'\n", tempinst.toChars());
    }
    tempinst.argsym = new ScopeDsymbol();
    tempinst.argsym.parent = _scope.parent;
    _scope = _scope.push(tempinst.argsym);
    _scope.tinst = tempinst;
    _scope.minst = tempinst.minst;
    //scope.stc = STC.none;

    if (sc.isKnownToHaveACompileTimeContext)
        _scope.knownACompileTimeOnlyContext = true;

    // Declare each template parameter as an alias for the argument type
    Scope* paramscope = _scope.push();
    paramscope.stc = STC.none;
    paramscope.visibility = Visibility(Visibility.Kind.public_); // https://issues.dlang.org/show_bug.cgi?id=14169
                                              // template parameters should be public
    tempinst.declareParameters(paramscope);
    paramscope.pop();

    // Add members of template instance to template instance symbol table
    //parent = scope.scopesym;
    tempinst.symtab = new DsymbolTable();

    tempinst.members.foreachDsymbol( (s)
    {
        static if (LOG)
        {
            printf("\t adding member '%s' %p kind %s to '%s'\n", s.toChars(), s, s.kind(), tempinst.toChars());
        }
        s.addMember(_scope, tempinst);
    });

    static if (LOG)
    {
        printf("adding members done\n");
    }

    /* See if there is only one member of template instance, and that
     * member has the same name as the template instance.
     * If so, this template instance becomes an alias for that member.
     */
    //printf("members.length = %d\n", tempinst.members.length);
    if (tempinst.members.length)
    {
        Dsymbol s;
        if (oneMembers(tempinst.members, s, tempdecl.ident) && s)
        {
            //printf("tempdecl.ident = %s, s = `%s %s`\n", tempdecl.ident.toChars(), s.kind(), s.toPrettyChars());
            //printf("setting aliasdecl\n");
            tempinst.aliasdecl = s;
        }
    }

    /* If function template declaration
     */
    if (argumentList.length > 0 && tempinst.aliasdecl)
    {
        if (auto fd = tempinst.aliasdecl.isFuncDeclaration())
        {
            /* Transmit fargs to type so that TypeFunction.dsymbolSemantic() can
             * resolve any "auto ref" storage classes.
             */
            if (fd.type)
                if (auto tf = fd.type.isTypeFunction())
                    tf.inferenceArguments = argumentList;
        }
    }

    // Do semantic() analysis on template instance members
    static if (LOG)
    {
        printf("\tdo semantic() on template instance members '%s'\n", tempinst.toChars());
    }
    Scope* sc2;
    sc2 = _scope.push(tempinst);
    //printf("enclosing = %d, sc.parent = %s\n", tempinst.enclosing, sc.parent.toChars());
    sc2.parent = tempinst;
    sc2.tinst = tempinst;
    sc2.minst = tempinst.minst;
    sc2.stc &= ~STC.deprecated_;
    tempinst.tryExpandMembers(sc2);

    tempinst.semanticRun = PASS.semanticdone;

    /* ConditionalDeclaration may introduce eponymous declaration,
     * so we should find it once again after semantic.
     */
    if (tempinst.members.length)
    {
        Dsymbol s;
        if (oneMembers(tempinst.members, s, tempdecl.ident) && s)
        {
            if (!tempinst.aliasdecl || tempinst.aliasdecl != s)
            {
                //printf("tempdecl.ident = %s, s = `%s %s`\n", tempdecl.ident.toChars(), s.kind(), s.toPrettyChars());
                //printf("setting aliasdecl 2\n");
                tempinst.aliasdecl = s;
            }
        }
    }

    if (global.errors != errorsave)
        goto Laftersemantic;

    /* If any of the instantiation members didn't get semantic() run
     * on them due to forward references, we cannot run semantic2()
     * or semantic3() yet.
     */
    {
        bool found_deferred_ad = false;
        for (size_t i = 0; i < Module.deferred.length; i++)
        {
            Dsymbol sd = Module.deferred[i];
            AggregateDeclaration ad = sd.isAggregateDeclaration();
            if (ad && ad.parent && ad.parent.isTemplateInstance())
            {
                //printf("deferred template aggregate: %s %s\n",
                //        sd.parent.toChars(), sd.toChars());
                found_deferred_ad = true;
                if (ad.parent == tempinst)
                {
                    ad.deferred = tempinst;
                    break;
                }
            }
        }
        if (found_deferred_ad || Module.deferred.length)
            goto Laftersemantic;
    }

    /* The problem is when to parse the initializer for a variable.
     * Perhaps VarDeclaration.dsymbolSemantic() should do it like it does
     * for initializers inside a function.
     */
    //if (sc.parent.isFuncDeclaration())
    {
        /* https://issues.dlang.org/show_bug.cgi?id=782
         * this has problems if the classes this depends on
         * are forward referenced. Find a way to defer semantic()
         * on this template.
         */
        tempinst.semantic2(sc2);
    }
    if (global.errors != errorsave)
        goto Laftersemantic;

    if ((sc.func || sc.fullinst) && !tempinst.tinst)
    {
        /* If a template is instantiated inside function, the whole instantiation
         * should be done at that position. But, immediate running semantic3 of
         * dependent templates may cause unresolved forward reference.
         * https://issues.dlang.org/show_bug.cgi?id=9050
         * To avoid the issue, don't run semantic3 until semantic and semantic2 done.
         */
        TemplateInstances deferred;
        tempinst.deferred = &deferred;

        //printf("Run semantic3 on %s\n", toChars());

        /* https://issues.dlang.org/show_bug.cgi?id=23965
         * DRuntime hooks are not deprecated, but may be used for deprecated
         * types. Deprecations are disabled while analysing hooks to avoid
         * spurious error messages.
         */
        auto saveUseDeprecated = global.params.useDeprecated;
        if (sc.isDeprecated() && isDRuntimeHook(tempinst.name))
            global.params.useDeprecated = DiagnosticReporting.off;

        tempinst.trySemantic3(sc2);

        global.params.useDeprecated = saveUseDeprecated;

        for (size_t i = 0; i < deferred.length; i++)
        {
            //printf("+ run deferred semantic3 on %s\n", deferred[i].toChars());
            deferred[i].semantic3(null);
        }

        tempinst.deferred = null;
    }
    else if (tempinst.tinst)
    {
        bool doSemantic3 = false;
        FuncDeclaration fd;
        if (tempinst.aliasdecl)
            fd = tempinst.aliasdecl.toAlias2().isFuncDeclaration();

        if (fd)
        {
            /* Template function instantiation should run semantic3 immediately
             * for attribute inference.
             */
            scope fld = fd.isFuncLiteralDeclaration();
            if (fld && fld.tok == TOK.reserved)
                doSemantic3 = true;
            else if (sc.func)
                doSemantic3 = true;
        }
        else if (sc.func)
        {
            /* A lambda function in template arguments might capture the
             * instantiated scope context. For the correct context inference,
             * all instantiated functions should run the semantic3 immediately.
             * See also compilable/test14973.d
             */
            foreach (oarg; tempinst.tdtypes)
            {
                auto s = getDsymbol(oarg);
                if (!s)
                    continue;

                if (auto td = s.isTemplateDeclaration())
                {
                    if (!td.literal)
                        continue;
                    assert(td.members && td.members.length == 1);
                    s = (*td.members)[0];
                }
                if (auto fld = s.isFuncLiteralDeclaration())
                {
                    if (fld.tok == TOK.reserved)
                    {
                        doSemantic3 = true;
                        break;
                    }
                }
            }
            //printf("[%s] %s doSemantic3 = %d\n", tempinst.tinst.loc.toChars(), tempinst.tinst.toChars(), doSemantic3);
        }
        if (doSemantic3)
            tempinst.trySemantic3(sc2);

        TemplateInstance ti = tempinst.tinst;
        int nest = 0;
        while (ti && !ti.deferred && ti.tinst)
        {
            ti = ti.tinst;
            if (++nest > global.recursionLimit)
            {
                global.gag = 0; // ensure error message gets printed
                .error(tempinst.loc, "%s `%s` recursive expansion", tempinst.kind, tempinst.toPrettyChars);
                fatal();
            }
        }
        if (ti && ti.deferred)
        {
            //printf("deferred semantic3 of %p %s, ti = %s, ti.deferred = %p\n", this, toChars(), ti.toChars());
            for (size_t i = 0;; i++)
            {
                if (i == ti.deferred.length)
                {
                    ti.deferred.push(tempinst);
                    break;
                }
                if ((*ti.deferred)[i] == tempinst)
                    break;
            }
        }
    }

    if (tempinst.aliasdecl)
    {
        /* https://issues.dlang.org/show_bug.cgi?id=13816
         * AliasDeclaration tries to resolve forward reference
         * twice (See inuse check in AliasDeclaration.toAlias()). It's
         * necessary to resolve mutual references of instantiated symbols, but
         * it will left a true recursive alias in tuple declaration - an
         * AliasDeclaration A refers TupleDeclaration B, and B contains A
         * in its elements.  To correctly make it an error, we strictly need to
         * resolve the alias of eponymous member.
         */
        tempinst.aliasdecl = tempinst.aliasdecl.toAlias2();

        // stop AliasAssign tuple building
        if (auto td = tempinst.aliasdecl.isTupleDeclaration())
            td.building = false;
    }

Laftersemantic:
    sc2.pop();
    _scope.pop();

    // Give additional context info if error occurred during instantiation
    if (global.errors != errorsave)
    {
        if (!tempinst.errors)
        {
            if (!tempdecl.literal)
                .error(tempinst.loc, "%s `%s` error instantiating", tempinst.kind, tempinst.toPrettyChars);
            if (tempinst.tinst)
                tempinst.tinst.printInstantiationTrace();
        }
        tempinst.errors = true;
        if (tempinst.gagged)
        {
            // Errors are gagged, so remove the template instance from the
            // instance/symbol lists we added it to and reset our state to
            // finish clean and so we can try to instantiate it again later
            // (see https://issues.dlang.org/show_bug.cgi?id=4302 and https://issues.dlang.org/show_bug.cgi?id=6602).
            tempdecl.removeInstance(tempdecl_instance_idx);
            if (target_symbol_list)
            {
                // Because we added 'this' in the last position above, we
                // should be able to remove it without messing other indices up.
                assert((*target_symbol_list)[target_symbol_list_idx] == tempinst);
                target_symbol_list.remove(target_symbol_list_idx);
                tempinst.memberOf = null;                    // no longer a member
            }
            tempinst.semanticRun = PASS.initial;
            tempinst.inst = null;
            tempinst.symtab = null;
        }
    }
    else if (errinst)
    {
        /* https://issues.dlang.org/show_bug.cgi?id=14541
         * If the previous gagged instance had failed by
         * circular references, currrent "error reproduction instantiation"
         * might succeed, because of the difference of instantiated context.
         * On such case, the cached error instance needs to be overridden by the
         * succeeded instance.
         */
        //printf("replaceInstance()\n");
        assert(errinst.errors);
        auto ti1 = TemplateInstanceBox(errinst);
        (cast(TemplateInstance[TemplateInstanceBox])tempdecl.instances).remove(ti1);

        auto ti2 = TemplateInstanceBox(tempinst);
        (*(cast(TemplateInstance[TemplateInstanceBox]*) &tempdecl.instances))[ti2] = tempinst;
    }

    static if (LOG)
    {
        printf("-TemplateInstance.dsymbolSemantic('%s', this=%p)\n", tempinst.toChars(), tempinst);
    }
}

/*****************************************
 * Determines if a TemplateInstance will need a nested
 * generation of the TemplateDeclaration.
 * Sets enclosing property if so, and returns != 0;
 */
private bool hasNestedArgs(TemplateInstance _this, Objects* args, bool isstatic)
{
    int nested = 0;
    //printf("TemplateInstance.hasNestedArgs('%s')\n", tempdecl.ident.toChars());

    // arguments from parent instances are also accessible
    if (!_this.enclosing)
    {
        if (TemplateInstance ti = _this.tempdecl.toParent().isTemplateInstance())
            _this.enclosing = ti.enclosing;
    }

    /* Search for the most deeply nested of `dparent` and `enclosing` assigning
     * `dparent` to `enclosing` if `dparent` is more nested than `enclosing`.
     *
     * Returns:
     *  `true` if an error should be reported
     */
    static bool search(Dsymbol dparent, ref Dsymbol enclosing)
    {
        if (!dparent || dparent.isModule)
            return false;
        if (!enclosing)
        {
            enclosing = dparent;
            return false;
        }
        if (enclosing == dparent)
            return false;

        /* Select the more deeply nested of the two.
         * Error if one is not nested inside the other.
         */
        for (Dsymbol p = enclosing; p; p = p.parent)
        {
            if (p == dparent)
                return false; // enclosing is most nested
        }
        for (Dsymbol p = dparent; p; p = p.parent)
        {
            if (p == enclosing)
            {
                enclosing = dparent;
                return false; // dparent is most nested
            }
        }
        //https://issues.dlang.org/show_bug.cgi?id=17870
        auto pc = dparent.isClassDeclaration();
        auto ec = enclosing.isClassDeclaration();
        if (pc && ec)
        {
            if (pc.isBaseOf(ec, null))
                return false;
            else if (ec.isBaseOf(pc, null))
            {
                enclosing = dparent;
                return false;
            }
        }
        return true;
    }
    int search2(Dsymbol sa)
    {
        Dsymbol dparent = sa.toParent2();
        if (search(dparent, _this.enclosing))
        {
            .error(_this.loc, "%s `%s` `%s` is nested in both `%s` and `%s`",
                   _this.kind, _this.toPrettyChars(), _this.toChars(),
                   _this.enclosing.toChars(), dparent.toChars());
            _this.errors = true;
        }
        //printf("\tnested inside %s as it references %s\n", enclosing.toChars(), sa.toChars());
        return 1;
    }
    int dsym(Dsymbol sa)
    {
        sa = sa.toAlias();
        TemplateDeclaration td = sa.isTemplateDeclaration();
        if (td)
        {
            TemplateInstance ti = sa.toParent().isTemplateInstance();
            if (ti && ti.enclosing)
                sa = ti;
        }
        TemplateInstance ti = sa.isTemplateInstance();
        Declaration d = sa.isDeclaration();
        if (td && td.literal)
            return search2(sa);
        if (ti && ti.enclosing)
            return search2(sa);
        if (d && !d.isDataseg()
              && !(d.storage_class & STC.manifest)
              && (!d.isFuncDeclaration() || d.isFuncDeclaration().isNested())
              && !_this.isTemplateMixin())
        {
            return search2(sa);
        }
        return 0;
    }
    /* A nested instance happens when an argument references a local
     * symbol that is on the stack.
     */
    foreach (o; *args)
    {
        if (Dsymbol sa = isDsymbol(o))
        {
            nested |= dsym(sa);
            continue;
        }
        else if (Tuple va = isTuple(o))
        {
            nested |= cast(int)_this.hasNestedArgs(&va.objects, isstatic);
            continue;
        }
        Expression ea = isExpression(o);
        if (!ea)
            continue;

        if (auto ve = ea.isVarExp())
        {
            nested |= dsym(ve.var);
            continue;
        }
        if (auto te = ea.isThisExp())
        {
            nested |= dsym(te.var);
            continue;
        }
        if (auto fe = ea.isFuncExp())
        {
            nested |= dsym(fe.td? fe.td : fe.fd);
            continue;
        }
        // Emulate Expression.toMangleBuffer call that had exist in TemplateInstance.genIdent.
        if (ea.op != EXP.int64 && ea.op != EXP.float64 && ea.op != EXP.complex80 && ea.op != EXP.null_ && ea.op != EXP.string_ && ea.op != EXP.arrayLiteral && ea.op != EXP.assocArrayLiteral && ea.op != EXP.structLiteral)
        {
            if (!ea.type.isTypeError())
                .error(ea.loc, "%s `%s` expression `%s` is not a valid template value argument", _this.kind, _this.toPrettyChars, ea.toChars());
            _this.errors = true;
        }
    }
    //printf("-TemplateInstance.hasNestedArgs('%s') = %d\n", tempdecl.ident.toChars(), nested);
    return nested != 0;
}

/// Pair of MATCHes
private struct MATCHpair
{
    MATCH mta;  /// match template parameters by initial template arguments
    MATCH mfa;  /// match template parameters by inferred template arguments

    debug this(MATCH mta, MATCH mfa)
    {
        assert(MATCH.min <= mta && mta <= MATCH.max);
        assert(MATCH.min <= mfa && mfa <= MATCH.max);
        this.mta = mta;
        this.mfa = mfa;
    }
}

/*****************************************
 * Append `ti` to the specific module `ti.members[]`
 */
private Dsymbols* appendToModuleMember(TemplateInstance ti)
{
    Module mi = ti.minst; // instantiated . inserted module

    //printf("%s.appendToModuleMember() enclosing = %s mi = %s\n",
    //    toPrettyChars(),
    //    enclosing ? enclosing.toPrettyChars() : null,
    //    mi ? mi.toPrettyChars() : null);
    if (global.params.allInst || !mi || mi.isRoot())
    {
        /* If the instantiated module is speculative or root, insert to the
         * member of a root module. Then:
         *  - semantic3 pass will get called on the instance members.
         *  - codegen pass will get a selection chance to do/skip it (needsCodegen()).
         */
        static Dsymbol getStrictEnclosing(TemplateInstance ti)
        {
            do
            {
                if (ti.enclosing)
                    return ti.enclosing;
                ti = ti.tempdecl.isInstantiated();
            } while (ti);
            return null;
        }

        Dsymbol enc = getStrictEnclosing(ti);
        // insert target is made stable by using the module
        // where tempdecl is declared.
        mi = (enc ? enc : ti.tempdecl).getModule();
        if (!mi.isRoot())
        {
            if (mi.importedFrom)
            {
                mi = mi.importedFrom;
                assert(mi.isRoot());
            }
            else
            {
                // This can happen when using the frontend as a library.
                // Append it to the non-root module.
            }
        }
    }
    else
    {
        /* If the instantiated module is non-root, insert to the member of the
         * non-root module. Then:
         *  - semantic3 pass won't be called on the instance.
         *  - codegen pass won't reach to the instance.
         * Unless it is re-appended to a root module later (with changed minst).
         */
    }
    //printf("\t-. mi = %s\n", mi.toPrettyChars());

    if (ti.memberOf) // already appended to some module
    {
        assert(mi.isRoot(), "can only re-append to a root module");
        if (ti.memberOf.isRoot())
            return null; // no need to move to another root module
    }

    Dsymbols* a = mi.members;
    a.push(ti);
    ti.memberOf = mi;
    if (mi.semanticRun >= PASS.semantic2done && mi.isRoot())
        addDeferredSemantic2(ti);
    if (mi.semanticRun >= PASS.semantic3done && mi.isRoot())
        addDeferredSemantic3(ti);
    return a;
}

private void expandMembers(TemplateInstance ti,Scope* sc2)
{
    ti.members.foreachDsymbol( (s) { s.setScope (sc2); } );

    ti.members.foreachDsymbol( (s) { s.importAll(sc2); } );

    if (!ti.aliasdecl)
    {
        /* static if's are crucial to evaluating aliasdecl correctly. But
         * evaluating the if/else bodies may require aliasdecl.
         * So, evaluate the condition for static if's, but not their if/else bodies.
         * Then try to set aliasdecl.
         * Later do the if/else bodies.
         * https://issues.dlang.org/show_bug.cgi?id=23598
         * It might be better to do this by attaching a lambda to the StaticIfDeclaration
         * to do the oneMembers call after the sid.include(sc2) is run as part of dsymbolSemantic().
         */
        bool done;
        void staticIfDg(Dsymbol s)
        {
            if (done || ti.aliasdecl)
                return;
            //printf("\t staticIfDg on '%s %s' in '%s'\n",  s.kind(), s.toChars(), this.toChars());
            if (!s.isStaticIfDeclaration())
            {
                //s.dsymbolSemantic(sc2);
                done = true;
                return;
            }
            auto sid = s.isStaticIfDeclaration();
            sid.include(sc2);
            if (ti.members.length)
            {
                Dsymbol sa;
                if (oneMembers(ti.members, sa, ti.tempdecl.ident) && sa)
                    ti.aliasdecl = sa;
            }
            done = true;
        }

        ti.members.foreachDsymbol(&staticIfDg);
    }

    void symbolDg(Dsymbol s)
    {
        //printf("\t semantic on '%s' %p kind %s in '%s'\n",  s.toChars(), s, s.kind(), this.toChars());
        //printf("test: enclosing = %d, sc2.parent = %s\n", enclosing, sc2.parent.toChars());
        //if (enclosing)
        //    s.parent = sc.parent;
        //printf("test3: enclosing = %d, s.parent = %s\n", enclosing, s.parent.toChars());
        s.dsymbolSemantic(sc2);
        //printf("test4: enclosing = %d, s.parent = %s\n", enclosing, s.parent.toChars());
        runDeferredSemantic();
    }

    ti.members.foreachDsymbol(&symbolDg);
}

private void tryExpandMembers(TemplateInstance ti, Scope* sc2)
{
    __gshared int nest;
    // extracted to a function to allow windows SEH to work without destructors in the same function
    //printf("%d\n", nest);
    if (++nest > global.recursionLimit)
    {
        global.gag = 0; // ensure error message gets printed
        .error(ti.loc, "%s `%s` recursive expansion exceeded allowed nesting limit", ti.kind, ti.toPrettyChars);
        fatal();
    }

    ti.expandMembers(sc2);

    nest--;
}

private void trySemantic3(TemplateInstance ti, Scope* sc2)
{
    // extracted to a function to allow windows SEH to work without destructors in the same function
    __gshared int nest;
    //printf("%d\n", nest);
    if (++nest > global.recursionLimit)
    {
        global.gag = 0; // ensure error message gets printed
        .error(ti.loc, "%s `%s` recursive expansion exceeded allowed nesting limit", ti.kind, ti.toPrettyChars);
        fatal();
    }

    semantic3(ti, sc2);

    --nest;
}

debug (FindExistingInstance)
{
    private __gshared uint nFound, nNotFound, nAdded, nRemoved;

    shared static ~this()
    {
        printf("debug (FindExistingInstance) nFound %u, nNotFound: %u, nAdded: %u, nRemoved: %u\n",
               nFound, nNotFound, nAdded, nRemoved);
    }
}

/******************************************************
 * Do template instance semantic for isAlias templates.
 * This is a greatly simplified version of templateInstanceSemantic().
 */
private
void aliasInstanceSemantic(TemplateInstance tempinst, Scope* sc, TemplateDeclaration tempdecl)
{
    //printf("[%s] aliasInstance.dsymbolSemantic('%s')\n", tempinst.loc.toChars(), tempinst.toChars());
    Scope* paramscope = sc.push();
    paramscope.stc = STC.none;
    paramscope.visibility = Visibility(Visibility.Kind.public_);

    TemplateTypeParameter ttp = (*tempdecl.parameters)[0].isTemplateTypeParameter();
    Type ta = tempinst.tdtypes[0].isType();
    tempdecl.computeOneMember();
    auto ad = tempdecl.onemember.isAliasDeclaration();

    // Note: qualifiers can be in both 'ad.type.mod' and 'ad.storage_class'
    Declaration d = new AliasDeclaration(tempinst.loc, ttp.ident, ta.addMod(ad.type.mod));
    d.storage_class |= STC.templateparameter | ad.storage_class;
    d.dsymbolSemantic(sc);

    paramscope.pop();

    tempinst.aliasdecl = d;

    tempinst.semanticRun = PASS.semanticdone;
}

/****************************************************
 * Given a new instance `tithis` of this TemplateDeclaration,
 * see if there already exists an instance.
 *
 * Params:
 *   td = template declaration
 *   tithis = template instance to check
 *   argumentList = For function templates, needed because different
 *                  `auto ref` resolutions create different instances,
 *                  even when template parameters are identical
 *
 * Returns: that existing instance, or `null` when it doesn't exist
 */
private TemplateInstance findExistingInstance(TemplateDeclaration td, TemplateInstance tithis,
                                      ArgumentList argumentList)
{
    //printf("findExistingInstance() %s\n", tithis.toChars());
    tithis.fargs = argumentList.arguments;
    tithis.fnames = argumentList.names;
    auto tibox = TemplateInstanceBox(tithis);
    auto p = tibox in cast(TemplateInstance[TemplateInstanceBox]) td.instances;
    debug (FindExistingInstance) ++(p ? nFound : nNotFound);
    //if (p) printf("\tfound %p\n", *p); else printf("\tnot found\n");
    return p ? *p : null;
}

/********************************************
 * Add instance ti to TemplateDeclaration's table of instances.
 * Return a handle we can use to later remove it if it fails instantiation.
 */
private TemplateInstance addInstance(TemplateDeclaration td, TemplateInstance ti)
{
    //printf("addInstance() %p %s\n", instances, ti.toChars());
    auto tibox = TemplateInstanceBox(ti);
    (*(cast(TemplateInstance[TemplateInstanceBox]*) &td.instances))[tibox] = ti;
    debug (FindExistingInstance) ++nAdded;
    return ti;
}

/*******************************************
 * Remove TemplateInstance from table of instances.
 * Input:
 *      handle returned by addInstance()
 */
private void removeInstance(TemplateDeclaration td, TemplateInstance ti)
{
    //printf("removeInstance() %s\n", ti.toChars());
    auto tibox = TemplateInstanceBox(ti);
    debug (FindExistingInstance) ++nRemoved;
    (cast(TemplateInstance[TemplateInstanceBox])td.instances).remove(tibox);
}

/******************************************************
 * Do template instance semantic for isAliasSeq templates.
 * This is a greatly simplified version of templateInstanceSemantic().
 */
private
void aliasSeqInstanceSemantic(TemplateInstance tempinst, Scope* sc, TemplateDeclaration tempdecl)
{
    //printf("[%s] aliasSeqInstance.dsymbolSemantic('%s')\n", tempinst.loc.toChars(), tempinst.toChars());
    Scope* paramscope = sc.push();
    paramscope.stc = STC.none;
    paramscope.visibility = Visibility(Visibility.Kind.public_);

    TemplateTupleParameter ttp = (*tempdecl.parameters)[0].isTemplateTupleParameter();
    Tuple va = tempinst.tdtypes[0].isTuple();
    Declaration d = new TupleDeclaration(tempinst.loc, ttp.ident, &va.objects);
    d.storage_class |= STC.templateparameter;
    d.dsymbolSemantic(sc);

    paramscope.pop();

    tempinst.aliasdecl = d;

    tempinst.semanticRun = PASS.semanticdone;
}

/**********************************************
 * Find template declaration corresponding to template instance.
 *
 * Returns:
 *      false if finding fails.
 * Note:
 *      This function is reentrant against error occurrence. If returns false,
 *      any members of this object won't be modified, and repetition call will
 *      reproduce same error.
 */
bool findTempDecl(TemplateInstance ti, Scope* sc, WithScopeSymbol* pwithsym)
{
    if (pwithsym)
        *pwithsym = null;

    if (ti.havetempdecl)
        return true;

    //printf("TemplateInstance.findTempDecl() %s\n", toChars());
    if (!ti.tempdecl)
    {
        /* Given:
         *    foo!( ... )
         * figure out which TemplateDeclaration foo refers to.
         */
        Identifier id = ti.name;
        Dsymbol scopesym;
        Dsymbol s = sc.search(ti.loc, id, scopesym);
        if (!s)
        {
            s = sc.search_correct(id);
            if (s)
                .error(ti.loc, "%s `%s` template `%s` is not defined, did you mean %s?", ti.kind, ti.toPrettyChars(), id.toChars(), s.toChars());
            else
                .error(ti.loc, "%s `%s` template `%s` is not defined", ti.kind, ti.toPrettyChars(), id.toChars());
            return false;
        }
        static if (LOG)
        {
            printf("It's an instance of '%s' kind '%s'\n", s.toChars(), s.kind());
            if (s.parent)
                printf("s.parent = '%s'\n", s.parent.toChars());
        }
        if (pwithsym)
            *pwithsym = scopesym.isWithScopeSymbol();

        /* We might have found an alias within a template when
         * we really want the template.
         */
        TemplateInstance ti2;
        if (s.parent && (ti2 = s.parent.isTemplateInstance()) !is null)
        {
            if (ti2.tempdecl && ti2.tempdecl.ident == id)
            {
                /* This is so that one can refer to the enclosing
                 * template, even if it has the same name as a member
                 * of the template, if it has a !(arguments)
                 */
                TemplateDeclaration td = ti2.tempdecl.isTemplateDeclaration();
                assert(td);
                if (td.overroot) // if not start of overloaded list of TemplateDeclaration's
                    td = td.overroot; // then get the start
                s = td;
            }
        }

        // The template might originate from a selective import which implies that
        // s is a lowered AliasDeclaration of the actual TemplateDeclaration.
        // This is the last place where we see the deprecated alias because it is
        // stripped below, so check if the selective import was deprecated.
        // See https://issues.dlang.org/show_bug.cgi?id=20840.
        if (s.isAliasDeclaration())
            s.checkDeprecated(ti.loc, sc);

        if (!ti.updateTempDecl(sc, s))
        {
            return false;
        }
    }
    assert(ti.tempdecl);

    // Look for forward references
    auto tovers = ti.tempdecl.isOverloadSet();
    foreach (size_t oi; 0 .. tovers ? tovers.a.length : 1)
    {
        Dsymbol dstart = tovers ? tovers.a[oi] : ti.tempdecl;
        int r = overloadApply(dstart, (Dsymbol s)
        {
            auto td = s.isTemplateDeclaration();
            if (!td)
                return 0;

            if (td.semanticRun == PASS.initial)
            {
                if (td._scope)
                {
                    // Try to fix forward reference. Ungag errors while doing so.
                    auto ungag = td.ungagSpeculative();
                    td.dsymbolSemantic(td._scope);
                }
                if (td.semanticRun == PASS.initial)
                {
                    .error(ti.loc, "%s `%s` `%s` forward references template declaration `%s`",
                           ti.kind, ti.toPrettyChars(), ti.toChars(), td.toChars());
                    return 1;
                }
            }
            return 0;
        });
        if (r)
            return false;
    }
    return true;
}

bool findMixinTempDecl(TemplateMixin tm, Scope* sc)
{
    // Follow qualifications to find the TemplateDeclaration
    if (!tm.tempdecl)
    {
        Expression e;
        Type t;
        Dsymbol s;
        tm.tqual.resolve(tm.loc, sc, e, t, s);
        if (!s)
        {
            .error(tm.loc, "%s `%s` is not defined", tm.kind, tm.toPrettyChars);
            return false;
        }
        s = s.toAlias();
        tm.tempdecl = s.isTemplateDeclaration();
        OverloadSet os = s.isOverloadSet();

        /* If an OverloadSet, look for a unique member that is a template declaration
         */
        if (os)
        {
            Dsymbol ds = null;
            foreach (i, sym; os.a)
            {
                Dsymbol s2 = sym.isTemplateDeclaration();
                if (s2)
                {
                    if (ds)
                    {
                        tm.tempdecl = os;
                        break;
                    }
                    ds = s2;
                }
            }
        }
        if (!tm.tempdecl)
        {
            .error(tm.loc, "%s `%s` - `%s` is a %s, not a template", tm.kind,
                   tm.toPrettyChars, s.toChars(), s.kind());
            return false;
        }
    }
    assert(tm.tempdecl);

    // Look for forward references
    auto tovers = tm.tempdecl.isOverloadSet();
    foreach (size_t oi; 0 .. tovers ? tovers.a.length : 1)
    {
        Dsymbol dstart = tovers ? tovers.a[oi] : tm.tempdecl;
        int r = overloadApply(dstart, (Dsymbol s)
        {
            auto td = s.isTemplateDeclaration();
            if (!td)
                return 0;

            if (td.semanticRun == PASS.initial)
            {
                if (td._scope)
                    td.dsymbolSemantic(td._scope);
                else
                {
                    tm.semanticRun = PASS.initial;
                    return 1;
                }
            }
            return 0;
        });
        if (r)
            return false;
    }
    return true;
}

/******************************************************
 * Verifies if the given Identifier is a DRuntime hook. It uses the hooks
 * defined in `id.d`.
 *
 * Params:
 *  id = Identifier to verify
 * Returns:
 *  true if `id` is a DRuntime hook
 *  false otherwise
 */
private bool isDRuntimeHook(Identifier id)
{
    return id == Id._d_HookTraceImpl ||
        id == Id._d_newclassT || id == Id._d_newclassTTrace ||
        id == Id._d_arraycatnTX || id == Id._d_arraycatnTXTrace ||
        id == Id._d_newThrowable || id == Id._d_delThrowable ||
        id == Id._d_arrayassign_l || id == Id._d_arrayassign_r ||
        id == Id._d_arraysetassign || id == Id._d_arraysetctor ||
        id == Id._d_arrayctor ||
        id == Id._d_arraysetlengthT ||
        id == Id._d_arraysetlengthTTrace ||
        id == Id._d_arrayappendT || id == Id._d_arrayappendTTrace ||
        id == Id._d_arrayappendcTX;
}

/******************************
 * See if two objects match
 * Params:
 *      o1 = first object
 *      o2 = second object
 * Returns: true if they match
 */
private bool match(RootObject o1, RootObject o2)
{
    enum log = false;

    static if (log)
    {
        printf("match() o1 = %p %s (%d), o2 = %p %s (%d)\n",
            o1, o1.toChars(), o1.dyncast(), o2, o2.toChars(), o2.dyncast());
    }

    bool yes()
    {
        static if (log)
            printf("\t. match\n");
        return true;
    }
    bool no()
    {
        static if (log)
            printf("\t. nomatch\n");
        return false;
    }
    /* A proper implementation of the various equals() overrides
     * should make it possible to just do o1.equals(o2), but
     * we'll do that another day.
     */
    /* Manifest constants should be compared by their values,
     * at least in template arguments.
     */

    if (auto t1 = isType(o1))
    {
        auto t2 = isType(o2);
        if (!t2)
            return no();

        static if (log)
        {
            printf("\tt1 = %s\n", t1.toChars());
            printf("\tt2 = %s\n", t2.toChars());
        }
        if (!t1.equals(t2))
            return no();

        return yes();
    }
    if (auto e1 = getExpression(o1))
    {
        auto e2 = getExpression(o2);
        if (!e2)
            return no();

        static if (log)
        {
            printf("\te1 = %s '%s' %s\n", e1.type ? e1.type.toChars() : "null", EXPtoString(e1.op).ptr, e1.toChars());
            printf("\te2 = %s '%s' %s\n", e2.type ? e2.type.toChars() : "null", EXPtoString(e2.op).ptr, e2.toChars());
        }

        // two expressions can be equal although they do not have the same
        // type; that happens when they have the same value. So check type
        // as well as expression equality to ensure templates are properly
        // matched.
        if (!(e1.type && e2.type && e1.type.equals(e2.type)) || !e1.equals(e2))
            return no();

        return yes();
    }
    if (auto s1 = isDsymbol(o1))
    {
        auto s2 = isDsymbol(o2);
        if (!s2)
            return no();

        static if (log)
        {
            printf("\ts1 = %s \n", s1.kind(), s1.toChars());
            printf("\ts2 = %s \n", s2.kind(), s2.toChars());
        }
        if (!s1.equals(s2))
            return no();
        if (s1.parent != s2.parent && !s1.isFuncDeclaration() && !s2.isFuncDeclaration())
            return no();

        return yes();
    }
    if (auto u1 = isTuple(o1))
    {
        auto u2 = isTuple(o2);
        if (!u2)
            return no();

        static if (log)
        {
            printf("\tu1 = %s\n", u1.toChars());
            printf("\tu2 = %s\n", u2.toChars());
        }
        if (!arrayObjectMatch(u1.objects, u2.objects))
            return no();

        return yes();
    }
    return yes();
}

/************************************
 * Match an array of them.
 */
private bool arrayObjectMatch(ref Objects oa1, ref Objects oa2)
{
    if (&oa1 == &oa2)
        return true;
    if (oa1.length != oa2.length)
        return false;
    immutable oa1dim = oa1.length;
    auto oa1d = oa1[].ptr;
    auto oa2d = oa2[].ptr;
    foreach (j; 0 .. oa1dim)
    {
        RootObject o1 = oa1d[j];
        RootObject o2 = oa2d[j];
        if (!match(o1, o2))
        {
            return false;
        }
    }
    return true;
}


/*************************************
 * Compare proposed template instantiation with existing template instantiation.
 * Note that this is not commutative because of the auto ref check.
 * Params:
 *  ti1 = proposed template instantiation
 *  ti2 = existing template instantiation
 * Returns:
 *  true for match
 */
private bool equalsx(TemplateInstance ti1, TemplateInstance ti2)
{
    //printf("this = %p, ti2 = %p\n", this, ti2);
    assert(ti1.tdtypes.length == ti2.tdtypes.length);

    // Nesting must match
    if (ti1.enclosing != ti2.enclosing)
    {
        //printf("test2 enclosing %s ti2.enclosing %s\n",
        //       enclosing ? enclosing.toChars() : "", ti2.enclosing ? ti.enclosing.toChars() : "");
        return false;
    }
    //printf("parent = %s, ti2.parent = %s\n", parent.toPrettyChars(), ti2.parent.toPrettyChars());

    if (!arrayObjectMatch(ti1.tdtypes, ti2.tdtypes))
        return false;

    /* Template functions may have different instantiations based on
     * "auto ref" parameters.
     */
    auto fd = ti2.toAlias().isFuncDeclaration();
    if (!fd)
        return true;
    if (fd.errors)
        return true;

    auto resolvedArgs = fd.type.isTypeFunction().resolveNamedArgs(
        ArgumentList(ti1.fargs, ti1.fnames), null);

    // resolvedArgs can be null when there's an error: fail_compilation/fail14669.d
    // In that case, equalsx returns true to prevent endless template instantiations
    // However, it can also mean the function was explicitly instantiated
    // without function arguments: fail_compilation/fail14669
    // Hence the following check:
    if (ti1.fargs && !resolvedArgs)
        return true;

    Expression[] args = resolvedArgs ? (*resolvedArgs)[] : [];

    auto fparameters = fd.getParameterList();
    size_t nfparams = fparameters.length;   // Num function parameters
    for (size_t j = 0; j < nfparams; j++)
    {
        Parameter fparam = fparameters[j];
        if (!(fparam.storageClass & STC.autoref) )      // if "auto ref"
            continue;

        Expression farg = (j < args.length) ? args[j] : fparam.defaultArg;
        // resolveNamedArgs strips trailing nulls / default params
        // when it doesn't anymore, the ternary can be replaced with:
        // assert(j < resolvedArgs.length);
        if (!farg)
            farg = fparam.defaultArg;
        if (!farg)
            return false;
        if (farg.isLvalue())
        {
            if (!(fparam.storageClass & STC.ref_))
                return false; // auto ref's don't match
        }
        else
        {
            if (fparam.storageClass & STC.ref_)
                return false; // auto ref's don't match
        }
    }
    return true;
}


/*******************************************
 * Match to a particular TemplateParameter.
 * Input:
 *      instLoc         location that the template is instantiated.
 *      tiargs[]        actual arguments to template instance
 *      i               i'th argument
 *      parameters[]    template parameters
 *      dedtypes[]      deduced arguments to template instance
 *      *psparam        set to symbol declared and initialized to dedtypes[i]
 */
MATCH matchArg(TemplateParameter tp, Loc instLoc, Scope* sc, Objects* tiargs, size_t i, TemplateParameters* parameters, ref Objects dedtypes, Declaration* psparam)
{
    MATCH matchArgNoMatch()
    {
        if (psparam)
            *psparam = null;
        return MATCH.nomatch;
    }

    MATCH matchArgParameter()
    {
        RootObject oarg;

        if (i < tiargs.length)
            oarg = (*tiargs)[i];
        else
        {
            // Get default argument instead
            oarg = tp.defaultArg(instLoc, sc);
            if (!oarg)
            {
                assert(i < dedtypes.length);
                // It might have already been deduced
                oarg = dedtypes[i];
                if (!oarg)
                    return matchArgNoMatch();
            }
        }
        return tp.matchArg(sc, oarg, i, parameters, dedtypes, psparam);
    }

    MATCH matchArgTuple(TemplateTupleParameter ttp)
    {
        /* The rest of the actual arguments (tiargs[]) form the match
         * for the variadic parameter.
         */
        assert(i + 1 == dedtypes.length); // must be the last one
        Tuple ovar;

        if (Tuple u = isTuple(dedtypes[i]))
        {
            // It has already been deduced
            ovar = u;
        }
        else if (i + 1 == tiargs.length && isTuple((*tiargs)[i]))
            ovar = isTuple((*tiargs)[i]);
        else
        {
            ovar = new Tuple();
            //printf("ovar = %p\n", ovar);
            if (i < tiargs.length)
            {
                //printf("i = %d, tiargs.length = %d\n", i, tiargs.length);
                ovar.objects.setDim(tiargs.length - i);
                foreach (j, ref obj; ovar.objects)
                    obj = (*tiargs)[i + j];
            }
        }
        return ttp.matchArg(sc, ovar, i, parameters, dedtypes, psparam);
    }

    if (auto ttp = tp.isTemplateTupleParameter())
        return matchArgTuple(ttp);

    return matchArgParameter();
}

private MATCH matchArg(TemplateParameter tp, Scope* sc, RootObject oarg, size_t i, TemplateParameters* parameters, ref Objects dedtypes, Declaration* psparam)
{
    MATCH matchArgNoMatch()
    {
        //printf("\tm = %d\n", MATCH.nomatch);
        if (psparam)
            *psparam = null;
        return MATCH.nomatch;
    }

    MATCH matchArgType(TemplateTypeParameter ttp)
    {
        //printf("TemplateTypeParameter.matchArg('%s')\n", ttp.ident.toChars());
        MATCH m = MATCH.exact;
        Type ta = isType(oarg);
        if (!ta)
        {
            //printf("%s %p %p %p\n", oarg.toChars(), isExpression(oarg), isDsymbol(oarg), isTuple(oarg));
            return matchArgNoMatch();
        }
        //printf("ta is %s\n", ta.toChars());

        if (ttp.specType)
        {
            if (!ta || ta == TemplateTypeParameter.tdummy)
                return matchArgNoMatch();

            //printf("\tcalling deduceType(): ta is %s, specType is %s\n", ta.toChars(), ttp.specType.toChars());
            MATCH m2 = deduceType(ta, sc, ttp.specType, *parameters, dedtypes);
            if (m2 == MATCH.nomatch)
            {
                //printf("\tfailed deduceType\n");
                return matchArgNoMatch();
            }

            if (m2 < m)
                m = m2;
            if (dedtypes[i])
            {
                Type t = cast(Type)dedtypes[i];

                if (ttp.dependent && !t.equals(ta)) // https://issues.dlang.org/show_bug.cgi?id=14357
                    return matchArgNoMatch();

                /* This is a self-dependent parameter. For example:
                 *  template X(T : T*) {}
                 *  template X(T : S!T, alias S) {}
                 */
                //printf("t = %s ta = %s\n", t.toChars(), ta.toChars());
                ta = t;
            }
        }
        else
        {
            if (dedtypes[i])
            {
                // Must match already deduced type
                Type t = cast(Type)dedtypes[i];

                if (!t.equals(ta))
                {
                    //printf("t = %s ta = %s\n", t.toChars(), ta.toChars());
                    return matchArgNoMatch();
                }
            }
            else
            {
                // So that matches with specializations are better
                m = MATCH.convert;
            }
        }
        dedtypes[i] = ta;

        if (psparam)
            *psparam = new AliasDeclaration(ttp.loc, ttp.ident, ta);
        //printf("\tm = %d\n", m);
        return ttp.dependent ? MATCH.exact : m;
    }

    MATCH matchArgValue(TemplateValueParameter tvp)
    {
        //printf("TemplateValueParameter.matchArg('%s')\n", tvp.ident.toChars());
        MATCH m = MATCH.exact;

        Expression ei = isExpression(oarg);
        Type vt;

        if (!ei && oarg)
        {
            Dsymbol si = isDsymbol(oarg);
            FuncDeclaration f = si ? si.isFuncDeclaration() : null;
            if (!f || !f.fbody || f.needThis())
                return matchArgNoMatch();

            ei = new VarExp(tvp.loc, f);
            ei = ei.expressionSemantic(sc);

            /* If a function is really property-like, and then
             * it's CTFEable, ei will be a literal expression.
             */
            const olderrors = global.startGagging();
            ei = resolveProperties(sc, ei);
            ei = ei.ctfeInterpret();
            if (global.endGagging(olderrors) || ei.op == EXP.error)
                return matchArgNoMatch();

            /* https://issues.dlang.org/show_bug.cgi?id=14520
             * A property-like function can match to both
             * TemplateAlias and ValueParameter. But for template overloads,
             * it should always prefer alias parameter to be consistent
             * template match result.
             *
             *   template X(alias f) { enum X = 1; }
             *   template X(int val) { enum X = 2; }
             *   int f1() { return 0; }  // CTFEable
             *   int f2();               // body-less function is not CTFEable
             *   enum x1 = X!f1;    // should be 1
             *   enum x2 = X!f2;    // should be 1
             *
             * e.g. The x1 value must be same even if the f1 definition will be moved
             *      into di while stripping body code.
             */
            m = MATCH.convert;
        }

        if (ei && ei.op == EXP.variable)
        {
            // Resolve const variables that we had skipped earlier
            ei = ei.ctfeInterpret();
        }

        //printf("\tvalType: %s, ty = %d\n", tvp.valType.toChars(), tvp.valType.ty);
        vt = tvp.valType.typeSemantic(tvp.loc, sc);
        //printf("ei: %s, ei.type: %s\n", ei.toChars(), ei.type.toChars());
        //printf("vt = %s\n", vt.toChars());

        if (ei.type)
        {
            MATCH m2 = ei.implicitConvTo(vt);
            //printf("m: %d\n", m);
            if (m2 < m)
                m = m2;
            if (m == MATCH.nomatch)
                return matchArgNoMatch();
            ei = ei.implicitCastTo(sc, vt);
            ei = ei.ctfeInterpret();
        }

        if (tvp.specValue)
        {
            if (ei is null || (cast(void*)ei.type in TemplateValueParameter.edummies &&
                               TemplateValueParameter.edummies[cast(void*)ei.type] == ei))
                return matchArgNoMatch();

            Expression e = tvp.specValue;

            sc = sc.startCTFE();
            e = e.expressionSemantic(sc);
            e = resolveProperties(sc, e);
            sc = sc.endCTFE();
            e = e.implicitCastTo(sc, vt);
            e = e.ctfeInterpret();

            ei = ei.syntaxCopy();
            sc = sc.startCTFE();
            ei = ei.expressionSemantic(sc);
            sc = sc.endCTFE();
            ei = ei.implicitCastTo(sc, vt);
            ei = ei.ctfeInterpret();
            //printf("\tei: %s, %s\n", ei.toChars(), ei.type.toChars());
            //printf("\te : %s, %s\n", e.toChars(), e.type.toChars());
            if (!ei.equals(e))
                return matchArgNoMatch();
        }
        else
        {
            if (dedtypes[i])
            {
                // Must match already deduced value
                Expression e = cast(Expression)dedtypes[i];
                if (!ei || !ei.equals(e))
                    return matchArgNoMatch();
            }
        }
        dedtypes[i] = ei;

        if (psparam)
        {
            Initializer _init = new ExpInitializer(tvp.loc, ei);
            Declaration sparam = new VarDeclaration(tvp.loc, vt, tvp.ident, _init);
            sparam.storage_class = STC.manifest;
            *psparam = sparam;
        }
        return tvp.dependent ? MATCH.exact : m;
    }

    MATCH matchArgAlias(TemplateAliasParameter tap)
    {
        //printf("TemplateAliasParameter.matchArg('%s')\n", tap.ident.toChars());
        MATCH m = MATCH.exact;
        Type ta = isType(oarg);
        RootObject sa = ta && !ta.deco ? null : getDsymbol(oarg);
        Expression ea = isExpression(oarg);
        if (ea)
        {
            if (auto te = ea.isThisExp())
                sa = te.var;
            else if (auto se = ea.isSuperExp())
                sa = se.var;
            else if (auto se = ea.isScopeExp())
                sa = se.sds;
        }
        if (sa)
        {
            if ((cast(Dsymbol)sa).isAggregateDeclaration())
                m = MATCH.convert;

            /* specType means the alias must be a declaration with a type
             * that matches specType.
             */
            if (tap.specType)
            {
                tap.specType = typeSemantic(tap.specType, tap.loc, sc);
                Declaration d = (cast(Dsymbol)sa).isDeclaration();
                if (!d)
                    return matchArgNoMatch();
                if (!d.type.equals(tap.specType))
                    return matchArgNoMatch();
            }
        }
        else
        {
            sa = oarg;
            if (ea)
            {
                if (tap.specType)
                {
                    if (!ea.type.equals(tap.specType))
                        return matchArgNoMatch();
                }
            }
            else if (ta && ta.ty == Tinstance && !tap.specAlias)
            {
                /* Specialized parameter should be preferred
                 * match to the template type parameter.
                 *  template X(alias a) {}                      // a == this
                 *  template X(alias a : B!A, alias B, A...) {} // B!A => ta
                 */
            }
            else if (sa && sa == TemplateTypeParameter.tdummy)
            {
                /* https://issues.dlang.org/show_bug.cgi?id=2025
                 * Aggregate Types should preferentially
                 * match to the template type parameter.
                 *  template X(alias a) {}  // a == this
                 *  template X(T) {}        // T => sa
                 */
            }
            else if (ta && ta.ty != Tident)
            {
                /* Match any type that's not a TypeIdentifier to alias parameters,
                 * but prefer type parameter.
                 * template X(alias a) { }  // a == ta
                 *
                 * TypeIdentifiers are excluded because they might be not yet resolved aliases.
                 */
                m = MATCH.convert;
            }
            else
                return matchArgNoMatch();
        }

        if (tap.specAlias)
        {
            if (sa == TemplateAliasParameter.sdummy)
                return matchArgNoMatch();
            // check specialization if template arg is a symbol
            Dsymbol sx = isDsymbol(sa);
            if (sa != tap.specAlias && sx)
            {
                Type talias = isType(tap.specAlias);
                if (!talias)
                    return matchArgNoMatch();

                TemplateInstance ti = sx.isTemplateInstance();
                if (!ti && sx.parent)
                {
                    ti = sx.parent.isTemplateInstance();
                    if (ti && ti.name != sx.ident)
                        return matchArgNoMatch();
                }
                if (!ti)
                    return matchArgNoMatch();

                Type t = new TypeInstance(Loc.initial, ti);
                MATCH m2 = deduceType(t, sc, talias, *parameters, dedtypes);
                if (m2 == MATCH.nomatch)
                    return matchArgNoMatch();
            }
            // check specialization if template arg is a type
            else if (ta)
            {
                if (Type tspec = isType(tap.specAlias))
                {
                    MATCH m2 = ta.implicitConvTo(tspec);
                    if (m2 == MATCH.nomatch)
                        return matchArgNoMatch();
                }
                else
                {
                    error(tap.loc, "template parameter specialization for a type must be a type and not `%s`",
                        tap.specAlias.toChars());
                    return matchArgNoMatch();
                }
            }
        }
        else if (dedtypes[i])
        {
            // Must match already deduced symbol
            RootObject si = dedtypes[i];
            if (!sa || si != sa)
                return matchArgNoMatch();
        }
        dedtypes[i] = sa;

        if (psparam)
        {
            if (Dsymbol s = isDsymbol(sa))
            {
                *psparam = new AliasDeclaration(tap.loc, tap.ident, s);
            }
            else if (Type t = isType(sa))
            {
                *psparam = new AliasDeclaration(tap.loc, tap.ident, t);
            }
            else
            {
                assert(ea);

                // Declare manifest constant
                Initializer _init = new ExpInitializer(tap.loc, ea);
                auto v = new VarDeclaration(tap.loc, null, tap.ident, _init);
                v.storage_class = STC.manifest;
                v.dsymbolSemantic(sc);
                *psparam = v;
            }
        }
        return tap.dependent ? MATCH.exact : m;
    }

    MATCH matchArgTuple(TemplateTupleParameter ttp)
    {
        //printf("TemplateTupleParameter.matchArg('%s')\n", ttp.ident.toChars());
        Tuple ovar = isTuple(oarg);
        if (!ovar)
            return MATCH.nomatch;
        if (dedtypes[i])
        {
            Tuple tup = isTuple(dedtypes[i]);
            if (!tup)
                return MATCH.nomatch;
            if (!match(tup, ovar))
                return MATCH.nomatch;
        }
        dedtypes[i] = ovar;

        if (psparam)
            *psparam = new TupleDeclaration(ttp.loc, ttp.ident, &ovar.objects);
        return ttp.dependent ? MATCH.exact : MATCH.convert;
    }

    if (auto ttp = tp.isTemplateTypeParameter())
        return matchArgType(ttp);
    if (auto tvp = tp.isTemplateValueParameter())
        return matchArgValue(tvp);
    if (auto tap = tp.isTemplateAliasParameter())
        return matchArgAlias(tap);
    if (auto ttp = tp.isTemplateTupleParameter())
        return matchArgTuple(ttp);
    assert(0);
}

/**********************************************
 * Confirm s is a valid template, then store it.
 * Input:
 *      sc
 *      s   candidate symbol of template. It may be:
 *          TemplateDeclaration
 *          FuncDeclaration with findTemplateDeclRoot() != NULL
 *          OverloadSet which contains candidates
 * Returns:
 *      true if updating succeeds.
 */
bool updateTempDecl(TemplateInstance ti, Scope* sc, Dsymbol s)
{
    if (!s)
        return ti.tempdecl !is null;

    Identifier id = ti.name;
    s = s.toAlias();

    /* If an OverloadSet, look for a unique member that is a template declaration
     */
    if (OverloadSet os = s.isOverloadSet())
    {
        s = null;
        foreach (s2; os.a)
        {
            if (FuncDeclaration f = s2.isFuncDeclaration())
                s2 = f.findTemplateDeclRoot();
            else
                s2 = s2.isTemplateDeclaration();
            if (s2)
            {
                if (s)
                {
                    ti.tempdecl = os;
                    return true;
                }
                s = s2;
            }
        }
        if (!s)
        {
            .error(ti.loc, "%s `%s` template `%s` is not defined", ti.kind, ti.toPrettyChars, id.toChars());
            return false;
        }
    }

    if (OverDeclaration od = s.isOverDeclaration())
    {
        ti.tempdecl = od; // TODO: more strict check
        return true;
    }

    /* It should be a TemplateDeclaration, not some other symbol
     */
    if (FuncDeclaration f = s.isFuncDeclaration())
        ti.tempdecl = f.findTemplateDeclRoot();
    else
        ti.tempdecl = s.isTemplateDeclaration();

    // We're done
    if (ti.tempdecl)
        return true;

    // Error already issued, just return `false`
    if (!s.parent && global.errors)
        return false;

    if (!s.parent && dmd.dsymbolsem.getType(s))
    {
        Dsymbol s2 = dmd.dsymbolsem.getType(s).toDsymbol(sc);
        if (!s2)
        {
            .error(ti.loc, "`%s` is not a valid template instance, because `%s` is not a template declaration but a type (`%s == %s`)", ti.toChars(), id.toChars(), id.toChars(), dmd.dsymbolsem.getType(s).kind());
            return false;
        }
        // because s can be the alias created for a TemplateParameter
        const AliasDeclaration ad = s.isAliasDeclaration();
        version (none)
        {
            if (ad && ad.isAliasedTemplateParameter())
                printf("`%s` is an alias created from a template parameter\n", s.toChars());
        }
        if (!ad || !ad.isAliasedTemplateParameter())
            s = s2;
    }

    TemplateInstance ti2 = s.parent ? s.parent.isTemplateInstance() : null;

    /* This avoids the VarDeclaration.toAlias() which runs semantic() too soon
     */
    static bool matchId(TemplateInstance _ti, Identifier id)
    {
        if (_ti.aliasdecl && _ti.aliasdecl.isVarDeclaration())
            return _ti.aliasdecl.isVarDeclaration().ident == id;
        return _ti.toAlias().ident == id;
    }

    if (ti2 && (ti2.name == s.ident || matchId(ti2, s.ident)) && ti2.tempdecl)
    {
        /* This is so that one can refer to the enclosing
         * template, even if it has the same name as a member
         * of the template, if it has a !(arguments)
         */
        TemplateDeclaration td = ti2.tempdecl.isTemplateDeclaration();
        assert(td);
        if (td.overroot) // if not start of overloaded list of TemplateDeclaration's
            td = td.overroot; // then get the start
        ti.tempdecl = td;
        return true;
    }
    else
    {
        .error(ti.loc, "%s `%s` `%s` is not a template declaration, it is a %s",
               ti.kind, ti.toPrettyChars, id.toChars(), s.kind());
        return false;
    }
}

/***************************************
 * Given that ti is an instance of this TemplateDeclaration,
 * deduce the types of the parameters to this, and store
 * those deduced types in dedtypes[].
 * Params:
 *  sc = context
 *  td = template
 *  ti = instance of td
 *  dedtypes = fill in with deduced types
 *  argumentList = arguments to template instance
 *  flag = 1 - don't do semantic() because of dummy types
 *         2 - don't change types in matchArg()
 * Returns: match level.
 */
public
MATCH matchWithInstance(Scope* sc, TemplateDeclaration td, TemplateInstance ti, ref Objects dedtypes, ArgumentList argumentList, int flag)
{
    enum LOGM = 0;
    static if (LOGM)
    {
        printf("\n+TemplateDeclaration.matchWithInstance(td = %s, ti = %s, flag = %d)\n", td.toChars(), ti.toChars(), flag);
    }
    version (none)
    {
        printf("dedtypes.length = %d, parameters.length = %d\n", dedtypes.length, parameters.length);
        if (ti.tiargs.length)
            printf("ti.tiargs.length = %d, [0] = %p\n", ti.tiargs.length, (*ti.tiargs)[0]);
    }
    MATCH nomatch()
    {
        static if (LOGM)
        {
            printf(" no match\n");
        }
        return MATCH.nomatch;
    }
    MATCH m;
    size_t dedtypes_dim = dedtypes.length;

    dedtypes.zero();

    if (td.errors)
        return MATCH.nomatch;

    size_t parameters_dim = td.parameters.length;
    const bool variadic = td.isVariadic() !is null;

    // If more arguments than parameters, no match
    if (ti.tiargs.length > parameters_dim && !variadic)
    {
        static if (LOGM)
        {
            printf(" no match: more arguments than parameters\n");
        }
        return MATCH.nomatch;
    }

    assert(dedtypes_dim == parameters_dim);
    assert(dedtypes_dim >= ti.tiargs.length || variadic);

    assert(td._scope);

    // Set up scope for template parameters
    Scope* paramscope = createScopeForTemplateParameters(td, ti, sc);

    // Attempt type deduction
    m = MATCH.exact;
    for (size_t i = 0; i < dedtypes_dim; i++)
    {
        MATCH m2;
        TemplateParameter tp = (*td.parameters)[i];
        Declaration sparam;

        //printf("\targument [%d]\n", i);
        static if (LOGM)
        {
            //printf("\targument [%d] is %s\n", i, oarg ? oarg.toChars() : "null");
            TemplateTypeParameter ttp = tp.isTemplateTypeParameter();
            if (ttp)
                printf("\tparameter[%d] is %s : %s\n", i, tp.ident.toChars(), ttp.specType ? ttp.specType.toChars() : "");
        }

        m2 = tp.matchArg(ti.loc, paramscope, ti.tiargs, i, td.parameters, dedtypes, &sparam);
        //printf("\tm2 = %d\n", m2);
        if (m2 == MATCH.nomatch)
        {
            version (none)
            {
                printf("\tmatchArg() for parameter %i failed\n", i);
            }
            return nomatch();
        }

        if (m2 < m)
            m = m2;

        if (!flag)
            sparam.dsymbolSemantic(paramscope);
        if (!paramscope.insert(sparam)) // TODO: This check can make more early
        {
            // in TemplateDeclaration.semantic, and
            // then we don't need to make sparam if flags == 0
            return nomatch();
        }
    }

    if (!flag)
    {
        /* Any parameter left without a type gets the type of
         * its corresponding arg
         */
        foreach (i, ref dedtype; dedtypes)
        {
            if (!dedtype)
            {
                assert(i < ti.tiargs.length);
                dedtype = cast(Type)(*ti.tiargs)[i];
            }
        }
    }

    if (m > MATCH.nomatch && td.constraint && !flag)
    {
        if (ti.hasNestedArgs(ti.tiargs, td.isstatic)) // TODO: should gag error
            ti.parent = ti.enclosing;
        else
            ti.parent = td.parent;

        // Similar to doHeaderInstantiation
        td.computeOneMember();
        FuncDeclaration fd = td.onemember ? td.onemember.isFuncDeclaration() : null;
        if (fd)
        {
            TypeFunction tf = fd.type.isTypeFunction().syntaxCopy();

            fd = new FuncDeclaration(fd.loc, fd.endloc, fd.ident, fd.storage_class, tf);
            fd.parent = ti;
            fd.inferRetType = true;

            // Shouldn't run semantic on default arguments and return type.
            foreach (ref param; *tf.parameterList.parameters)
                param.defaultArg = null;

            tf.next = null;
            tf.incomplete = true;

            // Resolve parameter types and 'auto ref's.
            tf.inferenceArguments = argumentList;
            const olderrors = global.startGagging();
            fd.type = tf.typeSemantic(td.loc, paramscope);
            global.endGagging(olderrors);
            if (fd.type.ty != Tfunction)
                return nomatch();
            fd.originalType = fd.type; // for mangling
        }

        // TODO: dedtypes => ti.tiargs ?
        if (!evaluateConstraint(td, ti, sc, paramscope, &dedtypes, fd))
            return nomatch();
    }

    static if (LOGM)
    {
        // Print out the results
        printf("--------------------------\n");
        printf("template %s\n", toChars());
        printf("instance %s\n", ti.toChars());
        if (m > MATCH.nomatch)
        {
            for (size_t i = 0; i < dedtypes_dim; i++)
            {
                TemplateParameter tp = (*parameters)[i];
                RootObject oarg;
                printf(" [%d]", i);
                if (i < ti.tiargs.length)
                    oarg = (*ti.tiargs)[i];
                else
                    oarg = null;
                tp.print(oarg, (*dedtypes)[i]);
            }
        }
        else
            return nomatch();
    }
    static if (LOGM)
    {
        printf(" match = %d\n", m);
    }

    paramscope.pop();
    static if (LOGM)
    {
        printf("-TemplateDeclaration.matchWithInstance(td = %s, ti = %s) = %d\n", td.toChars(), ti.toChars(), m);
    }
    return m;
}

/**
    Returns: true if the instances' innards are discardable.

    The idea of this function is to see if the template instantiation
    can be 100% replaced with its eponymous member. All other members
    can be discarded, even in the compiler to free memory (for example,
    the template could be expanded in a region allocator, deemed trivial,
    the end result copied back out independently and the entire region freed),
    and can be elided entirely from the binary.

    The current implementation affects code that generally looks like:

    ---
    template foo(args...) {
        some_basic_type_or_string helper() { .... }
        enum foo = helper();
    }
    ---

    since it was the easiest starting point of implementation but it can and
    should be expanded more later.
*/
bool isDiscardable(TemplateInstance ti)
{
    if (ti.aliasdecl is null)
        return false;

    auto v = ti.aliasdecl.isVarDeclaration();
    if (v is null)
        return false;

    if (!(v.storage_class & STC.manifest))
        return false;

    // Currently only doing basic types here because it is the easiest proof-of-concept
    // implementation with minimal risk of side effects, but it could likely be
    // expanded to any type that already exists outside this particular instance.
    if (!(v.type.equals(Type.tstring) || (v.type.isTypeBasic() !is null)))
        return false;

    // Static ctors and dtors, even in an eponymous enum template, are still run,
    // so if any of them are in here, we'd better not assume it is trivial lest
    // we break useful code
    foreach(member; *ti.members)
    {
        if(member.hasStaticCtorOrDtor())
            return false;
        if(member.isStaticDtorDeclaration())
            return false;
        if(member.isStaticCtorDeclaration())
            return false;
    }

    // but if it passes through this gauntlet... it should be fine. D code will
    // see only the eponymous member, outside stuff can never access it, even through
    // reflection; the outside world ought to be none the wiser. Even dmd should be
    // able to simply free the memory of everything except the final result.

    return true;
}


/***********************************************
 * Returns true if this is not instantiated in non-root module, and
 * is a part of non-speculative instantiatiation.
 *
 * Note: minst does not stabilize until semantic analysis is completed,
 * so don't call this function during semantic analysis to return precise result.
 */
bool needsCodegen(TemplateInstance ti)
{
    //printf("needsCodegen() %s\n", toChars());

    // minst is finalized after the 1st invocation.
    // tnext is only needed for the 1st invocation and
    // cleared for further invocations.
    TemplateInstance tnext = ti.tnext;
    TemplateInstance tinst = ti.tinst;
    ti.tnext = null;

    // Don't do codegen if the instance has errors,
    // is a dummy instance (see evaluateConstraint),
    // or is determined to be discardable.
    if (ti.errors || ti.inst is null || ti.inst.isDiscardable())
    {
        ti.minst = null; // mark as speculative
        return false;
    }

    // This should only be called on the primary instantiation.
    assert(ti is ti.inst);

    /* Hack for suppressing linking errors against template instances
    * of `_d_arrayliteralTX` (e.g. `_d_arrayliteralTX!(int[])`).
    *
    * This happens, for example, when a lib module is compiled with `preview=dip1000` and
    * the array literal is placed on stack instead of using the lowering. In this case,
    * if the root module is compiled without `preview=dip1000`, the compiler will consider
    * the template already instantiated within the lib module, and thus skip the codegen for it
    * in the root module object file, thinking that the linker will find the instance in the lib module.
    *
    * To bypass this edge case, we always do codegen for `_d_arrayliteralTX` template instances,
    * even if an instance already exists in non-root module.
    */
    if (ti.inst && ti.inst.name == Id._d_arrayliteralTX)
    {
        return true;
    }

    if (global.params.allInst)
    {
        // Do codegen if there is an instantiation from a root module, to maximize link-ability.
        static ThreeState needsCodegenAllInst(TemplateInstance tithis, TemplateInstance tinst)
        {
            // Do codegen if `this` is instantiated from a root module.
            if (tithis.minst && tithis.minst.isRoot())
                return ThreeState.yes;

            // Do codegen if the ancestor needs it.
            if (tinst && tinst.inst && tinst.inst.needsCodegen())
            {
                tithis.minst = tinst.inst.minst; // cache result
                assert(tithis.minst);
                assert(tithis.minst.isRoot());
                return ThreeState.yes;
            }
            return ThreeState.none;
        }

        if (const needsCodegen = needsCodegenAllInst(ti, tinst))
            return needsCodegen == ThreeState.yes ? true : false;

        // Do codegen if a sibling needs it.
        for (; tnext; tnext = tnext.tnext)
        {
            const needsCodegen = needsCodegenAllInst(tnext, tnext.tinst);
            if (needsCodegen == ThreeState.yes)
            {
                ti.minst = tnext.minst; // cache result
                assert(ti.minst);
                assert(ti.minst.isRoot());
                return true;
            }
            else if (!ti.minst && tnext.minst)
            {
                ti.minst = tnext.minst; // cache result from non-speculative sibling
                // continue searching
            }
            else if (needsCodegen != ThreeState.none)
                break;
        }

        // Elide codegen because there's no instantiation from any root modules.
        return false;
    }

    // Prefer instantiations from non-root modules, to minimize object code size.

    /* If a TemplateInstance is ever instantiated from a non-root module,
     * we do not have to generate code for it,
     * because it will be generated when the non-root module is compiled.
     *
     * But, if the non-root 'minst' imports any root modules, it might still need codegen.
     *
     * The problem is if A imports B, and B imports A, and both A
     * and B instantiate the same template, does the compilation of A
     * or the compilation of B do the actual instantiation?
     *
     * See https://issues.dlang.org/show_bug.cgi?id=2500.
     *
     * => Elide codegen if there is at least one instantiation from a non-root module
     *    which doesn't import any root modules.
     */
    static ThreeState needsCodegenRootOnly(TemplateInstance tithis, TemplateInstance tinst)
    {
        // If the ancestor isn't speculative,
        // 1. do codegen if the ancestor needs it
        // 2. elide codegen if the ancestor doesn't need it (non-root instantiation of ancestor incl. subtree)
        if (tinst && tinst.inst)
        {
            tinst = tinst.inst;
            const needsCodegen = tinst.needsCodegen(); // sets tinst.minst
            if (tinst.minst) // not speculative
            {
                tithis.minst = tinst.minst; // cache result
                return needsCodegen ? ThreeState.yes : ThreeState.no;
            }
        }

        // Elide codegen if `this` doesn't need it.
        if (tithis.minst && !tithis.minst.isRoot() && !tithis.minst.rootImports())
            return ThreeState.no;

        return ThreeState.none;
    }

    if (const needsCodegen = needsCodegenRootOnly(ti, tinst))
        return needsCodegen == ThreeState.yes ? true : false;

    // Elide codegen if a (non-speculative) sibling doesn't need it.
    for (; tnext; tnext = tnext.tnext)
    {
        const needsCodegen = needsCodegenRootOnly(tnext, tnext.tinst); // sets tnext.minst
        if (tnext.minst) // not speculative
        {
            if (needsCodegen == ThreeState.no)
            {
                ti.minst = tnext.minst; // cache result
                assert(!ti.minst.isRoot() && !ti.minst.rootImports());
                return false;
            }
            else if (!ti.minst)
            {
                ti.minst = tnext.minst; // cache result from non-speculative sibling
                // continue searching
            }
            else if (needsCodegen != ThreeState.none)
                break;
        }
    }

    // Unless `this` is still speculative (=> all further siblings speculative too),
    // do codegen because we found no guaranteed-codegen'd non-root instantiation.
    return ti.minst !is null;
}

/****************************
 * Check to see if constraint is satisfied.
 */
private bool evaluateConstraint(TemplateDeclaration td, TemplateInstance ti, Scope* sc, Scope* paramscope, Objects* dedargs, FuncDeclaration fd)
{
    /* Detect recursive attempts to instantiate this template declaration,
     * https://issues.dlang.org/show_bug.cgi?id=4072
     *  void foo(T)(T x) if (is(typeof(foo(x)))) { }
     *  static assert(!is(typeof(foo(7))));
     * Recursive attempts are regarded as a constraint failure.
     */
    /* There's a chicken-and-egg problem here. We don't know yet if this template
     * instantiation will be a local one (enclosing is set), and we won't know until
     * after selecting the correct template. Thus, function we're nesting inside
     * is not on the sc scope chain, and this can cause errors in FuncDeclaration.getLevel().
     * Workaround the problem by setting a flag to relax the checking on frame errors.
     */

    for (TemplatePrevious* p = td.previous; p; p = p.prev)
    {
        if (!arrayObjectMatch(*p.dedargs, *dedargs))
            continue;
        //printf("recursive, no match p.sc=%p %p %s\n", p.sc, this, this.toChars());
        /* It must be a subscope of p.sc, other scope chains are not recursive
         * instantiations.
         * the chain of enclosing scopes is broken by paramscope (its enclosing
         * scope is _scope, but paramscope.callsc is the instantiating scope). So
         * it's good enough to check the chain of callsc
         */
        for (Scope* scx = paramscope.callsc; scx; scx = scx.callsc)
        {
            // The first scx might be identical for nested eponymeous templates, e.g.
            // template foo() { void foo()() {...} }
            if (scx == p.sc && scx !is paramscope.callsc)
                return false;
        }
        /* BUG: should also check for ref param differences
         */
    }

    TemplatePrevious pr;
    pr.prev = td.previous;
    pr.sc = paramscope.callsc;
    pr.dedargs = dedargs;
    td.previous = &pr; // add this to threaded list

    Scope* scx = paramscope.push(ti);
    scx.parent = ti;
    scx.tinst = null;
    scx.minst = null;
    // Set SCOPE.constraint before declaring function parameters for the static condition
    // (previously, this was immediately before calling evalStaticCondition), so the
    // semantic pass knows not to issue deprecation warnings for these throw-away decls.
    // https://issues.dlang.org/show_bug.cgi?id=21831
    scx.inTemplateConstraint = true;

    assert(!ti.symtab);
    if (fd)
    {
        /* Declare all the function parameters as variables and add them to the scope
         * Making parameters is similar to FuncDeclaration.semantic3
         */
        auto tf = fd.type.isTypeFunction();

        scx.parent = fd;

        Parameters* fparameters = tf.parameterList.parameters;
        const nfparams = tf.parameterList.length;
        foreach (i, fparam; tf.parameterList)
        {
            fparam.storageClass &= (STC.IOR | STC.lazy_ | STC.final_ | STC.TYPECTOR | STC.nodtor);
            fparam.storageClass |= STC.parameter;
            if (tf.parameterList.varargs == VarArg.typesafe && i + 1 == nfparams)
            {
                fparam.storageClass |= STC.variadic;
                /* Don't need to set STC.scope_ because this will only
                 * be evaluated at compile time
                 */
            }
        }
        foreach (fparam; *fparameters)
        {
            if (!fparam.ident)
                continue;
            // don't add it, if it has no name
            auto v = new VarDeclaration(fparam.loc, fparam.type, fparam.ident, null);
            fparam.storageClass |= STC.parameter;
            v.storage_class = fparam.storageClass;
            v.dsymbolSemantic(scx);
            if (!ti.symtab)
                ti.symtab = new DsymbolTable();
            if (!scx.insert(v))
                .error(td.loc, "%s `%s` parameter `%s.%s` is already defined", td.kind, td.toPrettyChars, td.toChars(), v.toChars());
            else
                v.parent = fd;
        }
        if (td.isstatic)
            fd.storage_class |= STC.static_;
        declareThis(fd, scx);
    }

    td.lastConstraint = td.constraint.syntaxCopy();
    td.lastConstraintTiargs = ti.tiargs;
    td.lastConstraintNegs.setDim(0);

    import dmd.staticcond;

    assert(ti.inst is null);
    ti.inst = ti; // temporary instantiation to enable genIdent()
    bool errors;
    const bool result = evalStaticCondition(scx, td.constraint, td.lastConstraint, errors, &td.lastConstraintNegs);
    if (result || errors)
    {
        td.lastConstraint = null;
        td.lastConstraintTiargs = null;
        td.lastConstraintNegs.setDim(0);
    }
    ti.inst = null;
    ti.symtab = null;
    scx = scx.pop();
    td.previous = pr.prev; // unlink from threaded list
    if (errors)
        return false;
    return result;
}

/****************************
 * Destructively get the error message from the last constraint evaluation
 * Params:
 *      td = TemplateDeclaration
 *      tip = tip to show after printing all overloads
 */
const(char)* getConstraintEvalError(TemplateDeclaration td, ref const(char)* tip)
{
    import dmd.staticcond;
    // there will be a full tree view in verbose mode, and more compact list in the usual
    const full = global.params.v.verbose;
    uint count;
    const msg = visualizeStaticCondition(td.constraint, td.lastConstraint, td.lastConstraintNegs[], full, count);
    scope (exit)
    {
        td.lastConstraint = null;
        td.lastConstraintTiargs = null;
        td.lastConstraintNegs.setDim(0);
    }
    if (!msg)
        return null;

    OutBuffer buf;

    assert(td.parameters && td.lastConstraintTiargs);
    if (td.parameters.length > 0)
    {
        formatParamsWithTiargs(*td.parameters, *td.lastConstraintTiargs, td.isVariadic() !is null, buf);
        buf.writenl();
    }
    if (!full)
    {
        // choosing singular/plural
        const s = (count == 1) ?
            "  must satisfy the following constraint:" :
            "  must satisfy one of the following constraints:";
        buf.writestring(s);
        buf.writenl();
        // the constraints
        buf.writeByte('`');
        buf.writestring(msg);
        buf.writeByte('`');
    }
    else
    {
        buf.writestring("  whose parameters have the following constraints:");
        buf.writenl();
        const sep = "  `~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`";
        buf.writestring(sep);
        buf.writenl();
        // the constraints
        buf.writeByte('`');
        buf.writestring(msg);
        buf.writeByte('`');
        buf.writestring(sep);
        tip = "not satisfied constraints are marked with `>`";
    }
    return buf.extractChars();
}

/**********************************
 * Find the TemplateDeclaration that matches this TemplateInstance best.
 *
 * Params:
 *   ti    = TemplateInstance
 *   sc    = the scope this TemplateInstance resides in
 *   argumentList = function arguments in case of a template function
 *
 * Returns:
 *   `true` if a match was found, `false` otherwise
 */
bool findBestMatch(TemplateInstance ti, Scope* sc, ArgumentList argumentList)
{
    if (ti.havetempdecl)
    {
        TemplateDeclaration tempdecl = ti.tempdecl.isTemplateDeclaration();
        assert(tempdecl);
        assert(tempdecl._scope);
        // Deduce tdtypes
        ti.tdtypes.setDim(tempdecl.parameters.length);
        if (!matchWithInstance(sc, tempdecl, ti, ti.tdtypes, argumentList, 2))
        {
            .error(ti.loc, "%s `%s` incompatible arguments for template instantiation",
                   ti.kind, ti.toPrettyChars);
            return false;
        }
        // TODO: Normalizing tiargs for https://issues.dlang.org/show_bug.cgi?id=7469 is necessary?
        return true;
    }

    static if (LOG)
    {
        printf("TemplateInstance.findBestMatch()\n");
    }

    const errs = global.errors;
    TemplateDeclaration td_last = null;
    Objects dedtypes;

    /* Since there can be multiple TemplateDeclaration's with the same
     * name, look for the best match.
     */
    auto tovers = ti.tempdecl.isOverloadSet();
    foreach (size_t oi; 0 .. tovers ? tovers.a.length : 1)
    {
        TemplateDeclaration td_best;
        TemplateDeclaration td_ambig;
        MATCH m_best = MATCH.nomatch;

        Dsymbol dstart = tovers ? tovers.a[oi] : ti.tempdecl;
        overloadApply(dstart, (Dsymbol s)
        {
            auto td = s.isTemplateDeclaration();
            if (!td)
                return 0;
            if (td == td_best)   // skip duplicates
                return 0;

            //printf("td = %s\n", td.toPrettyChars());
            // If more arguments than parameters,
            // then this is no match.
            if (td.parameters.length < ti.tiargs.length)
            {
                if (!td.isVariadic())
                    return 0;
            }

            dedtypes.setDim(td.parameters.length);
            dedtypes.zero();
            assert(td.semanticRun != PASS.initial);

            MATCH m = matchWithInstance(sc, td, ti, dedtypes, argumentList, 0);
            //printf("matchWithInstance = %d\n", m);
            if (m == MATCH.nomatch) // no match at all
                return 0;
            if (m < m_best) goto Ltd_best;
            if (m > m_best) goto Ltd;

            // Disambiguate by picking the most specialized TemplateDeclaration
            {
            MATCH c1 = leastAsSpecialized(sc, td, td_best, argumentList);
            MATCH c2 = leastAsSpecialized(sc, td_best, td, argumentList);
            //printf("c1 = %d, c2 = %d\n", c1, c2);
            if (c1 > c2) goto Ltd;
            if (c1 < c2) goto Ltd_best;
            }

            td_ambig = td;
            return 0;

        Ltd_best:
            // td_best is the best match so far
            td_ambig = null;
            return 0;

        Ltd:
            // td is the new best match
            td_ambig = null;
            td_best = td;
            m_best = m;
            ti.tdtypes.setDim(dedtypes.length);
            memcpy(ti.tdtypes.tdata(), dedtypes.tdata(), ti.tdtypes.length * (void*).sizeof);
            return 0;
        });

        if (td_ambig)
        {
            .error(ti.loc, "%s `%s.%s` matches more than one template declaration:",
                td_best.kind(), td_best.parent.toPrettyChars(), td_best.ident.toChars());
            .errorSupplemental(td_best.loc, "`%s`\nand:", td_best.toChars());
            .errorSupplemental(td_ambig.loc, "`%s`", td_ambig.toChars());
            return false;
        }
        if (td_best)
        {
            if (!td_last)
                td_last = td_best;
            else if (td_last != td_best)
            {
                ScopeDsymbol.multiplyDefined(ti.loc, td_last, td_best);
                return false;
            }
        }
    }

    if (td_last)
    {
        /* https://issues.dlang.org/show_bug.cgi?id=7469
         * Normalize tiargs by using corresponding deduced
         * template value parameters and tuples for the correct mangling.
         *
         * By doing this before hasNestedArgs, CTFEable local variable will be
         * accepted as a value parameter. For example:
         *
         *  void foo() {
         *    struct S(int n) {}   // non-global template
         *    const int num = 1;   // CTFEable local variable
         *    S!num s;             // S!1 is instantiated, not S!num
         *  }
         */
        size_t dim = td_last.parameters.length - (td_last.isVariadic() ? 1 : 0);
        for (size_t i = 0; i < dim; i++)
        {
            if (ti.tiargs.length <= i)
                ti.tiargs.push(ti.tdtypes[i]);
            assert(i < ti.tiargs.length);

            auto tvp = (*td_last.parameters)[i].isTemplateValueParameter();
            if (!tvp)
                continue;
            assert(ti.tdtypes[i]);
            // tdtypes[i] is already normalized to the required type in matchArg

            (*ti.tiargs)[i] = ti.tdtypes[i];
        }
        if (td_last.isVariadic() && ti.tiargs.length == dim && ti.tdtypes[dim])
        {
            Tuple va = isTuple(ti.tdtypes[dim]);
            assert(va);
            ti.tiargs.pushSlice(va.objects[]);
        }
    }
    else if (ti.errors && ti.inst)
    {
        // instantiation was failed with error reporting
        assert(global.errors);
        return false;
    }
    else
    {
        auto tdecl = ti.tempdecl.isTemplateDeclaration();
        tdecl.computeOneMember();

        if (errs != global.errors)
            errorSupplemental(ti.loc, "while looking for match for `%s`", ti.toChars());
        else if (tdecl && !tdecl.overnext)
        {
            // Only one template, so we can give better error message
            const(char)* msg = "does not match template declaration";
            const(char)* tip;
            OutBuffer buf;
            HdrGenState hgs;
            hgs.skipConstraints = true;
            toCharsMaybeConstraints(tdecl, buf, hgs);
            const tmsg = buf.peekChars();
            const cmsg = tdecl.getConstraintEvalError(tip);
            if (cmsg)
            {
                .error(ti.loc, "%s `%s` %s `%s`\n%s", ti.kind, ti.toPrettyChars, msg, tmsg, cmsg);
                if (tip)
                    .tip(tip);
            }
            else
            {
                .error(ti.loc, "%s `%s` %s `%s`", ti.kind, ti.toPrettyChars, msg, tmsg);

                if (tdecl.parameters.length == ti.tiargs.length)
                {
                    // https://issues.dlang.org/show_bug.cgi?id=7352
                    // print additional information, e.g. `foo` is not a type
                    foreach (i, param; *tdecl.parameters)
                    {
                        MATCH match = param.matchArg(ti.loc, sc, ti.tiargs, i, tdecl.parameters, dedtypes, null);
                        auto arg = (*ti.tiargs)[i];
                        auto sym = arg.isDsymbol;
                        auto exp = arg.isExpression;

                        if (exp)
                            exp = exp.optimize(WANTvalue);

                        if (match == MATCH.nomatch &&
                            ((sym && sym.isFuncDeclaration) ||
                             (exp && exp.isVarExp)))
                        {
                            if (param.isTemplateTypeParameter)
                                errorSupplemental(ti.loc, "`%s` is not a type", arg.toChars);
                            else if (auto tvp = param.isTemplateValueParameter)
                                errorSupplemental(ti.loc, "`%s` is not of a value of type `%s`",
                                                  arg.toChars, tvp.valType.toChars);

                        }
                    }
                }
            }
        }
        else
        {
            .error(ti.loc, "%s `%s` does not match any template declaration", ti.kind(), ti.toPrettyChars());
            bool found;
            overloadApply(ti.tempdecl, (s){
                if (!found)
                    errorSupplemental(ti.loc, "Candidates are:");
                found = true;
                errorSupplemental(s.loc, "%s", s.toChars());
                return 0;
            });
        }
        return false;
    }

    /* The best match is td_last
     */
    ti.tempdecl = td_last;

    static if (LOG)
    {
        printf("\tIt's a match with template declaration '%s'\n", tempdecl.toChars());
    }
    return (errs == global.errors);
}

/*******************************************
 * Append to buf a textual representation of template parameters with their arguments.
 * Params:
 *  parameters = the template parameters
 *  tiargs = the correspondeing template arguments
 *  variadic = if it's a variadic argument list
 *  buf = where the text output goes
 */
private void formatParamsWithTiargs(ref TemplateParameters parameters, ref Objects tiargs, bool variadic, ref OutBuffer buf)
{
    buf.writestring("  with `");

    // write usual arguments line-by-line
    // skips trailing default ones - they are not present in `tiargs`
    const end = parameters.length - (variadic ? 1 : 0);
    size_t i;
    for (; i < tiargs.length && i < end; i++)
    {
        if (i)
        {
            buf.writeByte(',');
            buf.writenl();
            buf.writestring("       ");
        }
        write(buf, parameters[i]);
        buf.writestring(" = ");
        write(buf, tiargs[i]);
    }
    // write remaining variadic arguments on the last line
    if (variadic)
    {
        if (i)
        {
            buf.writeByte(',');
            buf.writenl();
            buf.writestring("       ");
        }
        write(buf, parameters[end]);
        buf.writestring(" = ");
        buf.writeByte('(');
        if (end < tiargs.length)
        {
            write(buf, tiargs[end]);
            foreach (j; parameters.length .. tiargs.length)
            {
                buf.writestring(", ");
                write(buf, tiargs[j]);
            }
        }
        buf.writeByte(')');
    }
    buf.writeByte('`');
}

/******************************
 * Create a scope for the parameters of the TemplateInstance
 * `ti` in the parent scope sc from the ScopeDsymbol paramsym.
 *
 * If paramsym is null a new ScopeDsymbol is used in place of
 * paramsym.
 * Params:
 *      td = template that ti is an instance of
 *      ti = the TemplateInstance whose parameters to generate the scope for.
 *      sc = the parent scope of ti
 * Returns:
 *      new scope for the parameters of ti
 */
private Scope* createScopeForTemplateParameters(TemplateDeclaration td, TemplateInstance ti, Scope* sc)
{
    ScopeDsymbol paramsym = new ScopeDsymbol();
    paramsym.parent = td._scope.parent;
    Scope* paramscope = td._scope.push(paramsym);
    paramscope.tinst = ti;
    paramscope.minst = sc.minst;
    paramscope.callsc = sc;
    paramscope.stc = STC.none;
    return paramscope;
}

/********************************************
 * Determine partial specialization order of `td` vs `td2`.
 * Params:
 *  sc = context
 *  td = first template
 *  td2 = second template
 *  argumentList = arguments to template
 * Returns:
 *      MATCH - td is at least as specialized as td2
 *      MATCH.nomatch - td2 is more specialized than td
 */
MATCH leastAsSpecialized(Scope* sc, TemplateDeclaration td, TemplateDeclaration td2, ArgumentList argumentList)
{
    enum LOG_LEASTAS = 0;
    static if (LOG_LEASTAS)
    {
        printf("%s.leastAsSpecialized(%s)\n", td.toChars(), td2.toChars());
    }

    /* This works by taking the template parameters to this template
     * declaration and feeding them to td2 as if it were a template
     * instance.
     * If it works, then this template is at least as specialized
     * as td2.
     */

    // Set type arguments to dummy template instance to be types
    // generated from the parameters to this template declaration
    auto tiargs = new Objects();
    tiargs.reserve(td.parameters.length);
    foreach (tp; *td.parameters)
    {
        if (tp.dependent)
            break;
        RootObject p = tp.dummyArg();
        if (!p) //TemplateTupleParameter
            break;

        tiargs.push(p);
    }
    scope TemplateInstance ti = new TemplateInstance(Loc.initial, td.ident, tiargs); // create dummy template instance

    // Temporary Array to hold deduced types
    Objects dedtypes = Objects(td2.parameters.length);

    // Attempt a type deduction
    MATCH m = matchWithInstance(sc, td2, ti, dedtypes, argumentList, 1);
    if (m > MATCH.nomatch)
    {
        /* A non-variadic template is more specialized than a
         * variadic one.
         */
        TemplateTupleParameter tp = td.isVariadic();
        if (tp && !tp.dependent && !td2.isVariadic())
            goto L1;

        static if (LOG_LEASTAS)
        {
            printf("  matches %d, so is least as specialized\n", m);
        }
        return m;
    }
L1:
    static if (LOG_LEASTAS)
    {
        printf("  doesn't match, so is not as specialized\n");
    }
    return MATCH.nomatch;
}

private RootObject defaultArg(TemplateParameter tp, Loc instLoc, Scope* sc)
{
    if (tp.isTemplateTupleParameter())
        return null;
    if (auto tpp = tp.isTemplateTypeParameter())
    {
        Type t = tpp.defaultType;
        if (t)
        {
            t = t.syntaxCopy();
            t = t.typeSemantic(tpp.loc, sc); // use the parameter loc
        }
        return t;
    }
    if (auto tap = tp.isTemplateAliasParameter())
    {
        RootObject da = tap.defaultAlias;
        if (auto ta = isType(tap.defaultAlias))
        {
            switch (ta.ty)
            {
            // If the default arg is a template, instantiate for each type
            case Tinstance:
            // same if the default arg is a mixin, traits, typeof
            // since the content might rely on a previous parameter
            // (https://issues.dlang.org/show_bug.cgi?id=23686)
            case Tmixin, Ttypeof, Ttraits :
                da = ta.syntaxCopy();
                break;
            default:
            }
        }

        RootObject o = aliasParameterSemantic(tap.loc, sc, da, null); // use the parameter loc
        return o;
    }
    if (auto tvp = tp.isTemplateValueParameter())
    {
        Expression e = tvp.defaultValue;
        if (!e)
            return null;

        e = e.syntaxCopy();
        Scope* sc2 = sc.push();
        sc2.inDefaultArg = true;
        e = e.expressionSemantic(sc2);
        sc2.pop();
        if (e is null)
            return null;
        if (auto te = e.isTemplateExp())
        {
            assert(sc && sc.tinst);
            if (te.td == sc.tinst.tempdecl)
            {
                // defaultValue is a reference to its template declaration
                // i.e: `template T(int arg = T)`
                // Raise error now before calling resolveProperties otherwise we'll
                // start looping on the expansion of the template instance.
                auto td = sc.tinst.tempdecl;
                .error(td.loc, "%s `%s` recursive template expansion", td.kind, td.toPrettyChars);
                return ErrorExp.get();
            }
        }
        if ((e = resolveProperties(sc, e)) is null)
            return null;
        e = e.resolveLoc(instLoc, sc); // use the instantiated loc
        e = e.optimize(WANTvalue);

        return e;
    }
    assert(0);
}

/*************************************************
 * Match function arguments against a specific template function.
 *
 * Params:
 *     td = template declaration for template instance
 *     ti = template instance. `ti.tdtypes` will be set to Expression/Type deduced template arguments
 *     sc = instantiation scope
 *     fd = Partially instantiated function declaration, which is set to an instantiated function declaration
 *     tthis = 'this' argument if !NULL
 *     argumentList = arguments to function
 *
 * Returns:
 *      match pair of initial and inferred template arguments
 */
private MATCHpair deduceFunctionTemplateMatch(TemplateDeclaration td, TemplateInstance ti, Scope* sc, ref FuncDeclaration fd, Type tthis, ArgumentList argumentList)
{
    version (none)
    {
        printf("\nTemplateDeclaration.deduceFunctionTemplateMatch() %s\n", td.toChars());
        for (size_t i = 0; i < (fargs ? fargs.length : 0); i++)
        {
            Expression e = (*fargs)[i];
            printf("\tfarg[%d] is %s, type is %s\n", cast(int) i, e.toChars(), e.type.toChars());
        }
        printf("fd = %s\n", fd.toChars());
        printf("fd.type = %s\n", fd.type.toChars());
        if (tthis)
            printf("tthis = %s\n", tthis.toChars());
    }

    assert(td._scope);

    auto dedargs = new Objects(td.parameters.length);
    dedargs.zero();

    Objects* dedtypes = &ti.tdtypes; // for T:T*, the dedargs is the T*, dedtypes is the T
    dedtypes.setDim(td.parameters.length);
    dedtypes.zero();

    if (td.errors || fd.errors)
        return MATCHpair(MATCH.nomatch, MATCH.nomatch);

    // Set up scope for parameters
    Scope* paramscope = createScopeForTemplateParameters(td, ti,sc);

    MATCHpair nomatch()
    {
        paramscope.pop();
        //printf("\tnomatch\n");
        return MATCHpair(MATCH.nomatch, MATCH.nomatch);
    }

    MATCHpair matcherror()
    {
        // todo: for the future improvement
        paramscope.pop();
        //printf("\terror\n");
        return MATCHpair(MATCH.nomatch, MATCH.nomatch);
    }
    // Mark the parameter scope as deprecated if the templated
    // function is deprecated (since paramscope.enclosing is the
    // calling scope already)
    paramscope.stc |= fd.storage_class & STC.deprecated_;

    TemplateTupleParameter tp = td.isVariadic();
    Tuple declaredTuple = null;

    version (none)
    {
        for (size_t i = 0; i < dedargs.length; i++)
        {
            printf("\tdedarg[%d] = ", i);
            RootObject oarg = (*dedargs)[i];
            if (oarg)
                printf("%s", oarg.toChars());
            printf("\n");
        }
    }

    size_t ntargs = 0; // array size of tiargs
    size_t inferStart = 0; // index of first template parameter to infer from function argument
    const Loc instLoc = ti.loc;
    MATCH matchTiargs = MATCH.exact;

    if (auto tiargs = ti.tiargs)
    {
        // Set initial template arguments
        ntargs = tiargs.length;
        size_t n = td.parameters.length;
        if (tp)
            n--;
        if (ntargs > n)
        {
            if (!tp)
                return nomatch();

            /* The extra initial template arguments
             * now form the tuple argument.
             */
            auto t = new Tuple(ntargs - n);
            assert(td.parameters.length);
            (*dedargs)[td.parameters.length - 1] = t;

            for (size_t i = 0; i < t.objects.length; i++)
            {
                t.objects[i] = (*tiargs)[n + i];
            }
            td.declareParameter(paramscope, tp, t);
            declaredTuple = t;
        }
        else
            n = ntargs;

        memcpy(dedargs.tdata(), tiargs.tdata(), n * (*dedargs.tdata()).sizeof);

        for (size_t i = 0; i < n; i++)
        {
            assert(i < td.parameters.length);
            Declaration sparam = null;
            MATCH m = (*td.parameters)[i].matchArg(instLoc, paramscope, dedargs, i, td.parameters, *dedtypes, &sparam);
            //printf("\tdeduceType m = %d\n", m);
            if (m == MATCH.nomatch)
                return nomatch();
            if (m < matchTiargs)
                matchTiargs = m;

            sparam.dsymbolSemantic(paramscope);
            if (!paramscope.insert(sparam))
                return nomatch();
        }
        if (n < td.parameters.length && !declaredTuple)
        {
            inferStart = n;
        }
        else
            inferStart = td.parameters.length;
        //printf("tiargs matchTiargs = %d\n", matchTiargs);
    }
    version (none)
    {
        for (size_t i = 0; i < dedargs.length; i++)
        {
            printf("\tdedarg[%d] = ", i);
            RootObject oarg = (*dedargs)[i];
            if (oarg)
                printf("%s", oarg.toChars());
            printf("\n");
        }
    }

    ParameterList fparameters = fd.getParameterList(); // function parameter list
    const nfparams = fparameters.length; // number of function parameters

    /* Check for match of function arguments with variadic template
     * parameter, such as:
     *
     * void foo(T, A...)(T t, A a);
     * void main() { foo(1,2,3); }
     */
    size_t fptupindex = IDX_NOTFOUND;
    if (tp) // if variadic
    {
        // TemplateTupleParameter always makes most lesser matching.
        matchTiargs = MATCH.convert;

        if (nfparams == 0 && argumentList.length != 0) // if no function parameters
        {
            if (!declaredTuple)
            {
                auto t = new Tuple();
                //printf("t = %p\n", t);
                (*dedargs)[td.parameters.length - 1] = t;
                td.declareParameter(paramscope, tp, t);
                declaredTuple = t;
            }
        }
        else
        {
            /* Figure out which of the function parameters matches
             * the tuple template parameter. Do this by matching
             * type identifiers.
             * Set the index of this function parameter to fptupindex.
             */
            for (fptupindex = 0; fptupindex < nfparams; fptupindex++)
            {
                auto fparam = (*fparameters.parameters)[fptupindex]; // fparameters[fptupindex] ?
                if (fparam.type.ty != Tident)
                    continue;
                TypeIdentifier tid = fparam.type.isTypeIdentifier();
                if (!tp.ident.equals(tid.ident) || tid.idents.length)
                    continue;

                if (fparameters.varargs != VarArg.none) // variadic function doesn't
                    return nomatch(); // go with variadic template

                goto L1;
            }
            fptupindex = IDX_NOTFOUND;
        L1:
        }
    }

    MATCH match = MATCH.exact;
    if (td.toParent().isModule())
        tthis = null;
    if (tthis)
    {
        bool hasttp = false;

        // Match 'tthis' to any TemplateThisParameter's
        foreach (param; *td.parameters)
        {
            if (auto ttp = param.isTemplateThisParameter())
            {
                hasttp = true;

                Type t = new TypeIdentifier(Loc.initial, ttp.ident);
                MATCH m = deduceType(tthis, paramscope, t, *td.parameters, *dedtypes);
                if (m == MATCH.nomatch)
                    return nomatch();
                if (m < match)
                    match = m; // pick worst match
            }
        }

        // Match attributes of tthis against attributes of fd
        if (fd.type && !fd.isCtorDeclaration() && !(td._scope.stc & STC.static_))
        {
            STC stc = td._scope.stc | fd.storage_class2;
            // Propagate parent storage class, https://issues.dlang.org/show_bug.cgi?id=5504
            Dsymbol p = td.parent;
            while (p.isTemplateDeclaration() || p.isTemplateInstance())
                p = p.parent;
            AggregateDeclaration ad = p.isAggregateDeclaration();
            if (ad)
                stc |= ad.storage_class;

            ubyte mod = fd.type.mod;
            if (stc & STC.immutable_)
                mod = MODFlags.immutable_;
            else
            {
                if (stc & (STC.shared_ | STC.synchronized_))
                    mod |= MODFlags.shared_;
                if (stc & STC.const_)
                    mod |= MODFlags.const_;
                if (stc & STC.wild)
                    mod |= MODFlags.wild;
            }

            ubyte thismod = tthis.mod;
            if (hasttp)
                mod = MODmerge(thismod, mod);
            MATCH m = MODmethodConv(thismod, mod);
            if (m == MATCH.nomatch)
                return nomatch();
            if (m < match)
                match = m;
        }
    }

    // Loop through the function parameters
    {
        //printf("%s\n\tnfargs = %d, nfparams = %d, tuple_dim = %d\n", toChars(), nfargs, nfparams, declaredTuple ? declaredTuple.objects.length : 0);
        //printf("\ttp = %p, fptupindex = %d, found = %d, declaredTuple = %s\n", tp, fptupindex, fptupindex != IDX_NOTFOUND, declaredTuple ? declaredTuple.toChars() : NULL);
        enum DEFAULT_ARGI = size_t.max - 10; // pseudo index signifying the parameter is expected to be assigned its default argument
        size_t argi = 0; // current argument index
        size_t argsConsumed = 0; // to ensure no excess arguments
        size_t nfargs2 = argumentList.length; // total number of arguments including applied defaultArgs
        uint inoutMatch = 0; // for debugging only
        Expression[] fargs = argumentList.arguments ? (*argumentList.arguments)[] : null;
        ArgumentLabel[] fnames = argumentList.names ? (*argumentList.names)[] : null;

        for (size_t parami = 0; parami < nfparams; parami++)
        {
            Parameter fparam = fparameters[parami];

            // Apply function parameter storage classes to parameter types
            Type prmtype = fparam.type.addStorageClass(fparam.storageClass);

            Expression farg;
            Identifier fname = argi < fnames.length ? fnames[argi].name : null;
            bool foundName = false;
            if (fparam.ident)
            {
                foreach (i; 0 .. fnames.length)
                {
                    if (fparam.ident == fnames[i].name)
                    {
                        argi = i;
                        foundName = true;
                        break;  //Exits the loop after a match
                    }
                }
            }
            if (fname && !foundName)
            {
                argi = DEFAULT_ARGI;
            }

            /* See function parameters which wound up
             * as part of a template tuple parameter.
             */
            if (fptupindex != IDX_NOTFOUND && parami == fptupindex && argi != DEFAULT_ARGI)
            {
                TypeIdentifier tid = prmtype.isTypeIdentifier();
                assert(tid);
                if (!declaredTuple)
                {
                    /* The types of the function arguments
                     * now form the tuple argument.
                     */
                    declaredTuple = new Tuple();
                    (*dedargs)[td.parameters.length - 1] = declaredTuple;

                    /* Count function parameters with no defaults following a tuple parameter.
                     * void foo(U, T...)(int y, T, U, double, int bar = 0) {}  // rem == 2 (U, double)
                     */
                    size_t rem = 0;
                    foreach (j; parami + 1 .. nfparams)
                    {
                        Parameter p = fparameters[j];
                        if (p.defaultArg)
                        {
                            break;
                        }
                        foreach(argLabel; fnames)
                        {
                            if (p.ident == argLabel.name)
                                break;
                        }
                        if (!reliesOnTemplateParameters(p.type, (*td.parameters)[inferStart .. td.parameters.length]))
                        {
                            Type pt = p.type.syntaxCopy().typeSemantic(fd.loc, paramscope);
                            if (auto ptt = pt.isTypeTuple())
                                rem += ptt.arguments.length;
                            else
                                rem += 1;
                        }
                        else
                        {
                            ++rem;
                        }
                    }

                    if (nfargs2 - argi < rem)
                        return nomatch();
                    declaredTuple.objects.setDim(nfargs2 - argi - rem);
                    foreach (i; 0 .. declaredTuple.objects.length)
                    {
                        farg = fargs[argi + i];

                        // Check invalid arguments to detect errors early.
                        if (farg.op == EXP.error || farg.type.ty == Terror)
                            return nomatch();

                        if (!fparam.isLazy() && farg.type.ty == Tvoid)
                            return nomatch();

                        Type tt;
                        MATCH m;
                        if (ubyte wm = deduceWildHelper(farg.type, &tt, tid))
                        {
                            inoutMatch |= wm;
                            m = MATCH.constant;
                        }
                        else
                        {
                            m = deduceTypeHelper(farg.type, tt, tid);
                        }
                        if (m == MATCH.nomatch)
                            return nomatch();
                        if (m < match)
                            match = m;

                        /* Remove top const for dynamic array types and pointer types
                         */
                        if ((tt.ty == Tarray || tt.ty == Tpointer) && !tt.isMutable() && (!(fparam.storageClass & STC.ref_) || (fparam.storageClass & STC.auto_) && !farg.isLvalue()))
                        {
                            tt = tt.mutableOf();
                        }
                        declaredTuple.objects[i] = tt;
                    }
                    td.declareParameter(paramscope, tp, declaredTuple);
                }
                else
                {
                    // https://issues.dlang.org/show_bug.cgi?id=6810
                    // If declared tuple is not a type tuple,
                    // it cannot be function parameter types.
                    for (size_t i = 0; i < declaredTuple.objects.length; i++)
                    {
                        if (!isType(declaredTuple.objects[i]))
                            return nomatch();
                    }
                }
                assert(declaredTuple);
                argi += declaredTuple.objects.length;
                argsConsumed += declaredTuple.objects.length;
                continue;
            }

            // If parameter type doesn't depend on inferred template parameters,
            // semantic it to get actual type.
            if (!reliesOnTemplateParameters(prmtype, (*td.parameters)[inferStart .. td.parameters.length]))
            {
                // should copy prmtype to avoid affecting semantic result
                prmtype = prmtype.syntaxCopy().typeSemantic(fd.loc, paramscope);

                if (TypeTuple tt = prmtype.isTypeTuple())
                {
                    const tt_dim = tt.arguments.length;
                    for (size_t j = 0; j < tt_dim; j++, ++argi, ++argsConsumed)
                    {
                        Parameter p = (*tt.arguments)[j];
                        if (j == tt_dim - 1 && fparameters.varargs == VarArg.typesafe &&
                            parami + 1 == nfparams && argi < fargs.length)
                        {
                            prmtype = p.type;
                            goto Lvarargs;
                        }
                        if (argi >= fargs.length)
                        {
                            if (p.defaultArg)
                                continue;

                            // https://issues.dlang.org/show_bug.cgi?id=19888
                            if (fparam.defaultArg)
                                break;

                            return nomatch();
                        }
                        farg = fargs[argi];
                        if (!farg.implicitConvTo(p.type))
                            return nomatch();
                    }
                    continue;
                }
            }

            if (argi >= fargs.length) // if not enough arguments
            {
                if (!fparam.defaultArg)
                    goto Lvarargs;

                /* https://issues.dlang.org/show_bug.cgi?id=2803
                 * Before the starting of type deduction from the function
                 * default arguments, set the already deduced parameters into paramscope.
                 * It's necessary to avoid breaking existing acceptable code. Cases:
                 *
                 * 1. Already deduced template parameters can appear in fparam.defaultArg:
                 *  auto foo(A, B)(A a, B b = A.stringof);
                 *  foo(1);
                 *  // at fparam == 'B b = A.string', A is equivalent with the deduced type 'int'
                 *
                 * 2. If prmtype depends on default-specified template parameter, the
                 * default type should be preferred.
                 *  auto foo(N = size_t, R)(R r, N start = 0)
                 *  foo([1,2,3]);
                 *  // at fparam `N start = 0`, N should be 'size_t' before
                 *  // the deduction result from fparam.defaultArg.
                 */
                if (argi == fargs.length)
                {
                    foreach (ref dedtype; *dedtypes)
                    {
                        Type at = isType(dedtype);
                        if (at && at.ty == Tnone)
                        {
                            TypeDeduced xt = cast(TypeDeduced)at;
                            dedtype = xt.tded; // 'unbox'
                        }
                    }
                    for (size_t i = ntargs; i < dedargs.length; i++)
                    {
                        TemplateParameter tparam = (*td.parameters)[i];

                        RootObject oarg = (*dedargs)[i];
                        RootObject oded = (*dedtypes)[i];
                        if (oarg)
                            continue;

                        if (oded)
                        {
                            if (tparam.specialization() || !tparam.isTemplateTypeParameter())
                            {
                                /* The specialization can work as long as afterwards
                                 * the oded == oarg
                                 */
                                (*dedargs)[i] = oded;
                                MATCH m2 = tparam.matchArg(instLoc, paramscope, dedargs, i, td.parameters, *dedtypes, null);
                                //printf("m2 = %d\n", m2);
                                if (m2 == MATCH.nomatch)
                                    return nomatch();
                                if (m2 < matchTiargs)
                                    matchTiargs = m2; // pick worst match
                                if (!rootObjectsEqual((*dedtypes)[i], oded))
                                    .error(td.loc, "%s `%s` specialization not allowed for deduced parameter `%s`",
                                        td.kind, td.toPrettyChars, td.kind, td.toPrettyChars, tparam.ident.toChars());
                            }
                            else
                            {
                                if (MATCH.convert < matchTiargs)
                                    matchTiargs = MATCH.convert;
                            }
                            (*dedargs)[i] = td.declareParameter(paramscope, tparam, oded);
                        }
                        else
                        {
                            oded = tparam.defaultArg(instLoc, paramscope);
                            if (oded)
                                (*dedargs)[i] = td.declareParameter(paramscope, tparam, oded);
                        }
                    }
                }

                if (argi != DEFAULT_ARGI)
                    nfargs2 = argi + 1;

                /* If prmtype does not depend on any template parameters:
                 *
                 *  auto foo(T)(T v, double x = 0);
                 *  foo("str");
                 *  // at fparam == 'double x = 0'
                 *
                 * or, if all template parameters in the prmtype are already deduced:
                 *
                 *  auto foo(R)(R range, ElementType!R sum = 0);
                 *  foo([1,2,3]);
                 *  // at fparam == 'ElementType!R sum = 0'
                 *
                 * Deducing prmtype from fparam.defaultArg is not necessary.
                 */
                if (prmtype.deco || prmtype.syntaxCopy().trySemantic(td.loc, paramscope))
                {
                    if (argi != DEFAULT_ARGI)
                    {
                        ++argi;
                        ++argsConsumed;
                    }
                    continue;
                }

                // Deduce prmtype from the defaultArg.
                farg = fparam.defaultArg.syntaxCopy();
                farg = farg.expressionSemantic(paramscope);
                farg = resolveProperties(paramscope, farg);
            }
            else
            {
                farg = fargs[argi];
            }
            {
                assert(farg);
                // Check invalid arguments to detect errors early.
                if (farg.op == EXP.error || farg.type.ty == Terror)
                    return nomatch();

                Type att = null;
            Lretry:
                version (none)
                {
                    printf("\tfarg.type   = %s\n", farg.type.toChars());
                    printf("\tfparam.type = %s\n", prmtype.toChars());
                }
                Type argtype = farg.type;

                if (!fparam.isLazy() && argtype.ty == Tvoid && farg.op != EXP.function_)
                    return nomatch();

                // https://issues.dlang.org/show_bug.cgi?id=12876
                // Optimize argument to allow CT-known length matching
                farg = farg.optimize(WANTvalue, fparam.isReference());
                //printf("farg = %s %s\n", farg.type.toChars(), farg.toChars());

                RootObject oarg = farg;

                if (farg.isFuncExp())
                {
                    // When assigning an untyped (void) lambda `x => y` to a `(F)(ref F)` parameter,
                    // we don't want to deduce type void creating a void parameter
                }
                else if ((fparam.storageClass & STC.ref_) && (!(fparam.storageClass & STC.auto_) || farg.isLvalue()))
                {
                    /* Allow expressions that have CT-known boundaries and type [] to match with [dim]
                     */
                    bool inferIndexType = (argtype.ty == Tarray) && (prmtype.ty == Tsarray || prmtype.ty == Taarray);
                    if (auto aaType = prmtype.isTypeAArray())
                    {
                        if (auto indexType = aaType.index.isTypeIdentifier())
                        {
                            inferIndexType = indexType.idents.length == 0;
                        }
                    }
                    if (inferIndexType)
                    {
                        if (StringExp se = farg.isStringExp())
                        {
                            argtype = se.type.nextOf().sarrayOf(se.len);
                        }
                        else if (ArrayLiteralExp ae = farg.isArrayLiteralExp())
                        {
                            argtype = ae.type.nextOf().sarrayOf(ae.elements.length);
                        }
                        else if (SliceExp se = farg.isSliceExp())
                        {
                            if (Type tsa = toStaticArrayType(se))
                                argtype = tsa;
                        }
                    }

                    oarg = argtype;
                }
                else if ((fparam.storageClass & STC.out_) == 0 &&
                         (argtype.ty == Tarray || argtype.ty == Tpointer) &&
                         templateParameterLookup(prmtype, td.parameters) != IDX_NOTFOUND &&
                         prmtype.isTypeIdentifier().idents.length == 0)
                {
                    /* The farg passing to the prmtype always make a copy. Therefore,
                     * we can shrink the set of the deduced type arguments for prmtype
                     * by adjusting top-qualifier of the argtype.
                     *
                     *  prmtype         argtype     ta
                     *  T            <- const(E)[]  const(E)[]
                     *  T            <- const(E[])  const(E)[]
                     *  qualifier(T) <- const(E)[]  const(E[])
                     *  qualifier(T) <- const(E[])  const(E[])
                     */
                    Type ta = argtype.castMod(prmtype.mod ? argtype.nextOf().mod : 0);
                    if (ta != argtype)
                    {
                        Expression ea = farg.copy();
                        ea.type = ta;
                        oarg = ea;
                    }
                }

                if (fparameters.varargs == VarArg.typesafe && parami + 1 == nfparams && argi + 1 < fargs.length)
                    goto Lvarargs;

                uint im = 0;
                MATCH m = deduceType(oarg, paramscope, prmtype, *td.parameters, *dedtypes, &im, inferStart);
                //printf("\tL%d deduceType m = %d, im = x%x, inoutMatch = x%x\n", __LINE__, m, im, inoutMatch);
                inoutMatch |= im;

                /* If no match, see if the argument can be matched by using
                 * implicit conversions.
                 */
                if (m == MATCH.nomatch && prmtype.deco)
                    m = farg.implicitConvTo(prmtype);

                if (m == MATCH.nomatch)
                {
                    AggregateDeclaration ad = isAggregate(farg.type);
                    if (ad && ad.aliasthis && !isRecursiveAliasThis(att, argtype))
                    {
                        // https://issues.dlang.org/show_bug.cgi?id=12537
                        // The isRecursiveAliasThis() call above

                        /* If a semantic error occurs while doing alias this,
                         * eg purity(https://issues.dlang.org/show_bug.cgi?id=7295),
                         * just regard it as not a match.
                         *
                         * We also save/restore sc.func.flags to avoid messing up
                         * attribute inference in the evaluation.
                        */
                        const oldflags = sc.func ? sc.func.saveFlags : 0;
                        auto e = resolveAliasThis(sc, farg, true);
                        if (sc.func)
                            sc.func.restoreFlags(oldflags);
                        if (e)
                        {
                            farg = e;
                            goto Lretry;
                        }
                    }
                }

                if (m > MATCH.nomatch && (fparam.storageClass & (STC.ref_ | STC.auto_)) == STC.ref_)
                {
                    if (!farg.isLvalue())
                    {
                        if ((farg.op == EXP.string_ || farg.op == EXP.slice) && (prmtype.ty == Tsarray || prmtype.ty == Taarray))
                        {
                            // Allow conversion from T[lwr .. upr] to ref T[upr-lwr]
                        }
                        else if (sc.previews.rvalueRefParam)
                        {
                            // Allow implicit conversion to ref
                        }
                        else
                            return nomatch();
                    }
                }
                if (m > MATCH.nomatch && (fparam.storageClass & STC.out_))
                {
                    if (!farg.isLvalue())
                        return nomatch();
                    if (!farg.type.isMutable()) // https://issues.dlang.org/show_bug.cgi?id=11916
                        return nomatch();
                }
                if (m == MATCH.nomatch && fparam.isLazy() && prmtype.ty == Tvoid && farg.type.ty != Tvoid)
                    m = MATCH.convert;
                if (m != MATCH.nomatch)
                {
                    if (m < match)
                        match = m; // pick worst match
                    if (argi != DEFAULT_ARGI)
                    {
                        argi++;
                        argsConsumed++;
                    }
                    continue;
                }
            }

        Lvarargs:
            /* The following code for variadic arguments closely
             * matches TypeFunction.callMatch()
             */
            if (!(fparameters.varargs == VarArg.typesafe && parami + 1 == nfparams))
                return nomatch();

            /* Check for match with function parameter T...
             */
            Type tb = prmtype.toBasetype();
            switch (tb.ty)
            {
                // 6764 fix - TypeAArray may be TypeSArray have not yet run semantic().
            case Tsarray:
            case Taarray:
                {
                    // Perhaps we can do better with this, see TypeFunction.callMatch()
                    if (TypeSArray tsa = tb.isTypeSArray())
                    {
                        dinteger_t sz = tsa.dim.toInteger();
                        if (sz != fargs.length - argi)
                            return nomatch();
                    }
                    else if (TypeAArray taa = tb.isTypeAArray())
                    {
                        Expression dim = new IntegerExp(instLoc, fargs.length - argi, Type.tsize_t);

                        size_t i = templateParameterLookup(taa.index, td.parameters);
                        if (i == IDX_NOTFOUND)
                        {
                            Expression e;
                            Type t;
                            Dsymbol s;
                            Scope* sco;

                            const errors = global.startGagging();
                            /* ref: https://issues.dlang.org/show_bug.cgi?id=11118
                             * The parameter isn't part of the template
                             * ones, let's try to find it in the
                             * instantiation scope 'sc' and the one
                             * belonging to the template itself. */
                            sco = sc;
                            taa.index.resolve(instLoc, sco, e, t, s);
                            if (!e)
                            {
                                sco = paramscope;
                                taa.index.resolve(instLoc, sco, e, t, s);
                            }
                            global.endGagging(errors);

                            if (!e)
                                return nomatch();

                            e = e.ctfeInterpret();
                            e = e.implicitCastTo(sco, Type.tsize_t);
                            e = e.optimize(WANTvalue);
                            if (!dim.equals(e))
                                return nomatch();
                        }
                        else
                        {
                            // This code matches code in TypeInstance.deduceType()
                            TemplateParameter tprm = (*td.parameters)[i];
                            TemplateValueParameter tvp = tprm.isTemplateValueParameter();
                            if (!tvp)
                                return nomatch();
                            Expression e = cast(Expression)(*dedtypes)[i];
                            if (e)
                            {
                                if (!dim.equals(e))
                                    return nomatch();
                            }
                            else
                            {
                                Type vt = tvp.valType.typeSemantic(Loc.initial, sc);
                                MATCH m = dim.implicitConvTo(vt);
                                if (m == MATCH.nomatch)
                                    return nomatch();
                                (*dedtypes)[i] = dim;
                            }
                        }
                    }
                    goto case Tarray;
                }
            case Tarray:
                {
                    TypeArray ta = cast(TypeArray)tb;
                    Type tret = fparam.isLazyArray();
                    for (; argi < fargs.length; argi++)
                    {
                        Expression arg = fargs[argi];
                        assert(arg);

                        MATCH m;
                        /* If lazy array of delegates,
                         * convert arg(s) to delegate(s)
                         */
                        if (tret)
                        {
                            if (ta.next.equals(arg.type))
                            {
                                m = MATCH.exact;
                            }
                            else
                            {
                                m = arg.implicitConvTo(tret);
                                if (m == MATCH.nomatch)
                                {
                                    if (tret.toBasetype().ty == Tvoid)
                                        m = MATCH.convert;
                                }
                            }
                        }
                        else
                        {
                            uint wm = 0;
                            m = deduceType(arg, paramscope, ta.next, *td.parameters, *dedtypes, &wm, inferStart);
                            inoutMatch |= wm;
                        }
                        if (m == MATCH.nomatch)
                            return nomatch();
                        if (m < match)
                            match = m;
                    }
                    goto Lmatch;
                }
            case Tclass:
            case Tident:
                goto Lmatch;

            default:
                return nomatch();
            }
            assert(0);
        }
        // printf(". argi = %d, nfargs = %d, nfargs2 = %d, argsConsumed = %d\n", cast(int) argi, cast(int) nfargs, cast(int) nfargs2, cast(int) argsConsumed);
        if (argsConsumed != nfargs2 && fparameters.varargs == VarArg.none)
            return nomatch();
    }

Lmatch:
    foreach (ref dedtype; *dedtypes)
    {
        if (Type at = isType(dedtype))
        {
            if (at.ty == Tnone)
            {
                TypeDeduced xt = cast(TypeDeduced)at;
                at = xt.tded; // 'unbox'
            }
            dedtype = at.merge2();
        }
    }
    for (size_t i = ntargs; i < dedargs.length; i++)
    {
        TemplateParameter tparam = (*td.parameters)[i];
        //printf("tparam[%d] = %s\n", i, tparam.ident.toChars());

        /* For T:T*, the dedargs is the T*, dedtypes is the T
         * But for function templates, we really need them to match
         */
        RootObject oarg = (*dedargs)[i];
        RootObject oded = (*dedtypes)[i];
        //printf("1dedargs[%d] = %p, dedtypes[%d] = %p\n", i, oarg, i, oded);
        //if (oarg) printf("oarg: %s\n", oarg.toChars());
        //if (oded) printf("oded: %s\n", oded.toChars());
        if (oarg)
            continue;

        if (oded)
        {
            if (tparam.specialization() || !tparam.isTemplateTypeParameter())
            {
                /* The specialization can work as long as afterwards
                 * the oded == oarg
                 */
                (*dedargs)[i] = oded;
                MATCH m2 = tparam.matchArg(instLoc, paramscope, dedargs, i, td.parameters, *dedtypes, null);
                //printf("m2 = %d\n", m2);
                if (m2 == MATCH.nomatch)
                    return nomatch();
                if (m2 < matchTiargs)
                    matchTiargs = m2; // pick worst match
                if (!rootObjectsEqual((*dedtypes)[i],oded))
                    .error(td.loc, "%s `%s` specialization not allowed for deduced parameter `%s`", td.kind, td.toPrettyChars, tparam.ident.toChars());
            }
            else
            {
                // Discussion: https://issues.dlang.org/show_bug.cgi?id=16484
                if (MATCH.convert < matchTiargs)
                    matchTiargs = MATCH.convert;
            }
        }
        else
        {
            oded = tparam.defaultArg(instLoc, paramscope);
            if (!oded)
            {
                // if tuple parameter and
                // tuple parameter was not in function parameter list and
                // we're one or more arguments short (i.e. no tuple argument)
                if (tparam == tp &&
                    fptupindex == IDX_NOTFOUND &&
                    ntargs <= dedargs.length - 1)
                {
                    // make tuple argument an empty tuple
                    oded = new Tuple();
                }
                else
                    return nomatch();
            }
            if (isError(oded))
                return matcherror();
            ntargs++;

            /* At the template parameter T, the picked default template argument
             * X!int should be matched to T in order to deduce dependent
             * template parameter A.
             *  auto foo(T : X!A = X!int, A...)() { ... }
             *  foo();  // T <-- X!int, A <-- (int)
             */
            if (tparam.specialization())
            {
                (*dedargs)[i] = oded;
                MATCH m2 = tparam.matchArg(instLoc, paramscope, dedargs, i, td.parameters, *dedtypes, null);
                //printf("m2 = %d\n", m2);
                if (m2 == MATCH.nomatch)
                    return nomatch();
                if (m2 < matchTiargs)
                    matchTiargs = m2; // pick worst match
                if (!rootObjectsEqual((*dedtypes)[i], oded))
                    .error(td.loc, "%s `%s` specialization not allowed for deduced parameter `%s`", td.kind, td.toPrettyChars, tparam.ident.toChars());
            }
        }
        oded = td.declareParameter(paramscope, tparam, oded);
        (*dedargs)[i] = oded;
    }

    /* https://issues.dlang.org/show_bug.cgi?id=7469
     * As same as the code for 7469 in findBestMatch,
     * expand a Tuple in dedargs to normalize template arguments.
     */
    if (auto d = dedargs.length)
    {
        if (auto va = isTuple((*dedargs)[d - 1]))
        {
            dedargs.setDim(d - 1);
            dedargs.insert(d - 1, &va.objects);
        }
    }
    ti.tiargs = dedargs; // update to the normalized template arguments.

    // Partially instantiate function for constraint and fd.leastAsSpecialized()
    {
        assert(paramscope.scopesym);
        Scope* sc2 = td._scope;
        sc2 = sc2.push(paramscope.scopesym);
        sc2 = sc2.push(ti);
        sc2.parent = ti;
        sc2.tinst = ti;
        sc2.minst = sc.minst;
        sc2.stc |= fd.storage_class & STC.deprecated_;

        fd = doHeaderInstantiation(td, ti, sc2, fd, tthis, argumentList);
        sc2 = sc2.pop();
        sc2 = sc2.pop();

        if (!fd)
            return nomatch();
    }

    if (td.constraint)
    {
        if (!evaluateConstraint(td, ti, sc, paramscope, dedargs, fd))
            return nomatch();
    }

    version (none)
    {
        for (size_t i = 0; i < dedargs.length; i++)
        {
            RootObject o = (*dedargs)[i];
            printf("\tdedargs[%d] = %d, %s\n", i, o.dyncast(), o.toChars());
        }
    }

    paramscope.pop();
    //printf("\tmatch %d\n", match);
    return MATCHpair(matchTiargs, match);
}

/*************************************************
 * Limited function template instantiation for using fd.leastAsSpecialized()
 */
private
FuncDeclaration doHeaderInstantiation(TemplateDeclaration td, TemplateInstance ti, Scope* sc2, FuncDeclaration fd, Type tthis, ArgumentList inferenceArguments)
{
    assert(fd);
    version (none)
    {
        printf("doHeaderInstantiation this = %s\n", toChars());
    }

    // function body and contracts are not need
    if (fd.isCtorDeclaration())
        fd = new CtorDeclaration(fd.loc, fd.endloc, fd.storage_class, fd.type.syntaxCopy());
    else
        fd = new FuncDeclaration(fd.loc, fd.endloc, fd.ident, fd.storage_class, fd.type.syntaxCopy());
    fd.parent = ti;

    assert(fd.type.ty == Tfunction);
    auto tf = fd.type.isTypeFunction();
    tf.inferenceArguments = inferenceArguments;

    if (tthis)
    {
        // Match 'tthis' to any TemplateThisParameter's
        bool hasttp = false;
        foreach (tp; *td.parameters)
        {
            TemplateThisParameter ttp = tp.isTemplateThisParameter();
            if (ttp)
                hasttp = true;
        }
        if (hasttp)
        {
            tf = tf.addSTC(ModToStc(tthis.mod)).isTypeFunction();
            assert(!tf.deco);
        }
    }

    Scope* scx = sc2.push();

    // Shouldn't run semantic on default arguments and return type.
    foreach (ref params; *tf.parameterList.parameters)
        params.defaultArg = null;
    tf.incomplete = true;

    if (fd.isCtorDeclaration())
    {
        // For constructors, emitting return type is necessary for
        // isReturnIsolated() in functionResolve.
        tf.isCtor = true;

        Dsymbol parent = td.toParentDecl();
        Type tret;
        AggregateDeclaration ad = parent.isAggregateDeclaration();
        if (!ad || parent.isUnionDeclaration())
        {
            tret = Type.tvoid;
        }
        else
        {
            tret = ad.handleType();
            assert(tret);
            tret = tret.addStorageClass(fd.storage_class | scx.stc);
            tret = tret.addMod(tf.mod);
        }
        tf.next = tret;
        if (ad && ad.isStructDeclaration())
            tf.isRef = 1;
        //printf("tf = %s\n", tf.toChars());
    }
    else
        tf.next = null;
    fd.type = tf;
    fd.type = fd.type.addSTC(scx.stc);
    fd.type = fd.type.typeSemantic(fd.loc, scx);
    scx = scx.pop();

    if (fd.type.ty != Tfunction)
        return null;

    fd.originalType = fd.type; // for mangling
    //printf("\t[%s] fd.type = %s, mod = %x, ", loc.toChars(), fd.type.toChars(), fd.type.mod);
    //printf("fd.needThis() = %d\n", fd.needThis());

    return fd;
}

/****************************************************
 * Declare parameters of template instance, initialize them with the
 * template instance arguments.
 */
void declareParameters(TemplateInstance ti, Scope* sc)
{
    TemplateDeclaration tempdecl = ti.tempdecl.isTemplateDeclaration();
    assert(tempdecl);

    //printf("TemplateInstance.declareParameters()\n");
    foreach (i, o; ti.tdtypes) // initializer for tp
    {
        TemplateParameter tp = (*tempdecl.parameters)[i];
        //printf("\ttdtypes[%d] = %p\n", i, o);
        declareParameter(tempdecl, sc, tp, o);
    }
}
/**************************************************
 * Declare template parameter tp with value o, and install it in the scope sc.
 */
private RootObject declareParameter(TemplateDeclaration td, Scope* sc, TemplateParameter tp, RootObject o)
{
    //printf("TemplateDeclaration.declareParameter('%s', o = %p)\n", tp.ident.toChars(), o);
    Type ta = isType(o);
    Expression ea = isExpression(o);
    Dsymbol sa = isDsymbol(o);
    Tuple va = isTuple(o);

    Declaration d;
    VarDeclaration v = null;

    if (ea)
    {
        if (ea.op == EXP.type)
            ta = ea.type;
        else if (auto se = ea.isScopeExp())
            sa = se.sds;
        else if (auto te = ea.isThisExp())
            sa = te.var;
        else if (auto se = ea.isSuperExp())
            sa = se.var;
        else if (auto fe = ea.isFuncExp())
        {
            if (fe.td)
                sa = fe.td;
            else
                sa = fe.fd;
        }
    }

    if (ta)
    {
        //printf("type %s\n", ta.toChars());
        auto ad = new AliasDeclaration(Loc.initial, tp.ident, ta);
        ad.storage_class |= STC.templateparameter;
        d = ad;
    }
    else if (sa)
    {
        //printf("Alias %s %s;\n", sa.ident.toChars(), tp.ident.toChars());
        auto ad = new AliasDeclaration(Loc.initial, tp.ident, sa);
        ad.storage_class |= STC.templateparameter;
        d = ad;
    }
    else if (ea)
    {
        // tdtypes.data[i] always matches ea here
        Initializer _init = new ExpInitializer(td.loc, ea);
        TemplateValueParameter tvp = tp.isTemplateValueParameter();
        Type t = tvp ? tvp.valType : null;
        v = new VarDeclaration(td.loc, t, tp.ident, _init);
        v.storage_class = STC.manifest | STC.templateparameter;
        d = v;
    }
    else if (va)
    {
        //printf("\ttuple\n");
        d = new TupleDeclaration(td.loc, tp.ident, &va.objects);
    }
    else
    {
        assert(0);
    }
    d.storage_class |= STC.templateparameter;

    if (ta)
    {
        Type t = ta;
        // consistent with Type.checkDeprecated()
        while (t.ty != Tenum)
        {
            if (!t.nextOf())
                break;
            t = (cast(TypeNext)t).next;
        }
        if (Dsymbol s = t.toDsymbol(sc))
        {
            if (s.isDeprecated())
                d.storage_class |= STC.deprecated_;
        }
    }
    else if (sa)
    {
        if (sa.isDeprecated())
            d.storage_class |= STC.deprecated_;
    }

    if (!sc.insert(d))
        .error(td.loc, "%s `%s` declaration `%s` is already defined", td.kind, td.toPrettyChars, tp.ident.toChars());
    d.dsymbolSemantic(sc);
    /* So the caller's o gets updated with the result of semantic() being run on o
     */
    if (v)
        o = v._init.initializerToExpression();
    return o;
}

/**********************************
 * Run semantic on the elements of `ti.tiargs`.
 * Input:
 *      ti = template instance whose `tiargs` should have semantic done
 *      sc = scope
 * Returns:
 *      false if one or more arguments have errors.
 * Note:
 *      This function is reentrant against error occurrence. If returns false,
 *      all elements of tiargs won't be modified.
 */
bool semanticTiargs(TemplateInstance ti, Scope* sc)
{
    //printf("+TemplateInstance.semanticTiargs() %s\n", toChars());
    if (ti.semantictiargsdone)
        return true;
    if (TemplateInstance_semanticTiargs(ti.loc, sc, ti.tiargs, 0))
    {
        // cache the result iff semantic analysis succeeded entirely
        ti.semantictiargsdone = 1;
        return true;
    }
    return false;
}

/**********************************
 * Run semantic of tiargs as arguments of template.
 * Input:
 *      loc
 *      sc
 *      tiargs  array of template arguments
 *      flags   1: replace const variables with their initializers
 *              2: don't devolve Parameter to Type
 *      atd     tuple being optimized. If found, it's not expanded here
 *              but in AliasAssign semantic.
 * Returns:
 *      false if one or more arguments have errors.
 */
bool TemplateInstance_semanticTiargs(Loc loc, Scope* sc, Objects* tiargs, int flags, TupleDeclaration atd = null)
{
    // Run semantic on each argument, place results in tiargs[]
    //printf("+TemplateInstance.semanticTiargs()\n");
    if (!tiargs)
        return true;
    bool err = false;

    // The arguments are not treated as part of a default argument,
    // because they are evaluated at compile time.
    const inCondition = sc.condition;
    sc = sc.push();
    sc.inDefaultArg = false;

    // https://issues.dlang.org/show_bug.cgi?id=24699
    sc.condition = inCondition;

    for (size_t j = 0; j < tiargs.length; j++)
    {
        RootObject o = (*tiargs)[j];
        Type ta = isType(o);
        Expression ea = isExpression(o);
        Dsymbol sa = isDsymbol(o);

        //printf("1: (*tiargs)[%d] = %p, s=%p, v=%p, ea=%p, ta=%p\n", j, o, isDsymbol(o), isTuple(o), ea, ta);
        if (ta)
        {
            //printf("type %s\n", ta.toChars());

            // It might really be an Expression or an Alias
            ta.resolve(loc, sc, ea, ta, sa, (flags & 1) != 0);
            if (ea)
                goto Lexpr;
            if (sa)
                goto Ldsym;
            if (ta is null)
            {
                assert(global.errors);
                ta = Type.terror;
            }

        Ltype:
            if (TypeTuple tt = ta.isTypeTuple())
            {
                // Expand tuple
                size_t dim = tt.arguments.length;
                tiargs.remove(j);
                if (dim)
                {
                    tiargs.reserve(dim);
                    foreach (i, arg; *tt.arguments)
                    {
                        if (flags & 2 && (arg.storageClass & STC.parameter))
                            tiargs.insert(j + i, arg);
                        else
                            tiargs.insert(j + i, arg.type);
                    }
                }
                j--;
                continue;
            }
            if (ta.ty == Terror)
            {
                err = true;
                continue;
            }
            (*tiargs)[j] = ta.merge2();
        }
        else if (ea)
        {
        Lexpr:
            //printf("+[%d] ea = %s %s\n", j, EXPtoString(ea.op).ptr, ea.toChars());
            if (flags & 1) // only used by __traits
            {
                ea = ea.expressionSemantic(sc);

                // must not interpret the args, excepting template parameters
                if (!ea.isVarExp() || (ea.isVarExp().var.storage_class & STC.templateparameter))
                {
                    ea = ea.optimize(WANTvalue);
                }
            }
            else
            {
                sc = sc.startCTFE();
                ea = ea.expressionSemantic(sc);
                sc = sc.endCTFE();

                if (auto varExp = ea.isVarExp())
                {
                    /* If the parameter is a function that is not called
                     * explicitly, i.e. `foo!func` as opposed to `foo!func()`,
                     * then it is a dsymbol, not the return value of `func()`
                     */
                    Declaration vd = varExp.var;
                    if (auto fd = vd.isFuncDeclaration())
                    {
                        sa = fd;
                        goto Ldsym;
                    }
                    /* Otherwise skip substituting a const var with
                     * its initializer. The problem is the initializer won't
                     * match with an 'alias' parameter. Instead, do the
                     * const substitution in TemplateValueParameter.matchArg().
                     */
                }
                else if (definitelyValueParameter(ea))
                {
                    if (ea.checkValue()) // check void expression
                        ea = ErrorExp.get();
                    const olderrs = global.errors;
                    ea = ea.ctfeInterpret();
                    if (global.errors != olderrs)
                        ea = ErrorExp.get();
                }
            }
            //printf("-[%d] ea = %s %s\n", j, EXPtoString(ea.op).ptr, ea.toChars());
            if (TupleExp te = ea.isTupleExp())
            {
                // Expand tuple
                size_t dim = te.exps.length;
                tiargs.remove(j);
                if (dim)
                {
                    tiargs.reserve(dim);
                    foreach (i, exp; *te.exps)
                        tiargs.insert(j + i, exp);
                }
                j--;
                continue;
            }
            if (ea.op == EXP.error)
            {
                err = true;
                continue;
            }
            (*tiargs)[j] = ea;

            if (ea.op == EXP.type)
            {
                ta = ea.type;
                goto Ltype;
            }
            if (ea.op == EXP.scope_)
            {
                sa = ea.isScopeExp().sds;
                goto Ldsym;
            }
            if (FuncExp fe = ea.isFuncExp())
            {
                /* A function literal, that is passed to template and
                 * already semanticed as function pointer, never requires
                 * outer frame. So convert it to global function is valid.
                 */
                if (fe.fd.tok == TOK.reserved && fe.type.ty == Tpointer)
                {
                    // change to non-nested
                    fe.fd.tok = TOK.function_;
                    fe.fd.vthis = null;
                }
                else if (fe.td)
                {
                    /* If template argument is a template lambda,
                     * get template declaration itself. */
                    //sa = fe.td;
                    //goto Ldsym;
                }
            }
            if (ea.op == EXP.dotVariable && !(flags & 1))
            {
                // translate expression to dsymbol.
                sa = ea.isDotVarExp().var;
                goto Ldsym;
            }
            if (auto te = ea.isTemplateExp())
            {
                sa = te.td;
                goto Ldsym;
            }
            if (ea.op == EXP.dotTemplateDeclaration && !(flags & 1))
            {
                // translate expression to dsymbol.
                sa = ea.isDotTemplateExp().td;
                goto Ldsym;
            }
            if (auto de = ea.isDotExp())
            {
                if (auto se = de.e2.isScopeExp())
                {
                    sa = se.sds;
                    goto Ldsym;
                }
            }
        }
        else if (sa)
        {
        Ldsym:
            //printf("dsym %s %s\n", sa.kind(), sa.toChars());
            if (sa.errors)
            {
                err = true;
                continue;
            }

            TupleDeclaration d = sa.toAlias().isTupleDeclaration();
            if (d)
            {
                if (d is atd)
                {
                    (*tiargs)[j] = d;
                    continue;
                }
                // Expand tuple
                tiargs.remove(j);
                tiargs.insert(j, d.objects);
                j--;
                continue;
            }
            if (FuncAliasDeclaration fa = sa.isFuncAliasDeclaration())
            {
                FuncDeclaration f = fa.toAliasFunc();
                if (!fa.hasOverloads && f.isUnique())
                {
                    // Strip FuncAlias only when the aliased function
                    // does not have any overloads.
                    sa = f;
                }
            }
            (*tiargs)[j] = sa;

            TemplateDeclaration td = sa.isTemplateDeclaration();
            if (td && td.semanticRun == PASS.initial && td.literal)
            {
                td.dsymbolSemantic(sc);
            }
            FuncDeclaration fd = sa.isFuncDeclaration();
            if (fd)
                functionSemantic(fd);
        }
        else if (isParameter(o))
        {
        }
        else
        {
            assert(0);
        }
        //printf("1: (*tiargs)[%d] = %p\n", j, (*tiargs)[j]);
    }
    sc.pop();
    version (none)
    {
        printf("-TemplateInstance.semanticTiargs()\n");
        for (size_t j = 0; j < tiargs.length; j++)
        {
            RootObject o = (*tiargs)[j];
            Type ta = isType(o);
            Expression ea = isExpression(o);
            Dsymbol sa = isDsymbol(o);
            Tuple va = isTuple(o);
            printf("\ttiargs[%d] = ta %p, ea %p, sa %p, va %p\n", j, ta, ea, sa, va);
        }
    }
    return !err;
}


/*************************************************
 * Given function arguments, figure out which template function
 * to expand, and return matching result.
 * Params:
 *      m           = matching result
 *      dstart      = the root of overloaded function templates
 *      loc         = instantiation location
 *      sc          = instantiation scope
 *      tiargs      = initial list of template arguments
 *      tthis       = if !NULL, the 'this' pointer argument
 *      argumentList= arguments to function
 *      errorHelper = delegate to send error message to if not null
 */
void functionResolve(ref MatchAccumulator m, Dsymbol dstart, Loc loc, Scope* sc, Objects* tiargs,
    Type tthis, ArgumentList argumentList, void delegate(const(char)*) scope errorHelper = null)
{
    version (none)
    {
        printf("functionResolve() dstart: %s (", dstart.toChars());
        for (size_t i = 0; i < (tiargs ? (*tiargs).length : 0); i++)
        {
            if (i) printf(", ");
            RootObject arg = (*tiargs)[i];
            printf("%s", arg.toChars());
        }
        printf(")(");
        for (size_t i = 0; i < (argumentList.arguments ? (*argumentList.arguments).length : 0); i++)
        {
            if (i) printf(", ");
            Expression arg = (*argumentList.arguments)[i];
            printf("%s %s", arg.type.toChars(), arg.toChars());
            //printf("\tty = %d\n", arg.type.ty);
        }
        printf(")\n");
        //printf("stc = %llx\n", dstart._scope.stc);
        //printf("match:t/f = %d/%d\n", ta_last, m.last);
    }

    // results
    int property = 0;   // 0: uninitialized
                        // 1: seen @property
                        // 2: not @property
    size_t ov_index = 0;
    TemplateDeclaration td_best;
    TemplateInstance ti_best;
    MATCH ta_last = m.last != MATCH.nomatch ? MATCH.exact : MATCH.nomatch;
    Type tthis_best;

    int applyFunction(FuncDeclaration fd)
    {
        // skip duplicates
        if (fd == m.lastf)
            return 0;
        // explicitly specified tiargs never match to non template function
        if (tiargs && tiargs.length > 0)
            return 0;

        // constructors need a valid scope in order to detect semantic errors
        if (!fd.isCtorDeclaration &&
            fd.semanticRun < PASS.semanticdone)
        {
            fd.ungagSpeculative();
            fd.dsymbolSemantic(null);
        }
        if (fd.semanticRun < PASS.semanticdone)
        {
            .error(loc, "forward reference to template `%s`", fd.toChars());
            return 1;
        }
        //printf("fd = %s %s, fargs = %s\n", fd.toChars(), fd.type.toChars(), fargs.toChars());
        auto tf = fd.type.isTypeFunction();

        int prop = tf.isProperty ? 1 : 2;
        if (property == 0)
            property = prop;
        else if (property != prop)
            error(fd.loc, "cannot overload both property and non-property functions");

        /* For constructors, qualifier check will be opposite direction.
         * Qualified constructor always makes qualified object, then will be checked
         * that it is implicitly convertible to tthis.
         */
        Type tthis_fd = fd.needThis() ? tthis : null;
        bool isCtorCall = tthis_fd && fd.isCtorDeclaration();
        if (isCtorCall)
        {
            //printf("%s tf.mod = x%x tthis_fd.mod = x%x %d\n", tf.toChars(),
            //        tf.mod, tthis_fd.mod, fd.isReturnIsolated());
            if (MODimplicitConv(tf.mod, tthis_fd.mod) ||
                tf.isWild() && tf.isShared() == tthis_fd.isShared() ||
                fd.isReturnIsolated())
            {
                /* && tf.isShared() == tthis_fd.isShared()*/
                // Uniquely constructed object can ignore shared qualifier.
                // TODO: Is this appropriate?
                tthis_fd = null;
            }
            else
                return 0;   // MATCH.nomatch
        }
        /* Fix Issue 17970:
           If a struct is declared as shared the dtor is automatically
           considered to be shared, but when the struct is instantiated
           the instance is no longer considered to be shared when the
           function call matching is done. The fix makes it so that if a
           struct declaration is shared, when the destructor is called,
           the instantiated struct is also considered shared.
        */
        if (auto dt = fd.isDtorDeclaration())
        {
            auto dtmod = dt.type.toTypeFunction();
            auto shared_dtor = dtmod.mod & MODFlags.shared_;
            auto shared_this = tthis_fd !is null ?
                tthis_fd.mod & MODFlags.shared_ : 0;
            if (shared_dtor && !shared_this)
                tthis_fd = dtmod;
            else if (shared_this && !shared_dtor && tthis_fd !is null)
                tf.mod = tthis_fd.mod;
        }
        const(char)* failMessage;
        MATCH mfa = callMatch(fd, tf, tthis_fd, argumentList, 0, errorHelper, sc);
        //printf("test1: mfa = %d\n", mfa);
        if (failMessage)
            errorHelper(failMessage);
        if (mfa == MATCH.nomatch)
            return 0;

        int firstIsBetter()
        {
            td_best = null;
            ti_best = null;
            ta_last = MATCH.exact;
            m.last = mfa;
            m.lastf = fd;
            tthis_best = tthis_fd;
            ov_index = 0;
            m.count = 1;
            return 0;
        }

        if (mfa > m.last) return firstIsBetter();
        if (mfa < m.last) return 0;

        /* See if one of the matches overrides the other.
         */
        assert(m.lastf);
        if (m.lastf.overrides(fd)) return 0;
        if (fd.overrides(m.lastf)) return firstIsBetter();

        /* Try to disambiguate using template-style partial ordering rules.
         * In essence, if f() and g() are ambiguous, if f() can call g(),
         * but g() cannot call f(), then pick f().
         * This is because f() is "more specialized."
         */
        {
            MATCH c1 = funcLeastAsSpecialized(fd, m.lastf, argumentList.names);
            MATCH c2 = funcLeastAsSpecialized(m.lastf, fd, argumentList.names);
            //printf("c1 = %d, c2 = %d\n", c1, c2);
            if (c1 > c2) return firstIsBetter();
            if (c1 < c2) return 0;
        }

        /* The 'overrides' check above does covariant checking only
         * for virtual member functions. It should do it for all functions,
         * but in order to not risk breaking code we put it after
         * the 'leastAsSpecialized' check.
         * In the future try moving it before.
         * I.e. a not-the-same-but-covariant match is preferred,
         * as it is more restrictive.
         */
        if (!m.lastf.type.equals(fd.type))
        {
            //printf("cov: %d %d\n", m.lastf.type.covariant(fd.type), fd.type.covariant(m.lastf.type));
            const lastCovariant = m.lastf.type.covariant(fd.type);
            const firstCovariant = fd.type.covariant(m.lastf.type);

            if (lastCovariant == Covariant.yes || lastCovariant == Covariant.no)
            {
                if (firstCovariant != Covariant.yes && firstCovariant != Covariant.no)
                {
                    return 0;
                }
            }
            else if (firstCovariant == Covariant.yes || firstCovariant == Covariant.no)
            {
                return firstIsBetter();
            }
        }

        /* If the two functions are the same function, like:
         *    int foo(int);
         *    int foo(int x) { ... }
         * then pick the one with the body.
         *
         * If none has a body then don't care because the same
         * real function would be linked to the decl (e.g from object file)
         */
        if (tf.equals(m.lastf.type) &&
            fd.storage_class == m.lastf.storage_class &&
            fd.parent == m.lastf.parent &&
            fd.visibility == m.lastf.visibility &&
            fd._linkage == m.lastf._linkage)
        {
            if (fd.fbody && !m.lastf.fbody)
                return firstIsBetter();
            if (!fd.fbody)
                return 0;
        }

        // https://issues.dlang.org/show_bug.cgi?id=14450
        // Prefer exact qualified constructor for the creating object type
        if (isCtorCall && tf.mod != m.lastf.type.mod)
        {
            if (tthis.mod == tf.mod) return firstIsBetter();
            if (tthis.mod == m.lastf.type.mod) return 0;
        }

        m.nextf = fd;
        m.count++;
        return 0;
    }

    int applyTemplate(TemplateDeclaration td)
    {
        //printf("applyTemplate(): td = %s\n", td.toChars());
        if (td == td_best)   // skip duplicates
            return 0;

        if (!sc)
            sc = td._scope; // workaround for Type.aliasthisOf

        if (td.semanticRun == PASS.initial && td._scope)
        {
            // Try to fix forward reference. Ungag errors while doing so.
            td.ungagSpeculative();
            td.dsymbolSemantic(td._scope);
        }
        if (td.semanticRun == PASS.initial)
        {
            .error(loc, "forward reference to template `%s`", td.toChars());
        Lerror:
            m.lastf = null;
            m.count = 0;
            m.last = MATCH.nomatch;
            return 1;
        }
        //printf("td = %s\n", td.toChars());
        td.computeOneMember();
        auto f = td.onemember ? td.onemember.isFuncDeclaration() : null;
        if (!f)
        {
            if (!tiargs)
                tiargs = new Objects();
            auto ti = new TemplateInstance(loc, td, tiargs);
            Objects dedtypes = Objects(td.parameters.length);
            assert(td.semanticRun != PASS.initial);
            MATCH mta = matchWithInstance(sc, td, ti, dedtypes, argumentList, 0);
            //printf("matchWithInstance = %d\n", mta);
            if (mta == MATCH.nomatch || mta < ta_last)   // no match or less match
                return 0;

            ti.templateInstanceSemantic(sc, argumentList);
            if (!ti.inst)               // if template failed to expand
                return 0;

            Dsymbol s = ti.inst.toAlias();
            FuncDeclaration fd;
            if (auto tdx = s.isTemplateDeclaration())
            {
                Objects dedtypesX;      // empty tiargs

                // https://issues.dlang.org/show_bug.cgi?id=11553
                // Check for recursive instantiation of tdx.
                for (TemplatePrevious* p = tdx.previous; p; p = p.prev)
                {
                    if (arrayObjectMatch(*p.dedargs, dedtypesX))
                    {
                        //printf("recursive, no match p.sc=%p %p %s\n", p.sc, this, this.toChars());
                        /* It must be a subscope of p.sc, other scope chains are not recursive
                         * instantiations.
                         */
                        for (Scope* scx = sc; scx; scx = scx.enclosing)
                        {
                            if (scx == p.sc)
                            {
                                error(loc, "recursive template expansion while looking for `%s.%s`", ti.toChars(), tdx.toChars());
                                goto Lerror;
                            }
                        }
                    }
                    /* BUG: should also check for ref param differences
                     */
                }

                TemplatePrevious pr;
                pr.prev = tdx.previous;
                pr.sc = sc;
                pr.dedargs = &dedtypesX;
                tdx.previous = &pr;             // add this to threaded list

                fd = resolveFuncCall(loc, sc, s, null, tthis, argumentList, FuncResolveFlag.quiet);

                tdx.previous = pr.prev;         // unlink from threaded list
            }
            else if (s.isFuncDeclaration())
            {
                fd = resolveFuncCall(loc, sc, s, null, tthis, argumentList, FuncResolveFlag.quiet);
            }
            else
                goto Lerror;

            if (!fd)
                return 0;

            if (fd.type.ty != Tfunction)
            {
                m.lastf = fd;   // to propagate "error match"
                m.count = 1;
                m.last = MATCH.nomatch;
                return 1;
            }

            Type tthis_fd = fd.needThis() && !fd.isCtorDeclaration() ? tthis : null;

            auto tf = fd.type.isTypeFunction();
            MATCH mfa = callMatch(fd, tf, tthis_fd, argumentList, 0, null, sc);
            if (mfa < m.last)
                return 0;

            if (mta < ta_last) goto Ltd_best2;
            if (mta > ta_last) goto Ltd2;

            if (mfa < m.last) goto Ltd_best2;
            if (mfa > m.last) goto Ltd2;

            // td_best and td are ambiguous
            //printf("Lambig2\n");
            m.nextf = fd;
            m.count++;
            return 0;

        Ltd_best2:
            return 0;

        Ltd2:
            // td is the new best match
            assert(td._scope);
            td_best = td;
            ti_best = null;
            property = 0;   // (backward compatibility)
            ta_last = mta;
            m.last = mfa;
            m.lastf = fd;
            tthis_best = tthis_fd;
            ov_index = 0;
            m.nextf = null;
            m.count = 1;
            return 0;
        }

        //printf("td = %s\n", td.toChars());
        for (size_t ovi = 0; f; f = f.overnext0, ovi++)
        {
            if (f.type.ty != Tfunction || f.errors)
                goto Lerror;

            /* This is a 'dummy' instance to evaluate constraint properly.
             */
            auto ti = new TemplateInstance(loc, td, tiargs);
            ti.parent = td.parent;  // Maybe calculating valid 'enclosing' is unnecessary.

            auto fd = f;
            MATCHpair x = td.deduceFunctionTemplateMatch(ti, sc, fd, tthis, argumentList);
            MATCH mta = x.mta;
            MATCH mfa = x.mfa;
            //printf("match:t/f = %d/%d\n", mta, mfa);
            if (!fd || mfa == MATCH.nomatch)
                continue;

            Type tthis_fd = fd.needThis() ? tthis : null;

            bool isCtorCall = tthis_fd && fd.isCtorDeclaration();
            if (isCtorCall)
            {
                // Constructor call requires additional check.
                auto tf = fd.type.isTypeFunction();
                assert(tf.next);
                if (MODimplicitConv(tf.mod, tthis_fd.mod) ||
                    tf.isWild() && tf.isShared() == tthis_fd.isShared() ||
                    fd.isReturnIsolated())
                {
                    tthis_fd = null;
                }
                else
                    continue;   // MATCH.nomatch

                // need to check here whether the constructor is the member of a struct
                // declaration that defines a copy constructor. This is already checked
                // in the semantic of CtorDeclaration, however, when matching functions,
                // the template instance is not expanded.
                // https://issues.dlang.org/show_bug.cgi?id=21613
                auto ad = fd.isThis();
                auto sd = ad.isStructDeclaration();
                if (checkHasBothRvalueAndCpCtor(sd, fd.isCtorDeclaration(), ti))
                    continue;
            }

            if (mta < ta_last) goto Ltd_best;
            if (mta > ta_last) goto Ltd;

            if (mfa < m.last) goto Ltd_best;
            if (mfa > m.last) goto Ltd;

            if (td_best)
            {
                // Disambiguate by picking the most specialized TemplateDeclaration
                MATCH c1 = leastAsSpecialized(sc, td, td_best, argumentList);
                MATCH c2 = leastAsSpecialized(sc, td_best, td, argumentList);
                //printf("1: c1 = %d, c2 = %d\n", c1, c2);
                if (c1 > c2) goto Ltd;
                if (c1 < c2) goto Ltd_best;
            }
            assert(fd && m.lastf);
            {
                // Disambiguate by tf.callMatch
                auto tf1 = fd.type.isTypeFunction();
                auto tf2 = m.lastf.type.isTypeFunction();
                MATCH c1 = callMatch(fd,      tf1, tthis_fd,   argumentList, 0, null, sc);
                MATCH c2 = callMatch(m.lastf, tf2, tthis_best, argumentList, 0, null, sc);
                //printf("2: c1 = %d, c2 = %d\n", c1, c2);
                if (c1 > c2) goto Ltd;
                if (c1 < c2) goto Ltd_best;
            }
            {
                // Disambiguate by picking the most specialized FunctionDeclaration
                MATCH c1 = funcLeastAsSpecialized(fd, m.lastf, argumentList.names);
                MATCH c2 = funcLeastAsSpecialized(m.lastf, fd, argumentList.names);
                //printf("3: c1 = %d, c2 = %d\n", c1, c2);
                if (c1 > c2) goto Ltd;
                if (c1 < c2) goto Ltd_best;
            }

            // https://issues.dlang.org/show_bug.cgi?id=14450
            // Prefer exact qualified constructor for the creating object type
            if (isCtorCall && fd.type.mod != m.lastf.type.mod)
            {
                if (tthis.mod == fd.type.mod) goto Ltd;
                if (tthis.mod == m.lastf.type.mod) goto Ltd_best;
            }

            m.nextf = fd;
            m.count++;
            continue;

        Ltd_best:           // td_best is the best match so far
            //printf("Ltd_best\n");
            continue;

        Ltd:                // td is the new best match
            //printf("Ltd\n");
            assert(td._scope);
            td_best = td;
            ti_best = ti;
            property = 0;   // (backward compatibility)
            ta_last = mta;
            m.last = mfa;
            m.lastf = fd;
            tthis_best = tthis_fd;
            ov_index = ovi;
            m.nextf = null;
            m.count = 1;
            continue;
        }
        return 0;
    }

    auto td = dstart.isTemplateDeclaration();
    if (td && td.funcroot)
        dstart = td.funcroot;
    overloadApply(dstart, (Dsymbol s)
    {
        if (s.errors)
            return 0;
        if (auto fd = s.isFuncDeclaration())
            return applyFunction(fd);
        if (auto td = s.isTemplateDeclaration())
            return applyTemplate(td);
        return 0;
    }, sc);

    //printf("td_best = %p, m.lastf = %p\n", td_best, m.lastf);
    if (td_best && ti_best && m.count == 1)
    {
        // Matches to template function
        td_best.computeOneMember();
        assert(td_best.onemember && td_best.onemember.isFuncDeclaration());
        /* The best match is td_best with arguments tdargs.
         * Now instantiate the template.
         */
        assert(td_best._scope);
        if (!sc)
            sc = td_best._scope; // workaround for Type.aliasthisOf

        auto ti = new TemplateInstance(loc, td_best, ti_best.tiargs);
        ti.templateInstanceSemantic(sc, argumentList);

        m.lastf = ti.toAlias().isFuncDeclaration();
        if (!m.lastf)
            goto Lnomatch;
        if (ti.errors)
        {
        Lerror:
            m.count = 1;
            assert(m.lastf);
            m.last = MATCH.nomatch;
            return;
        }

        // look forward instantiated overload function
        // Dsymbol.oneMembers is alredy called in TemplateInstance.semantic.
        // it has filled overnext0d
        while (ov_index--)
        {
            m.lastf = m.lastf.overnext0;
            assert(m.lastf);
        }

        tthis_best = m.lastf.needThis() && !m.lastf.isCtorDeclaration() ? tthis : null;

        if (m.lastf.type.ty == Terror)
            goto Lerror;
        auto tf = m.lastf.type.isTypeFunction();
        if (callMatch(m.lastf, tf, tthis_best, argumentList, 0, null, sc) == MATCH.nomatch)
            goto Lnomatch;

        /* As https://issues.dlang.org/show_bug.cgi?id=3682 shows,
         * a template instance can be matched while instantiating
         * that same template. Thus, the function type can be incomplete. Complete it.
         *
         * https://issues.dlang.org/show_bug.cgi?id=9208
         * For auto function, completion should be deferred to the end of
         * its semantic3. Should not complete it in here.
         */
        if (tf.next && !m.lastf.inferRetType)
        {
            m.lastf.type = tf.typeSemantic(loc, sc);
        }
    }
    else if (m.lastf)
    {
        // Matches to non template function,
        // or found matches were ambiguous.
        assert(m.count >= 1);
    }
    else
    {
    Lnomatch:
        m.count = 0;
        m.lastf = null;
        m.last = MATCH.nomatch;
    }
}
/* Create dummy argument based on parameter.
 */
private RootObject dummyArg(TemplateParameter tp)
{
    scope v = new DummyArgVisitor();
    tp.accept(v);
    return v.result;
}
private extern(C++) class DummyArgVisitor : Visitor
{
    RootObject result;

    alias visit = typeof(super).visit;
    override void visit(TemplateTypeParameter ttp)
    {
        Type t = ttp.specType;
        if (t)
        {
            result = t;
            return;
        }
        // Use this for alias-parameter's too (?)
        if (!ttp.tdummy)
            ttp.tdummy = new TypeIdentifier(ttp.loc, ttp.ident);
        result = ttp.tdummy;
    }

    override void visit(TemplateValueParameter tvp)
    {
        Expression e = tvp.specValue;
        if (e)
        {
            result = e;
            return;
        }

        // Create a dummy value
        auto pe = cast(void*)tvp.valType in tvp.edummies;
        if (pe)
        {
            result = *pe;
            return;
        }

        e = tvp.valType.defaultInit(Loc.initial);
        tvp.edummies[cast(void*)tvp.valType] = e;
        result = e;
    }

    override void visit(TemplateAliasParameter tap)
    {
        RootObject s = tap.specAlias;
        if (s)
        {
            result = s;
            return;
        }
        if (!tap.sdummy)
            tap.sdummy = new Dsymbol(DSYM.dsymbol);
        result = tap.sdummy;
    }

    override void visit(TemplateTupleParameter tap)
    {
        result = null;
    }
}

/**
 * Returns the common type of the 2 types.
 */
private Type rawTypeMerge(Type t1, Type t2)
{
    if (t1.equals(t2))
        return t1;
    if (t1.equivalent(t2))
        return t1.castMod(MODmerge(t1.mod, t2.mod));

    auto t1b = t1.toBasetype();
    auto t2b = t2.toBasetype();
    if (t1b.equals(t2b))
        return t1b;
    if (t1b.equivalent(t2b))
        return t1b.castMod(MODmerge(t1b.mod, t2b.mod));

    auto ty = implicitConvCommonTy(t1b.ty, t2b.ty);
    if (ty != Terror)
        return Type.basic[ty];

    return null;
}

private auto X(T, U)(T m, U n)
{
    return (m << 4) | n;
}

private ubyte deduceWildHelper(Type t, Type* at, Type tparam)
{
    if ((tparam.mod & MODFlags.wild) == 0)
        return 0;

    *at = null;

    switch (X(tparam.mod, t.mod))
    {
    case X(MODFlags.wild, 0):
    case X(MODFlags.wild, MODFlags.const_):
    case X(MODFlags.wild, MODFlags.shared_):
    case X(MODFlags.wild, MODFlags.shared_ | MODFlags.const_):
    case X(MODFlags.wild, MODFlags.immutable_):
    case X(MODFlags.wildconst, 0):
    case X(MODFlags.wildconst, MODFlags.const_):
    case X(MODFlags.wildconst, MODFlags.shared_):
    case X(MODFlags.wildconst, MODFlags.shared_ | MODFlags.const_):
    case X(MODFlags.wildconst, MODFlags.immutable_):
    case X(MODFlags.shared_ | MODFlags.wild, MODFlags.shared_):
    case X(MODFlags.shared_ | MODFlags.wild, MODFlags.shared_ | MODFlags.const_):
    case X(MODFlags.shared_ | MODFlags.wild, MODFlags.immutable_):
    case X(MODFlags.shared_ | MODFlags.wildconst, MODFlags.shared_):
    case X(MODFlags.shared_ | MODFlags.wildconst, MODFlags.shared_ | MODFlags.const_):
    case X(MODFlags.shared_ | MODFlags.wildconst, MODFlags.immutable_):
        {
            ubyte wm = (t.mod & ~MODFlags.shared_);
            if (wm == 0)
                wm = MODFlags.mutable;
            ubyte m = (t.mod & (MODFlags.const_ | MODFlags.immutable_)) | (tparam.mod & t.mod & MODFlags.shared_);
            *at = t.unqualify(m);
            return wm;
        }
    case X(MODFlags.wild, MODFlags.wild):
    case X(MODFlags.wild, MODFlags.wildconst):
    case X(MODFlags.wild, MODFlags.shared_ | MODFlags.wild):
    case X(MODFlags.wild, MODFlags.shared_ | MODFlags.wildconst):
    case X(MODFlags.wildconst, MODFlags.wild):
    case X(MODFlags.wildconst, MODFlags.wildconst):
    case X(MODFlags.wildconst, MODFlags.shared_ | MODFlags.wild):
    case X(MODFlags.wildconst, MODFlags.shared_ | MODFlags.wildconst):
    case X(MODFlags.shared_ | MODFlags.wild, MODFlags.shared_ | MODFlags.wild):
    case X(MODFlags.shared_ | MODFlags.wild, MODFlags.shared_ | MODFlags.wildconst):
    case X(MODFlags.shared_ | MODFlags.wildconst, MODFlags.shared_ | MODFlags.wild):
    case X(MODFlags.shared_ | MODFlags.wildconst, MODFlags.shared_ | MODFlags.wildconst):
        {
            *at = t.unqualify(tparam.mod & t.mod);
            return MODFlags.wild;
        }
    default:
        return 0;
    }
}

private MATCH deduceTypeHelper(Type t, out Type at, Type tparam)
{
    // 9*9 == 81 cases
    switch (X(tparam.mod, t.mod))
    {
    case X(0, 0):
    case X(0, MODFlags.const_):
    case X(0, MODFlags.wild):
    case X(0, MODFlags.wildconst):
    case X(0, MODFlags.shared_):
    case X(0, MODFlags.shared_ | MODFlags.const_):
    case X(0, MODFlags.shared_ | MODFlags.wild):
    case X(0, MODFlags.shared_ | MODFlags.wildconst):
    case X(0, MODFlags.immutable_):
        // foo(U)                       T                       => T
        // foo(U)                       const(T)                => const(T)
        // foo(U)                       inout(T)                => inout(T)
        // foo(U)                       inout(const(T))         => inout(const(T))
        // foo(U)                       shared(T)               => shared(T)
        // foo(U)                       shared(const(T))        => shared(const(T))
        // foo(U)                       shared(inout(T))        => shared(inout(T))
        // foo(U)                       shared(inout(const(T))) => shared(inout(const(T)))
        // foo(U)                       immutable(T)            => immutable(T)
        {
            at = t;
            return MATCH.exact;
        }
    case X(MODFlags.const_, MODFlags.const_):
    case X(MODFlags.wild, MODFlags.wild):
    case X(MODFlags.wildconst, MODFlags.wildconst):
    case X(MODFlags.shared_, MODFlags.shared_):
    case X(MODFlags.shared_ | MODFlags.const_, MODFlags.shared_ | MODFlags.const_):
    case X(MODFlags.shared_ | MODFlags.wild, MODFlags.shared_ | MODFlags.wild):
    case X(MODFlags.shared_ | MODFlags.wildconst, MODFlags.shared_ | MODFlags.wildconst):
    case X(MODFlags.immutable_, MODFlags.immutable_):
        // foo(const(U))                const(T)                => T
        // foo(inout(U))                inout(T)                => T
        // foo(inout(const(U)))         inout(const(T))         => T
        // foo(shared(U))               shared(T)               => T
        // foo(shared(const(U)))        shared(const(T))        => T
        // foo(shared(inout(U)))        shared(inout(T))        => T
        // foo(shared(inout(const(U)))) shared(inout(const(T))) => T
        // foo(immutable(U))            immutable(T)            => T
        {
            at = t.mutableOf().unSharedOf();
            return MATCH.exact;
        }
    case X(MODFlags.const_, MODFlags.shared_ | MODFlags.const_):
    case X(MODFlags.wild, MODFlags.shared_ | MODFlags.wild):
    case X(MODFlags.wildconst, MODFlags.shared_ | MODFlags.wildconst):
        // foo(const(U))                shared(const(T))        => shared(T)
        // foo(inout(U))                shared(inout(T))        => shared(T)
        // foo(inout(const(U)))         shared(inout(const(T))) => shared(T)
        {
            at = t.mutableOf();
            return MATCH.exact;
        }
    case X(MODFlags.const_, 0):
    case X(MODFlags.const_, MODFlags.wild):
    case X(MODFlags.const_, MODFlags.wildconst):
    case X(MODFlags.const_, MODFlags.shared_ | MODFlags.wild):
    case X(MODFlags.const_, MODFlags.shared_ | MODFlags.wildconst):
    case X(MODFlags.const_, MODFlags.immutable_):
    case X(MODFlags.shared_ | MODFlags.const_, MODFlags.immutable_):
        // foo(const(U))                T                       => T
        // foo(const(U))                inout(T)                => T
        // foo(const(U))                inout(const(T))         => T
        // foo(const(U))                shared(inout(T))        => shared(T)
        // foo(const(U))                shared(inout(const(T))) => shared(T)
        // foo(const(U))                immutable(T)            => T
        // foo(shared(const(U)))        immutable(T)            => T
        {
            at = t.mutableOf();
            return MATCH.constant;
        }
    case X(MODFlags.const_, MODFlags.shared_):
        // foo(const(U))                shared(T)               => shared(T)
        {
            at = t;
            return MATCH.constant;
        }
    case X(MODFlags.shared_, MODFlags.shared_ | MODFlags.const_):
    case X(MODFlags.shared_, MODFlags.shared_ | MODFlags.wild):
    case X(MODFlags.shared_, MODFlags.shared_ | MODFlags.wildconst):
        // foo(shared(U))               shared(const(T))        => const(T)
        // foo(shared(U))               shared(inout(T))        => inout(T)
        // foo(shared(U))               shared(inout(const(T))) => inout(const(T))
        {
            at = t.unSharedOf();
            return MATCH.exact;
        }
    case X(MODFlags.shared_ | MODFlags.const_, MODFlags.shared_):
        // foo(shared(const(U)))        shared(T)               => T
        {
            at = t.unSharedOf();
            return MATCH.constant;
        }
    case X(MODFlags.wildconst, MODFlags.immutable_):
    case X(MODFlags.shared_ | MODFlags.const_, MODFlags.shared_ | MODFlags.wildconst):
    case X(MODFlags.shared_ | MODFlags.wildconst, MODFlags.immutable_):
    case X(MODFlags.shared_ | MODFlags.wildconst, MODFlags.shared_ | MODFlags.wild):
        // foo(inout(const(U)))         immutable(T)            => T
        // foo(shared(const(U)))        shared(inout(const(T))) => T
        // foo(shared(inout(const(U)))) immutable(T)            => T
        // foo(shared(inout(const(U)))) shared(inout(T))        => T
        {
            at = t.unSharedOf().mutableOf();
            return MATCH.constant;
        }
    case X(MODFlags.shared_ | MODFlags.const_, MODFlags.shared_ | MODFlags.wild):
        // foo(shared(const(U)))        shared(inout(T))        => T
        {
            at = t.unSharedOf().mutableOf();
            return MATCH.constant;
        }
    case X(MODFlags.wild, 0):
    case X(MODFlags.wild, MODFlags.const_):
    case X(MODFlags.wild, MODFlags.wildconst):
    case X(MODFlags.wild, MODFlags.immutable_):
    case X(MODFlags.wild, MODFlags.shared_):
    case X(MODFlags.wild, MODFlags.shared_ | MODFlags.const_):
    case X(MODFlags.wild, MODFlags.shared_ | MODFlags.wildconst):
    case X(MODFlags.wildconst, 0):
    case X(MODFlags.wildconst, MODFlags.const_):
    case X(MODFlags.wildconst, MODFlags.wild):
    case X(MODFlags.wildconst, MODFlags.shared_):
    case X(MODFlags.wildconst, MODFlags.shared_ | MODFlags.const_):
    case X(MODFlags.wildconst, MODFlags.shared_ | MODFlags.wild):
    case X(MODFlags.shared_, 0):
    case X(MODFlags.shared_, MODFlags.const_):
    case X(MODFlags.shared_, MODFlags.wild):
    case X(MODFlags.shared_, MODFlags.wildconst):
    case X(MODFlags.shared_, MODFlags.immutable_):
    case X(MODFlags.shared_ | MODFlags.const_, 0):
    case X(MODFlags.shared_ | MODFlags.const_, MODFlags.const_):
    case X(MODFlags.shared_ | MODFlags.const_, MODFlags.wild):
    case X(MODFlags.shared_ | MODFlags.const_, MODFlags.wildconst):
    case X(MODFlags.shared_ | MODFlags.wild, 0):
    case X(MODFlags.shared_ | MODFlags.wild, MODFlags.const_):
    case X(MODFlags.shared_ | MODFlags.wild, MODFlags.wild):
    case X(MODFlags.shared_ | MODFlags.wild, MODFlags.wildconst):
    case X(MODFlags.shared_ | MODFlags.wild, MODFlags.immutable_):
    case X(MODFlags.shared_ | MODFlags.wild, MODFlags.shared_):
    case X(MODFlags.shared_ | MODFlags.wild, MODFlags.shared_ | MODFlags.const_):
    case X(MODFlags.shared_ | MODFlags.wild, MODFlags.shared_ | MODFlags.wildconst):
    case X(MODFlags.shared_ | MODFlags.wildconst, 0):
    case X(MODFlags.shared_ | MODFlags.wildconst, MODFlags.const_):
    case X(MODFlags.shared_ | MODFlags.wildconst, MODFlags.wild):
    case X(MODFlags.shared_ | MODFlags.wildconst, MODFlags.wildconst):
    case X(MODFlags.shared_ | MODFlags.wildconst, MODFlags.shared_):
    case X(MODFlags.shared_ | MODFlags.wildconst, MODFlags.shared_ | MODFlags.const_):
    case X(MODFlags.immutable_, 0):
    case X(MODFlags.immutable_, MODFlags.const_):
    case X(MODFlags.immutable_, MODFlags.wild):
    case X(MODFlags.immutable_, MODFlags.wildconst):
    case X(MODFlags.immutable_, MODFlags.shared_):
    case X(MODFlags.immutable_, MODFlags.shared_ | MODFlags.const_):
    case X(MODFlags.immutable_, MODFlags.shared_ | MODFlags.wild):
    case X(MODFlags.immutable_, MODFlags.shared_ | MODFlags.wildconst):
        // foo(inout(U))                T                       => nomatch
        // foo(inout(U))                const(T)                => nomatch
        // foo(inout(U))                inout(const(T))         => nomatch
        // foo(inout(U))                immutable(T)            => nomatch
        // foo(inout(U))                shared(T)               => nomatch
        // foo(inout(U))                shared(const(T))        => nomatch
        // foo(inout(U))                shared(inout(const(T))) => nomatch
        // foo(inout(const(U)))         T                       => nomatch
        // foo(inout(const(U)))         const(T)                => nomatch
        // foo(inout(const(U)))         inout(T)                => nomatch
        // foo(inout(const(U)))         shared(T)               => nomatch
        // foo(inout(const(U)))         shared(const(T))        => nomatch
        // foo(inout(const(U)))         shared(inout(T))        => nomatch
        // foo(shared(U))               T                       => nomatch
        // foo(shared(U))               const(T)                => nomatch
        // foo(shared(U))               inout(T)                => nomatch
        // foo(shared(U))               inout(const(T))         => nomatch
        // foo(shared(U))               immutable(T)            => nomatch
        // foo(shared(const(U)))        T                       => nomatch
        // foo(shared(const(U)))        const(T)                => nomatch
        // foo(shared(const(U)))        inout(T)                => nomatch
        // foo(shared(const(U)))        inout(const(T))         => nomatch
        // foo(shared(inout(U)))        T                       => nomatch
        // foo(shared(inout(U)))        const(T)                => nomatch
        // foo(shared(inout(U)))        inout(T)                => nomatch
        // foo(shared(inout(U)))        inout(const(T))         => nomatch
        // foo(shared(inout(U)))        immutable(T)            => nomatch
        // foo(shared(inout(U)))        shared(T)               => nomatch
        // foo(shared(inout(U)))        shared(const(T))        => nomatch
        // foo(shared(inout(U)))        shared(inout(const(T))) => nomatch
        // foo(shared(inout(const(U)))) T                       => nomatch
        // foo(shared(inout(const(U)))) const(T)                => nomatch
        // foo(shared(inout(const(U)))) inout(T)                => nomatch
        // foo(shared(inout(const(U)))) inout(const(T))         => nomatch
        // foo(shared(inout(const(U)))) shared(T)               => nomatch
        // foo(shared(inout(const(U)))) shared(const(T))        => nomatch
        // foo(immutable(U))            T                       => nomatch
        // foo(immutable(U))            const(T)                => nomatch
        // foo(immutable(U))            inout(T)                => nomatch
        // foo(immutable(U))            inout(const(T))         => nomatch
        // foo(immutable(U))            shared(T)               => nomatch
        // foo(immutable(U))            shared(const(T))        => nomatch
        // foo(immutable(U))            shared(inout(T))        => nomatch
        // foo(immutable(U))            shared(inout(const(T))) => nomatch
        return MATCH.nomatch;

    default:
        assert(0);
    }
}

private __gshared Expression emptyArrayElement = null;

/*
 * Returns `true` if `t` is a reference type, or an array of reference types.
 */
private bool isTopRef(Type t)
{
    auto tb = t.baseElemOf();
    return tb.ty == Tclass ||
           tb.ty == Taarray ||
           tb.ty == Tstruct && tb.hasPointers();
}

/*
 * Returns a valid `Loc` for semantic routines.
 * Some paths require a location derived from the first
 * template parameter when available.
 */
private Loc semanticLoc(scope ref TemplateParameters parameters)
{
    Loc loc;
    if (parameters.length)
        loc = parameters[0].loc;
    return loc;
}

private MATCH deduceAliasThis(Type t, Scope* sc, Type tparam,
    ref TemplateParameters parameters, ref Objects dedtypes, uint* wm)
{
    if (auto tc = t.isTypeClass())
    {
        if (tc.sym.aliasthis && !(tc.att & AliasThisRec.tracingDT))
        {
            if (auto ato = t.aliasthisOf())
            {
                tc.att = cast(AliasThisRec)(tc.att | AliasThisRec.tracingDT);
                auto m = deduceType(ato, sc, tparam, parameters, dedtypes, wm);
                tc.att = cast(AliasThisRec)(tc.att & ~AliasThisRec.tracingDT);
                return m;
            }
        }
    }
    else if (auto ts = t.isTypeStruct())
    {
        if (ts.sym.aliasthis && !(ts.att & AliasThisRec.tracingDT))
        {
            if (auto ato = t.aliasthisOf())
            {
                ts.att = cast(AliasThisRec)(ts.att | AliasThisRec.tracingDT);
                auto m = deduceType(ato, sc, tparam, parameters, dedtypes, wm);
                ts.att = cast(AliasThisRec)(ts.att & ~AliasThisRec.tracingDT);
                return m;
            }
        }
    }
    return MATCH.nomatch;
}

private MATCH deduceParentInstance(Scope* sc, Dsymbol sym, TypeInstance tpi,
    ref TemplateParameters parameters, ref Objects dedtypes, uint* wm)
{
    if (!tpi.idents.length)
        return MATCH.nomatch;

    RootObject id = tpi.idents[tpi.idents.length - 1];
    if (id.dyncast() != DYNCAST.identifier || !sym.ident.equals(cast(Identifier)id))
        return MATCH.nomatch;

    Type tparent = dmd.dsymbolsem.getType(sym.parent);
    if (!tparent)
        return MATCH.nomatch;

    tpi.idents.length--;
    auto m = deduceType(tparent, sc, tpi, parameters, dedtypes, wm);
    tpi.idents.length++;
    return m;
}

private MATCH matchAll(TypeDeduced td, Type tt)
{
    MATCH match = MATCH.exact;
    foreach (j, e; td.argexps)
    {
        assert(e);
        if (e == emptyArrayElement)
            continue;

        Type t = tt.addMod(td.tparams[j].mod).substWildTo(MODFlags.const_);

        MATCH m = e.implicitConvTo(t);
        if (match > m)
            match = m;
        if (match == MATCH.nomatch)
            break;
    }
    return match;
}
/****
 * Given an identifier, figure out which TemplateParameter it is.
 * Return IDX_NOTFOUND if not found.
 */
private size_t templateIdentifierLookup(Identifier id, TemplateParameters* parameters)
{
    for (size_t i = 0; i < parameters.length; i++)
    {
        TemplateParameter tp = (*parameters)[i];
        if (tp.ident.equals(id))
            return i;
    }
    return IDX_NOTFOUND;
}

private size_t templateParameterLookup(Type tparam, TemplateParameters* parameters)
{
    if (TypeIdentifier tident = tparam.isTypeIdentifier())
    {
        //printf("\ttident = '%s'\n", tident.toChars());
        return templateIdentifierLookup(tident.ident, parameters);
    }
    return IDX_NOTFOUND;
}
/* These form the heart of template argument deduction.
 * Given 'this' being the type argument to the template instance,
 * it is matched against the template declaration parameter specialization
 * 'tparam' to determine the type to be used for the parameter.
 * Example:
 *      template Foo(T:T*)      // template declaration
 *      Foo!(int*)              // template instantiation
 * Input:
 *      this = int*
 *      tparam = T*
 *      parameters = [ T:T* ]   // Array of TemplateParameter's
 * Output:
 *      dedtypes = [ int ]      // Array of Expression/Type's
 */
MATCH deduceType(scope RootObject o, scope Scope* sc, scope Type tparam,
    scope ref TemplateParameters parameters, scope ref Objects dedtypes,
    scope uint* wm = null, size_t inferStart = 0, bool ignoreAliasThis = false)
{
    extern (C++) final class DeduceType : Visitor
    {
        alias visit = Visitor.visit;
    public:
        MATCH result;

        extern (D) this() @safe
        {
            result = MATCH.nomatch;
        }

        override void visit(Type t)
        {
            if (!tparam)
                goto Lnomatch;

            if (t == tparam)
                goto Lexact;

            if (tparam.ty == Tident)
            {
                // Determine which parameter tparam is
                size_t i = templateParameterLookup(tparam, &parameters);
                if (i == IDX_NOTFOUND)
                {
                    if (!sc)
                        goto Lnomatch;

                    /* Need a loc to go with the semantic routine. */
                    Loc loc = semanticLoc(parameters);

                    /* BUG: what if tparam is a template instance, that
                     * has as an argument another Tident?
                     */
                    tparam = tparam.typeSemantic(loc, sc);
                    assert(tparam.ty != Tident);
                    result = deduceType(t, sc, tparam, parameters, dedtypes, wm);
                    return;
                }

                TemplateParameter tp = parameters[i];

                TypeIdentifier tident = tparam.isTypeIdentifier();
                if (tident.idents.length > 0)
                {
                    //printf("matching %s to %s\n", tparam.toChars(), t.toChars());
                    Dsymbol s = t.toDsymbol(sc);
                    for (size_t j = tident.idents.length; j-- > 0;)
                    {
                        RootObject id = tident.idents[j];
                        if (id.dyncast() == DYNCAST.identifier)
                        {
                            if (!s || !s.parent)
                                goto Lnomatch;
                            Dsymbol s2 = s.parent.search(Loc.initial, cast(Identifier)id);
                            if (!s2)
                                goto Lnomatch;
                            s2 = s2.toAlias();
                            //printf("[%d] s = %s %s, s2 = %s %s\n", j, s.kind(), s.toChars(), s2.kind(), s2.toChars());
                            if (s != s2)
                            {
                                if (Type tx = dmd.dsymbolsem.getType(s2))
                                {
                                    if (s != tx.toDsymbol(sc))
                                        goto Lnomatch;
                                }
                                else
                                    goto Lnomatch;
                            }
                            s = s.parent;
                        }
                        else
                            goto Lnomatch;
                    }
                    //printf("[e] s = %s\n", s?s.toChars():"(null)");
                    if (tp.isTemplateTypeParameter())
                    {
                        Type tt = dmd.dsymbolsem.getType(s);
                        if (!tt)
                            goto Lnomatch;
                        Type at = cast(Type)dedtypes[i];
                        if (at && at.ty == Tnone)
                            at = (cast(TypeDeduced)at).tded;
                        if (!at || tt.equals(at))
                        {
                            dedtypes[i] = tt;
                            goto Lexact;
                        }
                    }
                    if (tp.isTemplateAliasParameter())
                    {
                        Dsymbol s2 = cast(Dsymbol)dedtypes[i];
                        if (!s2 || s == s2)
                        {
                            dedtypes[i] = s;
                            goto Lexact;
                        }
                    }
                    goto Lnomatch;
                }

                // Found the corresponding parameter tp
                /+
                    https://issues.dlang.org/show_bug.cgi?id=23578
                    To pattern match:
                    static if (is(S!int == S!av, alias av))

                    We eventually need to deduce `int` (Tint32 [0]) and `av` (Tident).
                    Previously this would not get pattern matched at all, but now we check if the
                    template parameter `av` came from.

                    This note has been left to serve as a hint for further explorers into
                    how IsExp matching works.
                +/
                if (auto ta = tp.isTemplateAliasParameter())
                {
                    dedtypes[i] = t;
                    goto Lexact;
                }
                // (23578) - ensure previous behaviour for non-alias template params
                if (!tp.isTemplateTypeParameter())
                {
                    goto Lnomatch;
                }

                Type at = cast(Type)dedtypes[i];
                Type tt;
                if (ubyte wx = wm ? deduceWildHelper(t, &tt, tparam) : 0)
                {
                    // type vs (none)
                    if (!at)
                    {
                        dedtypes[i] = tt;
                        *wm |= wx;
                        result = MATCH.constant;
                        return;
                    }

                    // type vs expressions
                    if (at.ty == Tnone)
                    {
                        auto xt = cast(TypeDeduced)at;
                        result = xt.matchAll(tt);
                        if (result > MATCH.nomatch)
                        {
                            dedtypes[i] = tt;
                            if (result > MATCH.constant)
                                result = MATCH.constant; // limit level for inout matches
                        }
                        return;
                    }

                    // type vs type
                    if (tt.equals(at))
                    {
                        dedtypes[i] = tt; // Prefer current type match
                        goto Lconst;
                    }
                    if (tt.implicitConvTo(at.constOf()))
                    {
                        dedtypes[i] = at.constOf().mutableOf();
                        *wm |= MODFlags.const_;
                        goto Lconst;
                    }
                    if (at.implicitConvTo(tt.constOf()))
                    {
                        dedtypes[i] = tt.constOf().mutableOf();
                        *wm |= MODFlags.const_;
                        goto Lconst;
                    }
                    goto Lnomatch;
                }
                else if (MATCH m = deduceTypeHelper(t, tt, tparam))
                {
                    // type vs (none)
                    if (!at)
                    {
                        dedtypes[i] = tt;
                        result = m;
                        return;
                    }

                    // type vs expressions
                    if (at.ty == Tnone)
                    {
                        auto xt = cast(TypeDeduced)at;
                        result = xt.matchAll(tt);
                        if (result > MATCH.nomatch)
                        {
                            dedtypes[i] = tt;
                        }
                        return;
                    }

                    // type vs type
                    if (tt.equals(at))
                    {
                        goto Lexact;
                    }
                    if (tt.ty == Tclass && at.ty == Tclass)
                    {
                        result = tt.implicitConvTo(at);
                        return;
                    }
                    if (tt.ty == Tsarray && at.ty == Tarray && tt.nextOf().implicitConvTo(at.nextOf()) >= MATCH.constant)
                    {
                        goto Lexact;
                    }
                }
                goto Lnomatch;
            }

            if (tparam.ty == Ttypeof)
            {
                    /* Need a loc to go with the semantic routine. */
                    Loc loc = semanticLoc(parameters);

                tparam = tparam.typeSemantic(loc, sc);
            }
            if (t.ty != tparam.ty)
            {
                if (Dsymbol sym = t.toDsymbol(sc))
                {
                    if (sym.isforwardRef() && !tparam.deco)
                        goto Lnomatch;
                }

                MATCH m = t.implicitConvTo(tparam);
                if (m == MATCH.nomatch && !ignoreAliasThis)
                {
                    m = deduceAliasThis(t, sc, tparam, parameters, dedtypes, wm);
                }
                result = m;
                return;
            }

            if (t.nextOf())
            {
                if (tparam.deco && !tparam.hasWild())
                {
                    result = t.implicitConvTo(tparam);
                    return;
                }

                Type tpn = tparam.nextOf();
                if (wm && t.ty == Taarray && tparam.isWild())
                {
                    // https://issues.dlang.org/show_bug.cgi?id=12403
                    // In IFTI, stop inout matching on transitive part of AA types.
                    tpn = tpn.substWildTo(MODFlags.mutable);
                }

                result = deduceType(t.nextOf(), sc, tpn, parameters, dedtypes, wm);
                return;
            }

        Lexact:
            result = MATCH.exact;
            return;

        Lnomatch:
            result = MATCH.nomatch;
            return;

        Lconst:
            result = MATCH.constant;
        }

        override void visit(TypeVector t)
        {
            if (auto tp = tparam.isTypeVector())
            {
                result = deduceType(t.basetype, sc, tp.basetype, parameters, dedtypes, wm);
                return;
            }
            visit(cast(Type)t);
        }

        override void visit(TypeDArray t)
        {
            visit(cast(Type)t);
        }

        override void visit(TypeSArray t)
        {
            // Extra check that array dimensions must match
            if (!tparam)
            {
                visit(cast(Type)t);
                return;
            }

            if (tparam.ty == Tarray)
            {
                MATCH m = deduceType(t.next, sc, tparam.nextOf(), parameters, dedtypes, wm);
                result = (m >= MATCH.constant) ? MATCH.convert : MATCH.nomatch;
                return;
            }

            TemplateParameter tp = null;
            Expression edim = null;
            size_t i;
            if (auto tsa = tparam.isTypeSArray())
            {
                if (tsa.dim.isVarExp() && tsa.dim.isVarExp().var.storage_class & STC.templateparameter)
                {
                    Identifier id = tsa.dim.isVarExp().var.ident;
                    i = templateIdentifierLookup(id, &parameters);
                    assert(i != IDX_NOTFOUND);
                    tp = parameters[i];
                }
                else
                    edim = tsa.dim;
            }
            else if (auto taa = tparam.isTypeAArray())
            {
                i = templateParameterLookup(taa.index, &parameters);
                if (i != IDX_NOTFOUND)
                    tp = parameters[i];
                else
                {
                    Loc loc;
                    // The "type" (it hasn't been resolved yet) of the function parameter
                    // does not have a location but the parameter it is related to does,
                    // so we use that for the resolution (better error message).
                    if (inferStart < parameters.length)
                    {
                        TemplateParameter loctp = parameters[inferStart];
                        loc = loctp.loc;
                    }

                    Expression e;
                    Type tx;
                    Dsymbol s;
                    taa.index.resolve(loc, sc, e, tx, s);
                    edim = s ? getValue(s) : getValue(e);
                }
            }
            if ((tp && tp.matchArg(sc, t.dim, i, &parameters, dedtypes, null)) ||
                (edim && edim.isIntegerExp() && edim.toInteger() == t.dim.toInteger())
            )
            {
                result = deduceType(t.next, sc, tparam.nextOf(), parameters, dedtypes, wm);
                return;
            }

            visit(cast(Type)t);
        }

        override void visit(TypeAArray t)
        {
            // Extra check that index type must match
            if (tparam && tparam.ty == Taarray)
            {
                TypeAArray tp = tparam.isTypeAArray();
                if (!deduceType(t.index, sc, tp.index, parameters, dedtypes))
                {
                    result = MATCH.nomatch;
                    return;
                }
            }
            visit(cast(Type)t);
        }

        override void visit(TypeFunction t)
        {
            // Extra check that function characteristics must match
            if (!tparam)
                return visit(cast(Type)t);

            auto tp = tparam.isTypeFunction();
            if (!tp)
            {
                visit(cast(Type)t);
                return;
            }

            if (t.parameterList.varargs != tp.parameterList.varargs || t.linkage != tp.linkage)
            {
                result = MATCH.nomatch;
                return;
            }

            foreach (fparam; *tp.parameterList.parameters)
            {
                // https://issues.dlang.org/show_bug.cgi?id=2579
                // Apply function parameter storage classes to parameter types
                fparam.type = fparam.type.addStorageClass(fparam.storageClass);
                fparam.storageClass &= ~STC.TYPECTOR;

                // https://issues.dlang.org/show_bug.cgi?id=15243
                // Resolve parameter type if it's not related with template parameters
                if (!reliesOnTemplateParameters(fparam.type, parameters[inferStart .. parameters.length]))
                {
                    auto tx = fparam.type.typeSemantic(Loc.initial, sc);
                    if (tx.ty == Terror)
                    {
                        result = MATCH.nomatch;
                        return;
                    }
                    fparam.type = tx;
                }
            }

            const size_t nfargs = t.parameterList.length;
            size_t nfparams = tp.parameterList.length;

            if (!deduceFunctionTuple(t, tp, parameters, dedtypes, nfargs, nfparams))
            {
                result = MATCH.nomatch;
                return;
            }

        L2:
            assert(nfparams <= tp.parameterList.length);
            foreach (i, ap; tp.parameterList)
            {
                if (i == nfparams)
                    break;

                Parameter a = t.parameterList[i];

                if (!a.isCovariant(t.isRef, ap) ||
                    !deduceType(a.type, sc, ap.type, parameters, dedtypes))
                {
                    result = MATCH.nomatch;
                    return;
                }
            }

            visit(cast(Type)t);
        }

        override void visit(TypeIdentifier t)
        {
            // Extra check
            if (tparam && tparam.ty == Tident)
            {
                TypeIdentifier tp = tparam.isTypeIdentifier();
                for (size_t i = 0; i < t.idents.length; i++)
                {
                    RootObject id1 = t.idents[i];
                    RootObject id2 = tp.idents[i];
                    if (!rootObjectsEqual(id1, id2))
                    {
                        result = MATCH.nomatch;
                        return;
                    }
                }
            }
            visit(cast(Type)t);
        }

        override void visit(TypeInstance t)
        {
            // Extra check
            if (!tparam || tparam.ty != Tinstance || !t.tempinst.tempdecl)
            {
                visit(cast(Type)t);
                return;
            }

            TemplateDeclaration tempdecl = t.tempinst.tempdecl.isTemplateDeclaration();
            assert(tempdecl);

            TypeInstance tp = tparam.isTypeInstance();

            //printf("tempinst.tempdecl = %p\n", tempdecl);
            //printf("tp.tempinst.tempdecl = %p\n", tp.tempinst.tempdecl);
            if (!tp.tempinst.tempdecl)
            {
                //printf("tp.tempinst.name = '%s'\n", tp.tempinst.name.toChars());

                /* Handle case of:
                 *  template Foo(T : sa!(T), alias sa)
                 */
                size_t i = templateIdentifierLookup(tp.tempinst.name, &parameters);
                if (i == IDX_NOTFOUND)
                {
                    /* Didn't find it as a parameter identifier. Try looking
                     * it up and seeing if is an alias.
                     * https://issues.dlang.org/show_bug.cgi?id=1454
                     */
                    auto tid = new TypeIdentifier(tp.loc, tp.tempinst.name);
                    Type tx;
                    Expression e;
                    Dsymbol s;
                    tid.resolve(tp.loc, sc, e, tx, s);
                    if (tx)
                    {
                        s = tx.toDsymbol(sc);
                        if (TemplateInstance ti = s ? s.parent.isTemplateInstance() : null)
                        {
                            // https://issues.dlang.org/show_bug.cgi?id=14290
                            // Try to match with ti.tempecl,
                            // only when ti is an enclosing instance.
                            Dsymbol p = sc.parent;
                            while (p && p != ti)
                                p = p.parent;
                            if (p)
                                s = ti.tempdecl;
                        }
                    }
                    if (s)
                    {
                        s = s.toAlias();
                        TemplateDeclaration td = s.isTemplateDeclaration();
                        if (td)
                        {
                            if (td.overroot)
                                td = td.overroot;
                            for (; td; td = td.overnext)
                            {
                                if (td == tempdecl)
                                    goto L2;
                            }
                        }
                    }
                    goto Lnomatch;
                }

                TemplateParameter tpx = parameters[i];
                if (!tpx.matchArg(sc, tempdecl, i, &parameters, dedtypes, null))
                    goto Lnomatch;
            }
            else if (tempdecl != tp.tempinst.tempdecl)
                goto Lnomatch;

        L2:
            if (!resolveTemplateInstantiation(sc, &parameters, t.tempinst.tiargs, &t.tempinst.tdtypes, tempdecl, tp, &dedtypes))
                goto Lnomatch;

            visit(cast(Type)t);
            return;

        Lnomatch:
            //printf("no match\n");
            result = MATCH.nomatch;
        }

        override void visit(TypeStruct t)
        {
            /* If this struct is a template struct, and we're matching
             * it against a template instance, convert the struct type
             * to a template instance, too, and try again.
             */
            TemplateInstance ti = t.sym.parent.isTemplateInstance();

            if (tparam && tparam.ty == Tinstance)
            {
                if (ti && ti.toAlias() == t.sym)
                {
                    auto tx = new TypeInstance(Loc.initial, ti);
                    auto m = deduceType(tx, sc, tparam, parameters, dedtypes, wm);
                    // if we have a no match we still need to check alias this
                    if (m != MATCH.nomatch)
                    {
                        result = m;
                        return;
                    }
                }

                TypeInstance tpi = tparam.isTypeInstance();
                auto m = deduceParentInstance(sc, t.sym, tpi, parameters, dedtypes, wm);
                if (m != MATCH.nomatch)
                {
                    result = m;
                    return;
                }
            }

            // Extra check
            if (tparam && tparam.ty == Tstruct)
            {
                TypeStruct tp = tparam.isTypeStruct();

                //printf("\t%d\n", cast(MATCH) t.implicitConvTo(tp));
                if (wm && t.deduceWild(tparam, false))
                {
                    result = MATCH.constant;
                    return;
                }
                result = t.implicitConvTo(tp);
                return;
            }
            visit(cast(Type)t);
        }

        override void visit(TypeEnum t)
        {
            // Extra check
            if (tparam && tparam.ty == Tenum)
            {
                TypeEnum tp = tparam.isTypeEnum();
                if (t.sym == tp.sym)
                    visit(cast(Type)t);
                else
                    result = MATCH.nomatch;
                return;
            }
            Type tb = t.toBasetype();
            if (tb.ty == tparam.ty || tb.ty == Tsarray && tparam.ty == Taarray)
            {
                result = deduceType(tb, sc, tparam, parameters, dedtypes, wm);
                if (result == MATCH.exact)
                    result = MATCH.convert;
                return;
            }
            visit(cast(Type)t);
        }

        override void visit(TypeClass t)
        {
            //printf("TypeClass.deduceType(this = %s)\n", t.toChars());

            /* If this class is a template class, and we're matching
             * it against a template instance, convert the class type
             * to a template instance, too, and try again.
             */
            TemplateInstance ti = t.sym.parent.isTemplateInstance();

            if (tparam && tparam.ty == Tinstance)
            {
                if (ti && ti.toAlias() == t.sym)
                {
                    auto tx = new TypeInstance(Loc.initial, ti);
                    MATCH m = deduceType(tx, sc, tparam, parameters, dedtypes, wm);
                    // Even if the match fails, there is still a chance it could match
                    // a base class.
                    if (m != MATCH.nomatch)
                    {
                        result = m;
                        return;
                    }
                }

                TypeInstance tpi = tparam.isTypeInstance();
                auto m = deduceParentInstance(sc, t.sym, tpi, parameters, dedtypes, wm);
                if (m != MATCH.nomatch)
                {
                    result = m;
                    return;
                }

                // If it matches exactly or via implicit conversion, we're done
                visit(cast(Type)t);
                if (result != MATCH.nomatch)
                    return;

                /* There is still a chance to match via implicit conversion to
                 * a base class or interface. Because there could be more than one such
                 * match, we need to check them all.
                 */

                int numBaseClassMatches = 0; // Have we found an interface match?

                // Our best guess at dedtypes
                auto best = new Objects(dedtypes.length);

                ClassDeclaration s = t.sym;
                while (s && s.baseclasses.length > 0)
                {
                    // Test the base class
                    deduceBaseClassParameters(*(*s.baseclasses)[0], sc, tparam, parameters, dedtypes, *best, numBaseClassMatches);

                    // Test the interfaces inherited by the base class
                    foreach (b; s.interfaces)
                    {
                        deduceBaseClassParameters(*b, sc, tparam, parameters, dedtypes, *best, numBaseClassMatches);
                    }
                    s = (*s.baseclasses)[0].sym;
                }

                if (numBaseClassMatches == 0)
                {
                    result = MATCH.nomatch;
                    return;
                }

                // If we got at least one match, copy the known types into dedtypes
                memcpy(dedtypes.tdata(), best.tdata(), best.length * (void*).sizeof);
                result = MATCH.convert;
                return;
            }

            // Extra check
            if (tparam && tparam.ty == Tclass)
            {
                TypeClass tp = tparam.isTypeClass();

                //printf("\t%d\n", cast(MATCH) t.implicitConvTo(tp));
                if (wm && t.deduceWild(tparam, false))
                {
                    result = MATCH.constant;
                    return;
                }
                result = t.implicitConvTo(tp);
                return;
            }
            visit(cast(Type)t);
        }

        override void visit(Expression e)
        {
            //printf("Expression.deduceType(e = %s)\n", e.toChars());
            size_t i = templateParameterLookup(tparam, &parameters);
            if (i == IDX_NOTFOUND || tparam.isTypeIdentifier().idents.length > 0)
            {
                if (e == emptyArrayElement && tparam.ty == Tarray)
                {
                    Type tn = (cast(TypeNext)tparam).next;
                    result = deduceType(emptyArrayElement, sc, tn, parameters, dedtypes, wm);
                    return;
                }
                e.type.accept(this);
                return;
            }

            TemplateTypeParameter tp = parameters[i].isTemplateTypeParameter();
            if (!tp)
                return; // nomatch

            if (e == emptyArrayElement)
            {
                if (dedtypes[i])
                {
                    result = MATCH.exact;
                    return;
                }
                if (tp.defaultType)
                {
                    tp.defaultType.accept(this);
                    return;
                }
            }

            Type at = cast(Type)dedtypes[i];
            Type tt;
            if (ubyte wx = deduceWildHelper(e.type, &tt, tparam))
            {
                *wm |= wx;
                result = MATCH.constant;
            }
            else if (MATCH m = deduceTypeHelper(e.type, tt, tparam))
            {
                result = m;
            }
            else if (!isTopRef(e.type))
            {
                /* https://issues.dlang.org/show_bug.cgi?id=15653
                 * In IFTI, recognize top-qualifier conversions
                 * through the value copy, e.g.
                 *      int --> immutable(int)
                 *      immutable(string[]) --> immutable(string)[]
                 */
                tt = e.type.mutableOf();
                result = MATCH.convert;
            }
            else
                return; // nomatch

            // expression vs (none)
            if (!at)
            {
                dedtypes[i] = new TypeDeduced(tt, e, tparam);
                return;
            }

            TypeDeduced xt = null;
            if (at.ty == Tnone)
            {
                xt = cast(TypeDeduced)at;
                at = xt.tded;
            }

            // From previous matched expressions to current deduced type
            MATCH match1 = xt ? xt.matchAll(tt) : MATCH.nomatch;

            // From current expressions to previous deduced type
            Type pt = at.addMod(tparam.mod);
            if (*wm)
                pt = pt.substWildTo(*wm);
            MATCH match2 = e.implicitConvTo(pt);

            if (match1 > MATCH.nomatch && match2 > MATCH.nomatch)
            {
                if (at.implicitConvTo(tt) == MATCH.nomatch)
                    match1 = MATCH.nomatch; // Prefer at
                else if (tt.implicitConvTo(at) == MATCH.nomatch)
                    match2 = MATCH.nomatch; // Prefer tt
                else if (tt.isTypeBasic() && tt.ty == at.ty && tt.mod != at.mod)
                {
                    if (!tt.isMutable() && !at.isMutable())
                        tt = tt.mutableOf().addMod(MODmerge(tt.mod, at.mod));
                    else if (tt.isMutable())
                    {
                        if (at.mod == 0) // Prefer unshared
                            match1 = MATCH.nomatch;
                        else
                            match2 = MATCH.nomatch;
                    }
                    else if (at.isMutable())
                    {
                        if (tt.mod == 0) // Prefer unshared
                            match2 = MATCH.nomatch;
                        else
                            match1 = MATCH.nomatch;
                    }
                    //printf("tt = %s, at = %s\n", tt.toChars(), at.toChars());
                }
                else
                {
                    match1 = MATCH.nomatch;
                    match2 = MATCH.nomatch;
                }
            }
            if (match1 > MATCH.nomatch)
            {
                // Prefer current match: tt
                if (xt)
                    xt.update(tt, e, tparam);
                else
                    dedtypes[i] = tt;
                result = match1;
                return;
            }
            if (match2 > MATCH.nomatch)
            {
                // Prefer previous match: (*dedtypes)[i]
                if (xt)
                    xt.update(e, tparam);
                result = match2;
                return;
            }

            /* Deduce common type
             */
            if (Type t = rawTypeMerge(at, tt))
            {
                if (xt)
                    xt.update(t, e, tparam);
                else
                    dedtypes[i] = t;

                pt = tt.addMod(tparam.mod);
                if (*wm)
                    pt = pt.substWildTo(*wm);
                result = e.implicitConvTo(pt);
                return;
            }

            result = MATCH.nomatch;
        }

        private MATCH deduceEmptyArrayElement()
        {
            if (!emptyArrayElement)
            {
                emptyArrayElement = new IdentifierExp(Loc.initial, Id.p); // dummy
                emptyArrayElement.type = Type.tvoid;
            }
            assert(tparam.ty == Tarray);

            Type tn = (cast(TypeNext)tparam).next;
            return deduceType(emptyArrayElement, sc, tn, parameters, dedtypes, wm);
        }

        override void visit(NullExp e)
        {
            if (tparam.ty == Tarray && e.type.ty == Tnull)
            {
                // tparam:T[] <- e:null (void[])
                result = deduceEmptyArrayElement();
                return;
            }
            visit(cast(Expression)e);
        }

        override void visit(StringExp e)
        {
            Type taai;
            if (e.type.ty == Tarray && (tparam.ty == Tsarray || tparam.ty == Taarray && (taai = (cast(TypeAArray)tparam).index).ty == Tident && (cast(TypeIdentifier)taai).idents.length == 0))
            {
                // Consider compile-time known boundaries
                e.type.nextOf().sarrayOf(e.len).accept(this);
                return;
            }
            visit(cast(Expression)e);
        }

        override void visit(ArrayLiteralExp e)
        {
            // https://issues.dlang.org/show_bug.cgi?id=20092
            if (e.elements && e.elements.length && e.type.toBasetype().nextOf().ty == Tvoid)
            {
                result = deduceEmptyArrayElement();
                return;
            }
            if ((!e.elements || !e.elements.length) && e.type.toBasetype().nextOf().ty == Tvoid && tparam.ty == Tarray)
            {
                // tparam:T[] <- e:[] (void[])
                result = deduceEmptyArrayElement();
                return;
            }

            if (tparam.ty == Tarray && e.elements && e.elements.length)
            {
                Type tn = (cast(TypeDArray)tparam).next;
                result = MATCH.exact;
                if (e.basis)
                {
                    MATCH m = deduceType(e.basis, sc, tn, parameters, dedtypes, wm);
                    if (m < result)
                        result = m;
                }
                foreach (el; *e.elements)
                {
                    if (result == MATCH.nomatch)
                        break;
                    if (!el)
                        continue;
                    MATCH m = deduceType(el, sc, tn, parameters, dedtypes, wm);
                    if (m < result)
                        result = m;
                }
                return;
            }

            Type taai;
            if (e.type.ty == Tarray && (tparam.ty == Tsarray || tparam.ty == Taarray && (taai = (cast(TypeAArray)tparam).index).ty == Tident && (cast(TypeIdentifier)taai).idents.length == 0))
            {
                // Consider compile-time known boundaries
                e.type.nextOf().sarrayOf(e.elements.length).accept(this);
                return;
            }
            visit(cast(Expression)e);
        }

        override void visit(AssocArrayLiteralExp e)
        {
            if (tparam.ty == Taarray && e.keys && e.keys.length)
            {
                TypeAArray taa = cast(TypeAArray)tparam;
                result = MATCH.exact;
                foreach (i, key; *e.keys)
                {
                    MATCH m1 = deduceType(key, sc, taa.index, parameters, dedtypes, wm);
                    if (m1 < result)
                        result = m1;
                    if (result == MATCH.nomatch)
                        break;
                    MATCH m2 = deduceType((*e.values)[i], sc, taa.next, parameters, dedtypes, wm);
                    if (m2 < result)
                        result = m2;
                    if (result == MATCH.nomatch)
                        break;
                }
                return;
            }
            visit(cast(Expression)e);
        }

        override void visit(FuncExp e)
        {
            //printf("e.type = %s, tparam = %s\n", e.type.toChars(), tparam.toChars());
            if (e.td)
            {
                Type to = tparam;
                if (!to.nextOf())
                    return;
                auto tof = to.nextOf().isTypeFunction();
                if (!tof)
                    return;

                // Parameter types inference from 'tof'
                assert(e.td._scope);
                TypeFunction tf = e.fd.type.isTypeFunction();
                //printf("\ttof = %s\n", tof.toChars());
                //printf("\ttf  = %s\n", tf.toChars());
                const dim = tf.parameterList.length;

                if (tof.parameterList.length != dim || tof.parameterList.varargs != tf.parameterList.varargs)
                    return;

                auto tiargs = new Objects();
                tiargs.reserve(e.td.parameters.length);

                foreach (tp; *e.td.parameters)
                {
                    size_t u = 0;
                    foreach (i, p; tf.parameterList)
                    {
                        if (p.type.ty == Tident && (cast(TypeIdentifier)p.type).ident == tp.ident)
                            break;
                        ++u;
                    }
                    assert(u < dim);
                    Parameter pto = tof.parameterList[u];
                    if (!pto)
                        break;
                    Type t = pto.type.syntaxCopy(); // https://issues.dlang.org/show_bug.cgi?id=11774
                    if (reliesOnTemplateParameters(t, parameters[inferStart .. parameters.length]))
                        return;
                    t = t.typeSemantic(e.loc, sc);
                    if (t.ty == Terror)
                        return;
                    tiargs.push(t);
                }

                // Set target of return type inference
                if (!tf.next && tof.next)
                    e.fd.treq = tparam;

                auto ti = new TemplateInstance(e.loc, e.td, tiargs);
                Expression ex = (new ScopeExp(e.loc, ti)).expressionSemantic(e.td._scope);

                // Reset inference target for the later re-semantic
                e.fd.treq = null;

                if (ex.op == EXP.error)
                    return;
                if (ex.op != EXP.function_)
                    return;
                visit(ex.type);
                return;
            }

            Type t = e.type;

            if (t.ty == Tdelegate && tparam.ty == Tpointer)
                return;

            // Allow conversion from implicit function pointer to delegate
            if (e.tok == TOK.reserved && t.ty == Tpointer && tparam.ty == Tdelegate)
            {
                TypeFunction tf = t.nextOf().isTypeFunction();
                t = (new TypeDelegate(tf)).merge();
            }
            //printf("tparam = %s <= e.type = %s, t = %s\n", tparam.toChars(), e.type.toChars(), t.toChars());
            visit(t);
        }

        override void visit(SliceExp e)
        {
            Type taai;
            if (e.type.ty == Tarray && (tparam.ty == Tsarray || tparam.ty == Taarray && (taai = (cast(TypeAArray)tparam).index).ty == Tident && (cast(TypeIdentifier)taai).idents.length == 0))
            {
                // Consider compile-time known boundaries
                if (Type tsa = toStaticArrayType(e))
                {
                    tsa.accept(this);
                    if (result > MATCH.convert)
                        result = MATCH.convert; // match with implicit conversion at most
                    return;
                }
            }
            visit(cast(Expression)e);
        }

        override void visit(CommaExp e)
        {
            e.e2.accept(this);
        }
    }

    scope DeduceType v = new DeduceType();
    if (Type t = isType(o))
        t.accept(v);
    else if (Expression e = isExpression(o))
    {
        assert(wm);
        e.accept(v);
    }
    else
        assert(0);
    return v.result;
}


/* Helper for TypeClass.deduceType().
 * Classes can match with implicit conversion to a base class or interface.
 * This is complicated, because there may be more than one base class which
 * matches. In such cases, one or more parameters remain ambiguous.
 * For example,
 *
 *   interface I(X, Y) {}
 *   class C : I(uint, double), I(char, double) {}
 *   C x;
 *   foo(T, U)( I!(T, U) x)
 *
 *   deduces that U is double, but T remains ambiguous (could be char or uint).
 *
 * Given a baseclass b, and initial deduced types 'dedtypes', this function
 * tries to match tparam with b, and also tries all base interfaces of b.
 * If a match occurs, numBaseClassMatches is incremented, and the new deduced
 * types are ANDed with the current 'best' estimate for dedtypes.
 */
private void deduceBaseClassParameters(ref BaseClass b, Scope* sc, Type tparam, ref TemplateParameters parameters, ref Objects dedtypes, ref Objects best, ref int numBaseClassMatches)
{
    if (TemplateInstance parti = b.sym ? b.sym.parent.isTemplateInstance() : null)
    {
        // Make a temporary copy of dedtypes so we don't destroy it
        auto tmpdedtypes = new Objects(dedtypes.length);
        memcpy(tmpdedtypes.tdata(), dedtypes.tdata(), dedtypes.length * (void*).sizeof);

        auto t = new TypeInstance(Loc.initial, parti);
        MATCH m = deduceType(t, sc, tparam, parameters, *tmpdedtypes);
        if (m > MATCH.nomatch)
        {
            // If this is the first ever match, it becomes our best estimate
            if (numBaseClassMatches == 0)
                memcpy(best.tdata(), tmpdedtypes.tdata(), tmpdedtypes.length * (void*).sizeof);
            else
                for (size_t k = 0; k < tmpdedtypes.length; ++k)
                {
                    // If we've found more than one possible type for a parameter,
                    // mark it as unknown.
                    if ((*tmpdedtypes)[k] != best[k])
                        best[k] = dedtypes[k];
                }
            ++numBaseClassMatches;
        }
    }

    // Now recursively test the inherited interfaces
    foreach (ref bi; b.baseInterfaces)
    {
        deduceBaseClassParameters(bi, sc, tparam, parameters, dedtypes, best, numBaseClassMatches);
    }
}

private bool rootObjectsEqual(RootObject o1, RootObject o2)
{
    auto d = o1.dyncast();
    if (d != o2.dyncast())
        return false;
    bool check(T)(RootObject id1, RootObject id2)
    {
        return (cast(T)id1).equals(cast(T)id2);
    }
    with (DYNCAST) final switch(d)
    {
        case expression:   return check!Expression(o1, o2);
        case dsymbol:      return check!Dsymbol   (o1, o2);
        case type:         return check!Type      (o1, o2);
        case identifier: //return check!Identifier(o1, o2); // Identifier.equals checks `o1 is o2`
        case object:
        case tuple:
        case parameter:
        case statement:
        case condition:
        case templateparameter:
        case initializer:
            return o1 is o2;
    }
}
/*
 * Handle tuple matching for function parameters.
 * If the last parameter of `tp` is a template tuple parameter,
 * collect the corresponding argument types from `t`.
 * Params:
 *     t          = actual function type
 *     tp         = template function type
 *     parameters = template parameters
 *     dedtypes   = deduced types array
 *     nfargs     = number of arguments in `t`
 *     nfparams   = number of parameters in `tp` (updated on success)
 * Returns: `true` on success, `false` on mismatch.
 */
private bool deduceFunctionTuple(TypeFunction t, TypeFunction tp,
    ref TemplateParameters parameters, ref Objects dedtypes,
    size_t nfargs, ref size_t nfparams)
{
    if (nfparams == 0 || nfargs < nfparams - 1)
        return nfargs == nfparams;

    Parameter fparam = tp.parameterList[nfparams - 1];
    assert(fparam && fparam.type);
    if (fparam.type.ty != Tident)
        return nfargs == nfparams;

    TypeIdentifier tid = fparam.type.isTypeIdentifier();
    if (tid.idents.length != 0)
        return nfargs == nfparams;

    size_t tupi = 0;
    for (; tupi < parameters.length; ++tupi)
    {
        TemplateParameter tx = parameters[tupi];
        TemplateTupleParameter tup = tx.isTemplateTupleParameter();
        if (tup && tup.ident.equals(tid.ident))
            break;
    }
    if (tupi == parameters.length)
        return nfargs == nfparams;

    size_t tuple_dim = nfargs - (nfparams - 1);

    RootObject o = dedtypes[tupi];
    if (o)
    {
        Tuple tup = isTuple(o);
        if (!tup || tup.objects.length != tuple_dim)
            return false;
        for (size_t i = 0; i < tuple_dim; ++i)
        {
            if (!rootObjectsEqual(t.parameterList[nfparams - 1 + i].type,
                                  tup.objects[i]))
                return false;
        }
    }
    else
    {
        auto tup = new Tuple(tuple_dim);
        for (size_t i = 0; i < tuple_dim; ++i)
            tup.objects[i] = t.parameterList[nfparams - 1 + i].type;
        dedtypes[tupi] = tup;
    }
    --nfparams; // ignore tuple parameter for further deduction
    return true;
}

/********************
 * Match template `parameters` to the target template instance.
 * Example:
 *    struct Temp(U, int Z) {}
 *    void foo(T)(Temp!(T, 3));
 *    foo(Temp!(int, 3)());
 * Input:
 *    sc               = context
 *    parameters       = template params of foo -> [T]
 *    tiargs           = <Temp!(int, 3)>.tiargs  -> [int, 3]
 *    tdtypes          = <Temp!(int, 3)>.tdtypes -> [int, 3]
 *    tempdecl         = <struct Temp!(T, int Z)> -> [T, Z]
 *    tp               = <Temp!(T, 3)>
 * Output:
 *    dedtypes         = deduced params of `foo(Temp!(int, 3)())` -> [int]
 */
private bool resolveTemplateInstantiation(Scope* sc, TemplateParameters* parameters, Objects* tiargs, Objects* tdtypes, TemplateDeclaration tempdecl, TypeInstance tp, Objects* dedtypes)
{
    for (size_t i = 0; 1; i++)
    {
        //printf("\ttest: tempinst.tiargs[%zu]\n", i);
        RootObject o1 = null;
        if (i < tiargs.length)
            o1 = (*tiargs)[i];
        else if (i < tdtypes.length && i < tp.tempinst.tiargs.length)
        {
            // Pick up default arg
            o1 = (*tdtypes)[i];
        }
        else if (i >= tp.tempinst.tiargs.length)
            break;
        //printf("\ttest: o1 = %s\n", o1.toChars());
        if (i >= tp.tempinst.tiargs.length)
        {
            size_t dim = tempdecl.parameters.length - (tempdecl.isVariadic() ? 1 : 0);
            while (i < dim && ((*tempdecl.parameters)[i].dependent || (*tempdecl.parameters)[i].hasDefaultArg()))
            {
                i++;
            }
            if (i >= dim)
                break; // match if all remained parameters are dependent
            return false;
        }

        RootObject o2 = (*tp.tempinst.tiargs)[i];
        Type t2 = isType(o2);
        //printf("\ttest: o2 = %s\n", o2.toChars());
        size_t j = (t2 && t2.ty == Tident && i == tp.tempinst.tiargs.length - 1)
            ? templateParameterLookup(t2, parameters) : IDX_NOTFOUND;
        if (j != IDX_NOTFOUND && j == parameters.length - 1 &&
            (*parameters)[j].isTemplateTupleParameter())
        {
            /* Given:
                 *  struct A(B...) {}
                 *  alias A!(int, float) X;
                 *  static if (is(X Y == A!(Z), Z...)) {}
                 * deduce that Z is a tuple(int, float)
                 */

            /* Create tuple from remaining args
                 */
            size_t vtdim = (tempdecl.isVariadic() ? tiargs.length : tdtypes.length) - i;
            auto vt = new Tuple(vtdim);
            for (size_t k = 0; k < vtdim; k++)
            {
                RootObject o;
                if (k < tiargs.length)
                    o = (*tiargs)[i + k];
                else // Pick up default arg
                    o = (*tdtypes)[i + k];
                vt.objects[k] = o;
            }

            Tuple v = cast(Tuple)(*dedtypes)[j];
            if (v)
            {
                if (!match(v, vt))
                    return false;
            }
            else
                (*dedtypes)[j] = vt;
            break;
        }
        else if (!o1)
            break;

        Type t1 = isType(o1);
        Dsymbol s1 = isDsymbol(o1);
        Dsymbol s2 = isDsymbol(o2);
        Expression e1 = s1 ? getValue(s1) : getValue(isExpression(o1));
        Expression e2 = isExpression(o2);
        version (none)
        {
            Tuple v1 = isTuple(o1);
            Tuple v2 = isTuple(o2);
            if (t1)
                printf("t1 = %s\n", t1.toChars());
            if (t2)
                printf("t2 = %s\n", t2.toChars());
            if (e1)
                printf("e1 = %s\n", e1.toChars());
            if (e2)
                printf("e2 = %s\n", e2.toChars());
            if (s1)
                printf("s1 = %s\n", s1.toChars());
            if (s2)
                printf("s2 = %s\n", s2.toChars());
            if (v1)
                printf("v1 = %s\n", v1.toChars());
            if (v2)
                printf("v2 = %s\n", v2.toChars());
        }

        if (t1 && t2)
        {
            if (!deduceType(t1, sc, t2, *parameters, *dedtypes))
                return false;
        }
        else if (e1 && e2)
        {
        Le:
            e1 = e1.ctfeInterpret();

            /* If it is one of the template parameters for this template,
                 * we should not attempt to interpret it. It already has a value.
                 */
            if (e2.op == EXP.variable && (e2.isVarExp().var.storage_class & STC.templateparameter))
            {
                /*
                     * (T:Number!(e2), int e2)
                     */
                j = templateIdentifierLookup(e2.isVarExp().var.ident, parameters);
                if (j != IDX_NOTFOUND)
                    goto L1;
                // The template parameter was not from this template
                // (it may be from a parent template, for example)
            }

            e2 = e2.expressionSemantic(sc); // https://issues.dlang.org/show_bug.cgi?id=13417
            e2 = e2.ctfeInterpret();

            //printf("e1 = %s, type = %s %d\n", e1.toChars(), e1.type.toChars(), e1.type.ty);
            //printf("e2 = %s, type = %s %d\n", e2.toChars(), e2.type.toChars(), e2.type.ty);
            if (!e1.equals(e2))
            {
                if (!e2.implicitConvTo(e1.type))
                    return false;

                e2 = e2.implicitCastTo(sc, e1.type);
                e2 = e2.ctfeInterpret();
                if (!e1.equals(e2))
                    return false;
            }
        }
        else if (e1 && t2 && t2.ty == Tident)
        {
            j = templateParameterLookup(t2, parameters);
        L1:
            if (j == IDX_NOTFOUND)
            {
                t2.resolve((cast(TypeIdentifier)t2).loc, sc, e2, t2, s2);
                if (e2)
                    goto Le;
                return false;
            }
            if (!(*parameters)[j].matchArg(sc, e1, j, parameters, *dedtypes, null))
                return false;
        }
        else if (s1 && s2)
        {
        Ls:
            if (!s1.equals(s2))
                return false;
        }
        else if (s1 && t2 && t2.ty == Tident)
        {
            j = templateParameterLookup(t2, parameters);
            if (j == IDX_NOTFOUND)
            {
                t2.resolve((cast(TypeIdentifier)t2).loc, sc, e2, t2, s2);
                if (s2)
                    goto Ls;
                return false;
            }
            if (!(*parameters)[j].matchArg(sc, s1, j, parameters, *dedtypes, null))
                return false;
        }
        else
            return false;
    }
    return true;
}

private  Expression getValue(ref Dsymbol s)
{
    if (s)
    {
        if (VarDeclaration v = s.isVarDeclaration())
        {
            if (v.storage_class & STC.manifest)
                return v.getConstInitializer();
        }
    }
    return null;
}

/***********************
 * Try to get value from manifest constant
 */
private Expression getValue(Expression e)
{
    if (!e)
        return null;
    if (auto ve = e.isVarExp())
    {
        if (auto v = ve.var.isVarDeclaration())
        {
            if (v.storage_class & STC.manifest)
            {
                e = v.getConstInitializer();
            }
        }
    }
    return e;
}

private Expression getExpression(RootObject o)
{
    auto s = isDsymbol(o);
    return s ? .getValue(s) : .getValue(isExpression(o));
}
