
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/cond.c
 */

#include "root/dsystem.h"               // strcmp()

#include "mars.h"
#include "id.h"
#include "init.h"
#include "aggregate.h"
#include "declaration.h"
#include "identifier.h"
#include "expression.h"
#include "cond.h"
#include "module.h"
#include "template.h"
#include "mtype.h"
#include "scope.h"
#include "statement.h"
#include "arraytypes.h"
#include "tokens.h"

Expression *semantic(Expression *e, Scope *sc);
bool evalStaticCondition(Scope *sc, Expression *exp, Expression *e, bool &errors);

int findCondition(Strings *ids, Identifier *ident)
{
    if (ids)
    {
        for (size_t i = 0; i < ids->dim; i++)
        {
            const char *id = (*ids)[i];

            if (strcmp(id, ident->toChars()) == 0)
                return true;
        }
    }

    return false;
}

/* ============================================================ */

Condition::Condition(Loc loc)
{
    this->loc = loc;
    inc = 0;
}

/* ============================================================ */

StaticForeach::StaticForeach(Loc loc, ForeachStatement *aggrfe, ForeachRangeStatement *rangefe)
{
    assert(!!aggrfe ^ !!rangefe);
    this->loc = loc;
    this->aggrfe = aggrfe;
    this->rangefe = rangefe;
    this->needExpansion = false;
}

StaticForeach *StaticForeach::syntaxCopy()
{
    return new StaticForeach(
        loc,
        aggrfe ? (ForeachStatement *)aggrfe->syntaxCopy() : NULL,
        rangefe ? (ForeachRangeStatement *)rangefe->syntaxCopy() : NULL
    );
}

/*****************************************
 * Turn an aggregate which is an array into an expression tuple
 * of its elements. I.e., lower
 *     static foreach (x; [1, 2, 3, 4]) { ... }
 * to
 *     static foreach (x; AliasSeq!(1, 2, 3, 4)) { ... }
 */

static void lowerArrayAggregate(StaticForeach *sfe, Scope *sc)
{
    Expression *aggr = sfe->aggrfe->aggr;
    Expression *el = new ArrayLengthExp(aggr->loc, aggr);
    sc = sc->startCTFE();
    el = semantic(el, sc);
    sc = sc->endCTFE();
    el = el->optimize(WANTvalue);
    el = el->ctfeInterpret();
    if (el->op == TOKint64)
    {
        dinteger_t length = el->toInteger();
        Expressions *es = new Expressions();
        for (size_t i = 0; i < length; i++)
        {
            IntegerExp *index = new IntegerExp(sfe->loc, i, Type::tsize_t);
            Expression *value = new IndexExp(aggr->loc, aggr, index);
            es->push(value);
        }
        sfe->aggrfe->aggr = new TupleExp(aggr->loc, es);
        sfe->aggrfe->aggr = semantic(sfe->aggrfe->aggr, sc);
        sfe->aggrfe->aggr = sfe->aggrfe->aggr->optimize(WANTvalue);
    }
    else
    {
        sfe->aggrfe->aggr = new ErrorExp();
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

static Expression *wrapAndCall(Loc loc, Statement *s)
{
    TypeFunction *tf = new TypeFunction(new Parameters(), NULL, 0, LINKdefault, 0);
    FuncLiteralDeclaration *fd = new FuncLiteralDeclaration(loc, loc, tf, TOKreserved, NULL);
    fd->fbody = s;
    FuncExp *fe = new FuncExp(loc, fd);
    Expression *ce = new CallExp(loc, fe, new Expressions());
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

static Statement *createForeach(StaticForeach *sfe, Loc loc, Parameters *parameters, Statement *s)
{
    if (sfe->aggrfe)
    {
        return new ForeachStatement(loc, sfe->aggrfe->op, parameters, sfe->aggrfe->aggr->syntaxCopy(), s, loc);
    }
    else
    {
        assert(sfe->rangefe && parameters->dim == 1);
        return new ForeachRangeStatement(loc, sfe->rangefe->op, (*parameters)[0],
                                         sfe->rangefe->lwr->syntaxCopy(),
                                         sfe->rangefe->upr->syntaxCopy(), s, loc);
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

static TypeStruct *createTupleType(Loc loc, Expressions *e)
{   // TODO: move to druntime?
    Identifier *sid = Identifier::generateId("Tuple");
    StructDeclaration *sdecl = new StructDeclaration(loc, sid, false);
    sdecl->storage_class |= STCstatic;
    sdecl->members = new Dsymbols();
    Identifier *fid = Identifier::idPool("tuple");
    Type *ty = new TypeTypeof(loc, new TupleExp(loc, e));
    sdecl->members->push(new VarDeclaration(loc, ty, fid, NULL));
    TypeStruct *r = (TypeStruct *)sdecl->type;
    r->vtinfo = TypeInfoStructDeclaration::create(r); // prevent typeinfo from going to object file
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

static Expression *createTuple(Loc loc, TypeStruct *type, Expressions *e)
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

static void lowerNonArrayAggregate(StaticForeach *sfe, Scope *sc)
{
    size_t nvars = sfe->aggrfe ? sfe->aggrfe->parameters->dim : 1;
    Loc aloc = sfe->aggrfe ? sfe->aggrfe->aggr->loc : sfe->rangefe->lwr->loc;
    // We need three sets of foreach loop variables because the
    // lowering contains three foreach loops.
    Parameters *pparams[3] = {new Parameters(), new Parameters(), new Parameters()};
    for (size_t i = 0; i < nvars; i++)
    {
        for (size_t j = 0; j < 3; j++)
        {
            Parameters *params = pparams[j];
            Parameter *p = sfe->aggrfe ? (*sfe->aggrfe->parameters)[i] : sfe->rangefe->prm;
            params->push(new Parameter(p->storageClass, p->type, p->ident, NULL));
        }
    }
    Expression *res[2];
    TypeStruct *tplty = NULL;
    if (nvars == 1) // only one `static foreach` variable, generate identifiers.
    {
        for (size_t i = 0; i < 2; i++)
        {
            res[i] = new IdentifierExp(aloc, (*pparams[i])[0]->ident);
        }
    }
    else // multiple `static foreach` variables, generate tuples.
    {
        for (size_t i = 0; i < 2; i++)
        {
            Expressions *e = new Expressions();
            for (size_t j = 0; j < pparams[0]->dim; j++)
            {
                Parameter *p = (*pparams[i])[j];
                e->push(new IdentifierExp(aloc, p->ident));
            }
            if (!tplty)
            {
                tplty = createTupleType(aloc, e);
            }
            res[i] = createTuple(aloc, tplty, e);
        }
        sfe->needExpansion = true; // need to expand the tuples later
    }
    // generate remaining code for the new aggregate which is an
    // array (see documentation comment).
    if (sfe->rangefe)
    {
        sc = sc->startCTFE();
        sfe->rangefe->lwr = semantic(sfe->rangefe->lwr, sc);
        sfe->rangefe->lwr = resolveProperties(sc, sfe->rangefe->lwr);
        sfe->rangefe->upr = semantic(sfe->rangefe->upr, sc);
        sfe->rangefe->upr = resolveProperties(sc, sfe->rangefe->upr);
        sc = sc->endCTFE();
        sfe->rangefe->lwr = sfe->rangefe->lwr->optimize(WANTvalue);
        sfe->rangefe->lwr = sfe->rangefe->lwr->ctfeInterpret();
        sfe->rangefe->upr = sfe->rangefe->upr->optimize(WANTvalue);
        sfe->rangefe->upr = sfe->rangefe->upr->ctfeInterpret();
    }
    Statements *s1 = new Statements();
    Statements *sfebody = new Statements();
    if (tplty) sfebody->push(new ExpStatement(sfe->loc, tplty->sym));
    sfebody->push(new ReturnStatement(aloc, res[0]));
    s1->push(createForeach(sfe, aloc, pparams[0], new CompoundStatement(aloc, sfebody)));
    s1->push(new ExpStatement(aloc, new AssertExp(aloc, new IntegerExp(aloc, 0, Type::tint32))));
    Type *ety = new TypeTypeof(aloc, wrapAndCall(aloc, new CompoundStatement(aloc, s1)));
    Type *aty = ety->arrayOf();
    Identifier *idres = Identifier::generateId("__res");
    VarDeclaration *vard = new VarDeclaration(aloc, aty, idres, NULL);
    Statements *s2 = new Statements();
    s2->push(new ExpStatement(aloc, vard));
    Expression *catass = new CatAssignExp(aloc, new IdentifierExp(aloc, idres), res[1]);
    s2->push(createForeach(sfe, aloc, pparams[1], new ExpStatement(aloc, catass)));
    s2->push(new ReturnStatement(aloc, new IdentifierExp(aloc, idres)));
    Expression *aggr = wrapAndCall(aloc, new CompoundStatement(aloc, s2));
    sc = sc->startCTFE();
    aggr = semantic(aggr, sc);
    aggr = resolveProperties(sc, aggr);
    sc = sc->endCTFE();
    aggr = aggr->optimize(WANTvalue);
    aggr = aggr->ctfeInterpret();

    assert(!!sfe->aggrfe ^ !!sfe->rangefe);
    sfe->aggrfe = new ForeachStatement(sfe->loc, TOKforeach, pparams[2], aggr,
                                  sfe->aggrfe ? sfe->aggrfe->_body : sfe->rangefe->_body,
                                  sfe->aggrfe ? sfe->aggrfe->endloc : sfe->rangefe->endloc);
    sfe->rangefe = NULL;
    lowerArrayAggregate(sfe, sc); // finally, turn generated array into expression tuple
}

/*****************************************
 * Perform `static foreach` lowerings that are necessary in order
 * to finally expand the `static foreach` using
 * `ddmd.statementsem.makeTupleForeach`.
 */

void staticForeachPrepare(StaticForeach *sfe, Scope *sc)
{
    assert(sc);
    if (sfe->aggrfe)
    {
        sc = sc->startCTFE();
        sfe->aggrfe->aggr = semantic(sfe->aggrfe->aggr, sc);
        sc = sc->endCTFE();
        sfe->aggrfe->aggr = sfe->aggrfe->aggr->optimize(WANTvalue);
        Type *tab = sfe->aggrfe->aggr->type->toBasetype();
        if (tab->ty != Ttuple)
        {
            sfe->aggrfe->aggr = sfe->aggrfe->aggr->ctfeInterpret();
        }
    }

    if (sfe->aggrfe && sfe->aggrfe->aggr->type->toBasetype()->ty == Terror)
    {
        return;
    }

    if (!staticForeachReady(sfe))
    {
        if (sfe->aggrfe && sfe->aggrfe->aggr->type->toBasetype()->ty == Tarray)
        {
            lowerArrayAggregate(sfe, sc);
        }
        else
        {
            lowerNonArrayAggregate(sfe, sc);
        }
    }
}

/*****************************************
 * Returns:
 *     `true` iff ready to call `ddmd.statementsem.makeTupleForeach`.
 */

bool staticForeachReady(StaticForeach *sfe)
{
    return sfe->aggrfe && sfe->aggrfe->aggr && sfe->aggrfe->aggr->type &&
        sfe->aggrfe->aggr->type->toBasetype()->ty == Ttuple;
}

/* ============================================================ */

DVCondition::DVCondition(Module *mod, unsigned level, Identifier *ident)
        : Condition(Loc())
{
    this->mod = mod;
    this->level = level;
    this->ident = ident;
}

Condition *DVCondition::syntaxCopy()
{
    return this;        // don't need to copy
}

/* ============================================================ */

void DebugCondition::setGlobalLevel(unsigned level)
{
    global.params.debuglevel = level;
}

void DebugCondition::addGlobalIdent(const char *ident)
{
    if (!global.params.debugids)
        global.params.debugids = new Strings();
    global.params.debugids->push(ident);
}


DebugCondition::DebugCondition(Module *mod, unsigned level, Identifier *ident)
    : DVCondition(mod, level, ident)
{
}

// Helper for printing dependency information
void printDepsConditional(Scope *sc, DVCondition* condition, const char* depType)
{
    if (!global.params.moduleDeps || global.params.moduleDepsFile)
        return;
    OutBuffer *ob = global.params.moduleDeps;
    Module* imod = sc ? sc->instantiatingModule() : condition->mod;
    if (!imod)
        return;
    ob->writestring(depType);
    ob->writestring(imod->toPrettyChars());
    ob->writestring(" (");
    escapePath(ob, imod->srcfile->toChars());
    ob->writestring(") : ");
    if (condition->ident)
        ob->printf("%s\n", condition->ident->toChars());
    else
        ob->printf("%d\n", condition->level);
}


int DebugCondition::include(Scope *sc, ScopeDsymbol *)
{
    //printf("DebugCondition::include() level = %d, debuglevel = %d\n", level, global.params.debuglevel);
    if (inc == 0)
    {
        inc = 2;
        bool definedInModule = false;
        if (ident)
        {
            if (findCondition(mod->debugids, ident))
            {
                inc = 1;
                definedInModule = true;
            }
            else if (findCondition(global.params.debugids, ident))
                inc = 1;
            else
            {   if (!mod->debugidsNot)
                    mod->debugidsNot = new Strings();
                mod->debugidsNot->push(ident->toChars());
            }
        }
        else if (level <= global.params.debuglevel || level <= mod->debuglevel)
            inc = 1;
        if (!definedInModule)
            printDepsConditional(sc, this, "depsDebug ");
    }
    return (inc == 1);
}

/* ============================================================ */

void VersionCondition::setGlobalLevel(unsigned level)
{
    global.params.versionlevel = level;
}

static bool isReserved(const char *ident)
{
    static const char* reserved[] =
    {
        "DigitalMars",
        "GNU",
        "LDC",
        "SDC",
        "Windows",
        "Win32",
        "Win64",
        "linux",
        "OSX",
        "FreeBSD",
        "OpenBSD",
        "NetBSD",
        "DragonFlyBSD",
        "BSD",
        "Solaris",
        "Posix",
        "AIX",
        "Haiku",
        "SkyOS",
        "SysV3",
        "SysV4",
        "Hurd",
        "Android",
        "PlayStation",
        "PlayStation4",
        "Cygwin",
        "MinGW",
        "FreeStanding",
        "X86",
        "X86_64",
        "ARM",
        "ARM_Thumb",
        "ARM_SoftFloat",
        "ARM_SoftFP",
        "ARM_HardFloat",
        "AArch64",
        "Epiphany",
        "PPC",
        "PPC_SoftFloat",
        "PPC_HardFloat",
        "PPC64",
        "IA64",
        "MIPS32",
        "MIPS64",
        "MIPS_O32",
        "MIPS_N32",
        "MIPS_O64",
        "MIPS_N64",
        "MIPS_EABI",
        "MIPS_SoftFloat",
        "MIPS_HardFloat",
        "MSP430",
        "NVPTX",
        "NVPTX64",
        "RISCV32",
        "RISCV64",
        "SPARC",
        "SPARC_V8Plus",
        "SPARC_SoftFloat",
        "SPARC_HardFloat",
        "SPARC64",
        "S390",
        "S390X",
        "HPPA",
        "HPPA64",
        "SH",
        "Alpha",
        "Alpha_SoftFloat",
        "Alpha_HardFloat",
        "LittleEndian",
        "BigEndian",
        "ELFv1",
        "ELFv2",
        "CRuntime_Digitalmars",
        "CRuntime_Glibc",
        "CRuntime_Microsoft",
        "CRuntime_Musl",
        "CRuntime_UClibc",
        "CppRuntime_Clang",
        "CppRuntime_DigitalMars",
        "CppRuntime_Gcc",
        "CppRuntime_Microsoft",
        "CppRuntime_Sun",
        "D_Coverage",
        "D_Ddoc",
        "D_InlineAsm_X86",
        "D_InlineAsm_X86_64",
        "D_LP64",
        "D_X32",
        "D_HardFloat",
        "D_SoftFloat",
        "D_PIC",
        "D_SIMD",
        "D_Version2",
        "D_NoBoundsChecks",
        "unittest",
        "assert",
        "all",
        "none",
        NULL
    };

    for (unsigned i = 0; reserved[i]; i++)
    {
        if (strcmp(ident, reserved[i]) == 0)
            return true;
    }

    if (ident[0] == 'D' && ident[1] == '_')
        return true;
    return false;
}

void checkReserved(Loc loc, const char *ident)
{
    if (isReserved(ident))
        error(loc, "version identifier '%s' is reserved and cannot be set", ident);
}

void VersionCondition::addGlobalIdent(const char *ident)
{
    checkReserved(Loc(), ident);
    addPredefinedGlobalIdent(ident);
}

void VersionCondition::addPredefinedGlobalIdent(const char *ident)
{
    if (!global.params.versionids)
        global.params.versionids = new Strings();
    global.params.versionids->push(ident);
}


VersionCondition::VersionCondition(Module *mod, unsigned level, Identifier *ident)
    : DVCondition(mod, level, ident)
{
}

int VersionCondition::include(Scope *sc, ScopeDsymbol *)
{
    //printf("VersionCondition::include() level = %d, versionlevel = %d\n", level, global.params.versionlevel);
    //if (ident) printf("\tident = '%s'\n", ident->toChars());
    if (inc == 0)
    {
        inc = 2;
        bool definedInModule=false;
        if (ident)
        {
            if (findCondition(mod->versionids, ident))
            {
                inc = 1;
                definedInModule = true;
            }
            else if (findCondition(global.params.versionids, ident))
                inc = 1;
            else
            {
                if (!mod->versionidsNot)
                    mod->versionidsNot = new Strings();
                mod->versionidsNot->push(ident->toChars());
            }
        }
        else if (level <= global.params.versionlevel || level <= mod->versionlevel)
            inc = 1;
        if (!definedInModule && (!ident || (!isReserved(ident->toChars()) && ident != Id::_unittest && ident != Id::_assert)))
            printDepsConditional(sc, this, "depsVersion ");
    }
    return (inc == 1);
}

/**************************** StaticIfCondition *******************************/

StaticIfCondition::StaticIfCondition(Loc loc, Expression *exp)
    : Condition(loc)
{
    this->exp = exp;
}

Condition *StaticIfCondition::syntaxCopy()
{
    return new StaticIfCondition(loc, exp->syntaxCopy());
}

int StaticIfCondition::include(Scope *sc, ScopeDsymbol *sds)
{
    if (inc == 0)
    {
        if (!sc)
        {
            error(loc, "static if conditional cannot be at global scope");
            inc = 2;
            return 0;
        }

        sc = sc->push(sc->scopesym);
        sc->sds = sds;                  // sds gets any addMember()

        bool errors = false;
        bool result = evalStaticCondition(sc, exp, exp, errors);
        sc->pop();

        // Prevent repeated condition evaluation.
        // See: fail_compilation/fail7815.d
        if (inc != 0)
            return (inc == 1);
        if (errors)
            goto Lerror;
        if (result)
            inc = 1;
        else
            inc = 2;
    }
    return (inc == 1);

Lerror:
    if (!global.gag)
        inc = 2;                // so we don't see the error message again
    return 0;
}
