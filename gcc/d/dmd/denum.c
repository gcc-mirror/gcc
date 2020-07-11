
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/enum.c
 */

#include "root/dsystem.h"
#include "root/root.h"

#include "errors.h"
#include "enum.h"
#include "mtype.h"
#include "scope.h"
#include "id.h"
#include "expression.h"
#include "module.h"
#include "declaration.h"
#include "init.h"

Expression *semantic(Expression *e, Scope *sc);

/********************************* EnumDeclaration ****************************/

EnumDeclaration::EnumDeclaration(Loc loc, Identifier *id, Type *memtype)
    : ScopeDsymbol(id)
{
    //printf("EnumDeclaration() %s\n", toChars());
    this->loc = loc;
    type = new TypeEnum(this);
    this->memtype = memtype;
    maxval = NULL;
    minval = NULL;
    defaultval = NULL;
    sinit = NULL;
    isdeprecated = false;
    protection = Prot(Prot::undefined);
    parent = NULL;
    added = false;
    inuse = 0;
}

Dsymbol *EnumDeclaration::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    EnumDeclaration *ed = new EnumDeclaration(loc, ident,
        memtype ? memtype->syntaxCopy() : NULL);
    return ScopeDsymbol::syntaxCopy(ed);
}

void EnumDeclaration::setScope(Scope *sc)
{
    if (semanticRun > PASSinit)
        return;
    ScopeDsymbol::setScope(sc);
}

void EnumDeclaration::addMember(Scope *sc, ScopeDsymbol *sds)
{
    /* Anonymous enum members get added to enclosing scope.
     */
    ScopeDsymbol *scopesym = isAnonymous() ? sds : this;

    if (!isAnonymous())
    {
        ScopeDsymbol::addMember(sc, sds);

        if (!symtab)
            symtab = new DsymbolTable();
    }

    if (members)
    {
        for (size_t i = 0; i < members->length; i++)
        {
            EnumMember *em = (*members)[i]->isEnumMember();
            em->ed = this;
            //printf("add %s to scope %s\n", em->toChars(), scopesym->toChars());
            em->addMember(sc, isAnonymous() ? scopesym : this);
        }
    }
    added = true;
}


void EnumDeclaration::semantic(Scope *sc)
{
    //printf("EnumDeclaration::semantic(sd = %p, '%s') %s\n", sc->scopesym, sc->scopesym->toChars(), toChars());
    //printf("EnumDeclaration::semantic() %p %s\n", this, toChars());
    if (semanticRun >= PASSsemanticdone)
        return;             // semantic() already completed
    if (semanticRun == PASSsemantic)
    {
        assert(memtype);
        ::error(loc, "circular reference to enum base type %s", memtype->toChars());
        errors = true;
        semanticRun = PASSsemanticdone;
        return;
    }
    unsigned dprogress_save = Module::dprogress;

    Scope *scx = NULL;
    if (_scope)
    {
        sc = _scope;
        scx = _scope;            // save so we don't make redundant copies
        _scope = NULL;
    }

    if (!sc)
        return;

    parent = sc->parent;
    type = type->semantic(loc, sc);

    protection = sc->protection;
    if (sc->stc & STCdeprecated)
        isdeprecated = true;
    userAttribDecl = sc->userAttribDecl;

    semanticRun = PASSsemantic;

    if (!members && !memtype)               // enum ident;
    {
        semanticRun = PASSsemanticdone;
        return;
    }

    if (!symtab)
        symtab = new DsymbolTable();

    /* The separate, and distinct, cases are:
     *  1. enum { ... }
     *  2. enum : memtype { ... }
     *  3. enum ident { ... }
     *  4. enum ident : memtype { ... }
     *  5. enum ident : memtype;
     *  6. enum ident;
     */

    if (memtype)
    {
        memtype = memtype->semantic(loc, sc);

        /* Check to see if memtype is forward referenced
         */
        if (memtype->ty == Tenum)
        {
            EnumDeclaration *sym = (EnumDeclaration *)memtype->toDsymbol(sc);
            if (!sym->memtype || !sym->members || !sym->symtab || sym->_scope)
            {
                // memtype is forward referenced, so try again later
                _scope = scx ? scx : sc->copy();
                _scope->setNoFree();
                _scope->_module->addDeferredSemantic(this);
                Module::dprogress = dprogress_save;
                //printf("\tdeferring %s\n", toChars());
                semanticRun = PASSinit;
                return;
            }
        }
        if (memtype->ty == Tvoid)
        {
            error("base type must not be void");
            memtype = Type::terror;
        }
        if (memtype->ty == Terror)
        {
            errors = true;
            if (members)
            {
                for (size_t i = 0; i < members->length; i++)
                {
                    Dsymbol *s = (*members)[i];
                    s->errors = true;               // poison all the members
                }
            }
            semanticRun = PASSsemanticdone;
            return;
        }
    }

    semanticRun = PASSsemanticdone;

    if (!members)               // enum ident : memtype;
        return;

    if (members->length == 0)
    {
        error("enum %s must have at least one member", toChars());
        errors = true;
        return;
    }

    Module::dprogress++;

    Scope *sce;
    if (isAnonymous())
        sce = sc;
    else
    {
        sce = sc->push(this);
        sce->parent = this;
    }
    sce = sce->startCTFE();
    sce->setNoFree();                   // needed for getMaxMinValue()

    /* Each enum member gets the sce scope
     */
    for (size_t i = 0; i < members->length; i++)
    {
        EnumMember *em = (*members)[i]->isEnumMember();
        if (em)
            em->_scope = sce;
    }

    if (!added)
    {
        /* addMember() is not called when the EnumDeclaration appears as a function statement,
         * so we have to do what addMember() does and install the enum members in the right symbol
         * table
         */
        ScopeDsymbol *scopesym = NULL;
        if (isAnonymous())
        {
            /* Anonymous enum members get added to enclosing scope.
             */
            for (Scope *sct = sce; 1; sct = sct->enclosing)
            {
                assert(sct);
                if (sct->scopesym)
                {
                    scopesym = sct->scopesym;
                    if (!sct->scopesym->symtab)
                        sct->scopesym->symtab = new DsymbolTable();
                    break;
                }
            }
        }
        else
        {
            // Otherwise enum members are in the EnumDeclaration's symbol table
            scopesym = this;
        }

        for (size_t i = 0; i < members->length; i++)
        {
            EnumMember *em = (*members)[i]->isEnumMember();
            if (em)
            {
                em->ed = this;
                em->addMember(sc, scopesym);
            }
        }
    }

    for (size_t i = 0; i < members->length; i++)
    {
        EnumMember *em = (*members)[i]->isEnumMember();
        if (em)
            em->semantic(em->_scope);
    }
    //printf("defaultval = %lld\n", defaultval);

    //if (defaultval) printf("defaultval: %s %s\n", defaultval->toChars(), defaultval->type->toChars());
    //printf("members = %s\n", members->toChars());
}

/******************************
 * Get the value of the .max/.min property as an Expression
 * Input:
 *      id      Id::max or Id::min
 */

Expression *EnumDeclaration::getMaxMinValue(Loc loc, Identifier *id)
{
    //printf("EnumDeclaration::getMaxValue()\n");
    bool first = true;

    Expression **pval = (id == Id::max) ? &maxval : &minval;

    if (inuse)
    {
        error(loc, "recursive definition of .%s property", id->toChars());
        goto Lerrors;
    }
    if (*pval)
        goto Ldone;

    if (_scope)
        semantic(_scope);
    if (errors)
        goto Lerrors;
    if (semanticRun == PASSinit || !members)
    {
        if (isSpecial())
        {
            /* Allow these special enums to not need a member list
             */
            return memtype->getProperty(loc, id, 0);
        }

        error("is forward referenced looking for .%s", id->toChars());
        goto Lerrors;
    }
    if (!(memtype && memtype->isintegral()))
    {
        error(loc, "has no .%s property because base type %s is not an integral type",
                id->toChars(),
                memtype ? memtype->toChars() : "");
        goto Lerrors;
    }

    for (size_t i = 0; i < members->length; i++)
    {
        EnumMember *em = (*members)[i]->isEnumMember();
        if (!em)
            continue;
        if (em->errors)
            goto Lerrors;

        Expression *e = em->value();
        if (first)
        {
            *pval = e;
            first = false;
        }
        else
        {
            /* In order to work successfully with UDTs,
             * build expressions to do the comparisons,
             * and let the semantic analyzer and constant
             * folder give us the result.
             */

            /* Compute:
             *   if (e > maxval)
             *      maxval = e;
             */
            Expression *ec = new CmpExp(id == Id::max ? TOKgt : TOKlt, em->loc, e, *pval);
            inuse++;
            ec = ::semantic(ec, em->_scope);
            inuse--;
            ec = ec->ctfeInterpret();
            if (ec->toInteger())
                *pval = e;
        }
    }
Ldone:
  {
    Expression *e = *pval;
    if (e->op != TOKerror)
    {
        e = e->copy();
        e->loc = loc;
    }
    return e;
  }

Lerrors:
    *pval = new ErrorExp();
    return *pval;
}

/****************
 * Determine if enum is a 'special' one.
 * Returns:
 *  true if special
 */
bool EnumDeclaration::isSpecial() const
{
    return (ident == Id::__c_long ||
            ident == Id::__c_ulong ||
            ident == Id::__c_longlong ||
            ident == Id::__c_ulonglong ||
            ident == Id::__c_long_double) && memtype;
}

Expression *EnumDeclaration::getDefaultValue(Loc loc)
{
    //printf("EnumDeclaration::getDefaultValue() %p %s\n", this, toChars());
    if (defaultval)
        return defaultval;

    if (_scope)
        semantic(_scope);
    if (errors)
        goto Lerrors;
    if (semanticRun == PASSinit || !members)
    {
        if (isSpecial())
        {
            /* Allow these special enums to not need a member list
             */
            return memtype->defaultInit(loc);
        }

        error(loc, "forward reference of %s.init", toChars());
        goto Lerrors;
    }

    for (size_t i = 0; i < members->length; i++)
    {
        EnumMember *em = (*members)[i]->isEnumMember();
        if (em)
        {
            defaultval = em->value();
            return defaultval;
        }
    }

Lerrors:
    defaultval = new ErrorExp();
    return defaultval;
}

Type *EnumDeclaration::getMemtype(Loc loc)
{
    if (loc.linnum == 0)
        loc = this->loc;
    if (_scope)
    {
        /* Enum is forward referenced. We don't need to resolve the whole thing,
         * just the base type
         */
        if (memtype)
            memtype = memtype->semantic(loc, _scope);
        else
        {
            if (!isAnonymous() && members)
                memtype = Type::tint32;
        }
    }
    if (!memtype)
    {
        if (!isAnonymous() && members)
            memtype = Type::tint32;
        else
        {
            error(loc, "is forward referenced looking for base type");
            return Type::terror;
        }
    }
    return memtype;
}

bool EnumDeclaration::oneMember(Dsymbol **ps, Identifier *ident)
{
    if (isAnonymous())
        return Dsymbol::oneMembers(members, ps, ident);
    return Dsymbol::oneMember(ps, ident);
}

Type *EnumDeclaration::getType()
{
    return type;
}

const char *EnumDeclaration::kind() const
{
    return "enum";
}

bool EnumDeclaration::isDeprecated()
{
    return isdeprecated;
}

Prot EnumDeclaration::prot()
{
    return protection;
}

Dsymbol *EnumDeclaration::search(const Loc &loc, Identifier *ident, int flags)
{
    //printf("%s.EnumDeclaration::search('%s')\n", toChars(), ident->toChars());
    if (_scope)
    {
        // Try one last time to resolve this enum
        semantic(_scope);
    }

    if (!members || !symtab || _scope)
    {
        error("is forward referenced when looking for '%s'", ident->toChars());
        //*(char*)0=0;
        return NULL;
    }

    Dsymbol *s = ScopeDsymbol::search(loc, ident, flags);
    return s;
}

/********************************* EnumMember ****************************/

EnumMember::EnumMember(Loc loc, Identifier *id, Expression *value, Type *origType)
    : VarDeclaration(loc, NULL, id ? id : Id::empty, new ExpInitializer(loc, value))
{
    this->ed = NULL;
    this->origValue = value;
    this->origType = origType;
}

Expression *&EnumMember::value()
{
    return ((ExpInitializer*)_init)->exp;
}

Dsymbol *EnumMember::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    return new EnumMember(loc, ident,
        value() ? value()->syntaxCopy() : NULL,
        origType ? origType->syntaxCopy() : NULL);
}

const char *EnumMember::kind() const
{
    return "enum member";
}

void EnumMember::semantic(Scope *sc)
{
    //printf("EnumMember::semantic() %s\n", toChars());
    if (errors || semanticRun >= PASSsemanticdone)
        return;
    if (semanticRun == PASSsemantic)
    {
        error("circular reference to enum member");
    Lerrors:
        errors = true;
        semanticRun = PASSsemanticdone;
        return;
    }
    assert(ed);
    ed->semantic(sc);
    if (ed->errors)
        goto Lerrors;

    if (errors || semanticRun >= PASSsemanticdone)
        return;

    if (_scope)
        sc = _scope;
    if (!sc)
        return;

    semanticRun = PASSsemantic;

    protection = ed->isAnonymous() ? ed->protection : Prot(Prot::public_);
    linkage = LINKd;
    storage_class = STCmanifest;
    userAttribDecl = ed->isAnonymous() ? ed->userAttribDecl : NULL;

    // The first enum member is special
    bool first = (this == (*ed->members)[0]);

    if (origType)
    {
        origType = origType->semantic(loc, sc);
        type = origType;
        assert(value());          // "type id;" is not a valid enum member declaration
    }

    if (value())
    {
        Expression *e = value();
        assert(e->dyncast() == DYNCAST_EXPRESSION);
        e = ::semantic(e, sc);
        e = resolveProperties(sc, e);
        e = e->ctfeInterpret();
        if (e->op == TOKerror)
            goto Lerrors;
        if (first && !ed->memtype && !ed->isAnonymous())
        {
            ed->memtype = e->type;
            if (ed->memtype->ty == Terror)
            {
                ed->errors = true;
                goto Lerrors;
            }
            if (ed->memtype->ty != Terror)
            {
                /* Bugzilla 11746: All of named enum members should have same type
                 * with the first member. If the following members were referenced
                 * during the first member semantic, their types should be unified.
                 */
                for (size_t i = 0; i < ed->members->length; i++)
                {
                    EnumMember *em = (*ed->members)[i]->isEnumMember();
                    if (!em || em == this || em->semanticRun < PASSsemanticdone || em->origType)
                        continue;

                    //printf("[%d] em = %s, em->semanticRun = %d\n", i, toChars(), em->semanticRun);
                    Expression *ev = em->value();
                    ev = ev->implicitCastTo(sc, ed->memtype);
                    ev = ev->ctfeInterpret();
                    ev = ev->castTo(sc, ed->type);
                    if (ev->op == TOKerror)
                        ed->errors = true;
                    em->value() = ev;
                }
                if (ed->errors)
                {
                    ed->memtype = Type::terror;
                    goto Lerrors;
                }
            }
        }

        if (ed->memtype && !origType)
        {
            e = e->implicitCastTo(sc, ed->memtype);
            e = e->ctfeInterpret();

            // save origValue for better json output
            origValue = e;

            if (!ed->isAnonymous())
            {
                e = e->castTo(sc, ed->type);
                e = e->ctfeInterpret();
            }
        }
        else if (origType)
        {
            e = e->implicitCastTo(sc, origType);
            e = e->ctfeInterpret();
            assert(ed->isAnonymous());

            // save origValue for better json output
            origValue = e;
        }
        value() = e;
    }
    else if (first)
    {
        Type *t;
        if (ed->memtype)
            t = ed->memtype;
        else
        {
            t = Type::tint32;
            if (!ed->isAnonymous())
                ed->memtype = t;
        }
        Expression *e = new IntegerExp(loc, 0, Type::tint32);
        e = e->implicitCastTo(sc, t);
        e = e->ctfeInterpret();

        // save origValue for better json output
        origValue = e;

        if (!ed->isAnonymous())
        {
            e = e->castTo(sc, ed->type);
            e = e->ctfeInterpret();
        }
        value() = e;
    }
    else
    {
        /* Find the previous enum member,
         * and set this to be the previous value + 1
         */
        EnumMember *emprev = NULL;
        for (size_t i = 0; i < ed->members->length; i++)
        {
            EnumMember *em = (*ed->members)[i]->isEnumMember();
            if (em)
            {
                if (em == this)
                    break;
                emprev = em;
            }
        }
        assert(emprev);
        if (emprev->semanticRun < PASSsemanticdone)    // if forward reference
            emprev->semantic(emprev->_scope);    // resolve it
        if (emprev->errors)
            goto Lerrors;

        Expression *eprev = emprev->value();
        Type *tprev = eprev->type->equals(ed->type) ? ed->memtype : eprev->type;

        Expression *emax = tprev->getProperty(ed->loc, Id::max, 0);
        emax = ::semantic(emax, sc);
        emax = emax->ctfeInterpret();

        // Set value to (eprev + 1).
        // But first check that (eprev != emax)
        assert(eprev);
        Expression *e = new EqualExp(TOKequal, loc, eprev, emax);
        e = ::semantic(e, sc);
        e = e->ctfeInterpret();
        if (e->toInteger())
        {
            error("initialization with (%s.%s + 1) causes overflow for type '%s'", emprev->ed->toChars(), emprev->toChars(), ed->type->toBasetype()->toChars());
            goto Lerrors;
        }

        // Now set e to (eprev + 1)
        e = new AddExp(loc, eprev, new IntegerExp(loc, 1, Type::tint32));
        e = ::semantic(e, sc);
        e = e->castTo(sc, eprev->type);
        e = e->ctfeInterpret();

        // save origValue (without cast) for better json output
        if (e->op != TOKerror)  // avoid duplicate diagnostics
        {
            assert(emprev->origValue);
            origValue = new AddExp(loc, emprev->origValue, new IntegerExp(loc, 1, Type::tint32));
            origValue = ::semantic(origValue, sc);
            origValue = origValue->ctfeInterpret();
        }

        if (e->op == TOKerror)
            goto Lerrors;
        if (e->type->isfloating())
        {
            // Check that e != eprev (not always true for floats)
            Expression *etest = new EqualExp(TOKequal, loc, e, eprev);
            etest = ::semantic(etest, sc);
            etest = etest->ctfeInterpret();
            if (etest->toInteger())
            {
                error("has inexact value, due to loss of precision");
                goto Lerrors;
            }
        }
        value() = e;
    }
    if (!origType)
        type = value()->type;

    assert(origValue);
    semanticRun = PASSsemanticdone;
}

Expression *EnumMember::getVarExp(Loc loc, Scope *sc)
{
    semantic(sc);
    if (errors)
        return new ErrorExp();
    Expression *e = new VarExp(loc, this);
    return ::semantic(e, sc);
}
