
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/attrib.c
 */

#include "root/dsystem.h"               // memcmp()
#include "root/rmem.h"

#include "mars.h"
#include "init.h"
#include "declaration.h"
#include "attrib.h"
#include "cond.h"
#include "scope.h"
#include "id.h"
#include "expression.h"
#include "dsymbol.h"
#include "aggregate.h"
#include "module.h"
#include "parse.h"
#include "target.h"
#include "template.h"
#include "utf.h"
#include "mtype.h"

bool definitelyValueParameter(Expression *e);
Expression *semantic(Expression *e, Scope *sc);
StringExp *semanticString(Scope *sc, Expression *exp, const char *s);
Dsymbols *makeTupleForeachStaticDecl(Scope *sc, ForeachStatement *fs, Dsymbols *dbody, bool needExpansion);

/********************************* AttribDeclaration ****************************/

AttribDeclaration::AttribDeclaration(Dsymbols *decl)
        : Dsymbol()
{
    this->decl = decl;
}

Dsymbols *AttribDeclaration::include(Scope *)
{
    if (errors)
        return NULL;

    return decl;
}

int AttribDeclaration::apply(Dsymbol_apply_ft_t fp, void *param)
{
    Dsymbols *d = include(_scope);

    if (d)
    {
        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            if (s)
            {
                if (s->apply(fp, param))
                    return 1;
            }
        }
    }
    return 0;
}

/****************************************
 * Create a new scope if one or more given attributes
 * are different from the sc's.
 * If the returned scope != sc, the caller should pop
 * the scope after it used.
 */
Scope *AttribDeclaration::createNewScope(Scope *sc,
        StorageClass stc, LINK linkage, CPPMANGLE cppmangle, Prot protection,
        int explicitProtection, AlignDeclaration *aligndecl, PINLINE inlining)
{
    Scope *sc2 = sc;
    if (stc != sc->stc ||
        linkage != sc->linkage ||
        cppmangle != sc->cppmangle ||
        !protection.isSubsetOf(sc->protection) ||
        explicitProtection != sc->explicitProtection ||
        aligndecl != sc->aligndecl ||
        inlining != sc->inlining)
    {
        // create new one for changes
        sc2 = sc->copy();
        sc2->stc = stc;
        sc2->linkage = linkage;
        sc2->cppmangle = cppmangle;
        sc2->protection = protection;
        sc2->explicitProtection = explicitProtection;
        sc2->aligndecl = aligndecl;
        sc2->inlining = inlining;
    }
    return sc2;
}

/****************************************
 * A hook point to supply scope for members.
 * addMember, setScope, importAll, semantic, semantic2 and semantic3 will use this.
 */
Scope *AttribDeclaration::newScope(Scope *sc)
{
    return sc;
}

void AttribDeclaration::addMember(Scope *sc, ScopeDsymbol *sds)
{
    Dsymbols *d = include(sc);

    if (d)
    {
        Scope *sc2 = newScope(sc);

        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            //printf("\taddMember %s to %s\n", s->toChars(), sds->toChars());
            s->addMember(sc2, sds);
        }

        if (sc2 != sc)
            sc2->pop();
    }
}

void AttribDeclaration::setScope(Scope *sc)
{
    Dsymbols *d = include(sc);

    //printf("\tAttribDeclaration::setScope '%s', d = %p\n",toChars(), d);
    if (d)
    {
        Scope *sc2 = newScope(sc);

        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            s->setScope(sc2);
        }

        if (sc2 != sc)
            sc2->pop();
    }
}

void AttribDeclaration::importAll(Scope *sc)
{
    Dsymbols *d = include(sc);

    //printf("\tAttribDeclaration::importAll '%s', d = %p\n", toChars(), d);
    if (d)
    {
        Scope *sc2 = newScope(sc);

        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            s->importAll(sc2);
        }

        if (sc2 != sc)
            sc2->pop();
    }
}

void AttribDeclaration::semantic(Scope *sc)
{
    if (semanticRun != PASSinit)
        return;
    semanticRun = PASSsemantic;
    Dsymbols *d = include(sc);

    //printf("\tAttribDeclaration::semantic '%s', d = %p\n",toChars(), d);
    if (d)
    {
        Scope *sc2 = newScope(sc);

        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            s->semantic(sc2);
        }

        if (sc2 != sc)
            sc2->pop();
    }
    semanticRun = PASSsemanticdone;
}

void AttribDeclaration::semantic2(Scope *sc)
{
    Dsymbols *d = include(sc);

    if (d)
    {
        Scope *sc2 = newScope(sc);

        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            s->semantic2(sc2);
        }

        if (sc2 != sc)
            sc2->pop();
    }
}

void AttribDeclaration::semantic3(Scope *sc)
{
    Dsymbols *d = include(sc);

    if (d)
    {
        Scope *sc2 = newScope(sc);

        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            s->semantic3(sc2);
        }

        if (sc2 != sc)
            sc2->pop();
    }
}

void AttribDeclaration::addComment(const utf8_t *comment)
{
    //printf("AttribDeclaration::addComment %s\n", comment);
    if (comment)
    {
        Dsymbols *d = include(NULL);

        if (d)
        {
            for (size_t i = 0; i < d->length; i++)
            {
                Dsymbol *s = (*d)[i];
                //printf("AttribDeclaration::addComment %s\n", s->toChars());
                s->addComment(comment);
            }
        }
    }
}

void AttribDeclaration::setFieldOffset(AggregateDeclaration *ad, unsigned *poffset, bool isunion)
{
    Dsymbols *d = include(NULL);

    if (d)
    {
        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            s->setFieldOffset(ad, poffset, isunion);
        }
    }
}

bool AttribDeclaration::hasPointers()
{
    Dsymbols *d = include(NULL);

    if (d)
    {
        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            if (s->hasPointers())
                return true;
        }
    }
    return false;
}

bool AttribDeclaration::hasStaticCtorOrDtor()
{
    Dsymbols *d = include(NULL);

    if (d)
    {
        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            if (s->hasStaticCtorOrDtor())
                return true;
        }
    }
    return false;
}

const char *AttribDeclaration::kind() const
{
    return "attribute";
}

bool AttribDeclaration::oneMember(Dsymbol **ps, Identifier *ident)
{
    Dsymbols *d = include(NULL);

    return Dsymbol::oneMembers(d, ps, ident);
}

void AttribDeclaration::checkCtorConstInit()
{
    Dsymbols *d = include(NULL);

    if (d)
    {
        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            s->checkCtorConstInit();
        }
    }
}

/****************************************
 */

void AttribDeclaration::addLocalClass(ClassDeclarations *aclasses)
{
    Dsymbols *d = include(NULL);

    if (d)
    {
        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            s->addLocalClass(aclasses);
        }
    }
}

/************************* StorageClassDeclaration ****************************/

StorageClassDeclaration::StorageClassDeclaration(StorageClass stc, Dsymbols *decl)
        : AttribDeclaration(decl)
{
    this->stc = stc;
}

Dsymbol *StorageClassDeclaration::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    return new StorageClassDeclaration(stc, Dsymbol::arraySyntaxCopy(decl));
}

bool StorageClassDeclaration::oneMember(Dsymbol **ps, Identifier *ident)
{
    bool t = Dsymbol::oneMembers(decl, ps, ident);
    if (t && *ps)
    {
        /* This is to deal with the following case:
         * struct Tick {
         *   template to(T) { const T to() { ... } }
         * }
         * For eponymous function templates, the 'const' needs to get attached to 'to'
         * before the semantic analysis of 'to', so that template overloading based on the
         * 'this' pointer can be successful.
         */

        FuncDeclaration *fd = (*ps)->isFuncDeclaration();
        if (fd)
        {
            /* Use storage_class2 instead of storage_class otherwise when we do .di generation
             * we'll wind up with 'const const' rather than 'const'.
             */
            /* Don't think we need to worry about mutually exclusive storage classes here
             */
            fd->storage_class2 |= stc;
        }
    }
    return t;
}

void StorageClassDeclaration::addMember(Scope *sc, ScopeDsymbol *sds)
{
    Dsymbols *d = include(sc);
    if (d)
    {
        Scope *sc2 = newScope(sc);
        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            //printf("\taddMember %s to %s\n", s->toChars(), sds->toChars());
            // STClocal needs to be attached before the member is added to the scope (because it influences the parent symbol)
            if (Declaration *decl = s->isDeclaration())
            {
                decl->storage_class |= stc & STClocal;
                if (StorageClassDeclaration *sdecl = s->isStorageClassDeclaration())
                {
                    sdecl->stc |= stc & STClocal;
                }
            }
            s->addMember(sc2, sds);
        }
        if (sc2 != sc)
            sc2->pop();
    }
}

Scope *StorageClassDeclaration::newScope(Scope *sc)
{
    StorageClass scstc = sc->stc;

    /* These sets of storage classes are mutually exclusive,
     * so choose the innermost or most recent one.
     */
    if (stc & (STCauto | STCscope | STCstatic | STCextern | STCmanifest))
        scstc &= ~(STCauto | STCscope | STCstatic | STCextern | STCmanifest);
    if (stc & (STCauto | STCscope | STCstatic | STCtls | STCmanifest | STCgshared))
        scstc &= ~(STCauto | STCscope | STCstatic | STCtls | STCmanifest | STCgshared);
    if (stc & (STCconst | STCimmutable | STCmanifest))
        scstc &= ~(STCconst | STCimmutable | STCmanifest);
    if (stc & (STCgshared | STCshared | STCtls))
        scstc &= ~(STCgshared | STCshared | STCtls);
    if (stc & (STCsafe | STCtrusted | STCsystem))
        scstc &= ~(STCsafe | STCtrusted | STCsystem);
    scstc |= stc;
    //printf("scstc = x%llx\n", scstc);

    return createNewScope(sc, scstc, sc->linkage, sc->cppmangle,
        sc->protection, sc->explicitProtection, sc->aligndecl,
        sc->inlining);
}

/********************************* DeprecatedDeclaration ****************************/

DeprecatedDeclaration::DeprecatedDeclaration(Expression *msg, Dsymbols *decl)
        : StorageClassDeclaration(STCdeprecated, decl)
{
    this->msg = msg;
    this->msgstr = NULL;
}

Dsymbol *DeprecatedDeclaration::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    return new DeprecatedDeclaration(msg->syntaxCopy(), Dsymbol::arraySyntaxCopy(decl));
}

/**
 * Provides a new scope with `STCdeprecated` and `Scope.depdecl` set
 *
 * Calls `StorageClassDeclaration.newScope` (as it must be called or copied
 * in any function overriding `newScope`), then set the `Scope`'s depdecl.
 *
 * Returns:
 *   Always a new scope, to use for this `DeprecatedDeclaration`'s members.
 */
Scope *DeprecatedDeclaration::newScope(Scope *sc)
{
    Scope *scx = StorageClassDeclaration::newScope(sc);
    // The enclosing scope is deprecated as well
    if (scx == sc)
        scx = sc->push();
    scx->depdecl = this;
    return scx;
}

void DeprecatedDeclaration::setScope(Scope *sc)
{
    //printf("DeprecatedDeclaration::setScope() %p\n", this);
    if (decl)
        Dsymbol::setScope(sc); // for forward reference
    return AttribDeclaration::setScope(sc);
}

/**
 * Run the DeprecatedDeclaration's semantic2 phase then its members.
 *
 * The message set via a `DeprecatedDeclaration` can be either of:
 * - a string literal
 * - an enum
 * - a static immutable
 * So we need to call ctfe to resolve it.
 * Afterward forwards to the members' semantic2.
 */
void DeprecatedDeclaration::semantic2(Scope *sc)
{
    getMessage();
    StorageClassDeclaration::semantic2(sc);
}

const char *DeprecatedDeclaration::getMessage()
{
    if (Scope *sc = _scope)
    {
        _scope = NULL;

        sc = sc->startCTFE();
        msg = ::semantic(msg, sc);
        msg = resolveProperties(sc, msg);
        sc = sc->endCTFE();
        msg = msg->ctfeInterpret();

        if (StringExp *se = msg->toStringExp())
            msgstr = (char *)se->string;
        else
            msg->error("compile time constant expected, not '%s'", msg->toChars());
    }
    return msgstr;
}

/********************************* LinkDeclaration ****************************/

LinkDeclaration::LinkDeclaration(LINK p, Dsymbols *decl)
        : AttribDeclaration(decl)
{
    //printf("LinkDeclaration(linkage = %d, decl = %p)\n", p, decl);
    linkage = (p == LINKsystem) ? target.systemLinkage() : p;
}

LinkDeclaration *LinkDeclaration::create(LINK p, Dsymbols *decl)
{
    return new LinkDeclaration(p, decl);
}

Dsymbol *LinkDeclaration::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    return new LinkDeclaration(linkage, Dsymbol::arraySyntaxCopy(decl));
}

Scope *LinkDeclaration::newScope(Scope *sc)
{
    return createNewScope(sc, sc->stc, this->linkage, sc->cppmangle,
        sc->protection, sc->explicitProtection, sc->aligndecl,
        sc->inlining);
}

const char *LinkDeclaration::toChars()
{
    return "extern ()";
}

/********************************* CPPMangleDeclaration ****************************/

CPPMangleDeclaration::CPPMangleDeclaration(CPPMANGLE p, Dsymbols *decl)
        : AttribDeclaration(decl)
{
    //printf("CPPMangleDeclaration(cppmangle = %d, decl = %p)\n", p, decl);
    cppmangle = p;
}

Dsymbol *CPPMangleDeclaration::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    return new CPPMangleDeclaration(cppmangle, Dsymbol::arraySyntaxCopy(decl));
}

Scope *CPPMangleDeclaration::newScope(Scope *sc)
{
    return createNewScope(sc, sc->stc, LINKcpp, this->cppmangle,
        sc->protection, sc->explicitProtection, sc->aligndecl,
        sc->inlining);
}

const char *CPPMangleDeclaration::toChars()
{
    return "extern ()";
}

/********************************* ProtDeclaration ****************************/

/**
 * Params:
 *  loc = source location of attribute token
 *  p = protection attribute data
 *  decl = declarations which are affected by this protection attribute
 */
ProtDeclaration::ProtDeclaration(Loc loc, Prot p, Dsymbols *decl)
        : AttribDeclaration(decl)
{
    this->loc = loc;
    this->protection = p;
    this->pkg_identifiers = NULL;
    //printf("decl = %p\n", decl);
}

/**
 * Params:
 *  loc = source location of attribute token
 *  pkg_identifiers = list of identifiers for a qualified package name
 *  decl = declarations which are affected by this protection attribute
 */
ProtDeclaration::ProtDeclaration(Loc loc, Identifiers* pkg_identifiers, Dsymbols *decl)
        : AttribDeclaration(decl)
{
    this->loc = loc;
    this->protection.kind = Prot::package_;
    this->protection.pkg  = NULL;
    this->pkg_identifiers = pkg_identifiers;
}

Dsymbol *ProtDeclaration::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    if (protection.kind == Prot::package_)
        return new ProtDeclaration(this->loc, pkg_identifiers, Dsymbol::arraySyntaxCopy(decl));
    else
        return new ProtDeclaration(this->loc, protection, Dsymbol::arraySyntaxCopy(decl));
}

Scope *ProtDeclaration::newScope(Scope *sc)
{
    if (pkg_identifiers)
        semantic(sc);
    return createNewScope(sc, sc->stc, sc->linkage, sc->cppmangle,
        this->protection, 1, sc->aligndecl,
        sc->inlining);
}

void ProtDeclaration::addMember(Scope *sc, ScopeDsymbol *sds)
{
    if (pkg_identifiers)
    {
        Dsymbol* tmp;
        Package::resolve(pkg_identifiers, &tmp, NULL);
        protection.pkg = tmp ? tmp->isPackage() : NULL;
        pkg_identifiers = NULL;
    }

    if (protection.kind == Prot::package_ && protection.pkg && sc->_module)
    {
        Module *m = sc->_module;
        Package* pkg = m->parent ? m->parent->isPackage() : NULL;
        if (!pkg || !protection.pkg->isAncestorPackageOf(pkg))
            error("does not bind to one of ancestor packages of module '%s'",
               m->toPrettyChars(true));
    }

    return AttribDeclaration::addMember(sc, sds);
}

const char *ProtDeclaration::kind() const
{
    return "protection attribute";
}

const char *ProtDeclaration::toPrettyChars(bool)
{
    assert(protection.kind > Prot::undefined);

    OutBuffer buf;
    buf.writeByte('\'');
    protectionToBuffer(&buf, protection);
    buf.writeByte('\'');
    return buf.extractChars();
}

/********************************* AlignDeclaration ****************************/

AlignDeclaration::AlignDeclaration(Loc loc, Expression *ealign, Dsymbols *decl)
        : AttribDeclaration(decl)
{
    this->loc = loc;
    this->ealign = ealign;
    this->salign = 0;
}

Dsymbol *AlignDeclaration::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    return new AlignDeclaration(loc,
        ealign ? ealign->syntaxCopy() : NULL,
        Dsymbol::arraySyntaxCopy(decl));
}

Scope *AlignDeclaration::newScope(Scope *sc)
{
    return createNewScope(sc, sc->stc, sc->linkage, sc->cppmangle,
        sc->protection, sc->explicitProtection, this,
        sc->inlining);
}

void AlignDeclaration::semantic2(Scope *sc)
{
    getAlignment(sc);
    AttribDeclaration::semantic2(sc);
}

structalign_t AlignDeclaration::getAlignment(Scope *sc)
{
    if (salign != 0)
        return salign;

    if (!ealign)
        return salign = STRUCTALIGN_DEFAULT;

    sc = sc->startCTFE();
    ealign = ::semantic(ealign, sc);
    ealign = resolveProperties(sc, ealign);
    sc = sc->endCTFE();
    ealign = ealign->ctfeInterpret();

    if (ealign->op == TOKerror)
        return salign = STRUCTALIGN_DEFAULT;

    Type *tb = ealign->type->toBasetype();
    sinteger_t n = ealign->toInteger();

    if (n < 1 || n & (n - 1) || STRUCTALIGN_DEFAULT < n || !tb->isintegral())
    {
        ::error(loc, "alignment must be an integer positive power of 2, not %s", ealign->toChars());
        return salign = STRUCTALIGN_DEFAULT;
    }

    return salign = (structalign_t)n;
}

/********************************* AnonDeclaration ****************************/

AnonDeclaration::AnonDeclaration(Loc loc, bool isunion, Dsymbols *decl)
        : AttribDeclaration(decl)
{
    this->loc = loc;
    this->isunion = isunion;
    this->sem = 0;
    this->anonoffset = 0;
    this->anonstructsize = 0;
    this->anonalignsize = 0;
}

Dsymbol *AnonDeclaration::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    return new AnonDeclaration(loc, isunion, Dsymbol::arraySyntaxCopy(decl));
}

void AnonDeclaration::setScope(Scope *sc)
{
    //printf("AnonDeclaration::setScope() %p\n", this);
    if (decl)
        Dsymbol::setScope(sc);
    AttribDeclaration::setScope(sc);
}

void AnonDeclaration::semantic(Scope *sc)
{
    //printf("\tAnonDeclaration::semantic %s %p\n", isunion ? "union" : "struct", this);

    assert(sc->parent);

    Dsymbol *p = sc->parent->pastMixin();
    AggregateDeclaration *ad = p->isAggregateDeclaration();
    if (!ad)
    {
        ::error(loc, "%s can only be a part of an aggregate, not %s %s",
            kind(), p->kind(), p->toChars());
        errors = true;
        return;
    }

    if (decl)
    {
        sc = sc->push();
        sc->stc &= ~(STCauto | STCscope | STCstatic | STCtls | STCgshared);
        sc->inunion = isunion;
        sc->flags = 0;

        for (size_t i = 0; i < decl->length; i++)
        {
            Dsymbol *s = (*decl)[i];
            s->semantic(sc);
        }
        sc = sc->pop();
    }
}

void AnonDeclaration::setFieldOffset(AggregateDeclaration *ad, unsigned *poffset, bool isunion)
{
    //printf("\tAnonDeclaration::setFieldOffset %s %p\n", isunion ? "union" : "struct", this);

    if (decl)
    {
        /* This works by treating an AnonDeclaration as an aggregate 'member',
         * so in order to place that member we need to compute the member's
         * size and alignment.
         */

        size_t fieldstart = ad->fields.length;

        /* Hackishly hijack ad's structsize and alignsize fields
         * for use in our fake anon aggregate member.
         */
        unsigned savestructsize = ad->structsize;
        unsigned savealignsize  = ad->alignsize;
        ad->structsize = 0;
        ad->alignsize = 0;

        unsigned offset = 0;
        for (size_t i = 0; i < decl->length; i++)
        {
            Dsymbol *s = (*decl)[i];
            s->setFieldOffset(ad, &offset, this->isunion);
            if (this->isunion)
                offset = 0;
        }

        /* Bugzilla 13613: If the fields in this->members had been already
         * added in ad->fields, just update *poffset for the subsequent
         * field offset calculation.
         */
        if (fieldstart == ad->fields.length)
        {
            ad->structsize = savestructsize;
            ad->alignsize  = savealignsize;
            *poffset = ad->structsize;
            return;
        }

        anonstructsize = ad->structsize;
        anonalignsize  = ad->alignsize;
        ad->structsize = savestructsize;
        ad->alignsize  = savealignsize;

        // 0 sized structs are set to 1 byte
        // TODO: is this corect hebavior?
        if (anonstructsize == 0)
        {
            anonstructsize = 1;
            anonalignsize = 1;
        }

        assert(_scope);
        structalign_t alignment = _scope->alignment();

        /* Given the anon 'member's size and alignment,
         * go ahead and place it.
         */
        anonoffset = AggregateDeclaration::placeField(
                poffset,
                anonstructsize, anonalignsize, alignment,
                &ad->structsize, &ad->alignsize,
                isunion);

        // Add to the anon fields the base offset of this anonymous aggregate
        //printf("anon fields, anonoffset = %d\n", anonoffset);
        for (size_t i = fieldstart; i < ad->fields.length; i++)
        {
            VarDeclaration *v = ad->fields[i];
            //printf("\t[%d] %s %d\n", i, v->toChars(), v->offset);
            v->offset += anonoffset;
        }
    }
}

const char *AnonDeclaration::kind() const
{
    return (isunion ? "anonymous union" : "anonymous struct");
}

/********************************* PragmaDeclaration ****************************/

PragmaDeclaration::PragmaDeclaration(Loc loc, Identifier *ident, Expressions *args, Dsymbols *decl)
        : AttribDeclaration(decl)
{
    this->loc = loc;
    this->ident = ident;
    this->args = args;
}

Dsymbol *PragmaDeclaration::syntaxCopy(Dsymbol *s)
{
    //printf("PragmaDeclaration::syntaxCopy(%s)\n", toChars());
    assert(!s);
    return new PragmaDeclaration(loc, ident,
        Expression::arraySyntaxCopy(args),
        Dsymbol::arraySyntaxCopy(decl));
}

Scope *PragmaDeclaration::newScope(Scope *sc)
{
    if (ident == Id::Pinline)
    {
        PINLINE inlining = PINLINEdefault;
        if (!args || args->length == 0)
            inlining = PINLINEdefault;
        else if (args->length != 1)
        {
            error("one boolean expression expected for pragma(inline), not %d", args->length);
            args->setDim(1);
            (*args)[0] = new ErrorExp();
        }
        else
        {
            Expression *e = (*args)[0];

            if (e->op != TOKint64 || !e->type->equals(Type::tbool))
            {
                if (e->op != TOKerror)
                {
                    error("pragma(inline, true or false) expected, not %s", e->toChars());
                    (*args)[0] = new ErrorExp();
                }
            }
            else if (e->isBool(true))
                inlining = PINLINEalways;
            else if (e->isBool(false))
                inlining = PINLINEnever;
        }

        return createNewScope(sc, sc->stc, sc->linkage, sc->cppmangle,
            sc->protection, sc->explicitProtection, sc->aligndecl,
            inlining);
    }
    return sc;
}

static unsigned setMangleOverride(Dsymbol *s, char *sym)
{
    AttribDeclaration *ad = s->isAttribDeclaration();

    if (ad)
    {
        Dsymbols *decls = ad->include(NULL);
        unsigned nestedCount = 0;

        if (decls && decls->length)
            for (size_t i = 0; i < decls->length; ++i)
                nestedCount += setMangleOverride((*decls)[i], sym);

        return nestedCount;
    }
    else if (s->isFuncDeclaration() || s->isVarDeclaration())
    {
        s->isDeclaration()->mangleOverride = sym;
        return 1;
    }
    else
        return 0;
}

void PragmaDeclaration::semantic(Scope *sc)
{
    // Should be merged with PragmaStatement

    //printf("\tPragmaDeclaration::semantic '%s'\n",toChars());
    if (ident == Id::msg)
    {
        if (args)
        {
            for (size_t i = 0; i < args->length; i++)
            {
                Expression *e = (*args)[i];

                sc = sc->startCTFE();
                e = ::semantic(e, sc);
                e = resolveProperties(sc, e);
                sc = sc->endCTFE();

                // pragma(msg) is allowed to contain types as well as expressions
                e = ctfeInterpretForPragmaMsg(e);
                if (e->op == TOKerror)
                {
                    errorSupplemental(loc, "while evaluating pragma(msg, %s)", (*args)[i]->toChars());
                    return;
                }
                StringExp *se = e->toStringExp();
                if (se)
                {
                    se = se->toUTF8(sc);
                    fprintf(stderr, "%.*s", (int)se->len, (char *)se->string);
                }
                else
                    fprintf(stderr, "%s", e->toChars());
            }
            fprintf(stderr, "\n");
        }
        goto Lnodecl;
    }
    else if (ident == Id::lib)
    {
        if (!args || args->length != 1)
            error("string expected for library name");
        else
        {
            StringExp *se = semanticString(sc, (*args)[0], "library name");
            if (!se)
                goto Lnodecl;
            (*args)[0] = se;

            char *name = (char *)mem.xmalloc(se->len + 1);
            memcpy(name, se->string, se->len);
            name[se->len] = 0;
            if (global.params.verbose)
                message("library   %s", name);
            if (global.params.moduleDeps && !global.params.moduleDepsFile.length)
            {
                OutBuffer *ob = global.params.moduleDeps;
                Module *imod = sc->instantiatingModule();
                ob->writestring("depsLib ");
                ob->writestring(imod->toPrettyChars());
                ob->writestring(" (");
                escapePath(ob, imod->srcfile->toChars());
                ob->writestring(") : ");
                ob->writestring((char *) name);
                ob->writenl();
            }
            mem.xfree(name);
        }
        goto Lnodecl;
    }
    else if (ident == Id::startaddress)
    {
        if (!args || args->length != 1)
            error("function name expected for start address");
        else
        {
            /* Bugzilla 11980:
             * resolveProperties and ctfeInterpret call are not necessary.
             */
            Expression *e = (*args)[0];

            sc = sc->startCTFE();
            e = ::semantic(e, sc);
            sc = sc->endCTFE();

            (*args)[0] = e;
            Dsymbol *sa = getDsymbol(e);
            if (!sa || !sa->isFuncDeclaration())
                error("function name expected for start address, not '%s'", e->toChars());
        }
        goto Lnodecl;
    }
    else if (ident == Id::Pinline)
    {
        goto Ldecl;
    }
    else if (ident == Id::mangle)
    {
        if (!args)
            args = new Expressions();
        if (args->length != 1)
        {
            error("string expected for mangled name");
            args->setDim(1);
            (*args)[0] = new ErrorExp();    // error recovery
            goto Ldecl;
        }

        StringExp *se = semanticString(sc, (*args)[0], "mangled name");
        if (!se)
            goto Ldecl;
        (*args)[0] = se; // Will be used for later

        if (!se->len)
        {
            error("zero-length string not allowed for mangled name");
            goto Ldecl;
        }
        if (se->sz != 1)
        {
            error("mangled name characters can only be of type char");
            goto Ldecl;
        }

        /* Note: D language specification should not have any assumption about backend
         * implementation. Ideally pragma(mangle) can accept a string of any content.
         *
         * Therefore, this validation is compiler implementation specific.
         */
        for (size_t i = 0; i < se->len; )
        {
            utf8_t *p = (utf8_t *)se->string;
            dchar_t c = p[i];
            if (c < 0x80)
            {
                if ((c >= 'A' && c <= 'Z') ||
                    (c >= 'a' && c <= 'z') ||
                    (c >= '0' && c <= '9') ||
                    (c != 0 && strchr("$%().:?@[]_", c)))
                {
                    ++i;
                    continue;
                }
                else
                {
                    error("char 0x%02x not allowed in mangled name", c);
                    break;
                }
            }

            if (const char* msg = utf_decodeChar((utf8_t *)se->string, se->len, &i, &c))
            {
                error("%s", msg);
                break;
            }

            if (!isUniAlpha(c))
            {
                error("char 0x%04x not allowed in mangled name", c);
                break;
            }
        }
    }
    else if (global.params.ignoreUnsupportedPragmas)
    {
        if (global.params.verbose)
        {
            /* Print unrecognized pragmas
             */
            OutBuffer buf;
            buf.writestring(ident->toChars());
            if (args)
            {
                for (size_t i = 0; i < args->length; i++)
                {
                    Expression *e = (*args)[i];

                    sc = sc->startCTFE();
                    e = ::semantic(e, sc);
                    e = resolveProperties(sc, e);
                    sc = sc->endCTFE();

                    e = e->ctfeInterpret();
                    if (i == 0)
                        buf.writestring(" (");
                    else
                        buf.writeByte(',');
                    buf.writestring(e->toChars());
                }
                if (args->length)
                    buf.writeByte(')');
            }
            message("pragma    %s", buf.peekChars());
        }
        goto Lnodecl;
    }
    else
        error("unrecognized pragma(%s)", ident->toChars());

Ldecl:
    if (decl)
    {
        Scope *sc2 = newScope(sc);

        for (size_t i = 0; i < decl->length; i++)
        {
            Dsymbol *s = (*decl)[i];

            s->semantic(sc2);

            if (ident == Id::mangle)
            {
                assert(args && args->length == 1);
                if (StringExp *se = (*args)[0]->toStringExp())
                {
                    char *name = (char *)mem.xmalloc(se->len + 1);
                    memcpy(name, se->string, se->len);
                    name[se->len] = 0;

                    unsigned cnt = setMangleOverride(s, name);
                    if (cnt > 1)
                        error("can only apply to a single declaration");
                }
            }
        }

        if (sc2 != sc)
            sc2->pop();
    }
    return;

Lnodecl:
    if (decl)
    {
        error("pragma is missing closing ';'");
        goto Ldecl; // do them anyway, to avoid segfaults.
    }
}

const char *PragmaDeclaration::kind() const
{
    return "pragma";
}

/********************************* ConditionalDeclaration ****************************/

ConditionalDeclaration::ConditionalDeclaration(Condition *condition, Dsymbols *decl, Dsymbols *elsedecl)
        : AttribDeclaration(decl)
{
    //printf("ConditionalDeclaration::ConditionalDeclaration()\n");
    this->condition = condition;
    this->elsedecl = elsedecl;
}

Dsymbol *ConditionalDeclaration::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    return new ConditionalDeclaration(condition->syntaxCopy(),
        Dsymbol::arraySyntaxCopy(decl),
        Dsymbol::arraySyntaxCopy(elsedecl));
}

bool ConditionalDeclaration::oneMember(Dsymbol **ps, Identifier *ident)
{
    //printf("ConditionalDeclaration::oneMember(), inc = %d\n", condition->inc);
    if (condition->inc)
    {
        Dsymbols *d = condition->include(NULL) ? decl : elsedecl;
        return Dsymbol::oneMembers(d, ps, ident);
    }
    else
    {
        bool res = (Dsymbol::oneMembers(    decl, ps, ident) && *ps == NULL &&
                    Dsymbol::oneMembers(elsedecl, ps, ident) && *ps == NULL);
        *ps = NULL;
        return res;
    }
}

// Decide if 'then' or 'else' code should be included

Dsymbols *ConditionalDeclaration::include(Scope *sc)
{
    //printf("ConditionalDeclaration::include(sc = %p) _scope = %p\n", sc, _scope);

    if (errors)
        return NULL;

    assert(condition);
    return condition->include(_scope ? _scope : sc) ? decl : elsedecl;
}

void ConditionalDeclaration::setScope(Scope *sc)
{
    Dsymbols *d = include(sc);

    //printf("\tConditionalDeclaration::setScope '%s', d = %p\n",toChars(), d);
    if (d)
    {
       for (size_t i = 0; i < d->length; i++)
       {
           Dsymbol *s = (*d)[i];
           s->setScope(sc);
       }
    }
}

void ConditionalDeclaration::addComment(const utf8_t *comment)
{
    /* Because addComment is called by the parser, if we called
     * include() it would define a version before it was used.
     * But it's no problem to drill down to both decl and elsedecl,
     * so that's the workaround.
     */

    if (comment)
    {
        Dsymbols *d = decl;

        for (int j = 0; j < 2; j++)
        {
            if (d)
            {
                for (size_t i = 0; i < d->length; i++)
                {
                    Dsymbol *s = (*d)[i];
                    //printf("ConditionalDeclaration::addComment %s\n", s->toChars());
                    s->addComment(comment);
                }
            }
            d = elsedecl;
        }
    }
}

/***************************** StaticIfDeclaration ****************************/

StaticIfDeclaration::StaticIfDeclaration(Condition *condition,
        Dsymbols *decl, Dsymbols *elsedecl)
        : ConditionalDeclaration(condition, decl, elsedecl)
{
    //printf("StaticIfDeclaration::StaticIfDeclaration()\n");
    scopesym = NULL;
    addisdone = false;
    onStack = false;
}

Dsymbol *StaticIfDeclaration::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    return new StaticIfDeclaration(condition->syntaxCopy(),
        Dsymbol::arraySyntaxCopy(decl),
        Dsymbol::arraySyntaxCopy(elsedecl));
}

/****************************************
 * Different from other AttribDeclaration subclasses, include() call requires
 * the completion of addMember and setScope phases.
 */
Dsymbols *StaticIfDeclaration::include(Scope *sc)
{
    //printf("StaticIfDeclaration::include(sc = %p) _scope = %p\n", sc, _scope);

    if (errors || onStack)
        return NULL;
    onStack = true;
    Dsymbols *d;

    if (condition->inc == 0)
    {
        assert(scopesym);   // addMember is already done
        assert(_scope);      // setScope is already done

        d = ConditionalDeclaration::include(_scope);

        if (d && !addisdone)
        {
            // Add members lazily.
            for (size_t i = 0; i < d->length; i++)
            {
                Dsymbol *s = (*d)[i];
                s->addMember(_scope, scopesym);
            }

            // Set the member scopes lazily.
            for (size_t i = 0; i < d->length; i++)
            {
                Dsymbol *s = (*d)[i];
                s->setScope(_scope);
            }

            addisdone = true;
        }
        onStack = false;
        return d;
    }
    else
    {
        d = ConditionalDeclaration::include(sc);
        onStack = false;
        return d;
    }
}

void StaticIfDeclaration::addMember(Scope *, ScopeDsymbol *sds)
{
    //printf("StaticIfDeclaration::addMember() '%s'\n", toChars());
    /* This is deferred until the condition evaluated later (by the include() call),
     * so that expressions in the condition can refer to declarations
     * in the same scope, such as:
     *
     * template Foo(int i)
     * {
     *     const int j = i + 1;
     *     static if (j == 3)
     *         const int k;
     * }
     */
    this->scopesym = sds;
}

void StaticIfDeclaration::importAll(Scope *)
{
    // do not evaluate condition before semantic pass
}

void StaticIfDeclaration::setScope(Scope *sc)
{
    // do not evaluate condition before semantic pass

    // But do set the scope, in case we need it for forward referencing
    Dsymbol::setScope(sc);
}

void StaticIfDeclaration::semantic(Scope *sc)
{
    AttribDeclaration::semantic(sc);
}

const char *StaticIfDeclaration::kind() const
{
    return "static if";
}

/***************************** StaticForeachDeclaration ***********************/

/* Static foreach at declaration scope, like:
 *     static foreach (i; [0, 1, 2]){ }
 */

StaticForeachDeclaration::StaticForeachDeclaration(StaticForeach *sfe, Dsymbols *decl)
        : AttribDeclaration(decl)
{
    this->sfe = sfe;
    this->scopesym = NULL;
    this->onStack = false;
    this->cached = false;
    this->cache = NULL;
}

Dsymbol *StaticForeachDeclaration::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    return new StaticForeachDeclaration(
        sfe->syntaxCopy(),
        Dsymbol::arraySyntaxCopy(decl));
}

bool StaticForeachDeclaration::oneMember(Dsymbol **ps, Identifier *ident)
{
    // Required to support IFTI on a template that contains a
    // `static foreach` declaration.  `super.oneMember` calls
    // include with a `null` scope.  As `static foreach` requires
    // the scope for expansion, `oneMember` can only return a
    // precise result once `static foreach` has been expanded.
    if (cached)
    {
        return AttribDeclaration::oneMember(ps, ident);
    }
    *ps = NULL; // a `static foreach` declaration may in general expand to multiple symbols
    return false;
}

Dsymbols *StaticForeachDeclaration::include(Scope *)
{
    if (errors || onStack)
        return NULL;
    if (cached)
    {
        assert(!onStack);
        return cache;
    }
    onStack = true;

    if (_scope)
    {
        staticForeachPrepare(sfe, _scope); // lower static foreach aggregate
    }
    if (!staticForeachReady(sfe))
    {
        onStack = false;
        return NULL; // TODO: ok?
    }

    // expand static foreach
    Dsymbols *d = makeTupleForeachStaticDecl(_scope, sfe->aggrfe, decl, sfe->needExpansion);
    if (d) // process generated declarations
    {
        // Add members lazily.
        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            s->addMember(_scope, scopesym);
        }
        // Set the member scopes lazily.
        for (size_t i = 0; i < d->length; i++)
        {
            Dsymbol *s = (*d)[i];
            s->setScope(_scope);
        }
    }
    onStack = false;
    cached = true;
    cache = d;
    return d;
}

void StaticForeachDeclaration::addMember(Scope *, ScopeDsymbol *sds)
{
    // used only for caching the enclosing symbol
    this->scopesym = sds;
}

void StaticForeachDeclaration::addComment(const utf8_t *)
{
    // do nothing
    // change this to give semantics to documentation comments on static foreach declarations
}

void StaticForeachDeclaration::setScope(Scope *sc)
{
    // do not evaluate condition before semantic pass
    // But do set the scope, in case we need it for forward referencing
    Dsymbol::setScope(sc);
}

void StaticForeachDeclaration::importAll(Scope *)
{
    // do not evaluate aggregate before semantic pass
}

void StaticForeachDeclaration::semantic(Scope *sc)
{
    AttribDeclaration::semantic(sc);
}

const char *StaticForeachDeclaration::kind() const
{
    return "static foreach";
}

/***********************************************************
 * Collection of declarations that stores foreach index variables in a
 * local symbol table.  Other symbols declared within are forwarded to
 * another scope, like:
 *
 *      static foreach (i; 0 .. 10) // loop variables for different indices do not conflict.
 *      { // this body is expanded into 10 ForwardingAttribDeclarations, where `i` has storage class STClocal
 *          mixin("enum x" ~ to!string(i) ~ " = i"); // ok, can access current loop variable
 *      }
 *
 *      static foreach (i; 0.. 10)
 *      {
 *          pragma(msg, mixin("x" ~ to!string(i))); // ok, all 10 symbols are visible as they were forwarded to the global scope
 *      }
 *
 *      static assert (!is(typeof(i))); // loop index variable is not visible outside of the static foreach loop
 *
 * A StaticForeachDeclaration generates one
 * ForwardingAttribDeclaration for each expansion of its body.  The
 * AST of the ForwardingAttribDeclaration contains both the `static
 * foreach` variables and the respective copy of the `static foreach`
 * body.  The functionality is achieved by using a
 * ForwardingScopeDsymbol as the parent symbol for the generated
 * declarations.
 */

ForwardingAttribDeclaration::ForwardingAttribDeclaration(Dsymbols *decl)
        : AttribDeclaration(decl)
{
    sym = new ForwardingScopeDsymbol(NULL);
    sym->symtab = new DsymbolTable();
}

/**************************************
 * Use the ForwardingScopeDsymbol as the parent symbol for members.
 */
Scope *ForwardingAttribDeclaration::newScope(Scope *sc)
{
    return sc->push(sym);
}

/***************************************
 * Lazily initializes the scope to forward to.
 */
void ForwardingAttribDeclaration::addMember(Scope *sc, ScopeDsymbol *sds)
{
    parent = sym->parent = sym->forward = sds;
    return AttribDeclaration::addMember(sc, sym);
}

/***************************** CompileDeclaration *****************************/

// These are mixin declarations, like mixin("int x");

CompileDeclaration::CompileDeclaration(Loc loc, Expression *exp)
    : AttribDeclaration(NULL)
{
    //printf("CompileDeclaration(loc = %d)\n", loc.linnum);
    this->loc = loc;
    this->exp = exp;
    this->scopesym = NULL;
    this->compiled = false;
}

Dsymbol *CompileDeclaration::syntaxCopy(Dsymbol *)
{
    //printf("CompileDeclaration::syntaxCopy('%s')\n", toChars());
    return new CompileDeclaration(loc, exp->syntaxCopy());
}

void CompileDeclaration::addMember(Scope *, ScopeDsymbol *sds)
{
    //printf("CompileDeclaration::addMember(sc = %p, sds = %p, memnum = %d)\n", sc, sds, memnum);
    this->scopesym = sds;
}

void CompileDeclaration::setScope(Scope *sc)
{
    Dsymbol::setScope(sc);
}

void CompileDeclaration::compileIt(Scope *sc)
{
    //printf("CompileDeclaration::compileIt(loc = %d) %s\n", loc.linnum, exp->toChars());
    StringExp *se = semanticString(sc, exp, "argument to mixin");
    if (!se)
        return;
    se = se->toUTF8(sc);

    unsigned errors = global.errors;
    Parser p(loc, sc->_module, (utf8_t *)se->string, se->len, 0);
    p.nextToken();

    decl = p.parseDeclDefs(0);
    if (p.token.value != TOKeof)
        exp->error("incomplete mixin declaration (%s)", se->toChars());
    if (p.errors)
    {
        assert(global.errors != errors);
        decl = NULL;
    }
}

void CompileDeclaration::semantic(Scope *sc)
{
    //printf("CompileDeclaration::semantic()\n");

    if (!compiled)
    {
        compileIt(sc);
        AttribDeclaration::addMember(sc, scopesym);
        compiled = true;

        if (_scope && decl)
        {
            for (size_t i = 0; i < decl->length; i++)
            {
                Dsymbol *s = (*decl)[i];
                s->setScope(_scope);
            }
        }
    }
    AttribDeclaration::semantic(sc);
}

const char *CompileDeclaration::kind() const
{
    return "mixin";
}

/***************************** UserAttributeDeclaration *****************************/

UserAttributeDeclaration::UserAttributeDeclaration(Expressions *atts, Dsymbols *decl)
        : AttribDeclaration(decl)
{
    //printf("UserAttributeDeclaration()\n");
    this->atts = atts;
}

Dsymbol *UserAttributeDeclaration::syntaxCopy(Dsymbol *s)
{
    //printf("UserAttributeDeclaration::syntaxCopy('%s')\n", toChars());
    assert(!s);
    return new UserAttributeDeclaration(
        Expression::arraySyntaxCopy(this->atts),
        Dsymbol::arraySyntaxCopy(decl));
}

Scope *UserAttributeDeclaration::newScope(Scope *sc)
{
    Scope *sc2 = sc;
    if (atts && atts->length)
    {
        // create new one for changes
        sc2 = sc->copy();
        sc2->userAttribDecl = this;
    }
    return sc2;
}

void UserAttributeDeclaration::setScope(Scope *sc)
{
    //printf("UserAttributeDeclaration::setScope() %p\n", this);
    if (decl)
        Dsymbol::setScope(sc);  // for forward reference of UDAs

    return AttribDeclaration::setScope(sc);
}

void UserAttributeDeclaration::semantic(Scope *sc)
{
    //printf("UserAttributeDeclaration::semantic() %p\n", this);
    if (decl && !_scope)
        Dsymbol::setScope(sc);  // for function local symbols

    return AttribDeclaration::semantic(sc);
}

static void udaExpressionEval(Scope *sc, Expressions *exps)
{
    for (size_t i = 0; i < exps->length; i++)
    {
        Expression *e = (*exps)[i];
        if (e)
        {
            e = ::semantic(e, sc);
            if (definitelyValueParameter(e))
                e = e->ctfeInterpret();
            if (e->op == TOKtuple)
            {
                TupleExp *te = (TupleExp *)e;
                udaExpressionEval(sc, te->exps);
            }
            (*exps)[i] = e;
        }
    }
}

void UserAttributeDeclaration::semantic2(Scope *sc)
{
    if (decl && atts && atts->length && _scope)
    {
        _scope = NULL;
        udaExpressionEval(sc, atts);
    }

    AttribDeclaration::semantic2(sc);
}

Expressions *UserAttributeDeclaration::concat(Expressions *udas1, Expressions *udas2)
{
    Expressions *udas;
    if (!udas1 || udas1->length == 0)
        udas = udas2;
    else if (!udas2 || udas2->length == 0)
        udas = udas1;
    else
    {
        /* Create a new tuple that combines them
         * (do not append to left operand, as this is a copy-on-write operation)
         */
        udas = new Expressions();
        udas->push(new TupleExp(Loc(), udas1));
        udas->push(new TupleExp(Loc(), udas2));
    }
    return udas;
}

Expressions *UserAttributeDeclaration::getAttributes()
{
    if (Scope *sc = _scope)
    {
        _scope = NULL;
        arrayExpressionSemantic(atts, sc);
    }

    Expressions *exps = new Expressions();
    if (userAttribDecl)
        exps->push(new TupleExp(Loc(), userAttribDecl->getAttributes()));
    if (atts && atts->length)
        exps->push(new TupleExp(Loc(), atts));

    return exps;
}

const char *UserAttributeDeclaration::kind() const
{
    return "UserAttribute";
}
