
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/class.c
 */

#include "root/dsystem.h"               // mem{cpy|set}()
#include "root/root.h"
#include "root/rmem.h"

#include "errors.h"
#include "enum.h"
#include "init.h"
#include "attrib.h"
#include "declaration.h"
#include "aggregate.h"
#include "id.h"
#include "mtype.h"
#include "scope.h"
#include "module.h"
#include "expression.h"
#include "statement.h"
#include "template.h"
#include "target.h"
#include "objc.h"

bool symbolIsVisible(Dsymbol *origin, Dsymbol *s);
Objc *objc();


/********************************* ClassDeclaration ****************************/

ClassDeclaration *ClassDeclaration::object;
ClassDeclaration *ClassDeclaration::throwable;
ClassDeclaration *ClassDeclaration::exception;
ClassDeclaration *ClassDeclaration::errorException;
ClassDeclaration *ClassDeclaration::cpp_type_info_ptr;   // Object.__cpp_type_info_ptr

ClassDeclaration::ClassDeclaration(Loc loc, Identifier *id, BaseClasses *baseclasses, Dsymbols *members, bool inObject)
    : AggregateDeclaration(loc, id ? id : Identifier::generateId("__anonclass"))
{
    static const char msg[] = "only object.d can define this reserved class name";

    if (baseclasses)
    {
        // Actually, this is a transfer
        this->baseclasses = baseclasses;
    }
    else
        this->baseclasses = new BaseClasses();

    this->members = members;

    baseClass = NULL;

    interfaces.length = 0;
    interfaces.ptr = NULL;

    vtblInterfaces = NULL;

    //printf("ClassDeclaration(%s), dim = %d\n", id->toChars(), this->baseclasses->dim);

    // For forward references
    type = new TypeClass(this);

    staticCtor = NULL;
    staticDtor = NULL;

    vtblsym = NULL;
    vclassinfo = NULL;

    if (id)
    {
        // Look for special class names

        if (id == Id::__sizeof || id == Id::__xalignof || id == Id::_mangleof)
            error("illegal class name");

        // BUG: What if this is the wrong TypeInfo, i.e. it is nested?
        if (id->toChars()[0] == 'T')
        {
            if (id == Id::TypeInfo)
            {
                if (!inObject)
                    error("%s", msg);
                Type::dtypeinfo = this;
            }

            if (id == Id::TypeInfo_Class)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfoclass = this;
            }

            if (id == Id::TypeInfo_Interface)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfointerface = this;
            }

            if (id == Id::TypeInfo_Struct)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfostruct = this;
            }

            if (id == Id::TypeInfo_Pointer)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfopointer = this;
            }

            if (id == Id::TypeInfo_Array)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfoarray = this;
            }

            if (id == Id::TypeInfo_StaticArray)
            {
                //if (!inObject)
                //    Type::typeinfostaticarray->error("%s", msg);
                Type::typeinfostaticarray = this;
            }

            if (id == Id::TypeInfo_AssociativeArray)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfoassociativearray = this;
            }

            if (id == Id::TypeInfo_Enum)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfoenum = this;
            }

            if (id == Id::TypeInfo_Function)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfofunction = this;
            }

            if (id == Id::TypeInfo_Delegate)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfodelegate = this;
            }

            if (id == Id::TypeInfo_Tuple)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfotypelist = this;
            }

            if (id == Id::TypeInfo_Const)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfoconst = this;
            }

            if (id == Id::TypeInfo_Invariant)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfoinvariant = this;
            }

            if (id == Id::TypeInfo_Shared)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfoshared = this;
            }

            if (id == Id::TypeInfo_Wild)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfowild = this;
            }

            if (id == Id::TypeInfo_Vector)
            {
                if (!inObject)
                    error("%s", msg);
                Type::typeinfovector = this;
            }
        }

        if (id == Id::Object)
        {
            if (!inObject)
                error("%s", msg);
            object = this;
        }

        if (id == Id::Throwable)
        {
            if (!inObject)
                error("%s", msg);
            throwable = this;
        }

        if (id == Id::Exception)
        {
            if (!inObject)
                error("%s", msg);
            exception = this;
        }

        if (id == Id::Error)
        {
            if (!inObject)
                error("%s", msg);
            errorException = this;
        }

        if (id == Id::cpp_type_info_ptr)
        {
            if (!inObject)
                error("%s", msg);
            cpp_type_info_ptr = this;
        }
    }

    com = false;
    cpp = false;
    isscope = false;
    isabstract = ABSfwdref;
    inuse = 0;
    baseok = BASEOKnone;
    isobjc = false;
    cpp_type_info_ptr_sym = NULL;
}

ClassDeclaration *ClassDeclaration::create(Loc loc, Identifier *id, BaseClasses *baseclasses, Dsymbols *members, bool inObject)
{
    return new ClassDeclaration(loc, id, baseclasses, members, inObject);
}

Dsymbol *ClassDeclaration::syntaxCopy(Dsymbol *s)
{
    //printf("ClassDeclaration::syntaxCopy('%s')\n", toChars());
    ClassDeclaration *cd =
        s ? (ClassDeclaration *)s
          : new ClassDeclaration(loc, ident, NULL, NULL, false);

    cd->storage_class |= storage_class;

    cd->baseclasses->setDim(this->baseclasses->dim);
    for (size_t i = 0; i < cd->baseclasses->dim; i++)
    {
        BaseClass *b = (*this->baseclasses)[i];
        BaseClass *b2 = new BaseClass(b->type->syntaxCopy());
        (*cd->baseclasses)[i] = b2;
    }

    return ScopeDsymbol::syntaxCopy(cd);
}

Scope *ClassDeclaration::newScope(Scope *sc)
{
    Scope *sc2 = AggregateDeclaration::newScope(sc);
    if (isCOMclass())
    {
        if (global.params.isWindows)
            sc2->linkage = LINKwindows;
        else
        {
            /* This enables us to use COM objects under Linux and
             * work with things like XPCOM
             */
            sc2->linkage = LINKc;
        }
    }
    return sc2;
}

/* Bugzilla 12078, 12143 and 15733:
 * While resolving base classes and interfaces, a base may refer
 * the member of this derived class. In that time, if all bases of
 * this class can  be determined, we can go forward the semantc process
 * beyond the Lancestorsdone. To do the recursive semantic analysis,
 * temporarily set and unset `_scope` around exp().
 */
static Type *resolveBase(ClassDeclaration *cd, Scope *sc, Scope *&scx, Type *type)
{
    if (!scx)
    {
        scx = sc->copy();
        scx->setNoFree();
    }
    cd->_scope = scx;
    Type *t = type->semantic(cd->loc, sc);
    cd->_scope = NULL;
    return t;
}

static void resolveBase(ClassDeclaration *cd, Scope *sc, Scope *&scx, ClassDeclaration *sym)
{
    if (!scx)
    {
        scx = sc->copy();
        scx->setNoFree();
    }
    cd->_scope = scx;
    sym->semantic(NULL);
    cd->_scope = NULL;
}

static void badObjectDotD(ClassDeclaration *cd)
{
    cd->error("missing or corrupt object.d");
    fatal();
}

void ClassDeclaration::semantic(Scope *sc)
{
    //printf("ClassDeclaration::semantic(%s), type = %p, sizeok = %d, this = %p\n", toChars(), type, sizeok, this);
    //printf("\tparent = %p, '%s'\n", sc->parent, sc->parent ? sc->parent->toChars() : "");
    //printf("sc->stc = %x\n", sc->stc);

    //{ static int n;  if (++n == 20) *(char*)0=0; }

    if (semanticRun >= PASSsemanticdone)
        return;
    unsigned errors = global.errors;

    //printf("+ClassDeclaration::semantic(%s), type = %p, sizeok = %d, this = %p\n", toChars(), type, sizeok, this);

    Scope *scx = NULL;
    if (_scope)
    {
        sc = _scope;
        scx = _scope;            // save so we don't make redundant copies
        _scope = NULL;
    }

    if (!parent)
    {
        assert(sc->parent);
        parent = sc->parent;
    }

    if (this->errors)
        type = Type::terror;
    type = type->semantic(loc, sc);

    if (type->ty == Tclass && ((TypeClass *)type)->sym != this)
    {
        TemplateInstance *ti = ((TypeClass *)type)->sym->isInstantiated();
        if (ti && isError(ti))
            ((TypeClass *)type)->sym = this;
    }

    // Ungag errors when not speculative
    Ungag ungag = ungagSpeculative();

    if (semanticRun == PASSinit)
    {
        protection = sc->protection;

        storage_class |= sc->stc;
        if (storage_class & STCdeprecated)
            isdeprecated = true;
        if (storage_class & STCauto)
            error("storage class 'auto' is invalid when declaring a class, did you mean to use 'scope'?");
        if (storage_class & STCscope)
            isscope = true;
        if (storage_class & STCabstract)
            isabstract = ABSyes;

        userAttribDecl = sc->userAttribDecl;

        if (sc->linkage == LINKcpp)
            cpp = true;
        if (sc->linkage == LINKobjc)
            objc()->setObjc(this);
    }
    else if (symtab && !scx)
    {
        semanticRun = PASSsemanticdone;
        return;
    }
    semanticRun = PASSsemantic;

    if (baseok < BASEOKdone)
    {
        baseok = BASEOKin;

        // Expand any tuples in baseclasses[]
        for (size_t i = 0; i < baseclasses->dim; )
        {
            BaseClass *b = (*baseclasses)[i];
            b->type = resolveBase(this, sc, scx, b->type);

            Type *tb = b->type->toBasetype();
            if (tb->ty == Ttuple)
            {
                TypeTuple *tup = (TypeTuple *)tb;
                baseclasses->remove(i);
                size_t dim = Parameter::dim(tup->arguments);
                for (size_t j = 0; j < dim; j++)
                {
                    Parameter *arg = Parameter::getNth(tup->arguments, j);
                    b = new BaseClass(arg->type);
                    baseclasses->insert(i + j, b);
                }
            }
            else
                i++;
        }

        if (baseok >= BASEOKdone)
        {
            //printf("%s already semantic analyzed, semanticRun = %d\n", toChars(), semanticRun);
            if (semanticRun >= PASSsemanticdone)
                return;
            goto Lancestorsdone;
        }

        // See if there's a base class as first in baseclasses[]
        if (baseclasses->dim)
        {
            BaseClass *b = (*baseclasses)[0];
            Type *tb = b->type->toBasetype();
            TypeClass *tc = (tb->ty == Tclass) ? (TypeClass *)tb : NULL;
            if (!tc)
            {
                if (b->type != Type::terror)
                    error("base type must be class or interface, not %s", b->type->toChars());
                baseclasses->remove(0);
                goto L7;
            }

            if (tc->sym->isDeprecated())
            {
                if (!isDeprecated())
                {
                    // Deriving from deprecated class makes this one deprecated too
                    isdeprecated = true;

                    tc->checkDeprecated(loc, sc);
                }
            }

            if (tc->sym->isInterfaceDeclaration())
                goto L7;

            for (ClassDeclaration *cdb = tc->sym; cdb; cdb = cdb->baseClass)
            {
                if (cdb == this)
                {
                    error("circular inheritance");
                    baseclasses->remove(0);
                    goto L7;
                }
            }

            /* Bugzilla 11034: Essentially, class inheritance hierarchy
             * and instance size of each classes are orthogonal information.
             * Therefore, even if tc->sym->sizeof == SIZEOKnone,
             * we need to set baseClass field for class covariance check.
             */
            baseClass = tc->sym;
            b->sym = baseClass;

            if (tc->sym->_scope && tc->sym->baseok < BASEOKdone)
                resolveBase(this, sc, scx, tc->sym); // Try to resolve forward reference
            if (tc->sym->baseok < BASEOKdone)
            {
                //printf("\ttry later, forward reference of base class %s\n", tc->sym->toChars());
                if (tc->sym->_scope)
                    tc->sym->_scope->_module->addDeferredSemantic(tc->sym);
                baseok = BASEOKnone;
            }
         L7: ;
        }

        // Treat the remaining entries in baseclasses as interfaces
        // Check for errors, handle forward references
        for (size_t i = (baseClass ? 1 : 0); i < baseclasses->dim; )
        {
            BaseClass *b = (*baseclasses)[i];
            Type *tb = b->type->toBasetype();
            TypeClass *tc = (tb->ty == Tclass) ? (TypeClass *)tb : NULL;
            if (!tc || !tc->sym->isInterfaceDeclaration())
            {
                if (b->type != Type::terror)
                    error("base type must be interface, not %s", b->type->toChars());
                baseclasses->remove(i);
                continue;
            }

            // Check for duplicate interfaces
            for (size_t j = (baseClass ? 1 : 0); j < i; j++)
            {
                BaseClass *b2 = (*baseclasses)[j];
                if (b2->sym == tc->sym)
                {
                    error("inherits from duplicate interface %s", b2->sym->toChars());
                    baseclasses->remove(i);
                    continue;
                }
            }

            if (tc->sym->isDeprecated())
            {
                if (!isDeprecated())
                {
                    // Deriving from deprecated class makes this one deprecated too
                    isdeprecated = true;

                    tc->checkDeprecated(loc, sc);
                }
            }

            b->sym = tc->sym;

            if (tc->sym->_scope && tc->sym->baseok < BASEOKdone)
                resolveBase(this, sc, scx, tc->sym); // Try to resolve forward reference
            if (tc->sym->baseok < BASEOKdone)
            {
                //printf("\ttry later, forward reference of base %s\n", tc->sym->toChars());
                if (tc->sym->_scope)
                    tc->sym->_scope->_module->addDeferredSemantic(tc->sym);
                baseok = BASEOKnone;
            }
            i++;
        }
        if (baseok == BASEOKnone)
        {
            // Forward referencee of one or more bases, try again later
            _scope = scx ? scx : sc->copy();
            _scope->setNoFree();
            _scope->_module->addDeferredSemantic(this);
            //printf("\tL%d semantic('%s') failed due to forward references\n", __LINE__, toChars());
            return;
        }
        baseok = BASEOKdone;

        // If no base class, and this is not an Object, use Object as base class
        if (!baseClass && ident != Id::Object && !cpp)
        {
            if (!object || object->errors)
                badObjectDotD(this);

            Type *t = object->type;
            t = t->semantic(loc, sc)->toBasetype();
            if (t->ty == Terror)
                badObjectDotD(this);
            assert(t->ty == Tclass);
            TypeClass *tc = (TypeClass *)t;

            BaseClass *b = new BaseClass(tc);
            baseclasses->shift(b);

            baseClass = tc->sym;
            assert(!baseClass->isInterfaceDeclaration());
            b->sym = baseClass;
        }
        if (baseClass)
        {
            if (baseClass->storage_class & STCfinal)
                error("cannot inherit from final class %s", baseClass->toChars());

            // Inherit properties from base class
            if (baseClass->isCOMclass())
                com = true;
            if (baseClass->isCPPclass())
                cpp = true;
            if (baseClass->isscope)
                isscope = true;
            enclosing = baseClass->enclosing;
            storage_class |= baseClass->storage_class & STC_TYPECTOR;
        }

        interfaces.length = baseclasses->dim - (baseClass ? 1 : 0);
        interfaces.ptr = baseclasses->tdata() + (baseClass ? 1 : 0);

        for (size_t i = 0; i < interfaces.length; i++)
        {
            BaseClass *b = interfaces.ptr[i];
            // If this is an interface, and it derives from a COM interface,
            // then this is a COM interface too.
            if (b->sym->isCOMinterface())
                com = true;
            if (cpp && !b->sym->isCPPinterface())
            {
                ::error(loc, "C++ class '%s' cannot implement D interface '%s'",
                    toPrettyChars(), b->sym->toPrettyChars());
            }
        }

        interfaceSemantic(sc);
    }
Lancestorsdone:
    //printf("\tClassDeclaration::semantic(%s) baseok = %d\n", toChars(), baseok);

    if (!members)               // if opaque declaration
    {
        semanticRun = PASSsemanticdone;
        return;
    }
    if (!symtab)
    {
        symtab = new DsymbolTable();

        /* Bugzilla 12152: The semantic analysis of base classes should be finished
         * before the members semantic analysis of this class, in order to determine
         * vtbl in this class. However if a base class refers the member of this class,
         * it can be resolved as a normal forward reference.
         * Call addMember() and setScope() to make this class members visible from the base classes.
         */
        for (size_t i = 0; i < members->dim; i++)
        {
            Dsymbol *s = (*members)[i];
            s->addMember(sc, this);
        }

        Scope *sc2 = newScope(sc);

        /* Set scope so if there are forward references, we still might be able to
         * resolve individual members like enums.
         */
        for (size_t i = 0; i < members->dim; i++)
        {
            Dsymbol *s = (*members)[i];
            //printf("[%d] setScope %s %s, sc2 = %p\n", i, s->kind(), s->toChars(), sc2);
            s->setScope(sc2);
        }

        sc2->pop();
    }

    for (size_t i = 0; i < baseclasses->dim; i++)
    {
        BaseClass *b = (*baseclasses)[i];
        Type *tb = b->type->toBasetype();
        assert(tb->ty == Tclass);
        TypeClass *tc = (TypeClass *)tb;

        if (tc->sym->semanticRun < PASSsemanticdone)
        {
            // Forward referencee of one or more bases, try again later
            _scope = scx ? scx : sc->copy();
            _scope->setNoFree();
            if (tc->sym->_scope)
                tc->sym->_scope->_module->addDeferredSemantic(tc->sym);
            _scope->_module->addDeferredSemantic(this);
            //printf("\tL%d semantic('%s') failed due to forward references\n", __LINE__, toChars());
            return;
        }
    }

    if (baseok == BASEOKdone)
    {
        baseok = BASEOKsemanticdone;

        // initialize vtbl
        if (baseClass)
        {
            if (cpp && baseClass->vtbl.dim == 0)
            {
                error("C++ base class %s needs at least one virtual function", baseClass->toChars());
            }

            // Copy vtbl[] from base class
            vtbl.setDim(baseClass->vtbl.dim);
            memcpy(vtbl.tdata(), baseClass->vtbl.tdata(), sizeof(void *) * vtbl.dim);

            vthis = baseClass->vthis;
        }
        else
        {
            // No base class, so this is the root of the class hierarchy
            vtbl.setDim(0);
            if (vtblOffset())
                vtbl.push(this);            // leave room for classinfo as first member
        }

        /* If this is a nested class, add the hidden 'this'
         * member which is a pointer to the enclosing scope.
         */
        if (vthis)              // if inheriting from nested class
        {
            // Use the base class's 'this' member
            if (storage_class & STCstatic)
                error("static class cannot inherit from nested class %s", baseClass->toChars());
            if (toParent2() != baseClass->toParent2() &&
                (!toParent2() ||
                 !baseClass->toParent2()->getType() ||
                 !baseClass->toParent2()->getType()->isBaseOf(toParent2()->getType(), NULL)))
            {
                if (toParent2())
                {
                    error("is nested within %s, but super class %s is nested within %s",
                        toParent2()->toChars(),
                        baseClass->toChars(),
                        baseClass->toParent2()->toChars());
                }
                else
                {
                    error("is not nested, but super class %s is nested within %s",
                        baseClass->toChars(),
                        baseClass->toParent2()->toChars());
                }
                enclosing = NULL;
            }
        }
        else
            makeNested();
    }

    Scope *sc2 = newScope(sc);

    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        s->importAll(sc2);
    }

    // Note that members.dim can grow due to tuple expansion during semantic()
    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        s->semantic(sc2);
    }

    if (!determineFields())
    {
        assert(type == Type::terror);
        sc2->pop();
        return;
    }

    /* Following special member functions creation needs semantic analysis
     * completion of sub-structs in each field types.
     */
    for (size_t i = 0; i < fields.dim; i++)
    {
        VarDeclaration *v = fields[i];
        Type *tb = v->type->baseElemOf();
        if (tb->ty != Tstruct)
            continue;
        StructDeclaration *sd = ((TypeStruct *)tb)->sym;
        if (sd->semanticRun >= PASSsemanticdone)
            continue;

        sc2->pop();

        _scope = scx ? scx : sc->copy();
        _scope->setNoFree();
        _scope->_module->addDeferredSemantic(this);
        //printf("\tdeferring %s\n", toChars());
        return;
    }

    /* Look for special member functions.
     * They must be in this class, not in a base class.
     */

    // Can be in base class
    aggNew    =    (NewDeclaration *)search(Loc(), Id::classNew);
    aggDelete = (DeleteDeclaration *)search(Loc(), Id::classDelete);

    // Look for the constructor
    ctor = searchCtor();

    if (!ctor && noDefaultCtor)
    {
        // A class object is always created by constructor, so this check is legitimate.
        for (size_t i = 0; i < fields.dim; i++)
        {
            VarDeclaration *v = fields[i];
            if (v->storage_class & STCnodefaultctor)
                ::error(v->loc, "field %s must be initialized in constructor", v->toChars());
        }
    }

    // If this class has no constructor, but base class has a default
    // ctor, create a constructor:
    //    this() { }
    if (!ctor && baseClass && baseClass->ctor)
    {
        FuncDeclaration *fd = resolveFuncCall(loc, sc2, baseClass->ctor, NULL, type, NULL, 1);
        if (!fd) // try shared base ctor instead
            fd = resolveFuncCall(loc, sc2, baseClass->ctor, NULL, type->sharedOf(), NULL, 1);
        if (fd && !fd->errors)
        {
            //printf("Creating default this(){} for class %s\n", toChars());
            TypeFunction *btf = fd->type->toTypeFunction();
            TypeFunction *tf = new TypeFunction(NULL, NULL, 0, LINKd, fd->storage_class);
            tf->mod = btf->mod;
            tf->purity = btf->purity;
            tf->isnothrow = btf->isnothrow;
            tf->isnogc = btf->isnogc;
            tf->trust = btf->trust;

            CtorDeclaration *ctor = new CtorDeclaration(loc, Loc(), 0, tf);
            ctor->fbody = new CompoundStatement(Loc(), new Statements());

            members->push(ctor);
            ctor->addMember(sc, this);
            ctor->semantic(sc2);

            this->ctor = ctor;
            defaultCtor = ctor;
        }
        else
        {
            error("cannot implicitly generate a default ctor when base class %s is missing a default ctor",
                baseClass->toPrettyChars());
        }
    }

    dtor = buildDtor(this, sc2);

    if (FuncDeclaration *f = hasIdentityOpAssign(this, sc2))
    {
        if (!(f->storage_class & STCdisable))
            error(f->loc, "identity assignment operator overload is illegal");
    }

    inv = buildInv(this, sc2);

    Module::dprogress++;
    semanticRun = PASSsemanticdone;
    //printf("-ClassDeclaration.semantic(%s), type = %p\n", toChars(), type);
    //members.print();

    sc2->pop();

    if (type->ty == Tclass && ((TypeClass *)type)->sym != this)
    {
        // https://issues.dlang.org/show_bug.cgi?id=17492
        ClassDeclaration *cd = ((TypeClass *)type)->sym;
        error("already exists at %s. Perhaps in another function with the same name?", cd->loc.toChars());
    }

    if (global.errors != errors)
    {
        // The type is no good.
        type = Type::terror;
        this->errors = true;
        if (deferred)
            deferred->errors = true;
    }

    // Verify fields of a synchronized class are not public
    if (storage_class & STCsynchronized)
    {
        for (size_t i = 0; i < fields.dim; i++)
        {
            VarDeclaration *vd = fields[i];
            if (!vd->isThisDeclaration() &&
                !vd->prot().isMoreRestrictiveThan(Prot(PROTpublic)))
            {
                vd->error("Field members of a synchronized class cannot be %s",
                    protectionToChars(vd->prot().kind));
            }
        }
    }

    if (deferred && !global.gag)
    {
        deferred->semantic2(sc);
        deferred->semantic3(sc);
    }
    //printf("-ClassDeclaration::semantic(%s), type = %p, sizeok = %d, this = %p\n", toChars(), type, sizeok, this);
}

/*********************************************
 * Determine if 'this' is a base class of cd.
 * This is used to detect circular inheritance only.
 */

bool ClassDeclaration::isBaseOf2(ClassDeclaration *cd)
{
    if (!cd)
        return false;
    //printf("ClassDeclaration::isBaseOf2(this = '%s', cd = '%s')\n", toChars(), cd->toChars());
    for (size_t i = 0; i < cd->baseclasses->dim; i++)
    {
        BaseClass *b = (*cd->baseclasses)[i];
        if (b->sym == this || isBaseOf2(b->sym))
            return true;
    }
    return false;
}

/*******************************************
 * Determine if 'this' is a base class of cd.
 */

bool ClassDeclaration::isBaseOf(ClassDeclaration *cd, int *poffset)
{
    //printf("ClassDeclaration::isBaseOf(this = '%s', cd = '%s')\n", toChars(), cd->toChars());
    if (poffset)
        *poffset = 0;
    while (cd)
    {
        /* cd->baseClass might not be set if cd is forward referenced.
         */
        if (!cd->baseClass && cd->_scope && !cd->isInterfaceDeclaration())
        {
            cd->semantic(NULL);
            if (!cd->baseClass && cd->_scope)
                cd->error("base class is forward referenced by %s", toChars());
        }

        if (this == cd->baseClass)
            return true;

        cd = cd->baseClass;
    }
    return false;
}

/*********************************************
 * Determine if 'this' has complete base class information.
 * This is used to detect forward references in covariant overloads.
 */

bool ClassDeclaration::isBaseInfoComplete()
{
    return baseok >= BASEOKdone;
}

Dsymbol *ClassDeclaration::search(const Loc &loc, Identifier *ident, int flags)
{
    //printf("%s.ClassDeclaration::search('%s', flags=x%x)\n", toChars(), ident->toChars(), flags);

    //if (_scope) printf("%s baseok = %d\n", toChars(), baseok);
    if (_scope && baseok < BASEOKdone)
    {
        if (!inuse)
        {
            // must semantic on base class/interfaces
            ++inuse;
            semantic(NULL);
            --inuse;
        }
    }

    if (!members || !symtab)    // opaque or addMember is not yet done
    {
        error("is forward referenced when looking for '%s'", ident->toChars());
        //*(char*)0=0;
        return NULL;
    }

    Dsymbol *s = ScopeDsymbol::search(loc, ident, flags);

    // don't search imports of base classes
    if (flags & SearchImportsOnly)
        return s;

    if (!s)
    {
        // Search bases classes in depth-first, left to right order

        for (size_t i = 0; i < baseclasses->dim; i++)
        {
            BaseClass *b = (*baseclasses)[i];

            if (b->sym)
            {
                if (!b->sym->symtab)
                    error("base %s is forward referenced", b->sym->ident->toChars());
                else
                {
                    s = b->sym->search(loc, ident, flags);
                    if (!s)
                        continue;
                    else if (s == this) // happens if s is nested in this and derives from this
                        s = NULL;
                    else if (!(flags & IgnoreSymbolVisibility) && !(s->prot().kind == PROTprotected) && !symbolIsVisible(this, s))
                        s = NULL;
                    else
                        break;
                }
            }
        }
    }
    return s;
}

/************************************
 * Search base classes in depth-first, left-to-right order for
 * a class or interface named 'ident'.
 * Stops at first found. Does not look for additional matches.
 * Params:
 *  ident = identifier to search for
 * Returns:
 *  ClassDeclaration if found, null if not
 */
ClassDeclaration *ClassDeclaration::searchBase(Identifier *ident)
{
    for (size_t i = 0; i < baseclasses->dim; i++)
    {
        BaseClass *b = (*baseclasses)[i];
        ClassDeclaration *cdb = b->type->isClassHandle();
        if (!cdb)   // Bugzilla 10616
            return NULL;
        if (cdb->ident->equals(ident))
            return cdb;
        cdb = cdb->searchBase(ident);
        if (cdb)
            return cdb;
    }
    return NULL;
}

/****
 * Runs through the inheritance graph to set the BaseClass.offset fields.
 * Recursive in order to account for the size of the interface classes, if they are
 * more than just interfaces.
 * Params:
 *      cd = interface to look at
 *      baseOffset = offset of where cd will be placed
 * Returns:
 *      subset of instantiated size used by cd for interfaces
 */
static unsigned membersPlace(BaseClasses *vtblInterfaces, size_t &bi, ClassDeclaration *cd, unsigned baseOffset)
{
    //printf("    membersPlace(%s, %d)\n", cd->toChars(), baseOffset);
    unsigned offset = baseOffset;

    for (size_t i = 0; i < cd->interfaces.length; i++)
    {
        BaseClass *b = cd->interfaces.ptr[i];
        if (b->sym->sizeok != SIZEOKdone)
            b->sym->finalizeSize();
        assert(b->sym->sizeok == SIZEOKdone);

        if (!b->sym->alignsize)
            b->sym->alignsize = Target::ptrsize;
        cd->alignmember(b->sym->alignsize, b->sym->alignsize, &offset);
        assert(bi < vtblInterfaces->dim);
        BaseClass *bv = (*vtblInterfaces)[bi];
        if (b->sym->interfaces.length == 0)
        {
            //printf("\tvtblInterfaces[%d] b=%p b->sym = %s, offset = %d\n", bi, bv, bv->sym->toChars(), offset);
            bv->offset = offset;
            ++bi;
            // All the base interfaces down the left side share the same offset
            for (BaseClass *b2 = bv; b2->baseInterfaces.length; )
            {
                b2 = &b2->baseInterfaces.ptr[0];
                b2->offset = offset;
                //printf("\tvtblInterfaces[%d] b=%p   sym = %s, offset = %d\n", bi, b2, b2->sym->toChars(), b2->offset);
            }
        }
        membersPlace(vtblInterfaces, bi, b->sym, offset);
        //printf(" %s size = %d\n", b->sym->toChars(), b->sym->structsize);
        offset += b->sym->structsize;
        if (cd->alignsize < b->sym->alignsize)
            cd->alignsize = b->sym->alignsize;
    }
    return offset - baseOffset;
}

void ClassDeclaration::finalizeSize()
{
    assert(sizeok != SIZEOKdone);

    // Set the offsets of the fields and determine the size of the class
    if (baseClass)
    {
        assert(baseClass->sizeok == SIZEOKdone);

        alignsize = baseClass->alignsize;
        structsize = baseClass->structsize;
        if (cpp && global.params.isWindows)
            structsize = (structsize + alignsize - 1) & ~(alignsize - 1);
    }
    else if (isInterfaceDeclaration())
    {
        if (interfaces.length == 0)
        {
            alignsize = Target::ptrsize;
            structsize = Target::ptrsize;      // allow room for __vptr
        }
    }
    else
    {
        alignsize = Target::ptrsize;
        structsize = Target::ptrsize;      // allow room for __vptr
        if (!cpp)
            structsize += Target::ptrsize; // allow room for __monitor
    }

    //printf("finalizeSize() %s, sizeok = %d\n", toChars(), sizeok);
    size_t bi = 0;                  // index into vtblInterfaces[]

    // Add vptr's for any interfaces implemented by this class
    structsize += membersPlace(vtblInterfaces, bi, this, structsize);

    if (isInterfaceDeclaration())
    {
        sizeok = SIZEOKdone;
        return;
    }

    // FIXME: Currently setFieldOffset functions need to increase fields
    // to calculate each variable offsets. It can be improved later.
    fields.setDim(0);

    unsigned offset = structsize;
    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        s->setFieldOffset(this, &offset, false);
    }

    sizeok = SIZEOKdone;

    // Calculate fields[i]->overlapped
    checkOverlappedFields();
}

/**********************************************************
 * fd is in the vtbl[] for this class.
 * Return 1 if function is hidden (not findable through search).
 */

int isf(void *param, Dsymbol *s)
{
    FuncDeclaration *fd = s->isFuncDeclaration();
    if (!fd)
        return 0;
    //printf("param = %p, fd = %p %s\n", param, fd, fd->toChars());
    return (RootObject *)param == fd;
}

bool ClassDeclaration::isFuncHidden(FuncDeclaration *fd)
{
    //printf("ClassDeclaration::isFuncHidden(class = %s, fd = %s)\n", toChars(), fd->toPrettyChars());
    Dsymbol *s = search(Loc(), fd->ident, IgnoreAmbiguous | IgnoreErrors);
    if (!s)
    {
        //printf("not found\n");
        /* Because, due to a hack, if there are multiple definitions
         * of fd->ident, NULL is returned.
         */
        return false;
    }
    s = s->toAlias();
    OverloadSet *os = s->isOverloadSet();
    if (os)
    {
        for (size_t i = 0; i < os->a.dim; i++)
        {
            Dsymbol *s2 = os->a[i];
            FuncDeclaration *f2 = s2->isFuncDeclaration();
            if (f2 && overloadApply(f2, (void *)fd, &isf))
                return false;
        }
        return true;
    }
    else
    {
        FuncDeclaration *fdstart = s->isFuncDeclaration();
        //printf("%s fdstart = %p\n", s->kind(), fdstart);
        if (overloadApply(fdstart, (void *)fd, &isf))
            return false;

        return !fd->parent->isTemplateMixin();
    }
}

/****************
 * Find virtual function matching identifier and type.
 * Used to build virtual function tables for interface implementations.
 */

FuncDeclaration *ClassDeclaration::findFunc(Identifier *ident, TypeFunction *tf)
{
    //printf("ClassDeclaration::findFunc(%s, %s) %s\n", ident->toChars(), tf->toChars(), toChars());
    FuncDeclaration *fdmatch = NULL;
    FuncDeclaration *fdambig = NULL;

    ClassDeclaration *cd = this;
    Dsymbols *vtbl = &cd->vtbl;
    while (1)
    {
        for (size_t i = 0; i < vtbl->dim; i++)
        {
            FuncDeclaration *fd = (*vtbl)[i]->isFuncDeclaration();
            if (!fd)
                continue;               // the first entry might be a ClassInfo

            //printf("\t[%d] = %s\n", i, fd->toChars());
            if (ident == fd->ident &&
                fd->type->covariant(tf) == 1)
            {
                //printf("fd->parent->isClassDeclaration() = %p\n", fd->parent->isClassDeclaration());
                if (!fdmatch)
                    goto Lfd;
                if (fd == fdmatch)
                    goto Lfdmatch;

                {
                // Function type matcing: exact > covariant
                MATCH m1 = tf->equals(fd     ->type) ? MATCHexact : MATCHnomatch;
                MATCH m2 = tf->equals(fdmatch->type) ? MATCHexact : MATCHnomatch;
                if (m1 > m2)
                    goto Lfd;
                else if (m1 < m2)
                    goto Lfdmatch;
                }

                {
                MATCH m1 = (tf->mod == fd     ->type->mod) ? MATCHexact : MATCHnomatch;
                MATCH m2 = (tf->mod == fdmatch->type->mod) ? MATCHexact : MATCHnomatch;
                if (m1 > m2)
                    goto Lfd;
                else if (m1 < m2)
                    goto Lfdmatch;
                }

                {
                // The way of definition: non-mixin > mixin
                MATCH m1 = fd     ->parent->isClassDeclaration() ? MATCHexact : MATCHnomatch;
                MATCH m2 = fdmatch->parent->isClassDeclaration() ? MATCHexact : MATCHnomatch;
                if (m1 > m2)
                    goto Lfd;
                else if (m1 < m2)
                    goto Lfdmatch;
                }

                fdambig = fd;
                //printf("Lambig fdambig = %s %s [%s]\n", fdambig->toChars(), fdambig->type->toChars(), fdambig->loc.toChars());
                continue;

            Lfd:
                fdmatch = fd;
                fdambig = NULL;
                //printf("Lfd fdmatch = %s %s [%s]\n", fdmatch->toChars(), fdmatch->type->toChars(), fdmatch->loc.toChars());
                continue;

            Lfdmatch:
                continue;
            }
            //else printf("\t\t%d\n", fd->type->covariant(tf));
        }
        if (!cd)
            break;
        vtbl = &cd->vtblFinal;
        cd = cd->baseClass;
    }

    if (fdambig)
        error("ambiguous virtual function %s", fdambig->toChars());
    return fdmatch;
}

void ClassDeclaration::interfaceSemantic(Scope *)
{
    vtblInterfaces = new BaseClasses();
    vtblInterfaces->reserve(interfaces.length);

    for (size_t i = 0; i < interfaces.length; i++)
    {
        BaseClass *b = interfaces.ptr[i];
        vtblInterfaces->push(b);
        b->copyBaseInterfaces(vtblInterfaces);
    }
}

/****************************************
 */

bool ClassDeclaration::isCOMclass() const
{
    return com;
}

bool ClassDeclaration::isCOMinterface() const
{
    return false;
}

bool ClassDeclaration::isCPPclass() const
{
    return cpp;
}

bool ClassDeclaration::isCPPinterface() const
{
    return false;
}


/****************************************
 */

bool ClassDeclaration::isAbstract()
{
    if (isabstract != ABSfwdref)
        return isabstract == ABSyes;

    /* Bugzilla 11169: Resolve forward references to all class member functions,
     * and determine whether this class is abstract.
     */
    struct SearchAbstract
    {
        static int fp(Dsymbol *s, void *)
        {
            FuncDeclaration *fd = s->isFuncDeclaration();
            if (!fd)
                return 0;
            if (fd->storage_class & STCstatic)
                return 0;

            if (fd->_scope)
                fd->semantic(NULL);

            if (fd->isAbstract())
                return 1;
            return 0;
        }
    };

    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        if (s->apply(&SearchAbstract::fp, this))
        {
            isabstract = ABSyes;
            return true;
        }
    }

    /* Iterate inherited member functions and check their abstract attribute.
     */
    for (size_t i = 1; i < vtbl.dim; i++)
    {
        FuncDeclaration *fd = vtbl[i]->isFuncDeclaration();
        //if (fd) printf("\tvtbl[%d] = [%s] %s\n", i, fd->loc.toChars(), fd->toChars());
        if (!fd || fd->isAbstract())
        {
            isabstract = ABSyes;
            return true;
        }
    }

    isabstract = ABSno;
    return false;
}


/****************************************
 * Determine if slot 0 of the vtbl[] is reserved for something else.
 * For class objects, yes, this is where the classinfo ptr goes.
 * For COM interfaces, no.
 * For non-COM interfaces, yes, this is where the Interface ptr goes.
 * Returns:
 *      0       vtbl[0] is first virtual function pointer
 *      1       vtbl[0] is classinfo/interfaceinfo pointer
 */

int ClassDeclaration::vtblOffset() const
{
    return cpp ? 0 : 1;
}

/****************************************
 */

const char *ClassDeclaration::kind() const
{
    return "class";
}

/****************************************
 */

void ClassDeclaration::addLocalClass(ClassDeclarations *aclasses)
{
    aclasses->push(this);
}

/********************************* InterfaceDeclaration ****************************/

InterfaceDeclaration::InterfaceDeclaration(Loc loc, Identifier *id, BaseClasses *baseclasses)
    : ClassDeclaration(loc, id, baseclasses, NULL, false)
{
    if (id == Id::IUnknown)     // IUnknown is the root of all COM interfaces
    {
        com = true;
        cpp = true;             // IUnknown is also a C++ interface
    }
}

Dsymbol *InterfaceDeclaration::syntaxCopy(Dsymbol *s)
{
    InterfaceDeclaration *id =
        s ? (InterfaceDeclaration *)s
          : new InterfaceDeclaration(loc, ident, NULL);
    return ClassDeclaration::syntaxCopy(id);
}

Scope *InterfaceDeclaration::newScope(Scope *sc)
{
    Scope *sc2 = ClassDeclaration::newScope(sc);
    if (com)
        sc2->linkage = LINKwindows;
    else if (cpp)
        sc2->linkage = LINKcpp;
    else if (isobjc)
        sc2->linkage = LINKobjc;
    return sc2;
}

void InterfaceDeclaration::semantic(Scope *sc)
{
    //printf("InterfaceDeclaration::semantic(%s), type = %p\n", toChars(), type);
    if (semanticRun >= PASSsemanticdone)
        return;
    unsigned errors = global.errors;

    //printf("+InterfaceDeclaration.semantic(%s), type = %p\n", toChars(), type);

    Scope *scx = NULL;
    if (_scope)
    {
        sc = _scope;
        scx = _scope;            // save so we don't make redundant copies
        _scope = NULL;
    }

    if (!parent)
    {
        assert(sc->parent && sc->func);
        parent = sc->parent;
    }
    assert(parent && !isAnonymous());

    if (this->errors)
        type = Type::terror;
    type = type->semantic(loc, sc);

    if (type->ty == Tclass && ((TypeClass *)type)->sym != this)
    {
        TemplateInstance *ti = ((TypeClass *)type)->sym->isInstantiated();
        if (ti && isError(ti))
            ((TypeClass *)type)->sym = this;
    }

    // Ungag errors when not speculative
    Ungag ungag = ungagSpeculative();

    if (semanticRun == PASSinit)
    {
        protection = sc->protection;

        storage_class |= sc->stc;
        if (storage_class & STCdeprecated)
            isdeprecated = true;

        userAttribDecl = sc->userAttribDecl;
    }
    else if (symtab)
    {
        if (sizeok == SIZEOKdone || !scx)
        {
            semanticRun = PASSsemanticdone;
            return;
        }
    }
    semanticRun = PASSsemantic;

    if (baseok < BASEOKdone)
    {
        baseok = BASEOKin;

        // Expand any tuples in baseclasses[]
        for (size_t i = 0; i < baseclasses->dim; )
        {
            BaseClass *b = (*baseclasses)[i];
            b->type = resolveBase(this, sc, scx, b->type);

            Type *tb = b->type->toBasetype();
            if (tb->ty == Ttuple)
            {
                TypeTuple *tup = (TypeTuple *)tb;
                baseclasses->remove(i);
                size_t dim = Parameter::dim(tup->arguments);
                for (size_t j = 0; j < dim; j++)
                {
                    Parameter *arg = Parameter::getNth(tup->arguments, j);
                    b = new BaseClass(arg->type);
                    baseclasses->insert(i + j, b);
                }
            }
            else
                i++;
        }

        if (baseok >= BASEOKdone)
        {
            //printf("%s already semantic analyzed, semanticRun = %d\n", toChars(), semanticRun);
            if (semanticRun >= PASSsemanticdone)
                return;
            goto Lancestorsdone;
        }

        if (!baseclasses->dim && sc->linkage == LINKcpp)
            cpp = true;
        if (sc->linkage == LINKobjc)
            objc()->setObjc(this);

        // Check for errors, handle forward references
        for (size_t i = 0; i < baseclasses->dim; )
        {
            BaseClass *b = (*baseclasses)[i];
            Type *tb = b->type->toBasetype();
            TypeClass *tc = (tb->ty == Tclass) ? (TypeClass *)tb : NULL;
            if (!tc || !tc->sym->isInterfaceDeclaration())
            {
                if (b->type != Type::terror)
                    error("base type must be interface, not %s", b->type->toChars());
                baseclasses->remove(i);
                continue;
            }

            // Check for duplicate interfaces
            for (size_t j = 0; j < i; j++)
            {
                BaseClass *b2 = (*baseclasses)[j];
                if (b2->sym == tc->sym)
                {
                    error("inherits from duplicate interface %s", b2->sym->toChars());
                    baseclasses->remove(i);
                    continue;
                }
            }

            if (tc->sym == this || isBaseOf2(tc->sym))
            {
                error("circular inheritance of interface");
                baseclasses->remove(i);
                continue;
            }

            if (tc->sym->isDeprecated())
            {
                if (!isDeprecated())
                {
                    // Deriving from deprecated class makes this one deprecated too
                    isdeprecated = true;

                    tc->checkDeprecated(loc, sc);
                }
            }

            b->sym = tc->sym;

            if (tc->sym->_scope && tc->sym->baseok < BASEOKdone)
                resolveBase(this, sc, scx, tc->sym); // Try to resolve forward reference
            if (tc->sym->baseok < BASEOKdone)
            {
                //printf("\ttry later, forward reference of base %s\n", tc->sym->toChars());
                if (tc->sym->_scope)
                    tc->sym->_scope->_module->addDeferredSemantic(tc->sym);
                baseok = BASEOKnone;
            }
            i++;
        }
        if (baseok == BASEOKnone)
        {
            // Forward referencee of one or more bases, try again later
            _scope = scx ? scx : sc->copy();
            _scope->setNoFree();
            _scope->_module->addDeferredSemantic(this);
            return;
        }
        baseok = BASEOKdone;

        interfaces.length = baseclasses->dim;
        interfaces.ptr = baseclasses->tdata();

        for (size_t i = 0; i < interfaces.length; i++)
        {
            BaseClass *b = interfaces.ptr[i];
            // If this is an interface, and it derives from a COM interface,
            // then this is a COM interface too.
            if (b->sym->isCOMinterface())
                com = true;
            if (b->sym->isCPPinterface())
                cpp = true;
        }

        interfaceSemantic(sc);
    }
Lancestorsdone:

    if (!members)               // if opaque declaration
    {
        semanticRun = PASSsemanticdone;
        return;
    }
    if (!symtab)
        symtab = new DsymbolTable();

    for (size_t i = 0; i < baseclasses->dim; i++)
    {
        BaseClass *b = (*baseclasses)[i];
        Type *tb = b->type->toBasetype();
        assert(tb->ty == Tclass);
        TypeClass *tc = (TypeClass *)tb;

        if (tc->sym->semanticRun < PASSsemanticdone)
        {
            // Forward referencee of one or more bases, try again later
            _scope = scx ? scx : sc->copy();
            _scope->setNoFree();
            if (tc->sym->_scope)
                tc->sym->_scope->_module->addDeferredSemantic(tc->sym);
            _scope->_module->addDeferredSemantic(this);
            return;
        }
    }

    if (baseok == BASEOKdone)
    {
        baseok = BASEOKsemanticdone;

        // initialize vtbl
        if (vtblOffset())
            vtbl.push(this);                // leave room at vtbl[0] for classinfo

        // Cat together the vtbl[]'s from base interfaces
        for (size_t i = 0; i < interfaces.length; i++)
        {
            BaseClass *b = interfaces.ptr[i];

            // Skip if b has already appeared
            for (size_t k = 0; k < i; k++)
            {
                if (b == interfaces.ptr[k])
                    goto Lcontinue;
            }

            // Copy vtbl[] from base class
            if (b->sym->vtblOffset())
            {
                size_t d = b->sym->vtbl.dim;
                if (d > 1)
                {
                    vtbl.reserve(d - 1);
                    for (size_t j = 1; j < d; j++)
                        vtbl.push(b->sym->vtbl[j]);
                }
            }
            else
            {
                vtbl.append(&b->sym->vtbl);
            }

          Lcontinue:
            ;
        }
    }

    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        s->addMember(sc, this);
    }

    Scope *sc2 = newScope(sc);

    /* Set scope so if there are forward references, we still might be able to
     * resolve individual members like enums.
     */
    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        //printf("setScope %s %s\n", s->kind(), s->toChars());
        s->setScope(sc2);
    }

    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        s->importAll(sc2);
    }

    for (size_t i = 0; i < members->dim; i++)
    {
        Dsymbol *s = (*members)[i];
        s->semantic(sc2);
    }

    Module::dprogress++;
    semanticRun = PASSsemanticdone;
    //printf("-InterfaceDeclaration.semantic(%s), type = %p\n", toChars(), type);
    //members->print();

    sc2->pop();

    if (global.errors != errors)
    {
        // The type is no good.
        type = Type::terror;
    }

    assert(type->ty != Tclass || ((TypeClass *)type)->sym == this);
}

/*******************************************
 * Determine if 'this' is a base class of cd.
 * (Actually, if it is an interface supported by cd)
 * Output:
 *      *poffset        offset to start of class
 *                      OFFSET_RUNTIME  must determine offset at runtime
 * Returns:
 *      false   not a base
 *      true    is a base
 */

bool InterfaceDeclaration::isBaseOf(ClassDeclaration *cd, int *poffset)
{
    //printf("%s.InterfaceDeclaration::isBaseOf(cd = '%s')\n", toChars(), cd->toChars());
    assert(!baseClass);
    for (size_t j = 0; j < cd->interfaces.length; j++)
    {
        BaseClass *b = cd->interfaces.ptr[j];

        //printf("\tX base %s\n", b->sym->toChars());
        if (this == b->sym)
        {
            //printf("\tfound at offset %d\n", b->offset);
            if (poffset)
            {
                // don't return incorrect offsets https://issues.dlang.org/show_bug.cgi?id=16980
                *poffset = cd->sizeok == SIZEOKdone ? b->offset : OFFSET_FWDREF;
            }
            //printf("\tfound at offset %d\n", b->offset);
            return true;
        }
        if (isBaseOf(b, poffset))
            return true;
    }

    if (cd->baseClass && isBaseOf(cd->baseClass, poffset))
        return true;

    if (poffset)
        *poffset = 0;
    return false;
}

bool InterfaceDeclaration::isBaseOf(BaseClass *bc, int *poffset)
{
    //printf("%s.InterfaceDeclaration::isBaseOf(bc = '%s')\n", toChars(), bc->sym->toChars());
    for (size_t j = 0; j < bc->baseInterfaces.length; j++)
    {
        BaseClass *b = &bc->baseInterfaces.ptr[j];

        //printf("\tY base %s\n", b->sym->toChars());
        if (this == b->sym)
        {
            //printf("\tfound at offset %d\n", b->offset);
            if (poffset)
            {
                *poffset = b->offset;
            }
            return true;
        }
        if (isBaseOf(b, poffset))
        {
            return true;
        }
    }
    if (poffset)
        *poffset = 0;
    return false;
}

/****************************************
 * Determine if slot 0 of the vtbl[] is reserved for something else.
 * For class objects, yes, this is where the ClassInfo ptr goes.
 * For COM interfaces, no.
 * For non-COM interfaces, yes, this is where the Interface ptr goes.
 */

int InterfaceDeclaration::vtblOffset() const
{
    if (isCOMinterface() || isCPPinterface())
        return 0;
    return 1;
}

bool InterfaceDeclaration::isCOMinterface() const
{
    return com;
}

bool InterfaceDeclaration::isCPPinterface() const
{
    return cpp;
}

/*******************************************
 */

const char *InterfaceDeclaration::kind() const
{
    return "interface";
}


/******************************** BaseClass *****************************/

BaseClass::BaseClass()
{
    this->type = NULL;
    this->sym = NULL;
    this->offset = 0;

    this->baseInterfaces.length = 0;
    this->baseInterfaces.ptr = NULL;
}

BaseClass::BaseClass(Type *type)
{
    //printf("BaseClass(this = %p, '%s')\n", this, type->toChars());
    this->type = type;
    this->sym = NULL;
    this->offset = 0;

    this->baseInterfaces.length = 0;
    this->baseInterfaces.ptr = NULL;
}

/****************************************
 * Fill in vtbl[] for base class based on member functions of class cd.
 * Input:
 *      vtbl            if !=NULL, fill it in
 *      newinstance     !=0 means all entries must be filled in by members
 *                      of cd, not members of any base classes of cd.
 * Returns:
 *      true if any entries were filled in by members of cd (not exclusively
 *      by base classes)
 */

bool BaseClass::fillVtbl(ClassDeclaration *cd, FuncDeclarations *vtbl, int newinstance)
{
    bool result = false;

    //printf("BaseClass::fillVtbl(this='%s', cd='%s')\n", sym->toChars(), cd->toChars());
    if (vtbl)
        vtbl->setDim(sym->vtbl.dim);

    // first entry is ClassInfo reference
    for (size_t j = sym->vtblOffset(); j < sym->vtbl.dim; j++)
    {
        FuncDeclaration *ifd = sym->vtbl[j]->isFuncDeclaration();
        FuncDeclaration *fd;
        TypeFunction *tf;

        //printf("        vtbl[%d] is '%s'\n", j, ifd ? ifd->toChars() : "null");

        assert(ifd);
        // Find corresponding function in this class
        tf = ifd->type->toTypeFunction();
        fd = cd->findFunc(ifd->ident, tf);
        if (fd && !fd->isAbstract())
        {
            //printf("            found\n");
            // Check that calling conventions match
            if (fd->linkage != ifd->linkage)
                fd->error("linkage doesn't match interface function");

            // Check that it is current
            //printf("newinstance = %d fd->toParent() = %s ifd->toParent() = %s\n",
                //newinstance, fd->toParent()->toChars(), ifd->toParent()->toChars());
            if (newinstance && fd->toParent() != cd && ifd->toParent() == sym)
                cd->error("interface function '%s' is not implemented", ifd->toFullSignature());

            if (fd->toParent() == cd)
                result = true;
        }
        else
        {
            //printf("            not found %p\n", fd);
            // BUG: should mark this class as abstract?
            if (!cd->isAbstract())
                cd->error("interface function '%s' is not implemented", ifd->toFullSignature());

            fd = NULL;
        }
        if (vtbl)
            (*vtbl)[j] = fd;
    }

    return result;
}

void BaseClass::copyBaseInterfaces(BaseClasses *vtblInterfaces)
{
    //printf("+copyBaseInterfaces(), %s\n", sym->toChars());
//    if (baseInterfaces.length)
//      return;

    baseInterfaces.length = sym->interfaces.length;
    baseInterfaces.ptr = (BaseClass *)mem.xcalloc(baseInterfaces.length, sizeof(BaseClass));

    //printf("%s.copyBaseInterfaces()\n", sym->toChars());
    for (size_t i = 0; i < baseInterfaces.length; i++)
    {
        void *pb = &baseInterfaces.ptr[i];
        BaseClass *b2 = sym->interfaces.ptr[i];

        assert(b2->vtbl.dim == 0);      // should not be filled yet
        BaseClass *b = (BaseClass *)memcpy(pb, b2, sizeof(BaseClass));

        if (i)                          // single inheritance is i==0
            vtblInterfaces->push(b);    // only need for M.I.
        b->copyBaseInterfaces(vtblInterfaces);
    }
    //printf("-copyBaseInterfaces\n");
}
