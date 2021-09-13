
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
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

    //printf("ClassDeclaration(%s), dim = %d\n", id->toChars(), this->baseclasses->length);

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
    isscope = false;
    isabstract = ABSfwdref;
    inuse = 0;
    baseok = BASEOKnone;
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

    cd->baseclasses->setDim(this->baseclasses->length);
    for (size_t i = 0; i < cd->baseclasses->length; i++)
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
        /* This enables us to use COM objects under Linux and
         * work with things like XPCOM
         */
        sc2->linkage = target.systemLinkage();
    }
    return sc2;
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
    for (size_t i = 0; i < cd->baseclasses->length; i++)
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
        if (!cd->baseClass && cd->semanticRun < PASSsemanticdone && !cd->isInterfaceDeclaration())
        {
            dsymbolSemantic(cd, NULL);
            if (!cd->baseClass && cd->semanticRun < PASSsemanticdone)
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
            dsymbolSemantic(this, NULL);
            --inuse;
        }
    }

    if (!members || !symtab)    // opaque or addMember is not yet done
    {
        error("is forward referenced when looking for `%s`", ident->toChars());
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

        for (size_t i = 0; i < baseclasses->length; i++)
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
                    else if (!(flags & IgnoreSymbolVisibility) && !(s->prot().kind == Prot::protected_) && !symbolIsVisible(this, s))
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
    for (size_t i = 0; i < baseclasses->length; i++)
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
            b->sym->alignsize = target.ptrsize;
        cd->alignmember(b->sym->alignsize, b->sym->alignsize, &offset);
        assert(bi < vtblInterfaces->length);
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
        if (classKind == ClassKind::cpp)
            structsize = target.cpp.derivedClassOffset(baseClass);
        else
            structsize = baseClass->structsize;
    }
    else if (isInterfaceDeclaration())
    {
        if (interfaces.length == 0)
        {
            alignsize = target.ptrsize;
            structsize = target.ptrsize;      // allow room for __vptr
        }
    }
    else
    {
        alignsize = target.ptrsize;
        structsize = target.ptrsize;      // allow room for __vptr
        if (hasMonitor())
            structsize += target.ptrsize; // allow room for __monitor
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
    for (size_t i = 0; i < members->length; i++)
    {
        Dsymbol *s = (*members)[i];
        s->setFieldOffset(this, &offset, false);
    }

    sizeok = SIZEOKdone;

    // Calculate fields[i]->overlapped
    checkOverlappedFields();
}

/**************
 * Returns: true if there's a __monitor field
 */
bool ClassDeclaration::hasMonitor()
{
    return classKind == ClassKind::d;
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
        for (size_t i = 0; i < os->a.length; i++)
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
        for (size_t i = 0; i < vtbl->length; i++)
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
    return classKind == ClassKind::cpp;
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
                dsymbolSemantic(fd, NULL);

            if (fd->isAbstract())
                return 1;
            return 0;
        }
    };

    for (size_t i = 0; i < members->length; i++)
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
    for (size_t i = 1; i < vtbl.length; i++)
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
    return classKind == ClassKind::cpp ? 0 : 1;
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
        classKind = ClassKind::cpp; // IUnknown is also a C++ interface
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
    else if (classKind == ClassKind::cpp)
        sc2->linkage = LINKcpp;
    else if (classKind == ClassKind::objc)
        sc2->linkage = LINKobjc;
    return sc2;
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
    return classKind == ClassKind::cpp;
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
        vtbl->setDim(sym->vtbl.length);

    // first entry is ClassInfo reference
    for (size_t j = sym->vtblOffset(); j < sym->vtbl.length; j++)
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
                cd->error("interface function `%s` is not implemented", ifd->toFullSignature());

            if (fd->toParent() == cd)
                result = true;
        }
        else
        {
            //printf("            not found %p\n", fd);
            // BUG: should mark this class as abstract?
            if (!cd->isAbstract())
                cd->error("interface function `%s` is not implemented", ifd->toFullSignature());

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

        assert(b2->vtbl.length == 0);      // should not be filled yet
        BaseClass *b = (BaseClass *)memcpy(pb, b2, sizeof(BaseClass));

        if (i)                          // single inheritance is i==0
            vtblInterfaces->push(b);    // only need for M.I.
        b->copyBaseInterfaces(vtblInterfaces);
    }
    //printf("-copyBaseInterfaces\n");
}
