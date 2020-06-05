
/* Compiler implementation of the D programming language
 * Copyright (C) 2015-2020 by The D Language Foundation, All Rights Reserved
 * written by Michel Fortin
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/objc_stubs.c
 */

#include "objc.h"
#include "aggregate.h"
#include "scope.h"

class FuncDeclaration;

// MARK: ObjcSelector

ObjcSelector::ObjcSelector(const char *, size_t, size_t)
{
    printf("Should never be called when D_OBJC is false\n");
    assert(0);
}

ObjcSelector *ObjcSelector::lookup(const char *)
{
    printf("Should never be called when D_OBJC is false\n");
    assert(0);
    return NULL;
}

ObjcSelector *ObjcSelector::lookup(const char *, size_t, size_t)
{
    printf("Should never be called when D_OBJC is false\n");
    assert(0);
    return NULL;
}

ObjcSelector *ObjcSelector::create(FuncDeclaration *)
{
    printf("Should never be called when D_OBJC is false\n");
    assert(0);
    return NULL;
}

class UnsupportedObjc : public Objc
{
    void setObjc(ClassDeclaration *cd)
    {
        cd->error("Objective-C classes not supported");
    }

    void setObjc(InterfaceDeclaration *id)
    {
        id->error("Objective-C interfaces not supported");
    }

    void setSelector(FuncDeclaration *, Scope *)
    {
        // noop
    }

    void validateSelector(FuncDeclaration *)
    {
        // noop
    }

    void checkLinkage(FuncDeclaration *)
    {
        // noop
    }
};

static Objc *_objc;

Objc *objc()
{
    return _objc;
}

void Objc::_init()
{
    _objc = new UnsupportedObjc();
}
