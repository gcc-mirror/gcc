
/* Compiler implementation of the D programming language
 * Copyright (C) 2015-2023 by The D Language Foundation, All Rights Reserved
 * written by Michel Fortin
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/objc.h
 */

#pragma once

#include "root/dsystem.h"
#include "arraytypes.h"

class AggregateDeclaration;
class AttribDeclaration;
class ClassDeclaration;
class FuncDeclaration;
class Identifier;
class InterfaceDeclaration;

struct Scope;

struct ObjcSelector
{
    const char *stringvalue;
    size_t stringlen;
    size_t paramCount;

    static void _init();

    static ObjcSelector *create(FuncDeclaration *fdecl);
};

struct ObjcClassDeclaration
{
    d_bool isMeta;
    d_bool isExtern;

    Identifier* identifier;
    ClassDeclaration* classDeclaration;
    ClassDeclaration* metaclass;
    DArray<FuncDeclaration*> methodList;

    bool isRootClass() const;
};

struct ObjcFuncDeclaration
{
    ObjcSelector* selector;
    VarDeclaration* selectorParameter;
    d_bool isOptional;
};

class Objc
{
public:
    static void _init();

    virtual void setObjc(ClassDeclaration* cd) = 0;
    virtual void setObjc(InterfaceDeclaration*) = 0;
    virtual const char *toPrettyChars(ClassDeclaration *cd, bool qualifyTypes) const = 0;

    virtual void setSelector(FuncDeclaration*, Scope* sc) = 0;
    virtual void validateSelector(FuncDeclaration* fd) = 0;
    virtual void checkLinkage(FuncDeclaration* fd) = 0;
    virtual bool isVirtual(const FuncDeclaration*) const = 0;
    virtual void setAsOptional(FuncDeclaration *fd, Scope *sc) const = 0;
    virtual void validateOptional(FuncDeclaration *fd) const = 0;
    virtual ClassDeclaration* getParent(FuncDeclaration*, ClassDeclaration*) const = 0;
    virtual void addToClassMethodList(FuncDeclaration*, ClassDeclaration*) const = 0;
    virtual AggregateDeclaration* isThis(FuncDeclaration* fd) = 0;
    virtual VarDeclaration* createSelectorParameter(FuncDeclaration*, Scope*) const = 0;

    virtual void setMetaclass(InterfaceDeclaration* id, Scope*) const = 0;
    virtual void setMetaclass(ClassDeclaration* id, Scope*) const = 0;
    virtual ClassDeclaration* getRuntimeMetaclass(ClassDeclaration* cd) = 0;

    virtual void addSymbols(AttribDeclaration*, ClassDeclarations*, ClassDeclarations*) const = 0;
    virtual void addSymbols(ClassDeclaration*, ClassDeclarations*, ClassDeclarations*) const = 0;
};
