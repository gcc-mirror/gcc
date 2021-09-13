
/* Compiler implementation of the D programming language
 * Copyright (C) 2013-2021 by The D Language Foundation, All Rights Reserved
 * written by Iain Buclaw
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/target.h
 */

#pragma once

// This file contains a data structure that describes a back-end target.
// At present it is incomplete, but in future it should grow to contain
// most or all target machine and target O/S specific information.
#include "globals.h"
#include "tokens.h"

class ClassDeclaration;
class Dsymbol;
class Expression;
class FuncDeclaration;
class Parameter;
class Statement;
class Type;
class TypeFunction;
class TypeTuple;
struct OutBuffer;

struct TargetC
{
    unsigned longsize;            // size of a C 'long' or 'unsigned long' type
    unsigned long_doublesize;     // size of a C 'long double'
    Type *twchar_t;               // C 'wchar_t' type
};

struct TargetCPP
{
    bool reverseOverloads;    // with dmc and cl, overloaded functions are grouped and in reverse order
    bool exceptions;          // set if catching C++ exceptions is supported
    bool twoDtorInVtable;     // target C++ ABI puts deleting and non-deleting destructor into vtable

    const char *toMangle(Dsymbol *s);
    const char *typeInfoMangle(ClassDeclaration *cd);
    const char *thunkMangle(FuncDeclaration *fd, int offset);
    const char *typeMangle(Type *t);
    Type *parameterType(Parameter *p);
    bool fundamentalType(const Type *t, bool& isFundamental);
    unsigned derivedClassOffset(ClassDeclaration *baseClass);
};

struct TargetObjC
{
    bool supported;     // set if compiler can interface with Objective-C
};

struct Target
{
    // D ABI
    unsigned ptrsize;
    unsigned realsize;           // size a real consumes in memory
    unsigned realpad;            // 'padding' added to the CPU real size to bring it up to realsize
    unsigned realalignsize;      // alignment for reals
    unsigned classinfosize;      // size of 'ClassInfo'
    unsigned long long maxStaticDataSize;  // maximum size of static data

    // C ABI
    TargetC c;

    // C++ ABI
    TargetCPP cpp;

    // Objective-C ABI
    TargetObjC objc;

    template <typename T>
    struct FPTypeProperties
    {
        real_t max;
        real_t min_normal;
        real_t nan;
        real_t snan;
        real_t infinity;
        real_t epsilon;

        d_int64 dig;
        d_int64 mant_dig;
        d_int64 max_exp;
        d_int64 min_exp;
        d_int64 max_10_exp;
        d_int64 min_10_exp;
    };

    FPTypeProperties<float> FloatProperties;
    FPTypeProperties<double> DoubleProperties;
    FPTypeProperties<real_t> RealProperties;

private:
    Type *tvalist;

public:
    void _init(const Param& params);
    // Type sizes and support.
    unsigned alignsize(Type *type);
    unsigned fieldalign(Type *type);
    Type *va_listType(const Loc &loc, Scope *sc);  // get type of va_list
    int isVectorTypeSupported(int sz, Type *type);
    bool isVectorOpSupported(Type *type, TOK op, Type *t2 = NULL);
    // ABI and backend.
    LINK systemLinkage();
    TypeTuple *toArgTypes(Type *t);
    bool isReturnOnStack(TypeFunction *tf, bool needsThis);
    Expression *getTargetInfo(const char* name, const Loc& loc);
    bool libraryObjectMonitors(FuncDeclaration *fd, Statement *fbody);
};

extern Target target;
