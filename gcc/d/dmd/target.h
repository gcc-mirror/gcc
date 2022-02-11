
/* Compiler implementation of the D programming language
 * Copyright (C) 2013-2022 by The D Language Foundation, All Rights Reserved
 * written by Iain Buclaw
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
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
class TypeTuple;
class TypeFunction;

enum class CPU : unsigned char
{
    x87,
    mmx,
    sse,
    sse2,
    sse3,
    ssse3,
    sse4_1,
    sse4_2,
    avx,                // AVX1 instruction set
    avx2,               // AVX2 instruction set
    avx512,             // AVX-512 instruction set

    // Special values that don't survive past the command line processing
    baseline,           // (default) the minimum capability CPU
    native              // the machine the compiler is being run on
};

struct TargetC
{
    enum class Runtime : unsigned char
    {
        Unspecified,
        Bionic,
        DigitalMars,
        Glibc,
        Microsoft,
        Musl,
        Newlib,
        UClibc,
        WASI,
    };

    enum class BitFieldStyle : unsigned char
    {
        Unspecified,
        DM,                   // Digital Mars 32 bit C compiler
        MS,                   // Microsoft 32 and 64 bit C compilers
                              // https://docs.microsoft.com/en-us/cpp/c-language/c-bit-fields?view=msvc-160
                              // https://docs.microsoft.com/en-us/cpp/cpp/cpp-bit-fields?view=msvc-160
        Gcc_Clang,            // gcc and clang
    };

    uint8_t crtDestructorsSupported; // Not all platforms support crt_destructor
    uint8_t longsize;            // size of a C 'long' or 'unsigned long' type
    uint8_t long_doublesize;     // size of a C 'long double'
    uint8_t wchar_tsize;         // size of a C 'wchar_t' type
    Runtime runtime;
    BitFieldStyle bitFieldStyle; // different C compilers do it differently
};

struct TargetCPP
{
    enum class Runtime : unsigned char
    {
        Unspecified,
        Clang,
        DigitalMars,
        Gcc,
        Microsoft,
        Sun
    };
    bool reverseOverloads;    // with dmc and cl, overloaded functions are grouped and in reverse order
    bool exceptions;          // set if catching C++ exceptions is supported
    bool twoDtorInVtable;     // target C++ ABI puts deleting and non-deleting destructor into vtable
    bool wrapDtorInExternD;   // set if C++ dtors require a D wrapper to be callable from runtime
    Runtime runtime;

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
    typedef unsigned char OS;
    enum
    {
        /* These are mutually exclusive; one and only one is set.
         * Match spelling and casing of corresponding version identifiers
         */
        OS_Freestanding = 0,
        OS_linux        = 1,
        OS_Windows      = 2,
        OS_OSX          = 4,
        OS_OpenBSD      = 8,
        OS_FreeBSD      = 0x10,
        OS_Solaris      = 0x20,
        OS_DragonFlyBSD = 0x40,

        // Combination masks
        all = OS_linux | OS_Windows | OS_OSX | OS_OpenBSD | OS_FreeBSD | OS_Solaris | OS_DragonFlyBSD,
        Posix = OS_linux | OS_OSX | OS_OpenBSD | OS_FreeBSD | OS_Solaris | OS_DragonFlyBSD,
    };

    OS os;
    uint8_t osMajor;
    // D ABI
    uint8_t ptrsize;
    uint8_t realsize;           // size a real consumes in memory
    uint8_t realpad;            // 'padding' added to the CPU real size to bring it up to realsize
    uint8_t realalignsize;      // alignment for reals
    uint8_t classinfosize;      // size of 'ClassInfo'
    uint64_t maxStaticDataSize; // maximum size of static data

    // C ABI
    TargetC c;

    // C++ ABI
    TargetCPP cpp;

    // Objective-C ABI
    TargetObjC objc;

    DString architectureName;    // name of the platform architecture (e.g. X86_64)
    CPU cpu;                // CPU instruction set to target
    bool is64bit;           // generate 64 bit code for x86_64; true by default for 64 bit dmd
    bool isLP64;            // pointers are 64 bits

    // Environmental
    DString obj_ext;    /// extension for object files
    DString lib_ext;    /// extension for static library files
    DString dll_ext;    /// extension for dynamic library files
    bool run_noext;     /// allow -run sources without extensions
    bool mscoff;        /// for Win32: write COFF object files instead of OMF

    template <typename T>
    struct FPTypeProperties
    {
        real_t max;
        real_t min_normal;
        real_t nan;
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
    const Param *params;

public:
    void _init(const Param& params);
    // Type sizes and support.
    void setTriple(const char* _triple);
    unsigned alignsize(Type *type);
    unsigned fieldalign(Type *type);
    Type *va_listType(const Loc &loc, Scope *sc);  // get type of va_list
    int isVectorTypeSupported(int sz, Type *type);
    bool isVectorOpSupported(Type *type, EXP op, Type *t2 = NULL);
    // ABI and backend.
    LINK systemLinkage();
    TypeTuple *toArgTypes(Type *t);
    bool isReturnOnStack(TypeFunction *tf, bool needsThis);
    d_uns64 parameterSize(const Loc& loc, Type *t);
    bool preferPassByRef(Type *t);
    Expression *getTargetInfo(const char* name, const Loc& loc);
    bool isCalleeDestroyingArgs(TypeFunction* tf);
    bool libraryObjectMonitors(FuncDeclaration *fd, Statement *fbody);
    void addPredefinedGlobalIdentifiers() const;
};

extern Target target;
