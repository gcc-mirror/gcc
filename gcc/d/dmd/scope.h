
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/scope.h
 */

#pragma once

class Dsymbol;
class ScopeDsymbol;
class Identifier;
class Module;
class Statement;
class SwitchStatement;
class TryFinallyStatement;
class LabelStatement;
class ForeachStatement;
class ClassDeclaration;
class AggregateDeclaration;
class FuncDeclaration;
class UserAttributeDeclaration;
struct DocComment;
struct AA;
class TemplateInstance;

#include "dsymbol.h"

#if __GNUC__
// Requires a full definition for LINK
#include "globals.h"
#else
enum LINK;
enum PINLINE;
#endif

#define CSXthis_ctor    1       // called this()
#define CSXsuper_ctor   2       // called super()
#define CSXthis         4       // referenced this
#define CSXsuper        8       // referenced super
#define CSXlabel        0x10    // seen a label
#define CSXreturn       0x20    // seen a return statement
#define CSXany_ctor     0x40    // either this() or super() was called
#define CSXhalt         0x80    // assert(0)

// Flags that would not be inherited beyond scope nesting
#define SCOPEctor           0x0001  // constructor type
#define SCOPEcondition      0x0004  // inside static if/assert condition
#define SCOPEdebug          0x0008  // inside debug conditional

// Flags that would be inherited beyond scope nesting
#define SCOPEnoaccesscheck  0x0002  // don't do access checks
#define SCOPEconstraint     0x0010  // inside template constraint
#define SCOPEinvariant      0x0020  // inside invariant code
#define SCOPErequire        0x0040  // inside in contract code
#define SCOPEensure         0x0060  // inside out contract code
#define SCOPEcontract       0x0060  // [mask] we're inside contract code
#define SCOPEctfe           0x0080  // inside a ctfe-only expression
#define SCOPEcompile        0x0100  // inside __traits(compile)
#define SCOPEignoresymbolvisibility 0x0200  // ignore symbol visibility (Bugzilla 15907)

#define SCOPEfree           0x8000  // is on free list
#define SCOPEfullinst       0x10000 // fully instantiate templates
#define SCOPEalias          0x20000 // inside alias declaration

struct Scope
{
    Scope *enclosing;           // enclosing Scope

    Module *_module;            // Root module
    ScopeDsymbol *scopesym;     // current symbol
    FuncDeclaration *func;      // function we are in
    Dsymbol *parent;            // parent to use
    LabelStatement *slabel;     // enclosing labelled statement
    SwitchStatement *sw;        // enclosing switch statement
    TryFinallyStatement *tf;    // enclosing try finally statement
    ScopeGuardStatement *os;       // enclosing scope(xxx) statement
    Statement *sbreak;          // enclosing statement that supports "break"
    Statement *scontinue;       // enclosing statement that supports "continue"
    ForeachStatement *fes;      // if nested function for ForeachStatement, this is it
    Scope *callsc;              // used for __FUNCTION__, __PRETTY_FUNCTION__ and __MODULE__
    int inunion;                // we're processing members of a union
    int nofree;                 // set if shouldn't free it
    int noctor;                 // set if constructor calls aren't allowed
    int intypeof;               // in typeof(exp)
    VarDeclaration *lastVar;    // Previous symbol used to prevent goto-skips-init

    /* If  minst && !tinst, it's in definitely non-speculative scope (eg. module member scope).
     * If !minst && !tinst, it's in definitely speculative scope (eg. template constraint).
     * If  minst &&  tinst, it's in instantiated code scope without speculation.
     * If !minst &&  tinst, it's in instantiated code scope with speculation.
     */
    Module *minst;              // root module where the instantiated templates should belong to
    TemplateInstance *tinst;    // enclosing template instance

    unsigned callSuper;         // primitive flow analysis for constructors
    unsigned *fieldinit;
    size_t fieldinit_dim;

    AlignDeclaration *aligndecl;    // alignment for struct members

    LINK linkage;               // linkage for external functions
    CPPMANGLE cppmangle;        // C++ mangle type
    PINLINE inlining;            // inlining strategy for functions

    Prot protection;            // protection for class members
    int explicitProtection;     // set if in an explicit protection attribute

    StorageClass stc;           // storage class

    DeprecatedDeclaration *depdecl; // customized deprecation message

    unsigned flags;

    UserAttributeDeclaration *userAttribDecl;   // user defined attributes

    DocComment *lastdc;         // documentation comment for last symbol at this scope
    AA *anchorCounts;           // lookup duplicate anchor name count
    Identifier *prevAnchor;     // qualified symbol name of last doc anchor

    static Scope *freelist;
    static Scope *alloc();
    static Scope *createGlobal(Module *module);

    Scope();

    Scope *copy();

    Scope *push();
    Scope *push(ScopeDsymbol *ss);
    Scope *pop();

    Scope *startCTFE();
    Scope *endCTFE();

    void mergeCallSuper(Loc loc, unsigned cs);

    unsigned *saveFieldInit();
    void mergeFieldInit(Loc loc, unsigned *cses);

    Module *instantiatingModule();

    Dsymbol *search(Loc loc, Identifier *ident, Dsymbol **pscopesym, int flags = IgnoreNone);
    static void deprecation10378(Loc loc, Dsymbol *sold, Dsymbol *snew);
    Dsymbol *search_correct(Identifier *ident);
    static const char *search_correct_C(Identifier *ident);
    Dsymbol *insert(Dsymbol *s);

    ClassDeclaration *getClassScope();
    AggregateDeclaration *getStructClassScope();
    void setNoFree();

    structalign_t alignment();
};
