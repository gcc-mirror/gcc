
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/scope.h
 */

#pragma once

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
class CPPNamespaceDeclaration;

#include "dsymbol.h"

enum
{
    CSXthis_ctor  = 1,      // called this()
    CSXsuper_ctor = 2,      // called super()
    CSXthis       = 4,      // referenced this
    CSXsuper      = 8,      // referenced super
    CSXlabel      = 0x10,   // seen a label
    CSXreturn     = 0x20,   // seen a return statement
    CSXany_ctor   = 0x40,   // either this() or super() was called
    CSXhalt       = 0x80,   // assert(0)
};

enum
{
    // Flags that would not be inherited beyond scope nesting
    SCOPEctor          = 0x0001,  // constructor type
    SCOPEcondition     = 0x0004,  // inside static if/assert condition
    SCOPEdebug         = 0x0008,  // inside debug conditional

    // Flags that would be inherited beyond scope nesting
    SCOPEnoaccesscheck = 0x0002,  // don't do access checks
    SCOPEconstraint    = 0x0010,  // inside template constraint
    SCOPEinvariant     = 0x0020,  // inside invariant code
    SCOPErequire       = 0x0040,  // inside in contract code
    SCOPEensure        = 0x0060,  // inside out contract code
    SCOPEcontract      = 0x0060,  // [mask] we're inside contract code
    SCOPEctfe          = 0x0080,  // inside a ctfe-only expression
    SCOPEcompile       = 0x0100,  // inside __traits(compile)
    SCOPEignoresymbolvisibility = 0x0200,  // ignore symbol visibility (Bugzilla 15907)

    SCOPEfree          = 0x8000,  // is on free list
    SCOPEfullinst      = 0x10000, // fully instantiate templates
    SCOPEalias         = 0x20000, // inside alias declaration

    // The following are mutually exclusive
    SCOPEprintf        = 0x40000, // printf-style function
    SCOPEscanf         = 0x80000, // scanf-style function
};

struct Scope
{
    Scope *enclosing;           // enclosing Scope

    Module *_module;            // Root module
    ScopeDsymbol *scopesym;     // current symbol
    FuncDeclaration *func;      // function we are in
    Dsymbol *parent;            // parent to use
    LabelStatement *slabel;     // enclosing labelled statement
    SwitchStatement *sw;        // enclosing switch statement
    Statement *tryBody;         // enclosing _body of TryCatchStatement or TryFinallyStatement
    TryFinallyStatement *tf;    // enclosing try finally statement
    ScopeGuardStatement *os;       // enclosing scope(xxx) statement
    Statement *sbreak;          // enclosing statement that supports "break"
    Statement *scontinue;       // enclosing statement that supports "continue"
    ForeachStatement *fes;      // if nested function for ForeachStatement, this is it
    Scope *callsc;              // used for __FUNCTION__, __PRETTY_FUNCTION__ and __MODULE__
    Dsymbol *inunion;           // !=null if processing members of a union
    bool nofree;                // true if shouldn't free it
    bool inLoop;                // true if inside a loop (where constructor calls aren't allowed)
    int intypeof;               // in typeof(exp)
    VarDeclaration *lastVar;    // Previous symbol used to prevent goto-skips-init

    /* If  minst && !tinst, it's in definitely non-speculative scope (eg. module member scope).
     * If !minst && !tinst, it's in definitely speculative scope (eg. template constraint).
     * If  minst &&  tinst, it's in instantiated code scope without speculation.
     * If !minst &&  tinst, it's in instantiated code scope with speculation.
     */
    Module *minst;              // root module where the instantiated templates should belong to
    TemplateInstance *tinst;    // enclosing template instance

    unsigned char callSuper;    // primitive flow analysis for constructors
    unsigned char *fieldinit;
    size_t fieldinit_dim;

    AlignDeclaration *aligndecl;    // alignment for struct members

    /// C++ namespace this symbol belongs to
    CPPNamespaceDeclaration *namespace_;

    LINK linkage;               // linkage for external functions
    CPPMANGLE cppmangle;        // C++ mangle type
    PragmaDeclaration *inlining; // inlining strategy for functions

    Visibility visibility;            // visibility for class members
    int explicitVisibility;     // set if in an explicit visibility attribute

    StorageClass stc;           // storage class

    DeprecatedDeclaration *depdecl; // customized deprecation message

    unsigned flags;

    UserAttributeDeclaration *userAttribDecl;   // user defined attributes

    DocComment *lastdc;         // documentation comment for last symbol at this scope
    AA *anchorCounts;           // lookup duplicate anchor name count
    Identifier *prevAnchor;     // qualified symbol name of last doc anchor

    AliasDeclaration *aliasAsg; // if set, then aliasAsg is being assigned a new value,
                                // do not set wasRead for it
    Scope();

    Scope *copy();

    Scope *push();
    Scope *push(ScopeDsymbol *ss);
    Scope *pop();

    Scope *startCTFE();
    Scope *endCTFE();

    Dsymbol *search(const Loc &loc, Identifier *ident, Dsymbol **pscopesym, int flags = IgnoreNone);

    ClassDeclaration *getClassScope();
    AggregateDeclaration *getStructClassScope();

    structalign_t alignment();

    bool isDeprecated() const;
};
