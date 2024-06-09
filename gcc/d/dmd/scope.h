
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/scope.h
 */

#pragma once

class ErrorSink;
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

enum class CSX : uint16_t
{
    none       = 0,
    this_ctor  = 1,      // called this()
    super_ctor = 2,      // called super()
    label      = 4,      // seen a label
    return_    = 8,      // seen a return statement
    any_ctor   = 0x10,   // either this() or super() was called
    halt       = 0x20,   // assert(0)
};

enum class SCOPE
{
    // Flags that would not be inherited beyond scope nesting
    ctor          = 0x0001,  // constructor type
    noaccesscheck = 0x0002,  // don't do access checks
    condition     = 0x0004,  // inside static if/assert condition
    debug_        = 0x0008,  // inside debug conditional

    // Flags that would be inherited beyond scope nesting
    constraint    = 0x0010,  // inside template constraint
    invariant_    = 0x0020,  // inside invariant code
    require       = 0x0040,  // inside in contract code
    ensure        = 0x0060,  // inside out contract code
    contract      = 0x0060,  // [mask] we're inside contract code
    ctfe          = 0x0080,  // inside a ctfe-only expression
    compile       = 0x0100,  // inside __traits(compile)
    ignoresymbolvisibility = 0x0200,  // ignore symbol visibility (Bugzilla 15907)

    Cfile         = 0x0800,  // C semantics apply
    free          = 0x8000,  // is on free list
    fullinst      = 0x10000, // fully instantiate templates
    ctfeBlock     = 0x20000, // inside a `if (__ctfe)` block
    dip1000       = 0x40000, // dip1000 errors enabled for this scope
    dip25         = 0x80000, // dip25 errors enabled for this scope
};

struct Scope
{
    Scope *enclosing;           // enclosing Scope

    Module *_module;            // Root module
    ScopeDsymbol *scopesym;     // current symbol
    FuncDeclaration *func;      // function we are in
    VarDeclaration  *varDecl;   // variable we are in during semantic2
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
    d_bool nofree;                // true if shouldn't free it
    d_bool inLoop;                // true if inside a loop (where constructor calls aren't allowed)
    d_bool inDefaultArg;          // true if inside a default argument (where __FILE__, etc are evaluated at the call site)
    int intypeof;               // in typeof(exp)
    VarDeclaration *lastVar;    // Previous symbol used to prevent goto-skips-init
    ErrorSink *eSink;           // sink for error messages

    /* If  minst && !tinst, it's in definitely non-speculative scope (eg. module member scope).
     * If !minst && !tinst, it's in definitely speculative scope (eg. template constraint).
     * If  minst &&  tinst, it's in instantiated code scope without speculation.
     * If !minst &&  tinst, it's in instantiated code scope with speculation.
     */
    Module *minst;              // root module where the instantiated templates should belong to
    TemplateInstance *tinst;    // enclosing template instance

    CSX callSuper;              // primitive flow analysis for constructors
    CSX *fieldinit;
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

    Dsymbol *search(const Loc &loc, Identifier *ident, Dsymbol *&pscopesym, SearchOptFlags flags = (SearchOptFlags)SearchOpt::all);
};
