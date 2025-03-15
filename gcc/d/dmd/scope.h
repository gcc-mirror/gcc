
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
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

enum class Contract : uint8_t
{
    none = 0u,
    invariant_ = 1u,
    require = 2u,
    ensure = 3u,
};

struct Scope final
{
    Scope *enclosing;           // enclosing Scope

    Module *_module;            // Root module
    ScopeDsymbol *scopesym;     // current symbol
    FuncDeclaration *func;      // function we are in
    VarDeclaration  *varDecl;   // variable we are in during semantic2
    Dsymbol *parent;            // parent to use
    LabelStatement *slabel;     // enclosing labelled statement
    SwitchStatement *switchStatement; // enclosing switch statement
    Statement *tryBody;         // enclosing _body of TryCatchStatement or TryFinallyStatement
    TryFinallyStatement *tryFinally; // enclosing try finally statement
    ScopeGuardStatement *scopeGuard; // enclosing scope(xxx) statement
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

    uint16_t flags;
    uint16_t previews; // state of preview switches

    bool ctor() const;
    bool ctor(bool v);
    bool noAccessCheck() const;
    bool noAccessCheck(bool v);
    bool condition() const;
    bool condition(bool v);
    bool debug_() const;
    bool debug_(bool v);
    bool inTemplateConstraint() const;
    bool inTemplateConstraint(bool v);
    Contract contract() const;
    Contract contract(Contract v);
    bool ctfe() const;
    bool ctfe(bool v);
    bool traitsCompiles() const;
    bool traitsCompiles(bool v);
    bool ignoresymbolvisibility() const;
    bool ignoresymbolvisibility(bool v);
    bool inCfile() const;
    bool inCfile(bool v);
    bool canFree() const;
    bool canFree(bool v);
    bool fullinst() const;
    bool fullinst(bool v);
    bool ctfeBlock() const;
    bool ctfeBlock(bool v);

    UserAttributeDeclaration *userAttribDecl;   // user defined attributes

    DocComment *lastdc;         // documentation comment for last symbol at this scope
    AA *anchorCounts;           // lookup duplicate anchor name count
    Identifier *prevAnchor;     // qualified symbol name of last doc anchor

    AliasDeclaration *aliasAsg; // if set, then aliasAsg is being assigned a new value,
                                // do not set wasRead for it
    StructDeclaration *argStruct; // elimiate recursion when looking for rvalue construction

    Dsymbol *search(Loc loc, Identifier *ident, Dsymbol *&pscopesym, SearchOptFlags flags = (SearchOptFlags)SearchOpt::all);
};
