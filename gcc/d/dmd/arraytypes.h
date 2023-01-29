
/* Compiler implementation of the D programming language
 * Copyright (C) 2006-2023 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/arraytypes.h
 */

#pragma once

#include "root/array.h"
#include "root/bitarray.h"

typedef Array<class TemplateParameter *> TemplateParameters;

typedef Array<class Expression *> Expressions;

typedef Array<class Statement *> Statements;

typedef Array<struct BaseClass *> BaseClasses;

typedef Array<class ClassDeclaration *> ClassDeclarations;

typedef Array<class Dsymbol *> Dsymbols;

typedef Array<class RootObject *> Objects;

typedef Array<class DtorDeclaration *> DtorDeclarations;

typedef Array<class FuncDeclaration *> FuncDeclarations;

typedef Array<class Parameter *> Parameters;

typedef Array<class Identifier *> Identifiers;

typedef Array<class Initializer *> Initializers;

typedef Array<class VarDeclaration *> VarDeclarations;

typedef Array<class Type *> Types;
typedef Array<class Catch *> Catches;

typedef Array<class StaticDtorDeclaration *> StaticDtorDeclarations;

typedef Array<class SharedStaticDtorDeclaration *> SharedStaticDtorDeclarations;

typedef Array<class AliasDeclaration *> AliasDeclarations;

typedef Array<class Module *> Modules;

typedef Array<class CaseStatement *> CaseStatements;

typedef Array<class ScopeStatement *> ScopeStatements;

typedef Array<class GotoCaseStatement *> GotoCaseStatements;

typedef Array<class ReturnStatement *> ReturnStatements;

typedef Array<class GotoStatement *> GotoStatements;

typedef Array<class TemplateInstance *> TemplateInstances;

typedef Array<struct Ensure> Ensures;

typedef Array<struct Designator> Designators;

typedef Array<struct DesigInit> DesigInits;
