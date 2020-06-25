
/* Compiler implementation of the D programming language
 * Copyright (C) 2013-2020 by The D Language Foundation, All Rights Reserved
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/visitor.h
 */

#pragma once

#include "root/dsystem.h"

class Statement;
class ErrorStatement;
class PeelStatement;
class ExpStatement;
class DtorExpStatement;
class CompileStatement;
class CompoundStatement;
class CompoundDeclarationStatement;
class UnrolledLoopStatement;
class ScopeStatement;
class ForwardingStatement;
class WhileStatement;
class DoStatement;
class ForStatement;
class ForeachStatement;
class ForeachRangeStatement;
class StaticForeachStatement;
class IfStatement;
class ConditionalStatement;
class PragmaStatement;
class StaticAssertStatement;
class SwitchStatement;
class CaseStatement;
class CaseRangeStatement;
class DefaultStatement;
class GotoDefaultStatement;
class GotoCaseStatement;
class SwitchErrorStatement;
class ReturnStatement;
class BreakStatement;
class ContinueStatement;
class SynchronizedStatement;
class WithStatement;
class TryCatchStatement;
class TryFinallyStatement;
class ScopeGuardStatement;
class ThrowStatement;
class DebugStatement;
class GotoStatement;
class LabelStatement;
class AsmStatement;
class InlineAsmStatement;
class GccAsmStatement;
class CompoundAsmStatement;
class ImportStatement;

class Type;
class TypeError;
class TypeNext;
class TypeBasic;
class TypeVector;
class TypeArray;
class TypeSArray;
class TypeDArray;
class TypeAArray;
class TypePointer;
class TypeReference;
class TypeFunction;
class TypeDelegate;
class TypeQualified;
class TypeIdentifier;
class TypeInstance;
class TypeTypeof;
class TypeReturn;
class TypeStruct;
class TypeEnum;
class TypeClass;
class TypeTuple;
class TypeSlice;
class TypeNull;
class TypeTraits;

class Dsymbol;

class StaticAssert;
class DebugSymbol;
class VersionSymbol;
class EnumMember;
class Import;
class OverloadSet;
class LabelDsymbol;
class AliasThis;

class AttribDeclaration;
class StorageClassDeclaration;
class DeprecatedDeclaration;
class LinkDeclaration;
class CPPMangleDeclaration;
class ProtDeclaration;
class AlignDeclaration;
class AnonDeclaration;
class PragmaDeclaration;
class ConditionalDeclaration;
class StaticIfDeclaration;
class CompileDeclaration;
class StaticForeachDeclaration;
class UserAttributeDeclaration;
class ForwardingAttribDeclaration;

class ScopeDsymbol;
class TemplateDeclaration;
class TemplateInstance;
class TemplateMixin;
class EnumDeclaration;
class Package;
class Module;
class WithScopeSymbol;
class ArrayScopeSymbol;
class Nspace;

class AggregateDeclaration;
class StructDeclaration;
class UnionDeclaration;
class ClassDeclaration;
class InterfaceDeclaration;

class Declaration;
class TupleDeclaration;
class AliasDeclaration;
class OverDeclaration;
class VarDeclaration;
class SymbolDeclaration;
class ThisDeclaration;

class TypeInfoDeclaration;
class TypeInfoStructDeclaration;
class TypeInfoClassDeclaration;
class TypeInfoInterfaceDeclaration;
class TypeInfoPointerDeclaration;
class TypeInfoArrayDeclaration;
class TypeInfoStaticArrayDeclaration;
class TypeInfoAssociativeArrayDeclaration;
class TypeInfoEnumDeclaration;
class TypeInfoFunctionDeclaration;
class TypeInfoDelegateDeclaration;
class TypeInfoTupleDeclaration;
class TypeInfoConstDeclaration;
class TypeInfoInvariantDeclaration;
class TypeInfoSharedDeclaration;
class TypeInfoWildDeclaration;
class TypeInfoVectorDeclaration;

class FuncDeclaration;
class FuncAliasDeclaration;
class FuncLiteralDeclaration;
class CtorDeclaration;
class PostBlitDeclaration;
class DtorDeclaration;
class StaticCtorDeclaration;
class SharedStaticCtorDeclaration;
class StaticDtorDeclaration;
class SharedStaticDtorDeclaration;
class InvariantDeclaration;
class UnitTestDeclaration;
class NewDeclaration;
class DeleteDeclaration;

class Initializer;
class VoidInitializer;
class ErrorInitializer;
class StructInitializer;
class ArrayInitializer;
class ExpInitializer;

class Expression;
class IntegerExp;
class ErrorExp;
class RealExp;
class ComplexExp;
class IdentifierExp;
class DollarExp;
class DsymbolExp;
class ThisExp;
class SuperExp;
class NullExp;
class StringExp;
class TupleExp;
class ArrayLiteralExp;
class AssocArrayLiteralExp;
class StructLiteralExp;
class TypeExp;
class ScopeExp;
class TemplateExp;
class NewExp;
class NewAnonClassExp;
class SymbolExp;
class SymOffExp;
class VarExp;
class OverExp;
class FuncExp;
class DeclarationExp;
class TypeidExp;
class TraitsExp;
class HaltExp;
class IsExp;
class UnaExp;
class BinExp;
class BinAssignExp;
class CompileExp;
class ImportExp;
class AssertExp;
class DotIdExp;
class DotTemplateExp;
class DotVarExp;
class DotTemplateInstanceExp;
class DelegateExp;
class DotTypeExp;
class CallExp;
class AddrExp;
class PtrExp;
class NegExp;
class UAddExp;
class ComExp;
class NotExp;
class DeleteExp;
class CastExp;
class VectorExp;
class VectorArrayExp;
class SliceExp;
class ArrayLengthExp;
class IntervalExp;
class DelegatePtrExp;
class DelegateFuncptrExp;
class ArrayExp;
class DotExp;
class CommaExp;
class IndexExp;
class PostExp;
class PreExp;
class AssignExp;
class ConstructExp;
class BlitExp;
class AddAssignExp;
class MinAssignExp;
class MulAssignExp;
class DivAssignExp;
class ModAssignExp;
class AndAssignExp;
class OrAssignExp;
class XorAssignExp;
class PowAssignExp;
class ShlAssignExp;
class ShrAssignExp;
class UshrAssignExp;
class CatAssignExp;
class AddExp;
class MinExp;
class CatExp;
class MulExp;
class DivExp;
class ModExp;
class PowExp;
class ShlExp;
class ShrExp;
class UshrExp;
class AndExp;
class OrExp;
class XorExp;
class LogicalExp;
class CmpExp;
class InExp;
class RemoveExp;
class EqualExp;
class IdentityExp;
class CondExp;
class DefaultInitExp;
class FileInitExp;
class LineInitExp;
class ModuleInitExp;
class FuncInitExp;
class PrettyFuncInitExp;
class ClassReferenceExp;
class VoidInitExp;
class ThrownExceptionExp;

class TemplateParameter;
class TemplateTypeParameter;
class TemplateThisParameter;
class TemplateValueParameter;
class TemplateAliasParameter;
class TemplateTupleParameter;

class Condition;
class DVCondition;
class DebugCondition;
class VersionCondition;
class StaticIfCondition;

class Parameter;

class Visitor
{
public:
    virtual void visit(Statement *) { assert(0); }
    virtual void visit(ErrorStatement *s) { visit((Statement *)s); }
    virtual void visit(PeelStatement *s) { visit((Statement *)s); }
    virtual void visit(ExpStatement *s) { visit((Statement *)s); }
    virtual void visit(DtorExpStatement *s) { visit((ExpStatement *)s); }
    virtual void visit(CompileStatement *s) { visit((Statement *)s); }
    virtual void visit(CompoundStatement *s) { visit((Statement *)s); }
    virtual void visit(CompoundDeclarationStatement *s) { visit((CompoundStatement *)s); }
    virtual void visit(UnrolledLoopStatement *s) { visit((Statement *)s); }
    virtual void visit(ScopeStatement *s) { visit((Statement *)s); }
    virtual void visit(ForwardingStatement *s) { visit((Statement *)s); }
    virtual void visit(WhileStatement *s) { visit((Statement *)s); }
    virtual void visit(DoStatement *s) { visit((Statement *)s); }
    virtual void visit(ForStatement *s) { visit((Statement *)s); }
    virtual void visit(ForeachStatement *s) { visit((Statement *)s); }
    virtual void visit(ForeachRangeStatement *s) { visit((Statement *)s); }
    virtual void visit(StaticForeachStatement *s) { visit((Statement *)s); }
    virtual void visit(IfStatement *s) { visit((Statement *)s); }
    virtual void visit(ConditionalStatement *s) { visit((Statement *)s); }
    virtual void visit(PragmaStatement *s) { visit((Statement *)s); }
    virtual void visit(StaticAssertStatement *s) { visit((Statement *)s); }
    virtual void visit(SwitchStatement *s) { visit((Statement *)s); }
    virtual void visit(CaseStatement *s) { visit((Statement *)s); }
    virtual void visit(CaseRangeStatement *s) { visit((Statement *)s); }
    virtual void visit(DefaultStatement *s) { visit((Statement *)s); }
    virtual void visit(GotoDefaultStatement *s) { visit((Statement *)s); }
    virtual void visit(GotoCaseStatement *s) { visit((Statement *)s); }
    virtual void visit(SwitchErrorStatement *s) { visit((Statement *)s); }
    virtual void visit(ReturnStatement *s) { visit((Statement *)s); }
    virtual void visit(BreakStatement *s) { visit((Statement *)s); }
    virtual void visit(ContinueStatement *s) { visit((Statement *)s); }
    virtual void visit(SynchronizedStatement *s) { visit((Statement *)s); }
    virtual void visit(WithStatement *s) { visit((Statement *)s); }
    virtual void visit(TryCatchStatement *s) { visit((Statement *)s); }
    virtual void visit(TryFinallyStatement *s) { visit((Statement *)s); }
    virtual void visit(ScopeGuardStatement *s) { visit((Statement *)s); }
    virtual void visit(ThrowStatement *s) { visit((Statement *)s); }
    virtual void visit(DebugStatement *s) { visit((Statement *)s); }
    virtual void visit(GotoStatement *s) { visit((Statement *)s); }
    virtual void visit(LabelStatement *s) { visit((Statement *)s); }
    virtual void visit(AsmStatement *s) { visit((Statement *)s); }
    virtual void visit(InlineAsmStatement *s) { visit((AsmStatement *)s); }
    virtual void visit(GccAsmStatement *s) { visit((AsmStatement *)s); }
    virtual void visit(CompoundAsmStatement *s) { visit((CompoundStatement *)s); }
    virtual void visit(ImportStatement *s) { visit((Statement *)s); }

    virtual void visit(Type *) { assert(0); }
    virtual void visit(TypeError *t) { visit((Type *)t); }
    virtual void visit(TypeNext *t) { visit((Type *)t); }
    virtual void visit(TypeBasic *t) { visit((Type *)t); }
    virtual void visit(TypeVector *t) { visit((Type *)t); }
    virtual void visit(TypeArray *t) { visit((TypeNext *)t); }
    virtual void visit(TypeSArray *t) { visit((TypeArray *)t); }
    virtual void visit(TypeDArray *t) { visit((TypeArray *)t); }
    virtual void visit(TypeAArray *t) { visit((TypeArray *)t); }
    virtual void visit(TypePointer *t) { visit((TypeNext *)t); }
    virtual void visit(TypeReference *t) { visit((TypeNext *)t); }
    virtual void visit(TypeFunction *t) { visit((TypeNext *)t); }
    virtual void visit(TypeDelegate *t) { visit((TypeNext *)t); }
    virtual void visit(TypeQualified *t) { visit((Type *)t); }
    virtual void visit(TypeIdentifier *t) { visit((TypeQualified *)t); }
    virtual void visit(TypeInstance *t) { visit((TypeQualified *)t); }
    virtual void visit(TypeTypeof *t) { visit((TypeQualified *)t); }
    virtual void visit(TypeReturn *t) { visit((TypeQualified *)t); }
    virtual void visit(TypeStruct *t) { visit((Type *)t); }
    virtual void visit(TypeEnum *t) { visit((Type *)t); }
    virtual void visit(TypeClass *t) { visit((Type *)t); }
    virtual void visit(TypeTuple *t) { visit((Type *)t); }
    virtual void visit(TypeSlice *t) { visit((TypeNext *)t); }
    virtual void visit(TypeNull *t) { visit((Type *)t); }
    virtual void visit(TypeTraits *t) { visit((Type *)t); }

    virtual void visit(Dsymbol *) { assert(0); }

    virtual void visit(StaticAssert *s) { visit((Dsymbol *)s); }
    virtual void visit(DebugSymbol *s) { visit((Dsymbol *)s); }
    virtual void visit(VersionSymbol *s) { visit((Dsymbol *)s); }
    virtual void visit(EnumMember *s) { visit((VarDeclaration *)s); }
    virtual void visit(Import *s) { visit((Dsymbol *)s); }
    virtual void visit(OverloadSet *s) { visit((Dsymbol *)s); }
    virtual void visit(LabelDsymbol *s) { visit((Dsymbol *)s); }
    virtual void visit(AliasThis *s) { visit((Dsymbol *)s); }

    virtual void visit(AttribDeclaration *s) { visit((Dsymbol *)s); }
    virtual void visit(StorageClassDeclaration *s) { visit((AttribDeclaration *)s); }
    virtual void visit(DeprecatedDeclaration *s) { visit((StorageClassDeclaration *)s); }
    virtual void visit(LinkDeclaration *s) { visit((AttribDeclaration *)s); }
    virtual void visit(CPPMangleDeclaration *s) { visit((AttribDeclaration *)s); }
    virtual void visit(ProtDeclaration *s) { visit((AttribDeclaration *)s); }
    virtual void visit(AlignDeclaration *s) { visit((AttribDeclaration *)s); }
    virtual void visit(AnonDeclaration *s) { visit((AttribDeclaration *)s); }
    virtual void visit(PragmaDeclaration *s) { visit((AttribDeclaration *)s); }
    virtual void visit(ConditionalDeclaration *s) { visit((AttribDeclaration *)s); }
    virtual void visit(StaticIfDeclaration *s) { visit((ConditionalDeclaration *)s); }
    virtual void visit(StaticForeachDeclaration *s) { visit((AttribDeclaration *)s); }
    virtual void visit(CompileDeclaration *s) { visit((AttribDeclaration *)s); }
    virtual void visit(UserAttributeDeclaration *s) { visit((AttribDeclaration *)s); }
    virtual void visit(ForwardingAttribDeclaration *s) { visit((AttribDeclaration *)s); }

    virtual void visit(ScopeDsymbol *s) { visit((Dsymbol *)s); }
    virtual void visit(TemplateDeclaration *s) { visit((ScopeDsymbol *)s); }
    virtual void visit(TemplateInstance *s) { visit((ScopeDsymbol *)s); }
    virtual void visit(TemplateMixin *s) { visit((TemplateInstance *)s); }
    virtual void visit(EnumDeclaration *s) { visit((ScopeDsymbol *)s); }
    virtual void visit(Package *s) { visit((ScopeDsymbol *)s); }
    virtual void visit(Module *s) { visit((Package *)s); }
    virtual void visit(WithScopeSymbol *s) { visit((ScopeDsymbol *)s); }
    virtual void visit(ArrayScopeSymbol *s) { visit((ScopeDsymbol *)s); }
    virtual void visit(Nspace *s) { visit((ScopeDsymbol *)s); }

    virtual void visit(AggregateDeclaration *s) { visit((ScopeDsymbol *)s); }
    virtual void visit(StructDeclaration *s) { visit((AggregateDeclaration *)s); }
    virtual void visit(UnionDeclaration *s) { visit((StructDeclaration *)s); }
    virtual void visit(ClassDeclaration *s) { visit((AggregateDeclaration *)s); }
    virtual void visit(InterfaceDeclaration *s) { visit((ClassDeclaration *)s); }

    virtual void visit(Declaration *s) { visit((Dsymbol *)s); }
    virtual void visit(TupleDeclaration *s) { visit((Declaration *)s); }
    virtual void visit(AliasDeclaration *s) { visit((Declaration *)s); }
    virtual void visit(OverDeclaration *s) { visit((Declaration *)s); }
    virtual void visit(VarDeclaration *s) { visit((Declaration *)s); }
    virtual void visit(SymbolDeclaration *s) { visit((Declaration *)s); }
    virtual void visit(ThisDeclaration *s) { visit((VarDeclaration *)s); }

    virtual void visit(TypeInfoDeclaration *s) { visit((VarDeclaration *)s); }
    virtual void visit(TypeInfoStructDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoClassDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoInterfaceDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoPointerDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoArrayDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoStaticArrayDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoAssociativeArrayDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoEnumDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoFunctionDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoDelegateDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoTupleDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoConstDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoInvariantDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoSharedDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoWildDeclaration *s) { visit((TypeInfoDeclaration *)s); }
    virtual void visit(TypeInfoVectorDeclaration *s) { visit((TypeInfoDeclaration *)s); }

    virtual void visit(FuncDeclaration *s) { visit((Declaration *)s); }
    virtual void visit(FuncAliasDeclaration *s) { visit((FuncDeclaration *)s); }
    virtual void visit(FuncLiteralDeclaration *s) { visit((FuncDeclaration *)s); }
    virtual void visit(CtorDeclaration *s) { visit((FuncDeclaration *)s); }
    virtual void visit(PostBlitDeclaration *s) { visit((FuncDeclaration *)s); }
    virtual void visit(DtorDeclaration *s) { visit((FuncDeclaration *)s); }
    virtual void visit(StaticCtorDeclaration *s) { visit((FuncDeclaration *)s); }
    virtual void visit(SharedStaticCtorDeclaration *s) { visit((StaticCtorDeclaration *)s); }
    virtual void visit(StaticDtorDeclaration *s) { visit((FuncDeclaration *)s); }
    virtual void visit(SharedStaticDtorDeclaration *s) { visit((StaticDtorDeclaration *)s); }
    virtual void visit(InvariantDeclaration *s) { visit((FuncDeclaration *)s); }
    virtual void visit(UnitTestDeclaration *s) { visit((FuncDeclaration *)s); }
    virtual void visit(NewDeclaration *s) { visit((FuncDeclaration *)s); }
    virtual void visit(DeleteDeclaration *s) { visit((FuncDeclaration *)s); }

    virtual void visit(Initializer *) { assert(0); }
    virtual void visit(VoidInitializer *i) { visit((Initializer *)i); }
    virtual void visit(ErrorInitializer *i) { visit((Initializer *)i); }
    virtual void visit(StructInitializer *i) { visit((Initializer *)i); }
    virtual void visit(ArrayInitializer *i) { visit((Initializer *)i); }
    virtual void visit(ExpInitializer *i) { visit((Initializer *)i); }

    virtual void visit(Expression *) { assert(0); }
    virtual void visit(IntegerExp *e) { visit((Expression *)e); }
    virtual void visit(ErrorExp *e) { visit((Expression *)e); }
    virtual void visit(RealExp *e) { visit((Expression *)e); }
    virtual void visit(ComplexExp *e) { visit((Expression *)e); }
    virtual void visit(IdentifierExp *e) { visit((Expression *)e); }
    virtual void visit(DollarExp *e) { visit((IdentifierExp *)e); }
    virtual void visit(DsymbolExp *e) { visit((Expression *)e); }
    virtual void visit(ThisExp *e) { visit((Expression *)e); }
    virtual void visit(SuperExp *e) { visit((ThisExp *)e); }
    virtual void visit(NullExp *e) { visit((Expression *)e); }
    virtual void visit(StringExp *e) { visit((Expression *)e); }
    virtual void visit(TupleExp *e) { visit((Expression *)e); }
    virtual void visit(ArrayLiteralExp *e) { visit((Expression *)e); }
    virtual void visit(AssocArrayLiteralExp *e) { visit((Expression *)e); }
    virtual void visit(StructLiteralExp *e) { visit((Expression *)e); }
    virtual void visit(TypeExp *e) { visit((Expression *)e); }
    virtual void visit(ScopeExp *e) { visit((Expression *)e); }
    virtual void visit(TemplateExp *e) { visit((Expression *)e); }
    virtual void visit(NewExp *e) { visit((Expression *)e); }
    virtual void visit(NewAnonClassExp *e) { visit((Expression *)e); }
    virtual void visit(SymbolExp *e) { visit((Expression *)e); }
    virtual void visit(SymOffExp *e) { visit((SymbolExp *)e); }
    virtual void visit(VarExp *e) { visit((SymbolExp *)e); }
    virtual void visit(OverExp *e) { visit((Expression *)e); }
    virtual void visit(FuncExp *e) { visit((Expression *)e); }
    virtual void visit(DeclarationExp *e) { visit((Expression *)e); }
    virtual void visit(TypeidExp *e) { visit((Expression *)e); }
    virtual void visit(TraitsExp *e) { visit((Expression *)e); }
    virtual void visit(HaltExp *e) { visit((Expression *)e); }
    virtual void visit(IsExp *e) { visit((Expression *)e); }
    virtual void visit(UnaExp *e) { visit((Expression *)e); }
    virtual void visit(BinExp *e) { visit((Expression *)e); }
    virtual void visit(BinAssignExp *e) { visit((BinExp *)e); }
    virtual void visit(CompileExp *e) { visit((UnaExp *)e); }
    virtual void visit(ImportExp *e) { visit((UnaExp *)e); }
    virtual void visit(AssertExp *e) { visit((UnaExp *)e); }
    virtual void visit(DotIdExp *e) { visit((UnaExp *)e); }
    virtual void visit(DotTemplateExp *e) { visit((UnaExp *)e); }
    virtual void visit(DotVarExp *e) { visit((UnaExp *)e); }
    virtual void visit(DotTemplateInstanceExp *e) { visit((UnaExp *)e); }
    virtual void visit(DelegateExp *e) { visit((UnaExp *)e); }
    virtual void visit(DotTypeExp *e) { visit((UnaExp *)e); }
    virtual void visit(CallExp *e) { visit((UnaExp *)e); }
    virtual void visit(AddrExp *e) { visit((UnaExp *)e); }
    virtual void visit(PtrExp *e) { visit((UnaExp *)e); }
    virtual void visit(NegExp *e) { visit((UnaExp *)e); }
    virtual void visit(UAddExp *e) { visit((UnaExp *)e); }
    virtual void visit(ComExp *e) { visit((UnaExp *)e); }
    virtual void visit(NotExp *e) { visit((UnaExp *)e); }
    virtual void visit(DeleteExp *e) { visit((UnaExp *)e); }
    virtual void visit(CastExp *e) { visit((UnaExp *)e); }
    virtual void visit(VectorExp *e) { visit((UnaExp *)e); }
    virtual void visit(VectorArrayExp *e) { visit((UnaExp *)e); }
    virtual void visit(SliceExp *e) { visit((UnaExp *)e); }
    virtual void visit(ArrayLengthExp *e) { visit((UnaExp *)e); }
    virtual void visit(IntervalExp *e) { visit((Expression *)e); }
    virtual void visit(DelegatePtrExp *e) { visit((UnaExp *)e); }
    virtual void visit(DelegateFuncptrExp *e) { visit((UnaExp *)e); }
    virtual void visit(ArrayExp *e) { visit((UnaExp *)e); }
    virtual void visit(DotExp *e) { visit((BinExp *)e); }
    virtual void visit(CommaExp *e) { visit((BinExp *)e); }
    virtual void visit(IndexExp *e) { visit((BinExp *)e); }
    virtual void visit(PostExp *e) { visit((BinExp *)e); }
    virtual void visit(PreExp *e) { visit((UnaExp *)e); }
    virtual void visit(AssignExp *e) { visit((BinExp *)e); }
    virtual void visit(ConstructExp *e) { visit((AssignExp *)e); }
    virtual void visit(BlitExp *e) { visit((AssignExp *)e); }
    virtual void visit(AddAssignExp *e) { visit((BinAssignExp *)e); }
    virtual void visit(MinAssignExp *e) { visit((BinAssignExp *)e); }
    virtual void visit(MulAssignExp *e) { visit((BinAssignExp *)e); }
    virtual void visit(DivAssignExp *e) { visit((BinAssignExp *)e); }
    virtual void visit(ModAssignExp *e) { visit((BinAssignExp *)e); }
    virtual void visit(AndAssignExp *e) { visit((BinAssignExp *)e); }
    virtual void visit(OrAssignExp *e) { visit((BinAssignExp *)e); }
    virtual void visit(XorAssignExp *e) { visit((BinAssignExp *)e); }
    virtual void visit(PowAssignExp *e) { visit((BinAssignExp *)e); }
    virtual void visit(ShlAssignExp *e) { visit((BinAssignExp *)e); }
    virtual void visit(ShrAssignExp *e) { visit((BinAssignExp *)e); }
    virtual void visit(UshrAssignExp *e) { visit((BinAssignExp *)e); }
    virtual void visit(CatAssignExp *e) { visit((BinAssignExp *)e); }
    virtual void visit(AddExp *e) { visit((BinExp *)e); }
    virtual void visit(MinExp *e) { visit((BinExp *)e); }
    virtual void visit(CatExp *e) { visit((BinExp *)e); }
    virtual void visit(MulExp *e) { visit((BinExp *)e); }
    virtual void visit(DivExp *e) { visit((BinExp *)e); }
    virtual void visit(ModExp *e) { visit((BinExp *)e); }
    virtual void visit(PowExp *e) { visit((BinExp *)e); }
    virtual void visit(ShlExp *e) { visit((BinExp *)e); }
    virtual void visit(ShrExp *e) { visit((BinExp *)e); }
    virtual void visit(UshrExp *e) { visit((BinExp *)e); }
    virtual void visit(AndExp *e) { visit((BinExp *)e); }
    virtual void visit(OrExp *e) { visit((BinExp *)e); }
    virtual void visit(XorExp *e) { visit((BinExp *)e); }
    virtual void visit(LogicalExp *e) { visit((BinExp *)e); }
    virtual void visit(CmpExp *e) { visit((BinExp *)e); }
    virtual void visit(InExp *e) { visit((BinExp *)e); }
    virtual void visit(RemoveExp *e) { visit((BinExp *)e); }
    virtual void visit(EqualExp *e) { visit((BinExp *)e); }
    virtual void visit(IdentityExp *e) { visit((BinExp *)e); }
    virtual void visit(CondExp *e) { visit((BinExp *)e); }
    virtual void visit(DefaultInitExp *e) { visit((Expression *)e); }
    virtual void visit(FileInitExp *e) { visit((DefaultInitExp *)e); }
    virtual void visit(LineInitExp *e) { visit((DefaultInitExp *)e); }
    virtual void visit(ModuleInitExp *e) { visit((DefaultInitExp *)e); }
    virtual void visit(FuncInitExp *e) { visit((DefaultInitExp *)e); }
    virtual void visit(PrettyFuncInitExp *e) { visit((DefaultInitExp *)e); }
    virtual void visit(ClassReferenceExp *e) { visit((Expression *)e); }
    virtual void visit(VoidInitExp *e) { visit((Expression *)e); }
    virtual void visit(ThrownExceptionExp *e) { visit((Expression *)e); }

    virtual void visit(TemplateParameter *) { assert(0); }
    virtual void visit(TemplateTypeParameter *tp) { visit((TemplateParameter *)tp); }
    virtual void visit(TemplateThisParameter *tp) { visit((TemplateTypeParameter *)tp); }
    virtual void visit(TemplateValueParameter *tp) { visit((TemplateParameter *)tp); }
    virtual void visit(TemplateAliasParameter *tp) { visit((TemplateParameter *)tp); }
    virtual void visit(TemplateTupleParameter *tp) { visit((TemplateParameter *)tp); }

    virtual void visit(Condition *) { assert(0); }
    virtual void visit(DVCondition *c) { visit((Condition *)c); }
    virtual void visit(DebugCondition *c) { visit((DVCondition *)c); }
    virtual void visit(VersionCondition *c) { visit((DVCondition *)c); }
    virtual void visit(StaticIfCondition *c) { visit((Condition *)c); }

    virtual void visit(Parameter *) { assert(0); }
};

class StoppableVisitor : public Visitor
{
public:
    bool stop;
    StoppableVisitor() : stop(false) {}
};
