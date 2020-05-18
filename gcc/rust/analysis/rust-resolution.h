#pragma once

#include "rust-system.h"
#include "rust-ast-full.h"
#include "rust-ast-visitor.h"
#include "rust-scan.h"
#include "scope.h"

namespace Rust {
namespace Analysis {

class Resolution : public AST::ASTVisitor
{
public:
  ~Resolution (){};

  // visitor impl
  // rust-ast.h
  // virtual void visit(AttrInput& attr_input);
  // virtual void visit(TokenTree& token_tree);
  // virtual void visit(MacroMatch& macro_match);
  virtual void visit (AST::Token &tok) = 0;
  virtual void visit (AST::DelimTokenTree &delim_tok_tree) = 0;
  virtual void visit (AST::AttrInputMetaItemContainer &input) = 0;
  // virtual void visit(MetaItem& meta_item) = 0;
  // virtual void vsit(Stmt& stmt) = 0;
  // virtual void visit(Expr& expr) = 0;
  virtual void visit (AST::IdentifierExpr &ident_expr) = 0;
  // virtual void visit(Pattern& pattern) = 0;
  // virtual void visit(Type& type) = 0;
  // virtual void visit(TypeParamBound& type_param_bound) = 0;
  virtual void visit (AST::Lifetime &lifetime) = 0;
  // virtual void visit(GenericParam& generic_param) = 0;
  virtual void visit (AST::LifetimeParam &lifetime_param) = 0;
  // virtual void visit(TraitItem& trait_item) = 0;
  // virtual void visit(InherentImplItem& inherent_impl_item) = 0;
  // virtual void visit(TraitImplItem& trait_impl_item) = 0;
  virtual void visit (AST::MacroInvocationSemi &macro) = 0;

  // rust-path.h
  virtual void visit (AST::PathInExpression &path) = 0;
  virtual void visit (AST::TypePathSegment &segment) = 0;
  virtual void visit (AST::TypePathSegmentGeneric &segment) = 0;
  virtual void visit (AST::TypePathSegmentFunction &segment) = 0;
  virtual void visit (AST::TypePath &path) = 0;
  virtual void visit (AST::QualifiedPathInExpression &path) = 0;
  virtual void visit (AST::QualifiedPathInType &path) = 0;

  // rust-expr.h
  virtual void visit (AST::LiteralExpr &expr) = 0;
  virtual void visit (AST::AttrInputLiteral &attr_input) = 0;
  virtual void visit (AST::MetaItemLitExpr &meta_item) = 0;
  virtual void visit (AST::MetaItemPathLit &meta_item) = 0;
  virtual void visit (AST::BorrowExpr &expr) = 0;
  virtual void visit (AST::DereferenceExpr &expr) = 0;
  virtual void visit (AST::ErrorPropagationExpr &expr) = 0;
  virtual void visit (AST::NegationExpr &expr) = 0;
  virtual void visit (AST::ArithmeticOrLogicalExpr &expr) = 0;
  virtual void visit (AST::ComparisonExpr &expr) = 0;
  virtual void visit (AST::LazyBooleanExpr &expr) = 0;
  virtual void visit (AST::TypeCastExpr &expr) = 0;
  virtual void visit (AST::AssignmentExpr &expr) = 0;
  virtual void visit (AST::CompoundAssignmentExpr &expr) = 0;
  virtual void visit (AST::GroupedExpr &expr) = 0;
  // virtual void visit(ArrayElems& elems) = 0;
  virtual void visit (AST::ArrayElemsValues &elems) = 0;
  virtual void visit (AST::ArrayElemsCopied &elems) = 0;
  virtual void visit (AST::ArrayExpr &expr) = 0;
  virtual void visit (AST::ArrayIndexExpr &expr) = 0;
  virtual void visit (AST::TupleExpr &expr) = 0;
  virtual void visit (AST::TupleIndexExpr &expr) = 0;
  virtual void visit (AST::StructExprStruct &expr) = 0;
  // virtual void visit(StructExprField& field) = 0;
  virtual void visit (AST::StructExprFieldIdentifier &field) = 0;
  virtual void visit (AST::StructExprFieldIdentifierValue &field) = 0;
  virtual void visit (AST::StructExprFieldIndexValue &field) = 0;
  virtual void visit (AST::StructExprStructFields &expr) = 0;
  virtual void visit (AST::StructExprStructBase &expr) = 0;
  virtual void visit (AST::StructExprTuple &expr) = 0;
  virtual void visit (AST::StructExprUnit &expr) = 0;
  // virtual void visit(EnumExprField& field) = 0;
  virtual void visit (AST::EnumExprFieldIdentifier &field) = 0;
  virtual void visit (AST::EnumExprFieldIdentifierValue &field) = 0;
  virtual void visit (AST::EnumExprFieldIndexValue &field) = 0;
  virtual void visit (AST::EnumExprStruct &expr) = 0;
  virtual void visit (AST::EnumExprTuple &expr) = 0;
  virtual void visit (AST::EnumExprFieldless &expr) = 0;
  virtual void visit (AST::CallExpr &expr) = 0;
  virtual void visit (AST::MethodCallExpr &expr) = 0;
  virtual void visit (AST::FieldAccessExpr &expr) = 0;
  virtual void visit (AST::ClosureExprInner &expr) = 0;
  virtual void visit (AST::BlockExpr &expr) = 0;
  virtual void visit (AST::ClosureExprInnerTyped &expr) = 0;
  virtual void visit (AST::ContinueExpr &expr) = 0;
  virtual void visit (AST::BreakExpr &expr) = 0;
  virtual void visit (AST::RangeFromToExpr &expr) = 0;
  virtual void visit (AST::RangeFromExpr &expr) = 0;
  virtual void visit (AST::RangeToExpr &expr) = 0;
  virtual void visit (AST::RangeFullExpr &expr) = 0;
  virtual void visit (AST::RangeFromToInclExpr &expr) = 0;
  virtual void visit (AST::RangeToInclExpr &expr) = 0;
  virtual void visit (AST::ReturnExpr &expr) = 0;
  virtual void visit (AST::UnsafeBlockExpr &expr) = 0;
  virtual void visit (AST::LoopExpr &expr) = 0;
  virtual void visit (AST::WhileLoopExpr &expr) = 0;
  virtual void visit (AST::WhileLetLoopExpr &expr) = 0;
  virtual void visit (AST::ForLoopExpr &expr) = 0;
  virtual void visit (AST::IfExpr &expr) = 0;
  virtual void visit (AST::IfExprConseqElse &expr) = 0;
  virtual void visit (AST::IfExprConseqIf &expr) = 0;
  virtual void visit (AST::IfExprConseqIfLet &expr) = 0;
  virtual void visit (AST::IfLetExpr &expr) = 0;
  virtual void visit (AST::IfLetExprConseqElse &expr) = 0;
  virtual void visit (AST::IfLetExprConseqIf &expr) = 0;
  virtual void visit (AST::IfLetExprConseqIfLet &expr) = 0;
  // virtual void visit(MatchCase& match_case) = 0;
  virtual void visit (AST::MatchCaseBlockExpr &match_case) = 0;
  virtual void visit (AST::MatchCaseExpr &match_case) = 0;
  virtual void visit (AST::MatchExpr &expr) = 0;
  virtual void visit (AST::AwaitExpr &expr) = 0;
  virtual void visit (AST::AsyncBlockExpr &expr) = 0;

  // rust-item.h
  virtual void visit (AST::TypeParam &param) = 0;
  // virtual void visit(WhereClauseItem& item) = 0;
  virtual void visit (AST::LifetimeWhereClauseItem &item) = 0;
  virtual void visit (AST::TypeBoundWhereClauseItem &item) = 0;
  virtual void visit (AST::Method &method) = 0;
  virtual void visit (AST::ModuleBodied &module) = 0;
  virtual void visit (AST::ModuleNoBody &module) = 0;
  virtual void visit (AST::ExternCrate &crate) = 0;
  // virtual void visit(UseTree& use_tree) = 0;
  virtual void visit (AST::UseTreeGlob &use_tree) = 0;
  virtual void visit (AST::UseTreeList &use_tree) = 0;
  virtual void visit (AST::UseTreeRebind &use_tree) = 0;
  virtual void visit (AST::UseDeclaration &use_decl) = 0;
  virtual void visit (AST::Function &function) = 0;
  virtual void visit (AST::TypeAlias &type_alias) = 0;
  virtual void visit (AST::StructStruct &struct_item) = 0;
  virtual void visit (AST::TupleStruct &tuple_struct) = 0;
  virtual void visit (AST::EnumItem &item) = 0;
  virtual void visit (AST::EnumItemTuple &item) = 0;
  virtual void visit (AST::EnumItemStruct &item) = 0;
  virtual void visit (AST::EnumItemDiscriminant &item) = 0;
  virtual void visit (AST::Enum &enum_item) = 0;
  virtual void visit (AST::Union &union_item) = 0;
  virtual void visit (AST::ConstantItem &const_item) = 0;
  virtual void visit (AST::StaticItem &static_item) = 0;
  virtual void visit (AST::TraitItemFunc &item) = 0;
  virtual void visit (AST::TraitItemMethod &item) = 0;
  virtual void visit (AST::TraitItemConst &item) = 0;
  virtual void visit (AST::TraitItemType &item) = 0;
  virtual void visit (AST::Trait &trait) = 0;
  virtual void visit (AST::InherentImpl &impl) = 0;
  virtual void visit (AST::TraitImpl &impl) = 0;
  // virtual void visit(ExternalItem& item) = 0;
  virtual void visit (AST::ExternalStaticItem &item) = 0;
  virtual void visit (AST::ExternalFunctionItem &item) = 0;
  virtual void visit (AST::ExternBlock &block) = 0;

  // rust-macro.h
  virtual void visit (AST::MacroMatchFragment &match) = 0;
  virtual void visit (AST::MacroMatchRepetition &match) = 0;
  virtual void visit (AST::MacroMatcher &matcher) = 0;
  virtual void visit (AST::MacroRulesDefinition &rules_def) = 0;
  virtual void visit (AST::MacroInvocation &macro_invoc) = 0;
  virtual void visit (AST::MetaItemPath &meta_item) = 0;
  virtual void visit (AST::MetaItemSeq &meta_item) = 0;
  virtual void visit (AST::MetaWord &meta_item) = 0;
  virtual void visit (AST::MetaNameValueStr &meta_item) = 0;
  virtual void visit (AST::MetaListPaths &meta_item) = 0;
  virtual void visit (AST::MetaListNameValueStr &meta_item) = 0;

  // rust-pattern.h
  virtual void visit (AST::LiteralPattern &pattern) = 0;
  virtual void visit (AST::IdentifierPattern &pattern) = 0;
  virtual void visit (AST::WildcardPattern &pattern) = 0;
  // virtual void visit(RangePatternBound& bound) = 0;
  virtual void visit (AST::RangePatternBoundLiteral &bound) = 0;
  virtual void visit (AST::RangePatternBoundPath &bound) = 0;
  virtual void visit (AST::RangePatternBoundQualPath &bound) = 0;
  virtual void visit (AST::RangePattern &pattern) = 0;
  virtual void visit (AST::ReferencePattern &pattern) = 0;
  // virtual void visit(StructPatternField& field) = 0;
  virtual void visit (AST::StructPatternFieldTuplePat &field) = 0;
  virtual void visit (AST::StructPatternFieldIdentPat &field) = 0;
  virtual void visit (AST::StructPatternFieldIdent &field) = 0;
  virtual void visit (AST::StructPattern &pattern) = 0;
  // virtual void visit(TupleStructItems& tuple_items) = 0;
  virtual void visit (AST::TupleStructItemsNoRange &tuple_items) = 0;
  virtual void visit (AST::TupleStructItemsRange &tuple_items) = 0;
  virtual void visit (AST::TupleStructPattern &pattern) = 0;
  // virtual void visit(TuplePatternItems& tuple_items) = 0;
  virtual void visit (AST::TuplePatternItemsMultiple &tuple_items) = 0;
  virtual void visit (AST::TuplePatternItemsRanged &tuple_items) = 0;
  virtual void visit (AST::TuplePattern &pattern) = 0;
  virtual void visit (AST::GroupedPattern &pattern) = 0;
  virtual void visit (AST::SlicePattern &pattern) = 0;

  // rust-stmt.h
  virtual void visit (AST::EmptyStmt &stmt) = 0;
  virtual void visit (AST::LetStmt &stmt) = 0;
  virtual void visit (AST::ExprStmtWithoutBlock &stmt) = 0;
  virtual void visit (AST::ExprStmtWithBlock &stmt) = 0;

  // rust-type.h
  virtual void visit (AST::TraitBound &bound) = 0;
  virtual void visit (AST::ImplTraitType &type) = 0;
  virtual void visit (AST::TraitObjectType &type) = 0;
  virtual void visit (AST::ParenthesisedType &type) = 0;
  virtual void visit (AST::ImplTraitTypeOneBound &type) = 0;
  virtual void visit (AST::TraitObjectTypeOneBound &type) = 0;
  virtual void visit (AST::TupleType &type) = 0;
  virtual void visit (AST::NeverType &type) = 0;
  virtual void visit (AST::RawPointerType &type) = 0;
  virtual void visit (AST::ReferenceType &type) = 0;
  virtual void visit (AST::ArrayType &type) = 0;
  virtual void visit (AST::SliceType &type) = 0;
  virtual void visit (AST::InferredType &type) = 0;
  virtual void visit (AST::BareFunctionType &type) = 0;

private:
  virtual bool go () = 0;

protected:
  Resolution (AST::Crate &crate, TopLevelScan &toplevel)
    : crate (crate), toplevel (toplevel)
  {
    typeScope.Push ();
    scope.Push ();
  };

  Scope<AST::Type *> scope;
  Scope<AST::Type *> typeScope;
  AST::Crate &crate;
  TopLevelScan &toplevel;

  std::vector<AST::IdentifierPattern> letPatternBuffer;
  std::vector<AST::Type *> typeBuffer;
  std::vector<std::string> typeComparisonBuffer;
  std::vector<AST::Function *> functionLookup;
};

} // namespace Analysis
} // namespace Rust
