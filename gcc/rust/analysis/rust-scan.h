#pragma once

#include "rust-system.h"
#include "rust-ast-full.h"
#include "rust-ast-visitor.h"
#include "scope.h"

namespace Rust {
namespace Analysis {

class TopLevelScan : public AST::ASTVisitor
{
public:
  TopLevelScan (AST::Crate &crate);

  ~TopLevelScan ();

  AST::Function *lookupFunction (AST::Expr *expr);

  // visitor impl
  // rust-ast.h
  // virtual void visit(AttrInput& attr_input);
  // virtual void visit(TokenTree& token_tree);
  // virtual void visit(MacroMatch& macro_match);
  virtual void visit (AST::Token &tok);
  virtual void visit (AST::DelimTokenTree &delim_tok_tree);
  virtual void visit (AST::AttrInputMetaItemContainer &input);
  // virtual void visit(MetaItem& meta_item);
  // virtual void vsit(Stmt& stmt);
  // virtual void visit(Expr& expr);
  virtual void visit (AST::IdentifierExpr &ident_expr);
  // virtual void visit(Pattern& pattern);
  // virtual void visit(Type& type);
  // virtual void visit(TypeParamBound& type_param_bound);
  virtual void visit (AST::Lifetime &lifetime);
  // virtual void visit(GenericParam& generic_param);
  virtual void visit (AST::LifetimeParam &lifetime_param);
  // virtual void visit(TraitItem& trait_item);
  // virtual void visit(InherentImplItem& inherent_impl_item);
  // virtual void visit(TraitImplItem& trait_impl_item);
  virtual void visit (AST::MacroInvocationSemi &macro);

  // rust-path.h
  virtual void visit (AST::PathInExpression &path);
  virtual void visit (AST::TypePathSegment &segment);
  virtual void visit (AST::TypePathSegmentGeneric &segment);
  virtual void visit (AST::TypePathSegmentFunction &segment);
  virtual void visit (AST::TypePath &path);
  virtual void visit (AST::QualifiedPathInExpression &path);
  virtual void visit (AST::QualifiedPathInType &path);

  // rust-expr.h
  virtual void visit (AST::LiteralExpr &expr);
  virtual void visit (AST::AttrInputLiteral &attr_input);
  virtual void visit (AST::MetaItemLitExpr &meta_item);
  virtual void visit (AST::MetaItemPathLit &meta_item);
  virtual void visit (AST::BorrowExpr &expr);
  virtual void visit (AST::DereferenceExpr &expr);
  virtual void visit (AST::ErrorPropagationExpr &expr);
  virtual void visit (AST::NegationExpr &expr);
  virtual void visit (AST::ArithmeticOrLogicalExpr &expr);
  virtual void visit (AST::ComparisonExpr &expr);
  virtual void visit (AST::LazyBooleanExpr &expr);
  virtual void visit (AST::TypeCastExpr &expr);
  virtual void visit (AST::AssignmentExpr &expr);
  virtual void visit (AST::CompoundAssignmentExpr &expr);
  virtual void visit (AST::GroupedExpr &expr);
  // virtual void visit(ArrayElems& elems);
  virtual void visit (AST::ArrayElemsValues &elems);
  virtual void visit (AST::ArrayElemsCopied &elems);
  virtual void visit (AST::ArrayExpr &expr);
  virtual void visit (AST::ArrayIndexExpr &expr);
  virtual void visit (AST::TupleExpr &expr);
  virtual void visit (AST::TupleIndexExpr &expr);
  virtual void visit (AST::StructExprStruct &expr);
  // virtual void visit(StructExprField& field);
  virtual void visit (AST::StructExprFieldIdentifier &field);
  virtual void visit (AST::StructExprFieldIdentifierValue &field);
  virtual void visit (AST::StructExprFieldIndexValue &field);
  virtual void visit (AST::StructExprStructFields &expr);
  virtual void visit (AST::StructExprStructBase &expr);
  virtual void visit (AST::StructExprTuple &expr);
  virtual void visit (AST::StructExprUnit &expr);
  // virtual void visit(EnumExprField& field);
  virtual void visit (AST::EnumExprFieldIdentifier &field);
  virtual void visit (AST::EnumExprFieldIdentifierValue &field);
  virtual void visit (AST::EnumExprFieldIndexValue &field);
  virtual void visit (AST::EnumExprStruct &expr);
  virtual void visit (AST::EnumExprTuple &expr);
  virtual void visit (AST::EnumExprFieldless &expr);
  virtual void visit (AST::CallExpr &expr);
  virtual void visit (AST::MethodCallExpr &expr);
  virtual void visit (AST::FieldAccessExpr &expr);
  virtual void visit (AST::ClosureExprInner &expr);
  virtual void visit (AST::BlockExpr &expr);
  virtual void visit (AST::ClosureExprInnerTyped &expr);
  virtual void visit (AST::ContinueExpr &expr);
  virtual void visit (AST::BreakExpr &expr);
  virtual void visit (AST::RangeFromToExpr &expr);
  virtual void visit (AST::RangeFromExpr &expr);
  virtual void visit (AST::RangeToExpr &expr);
  virtual void visit (AST::RangeFullExpr &expr);
  virtual void visit (AST::RangeFromToInclExpr &expr);
  virtual void visit (AST::RangeToInclExpr &expr);
  virtual void visit (AST::ReturnExpr &expr);
  virtual void visit (AST::UnsafeBlockExpr &expr);
  virtual void visit (AST::LoopExpr &expr);
  virtual void visit (AST::WhileLoopExpr &expr);
  virtual void visit (AST::WhileLetLoopExpr &expr);
  virtual void visit (AST::ForLoopExpr &expr);
  virtual void visit (AST::IfExpr &expr);
  virtual void visit (AST::IfExprConseqElse &expr);
  virtual void visit (AST::IfExprConseqIf &expr);
  virtual void visit (AST::IfExprConseqIfLet &expr);
  virtual void visit (AST::IfLetExpr &expr);
  virtual void visit (AST::IfLetExprConseqElse &expr);
  virtual void visit (AST::IfLetExprConseqIf &expr);
  virtual void visit (AST::IfLetExprConseqIfLet &expr);
  // virtual void visit(MatchCase& match_case);
  // virtual void visit (AST::MatchCaseBlockExpr &match_case);
  // virtual void visit (AST::MatchCaseExpr &match_case);
  virtual void visit (AST::MatchExpr &expr);
  virtual void visit (AST::AwaitExpr &expr);
  virtual void visit (AST::AsyncBlockExpr &expr);

  // rust-item.h
  virtual void visit (AST::TypeParam &param);
  // virtual void visit(WhereClauseItem& item);
  virtual void visit (AST::LifetimeWhereClauseItem &item);
  virtual void visit (AST::TypeBoundWhereClauseItem &item);
  virtual void visit (AST::Method &method);
  virtual void visit (AST::ModuleBodied &module);
  virtual void visit (AST::ModuleNoBody &module);
  virtual void visit (AST::ExternCrate &crate);
  // virtual void visit(UseTree& use_tree);
  virtual void visit (AST::UseTreeGlob &use_tree);
  virtual void visit (AST::UseTreeList &use_tree);
  virtual void visit (AST::UseTreeRebind &use_tree);
  virtual void visit (AST::UseDeclaration &use_decl);
  virtual void visit (AST::Function &function);
  virtual void visit (AST::TypeAlias &type_alias);
  virtual void visit (AST::StructStruct &struct_item);
  virtual void visit (AST::TupleStruct &tuple_struct);
  virtual void visit (AST::EnumItem &item);
  virtual void visit (AST::EnumItemTuple &item);
  virtual void visit (AST::EnumItemStruct &item);
  virtual void visit (AST::EnumItemDiscriminant &item);
  virtual void visit (AST::Enum &enum_item);
  virtual void visit (AST::Union &union_item);
  virtual void visit (AST::ConstantItem &const_item);
  virtual void visit (AST::StaticItem &static_item);
  virtual void visit (AST::TraitItemFunc &item);
  virtual void visit (AST::TraitItemMethod &item);
  virtual void visit (AST::TraitItemConst &item);
  virtual void visit (AST::TraitItemType &item);
  virtual void visit (AST::Trait &trait);
  virtual void visit (AST::InherentImpl &impl);
  virtual void visit (AST::TraitImpl &impl);
  // virtual void visit(ExternalItem& item);
  virtual void visit (AST::ExternalStaticItem &item);
  virtual void visit (AST::ExternalFunctionItem &item);
  virtual void visit (AST::ExternBlock &block);

  // rust-macro.h
  virtual void visit (AST::MacroMatchFragment &match);
  virtual void visit (AST::MacroMatchRepetition &match);
  virtual void visit (AST::MacroMatcher &matcher);
  virtual void visit (AST::MacroRulesDefinition &rules_def);
  virtual void visit (AST::MacroInvocation &macro_invoc);
  virtual void visit (AST::MetaItemPath &meta_item);
  virtual void visit (AST::MetaItemSeq &meta_item);
  virtual void visit (AST::MetaWord &meta_item);
  virtual void visit (AST::MetaNameValueStr &meta_item);
  virtual void visit (AST::MetaListPaths &meta_item);
  virtual void visit (AST::MetaListNameValueStr &meta_item);

  // rust-pattern.h
  virtual void visit (AST::LiteralPattern &pattern);
  virtual void visit (AST::IdentifierPattern &pattern);
  virtual void visit (AST::WildcardPattern &pattern);
  // virtual void visit(RangePatternBound& bound);
  virtual void visit (AST::RangePatternBoundLiteral &bound);
  virtual void visit (AST::RangePatternBoundPath &bound);
  virtual void visit (AST::RangePatternBoundQualPath &bound);
  virtual void visit (AST::RangePattern &pattern);
  virtual void visit (AST::ReferencePattern &pattern);
  // virtual void visit(StructPatternField& field);
  virtual void visit (AST::StructPatternFieldTuplePat &field);
  virtual void visit (AST::StructPatternFieldIdentPat &field);
  virtual void visit (AST::StructPatternFieldIdent &field);
  virtual void visit (AST::StructPattern &pattern);
  // virtual void visit(TupleStructItems& tuple_items);
  virtual void visit (AST::TupleStructItemsNoRange &tuple_items);
  virtual void visit (AST::TupleStructItemsRange &tuple_items);
  virtual void visit (AST::TupleStructPattern &pattern);
  // virtual void visit(TuplePatternItems& tuple_items);
  virtual void visit (AST::TuplePatternItemsMultiple &tuple_items);
  virtual void visit (AST::TuplePatternItemsRanged &tuple_items);
  virtual void visit (AST::TuplePattern &pattern);
  virtual void visit (AST::GroupedPattern &pattern);
  virtual void visit (AST::SlicePattern &pattern);

  // rust-stmt.h
  virtual void visit (AST::EmptyStmt &stmt);
  virtual void visit (AST::LetStmt &stmt);
  virtual void visit (AST::ExprStmtWithoutBlock &stmt);
  virtual void visit (AST::ExprStmtWithBlock &stmt);

  // rust-type.h
  virtual void visit (AST::TraitBound &bound);
  virtual void visit (AST::ImplTraitType &type);
  virtual void visit (AST::TraitObjectType &type);
  virtual void visit (AST::ParenthesisedType &type);
  virtual void visit (AST::ImplTraitTypeOneBound &type);
  virtual void visit (AST::TraitObjectTypeOneBound &type);
  virtual void visit (AST::TupleType &type);
  virtual void visit (AST::NeverType &type);
  virtual void visit (AST::RawPointerType &type);
  virtual void visit (AST::ReferenceType &type);
  virtual void visit (AST::ArrayType &type);
  virtual void visit (AST::SliceType &type);
  virtual void visit (AST::InferredType &type);
  virtual void visit (AST::BareFunctionType &type);

private:
  std::map<std::string, AST::Function *> functions;
  AST::Crate &crate;

  std::vector<AST::Function *> fnLookup;
};

} // namespace Analysis
} // namespace Rust
