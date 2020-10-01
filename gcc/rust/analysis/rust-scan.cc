#include "rust-scan.h"
#include "rust-diagnostics.h"

namespace Rust {
namespace Analysis {

TopLevelScan::TopLevelScan (AST::Crate &crate) : crate (crate)
{
  for (auto &item : crate.items)
    item->accept_vis (*this);
}

TopLevelScan::~TopLevelScan () {}

AST::Function *
TopLevelScan::lookupFunction (AST::Expr *expr)
{
  auto before = fnLookup.size ();
  expr->accept_vis (*this);
  if (fnLookup.size () > before)
    {
      AST::Function *fndecl = fnLookup.back ();
      fnLookup.pop_back ();
      return fndecl;
    }
  return NULL;
}

void
TopLevelScan::visit (AST::Token &tok)
{}

void
TopLevelScan::visit (AST::DelimTokenTree &delim_tok_tree)
{}

void
TopLevelScan::visit (AST::AttrInputMetaItemContainer &input)
{}

void
TopLevelScan::visit (AST::IdentifierExpr &ident_expr)
{}

void
TopLevelScan::visit (AST::Lifetime &lifetime)
{}

void
TopLevelScan::visit (AST::LifetimeParam &lifetime_param)
{}

void
TopLevelScan::visit (AST::MacroInvocationSemi &macro)
{}

// rust-path.h
void
TopLevelScan::visit (AST::PathInExpression &path)
{
  auto it = functions.find (path.as_string ());
  bool foundFndecl = it != functions.end ();
  if (foundFndecl)
    {
      fnLookup.push_back (it->second);
      return;
    }
}

void
TopLevelScan::visit (AST::TypePathSegment &segment)
{}
void
TopLevelScan::visit (AST::TypePathSegmentGeneric &segment)
{}

void
TopLevelScan::visit (AST::TypePathSegmentFunction &segment)
{}

void
TopLevelScan::visit (AST::TypePath &path)
{}

void
TopLevelScan::visit (AST::QualifiedPathInExpression &path)
{}

void
TopLevelScan::visit (AST::QualifiedPathInType &path)
{}

// rust-expr.h
void
TopLevelScan::visit (AST::LiteralExpr &expr)
{}

void
TopLevelScan::visit (AST::AttrInputLiteral &attr_input)
{}

void
TopLevelScan::visit (AST::MetaItemLitExpr &meta_item)
{}

void
TopLevelScan::visit (AST::MetaItemPathLit &meta_item)
{}

void
TopLevelScan::visit (AST::BorrowExpr &expr)
{}
void
TopLevelScan::visit (AST::DereferenceExpr &expr)
{}
void
TopLevelScan::visit (AST::ErrorPropagationExpr &expr)
{}
void
TopLevelScan::visit (AST::NegationExpr &expr)
{}

void
TopLevelScan::visit (AST::ArithmeticOrLogicalExpr &expr)
{}

void
TopLevelScan::visit (AST::ComparisonExpr &expr)
{}

void
TopLevelScan::visit (AST::LazyBooleanExpr &expr)
{}

void
TopLevelScan::visit (AST::TypeCastExpr &expr)
{}

void
TopLevelScan::visit (AST::AssignmentExpr &expr)
{}

void
TopLevelScan::visit (AST::CompoundAssignmentExpr &expr)
{}

void
TopLevelScan::visit (AST::GroupedExpr &expr)
{}
// void TopLevelScan::visit(ArrayElems& elems) {}
void
TopLevelScan::visit (AST::ArrayElemsValues &elems)
{}
void
TopLevelScan::visit (AST::ArrayElemsCopied &elems)
{}
void
TopLevelScan::visit (AST::ArrayExpr &expr)
{}
void
TopLevelScan::visit (AST::ArrayIndexExpr &expr)
{}
void
TopLevelScan::visit (AST::TupleExpr &expr)
{}
void
TopLevelScan::visit (AST::TupleIndexExpr &expr)
{}
void
TopLevelScan::visit (AST::StructExprStruct &expr)
{}
// void TopLevelScan::visit(StructExprField& field) {}
void
TopLevelScan::visit (AST::StructExprFieldIdentifier &field)
{}
void
TopLevelScan::visit (AST::StructExprFieldIdentifierValue &field)
{}
void
TopLevelScan::visit (AST::StructExprFieldIndexValue &field)
{}
void
TopLevelScan::visit (AST::StructExprStructFields &expr)
{}
void
TopLevelScan::visit (AST::StructExprStructBase &expr)
{}
void
TopLevelScan::visit (AST::StructExprTuple &expr)
{}
void
TopLevelScan::visit (AST::StructExprUnit &expr)
{}
// void TopLevelScan::visit(EnumExprField& field) {}
void
TopLevelScan::visit (AST::EnumExprFieldIdentifier &field)
{}
void
TopLevelScan::visit (AST::EnumExprFieldIdentifierValue &field)
{}
void
TopLevelScan::visit (AST::EnumExprFieldIndexValue &field)
{}
void
TopLevelScan::visit (AST::EnumExprStruct &expr)
{}
void
TopLevelScan::visit (AST::EnumExprTuple &expr)
{}
void
TopLevelScan::visit (AST::EnumExprFieldless &expr)
{}

void
TopLevelScan::visit (AST::CallExpr &expr)
{}

void
TopLevelScan::visit (AST::MethodCallExpr &expr)
{}
void
TopLevelScan::visit (AST::FieldAccessExpr &expr)
{}
void
TopLevelScan::visit (AST::ClosureExprInner &expr)
{}
void
TopLevelScan::visit (AST::BlockExpr &expr)
{}
void
TopLevelScan::visit (AST::ClosureExprInnerTyped &expr)
{}
void
TopLevelScan::visit (AST::ContinueExpr &expr)
{}
void
TopLevelScan::visit (AST::BreakExpr &expr)
{}
void
TopLevelScan::visit (AST::RangeFromToExpr &expr)
{}
void
TopLevelScan::visit (AST::RangeFromExpr &expr)
{}
void
TopLevelScan::visit (AST::RangeToExpr &expr)
{}
void
TopLevelScan::visit (AST::RangeFullExpr &expr)
{}
void
TopLevelScan::visit (AST::RangeFromToInclExpr &expr)
{}
void
TopLevelScan::visit (AST::RangeToInclExpr &expr)
{}
void
TopLevelScan::visit (AST::ReturnExpr &expr)
{}
void
TopLevelScan::visit (AST::UnsafeBlockExpr &expr)
{}
void
TopLevelScan::visit (AST::LoopExpr &expr)
{}
void
TopLevelScan::visit (AST::WhileLoopExpr &expr)
{}
void
TopLevelScan::visit (AST::WhileLetLoopExpr &expr)
{}
void
TopLevelScan::visit (AST::ForLoopExpr &expr)
{}
void
TopLevelScan::visit (AST::IfExpr &expr)
{}
void
TopLevelScan::visit (AST::IfExprConseqElse &expr)
{}
void
TopLevelScan::visit (AST::IfExprConseqIf &expr)
{}
void
TopLevelScan::visit (AST::IfExprConseqIfLet &expr)
{}
void
TopLevelScan::visit (AST::IfLetExpr &expr)
{}
void
TopLevelScan::visit (AST::IfLetExprConseqElse &expr)
{}
void
TopLevelScan::visit (AST::IfLetExprConseqIf &expr)
{}
void
TopLevelScan::visit (AST::IfLetExprConseqIfLet &expr)
{}
// void TopLevelScan::visit(MatchCase& match_case) {}
/*void
TopLevelScan::visit (AST::MatchCaseBlockExpr &match_case)
{}*/
/*void
TopLevelScan::visit (AST::MatchCaseExpr &match_case)
{}*/
void
TopLevelScan::visit (AST::MatchExpr &expr)
{}
void
TopLevelScan::visit (AST::AwaitExpr &expr)
{}
void
TopLevelScan::visit (AST::AsyncBlockExpr &expr)
{}

// rust-item.h
void
TopLevelScan::visit (AST::TypeParam &param)
{}
// void TopLevelScan::visit(WhereClauseItem& item) {}
void
TopLevelScan::visit (AST::LifetimeWhereClauseItem &item)
{}
void
TopLevelScan::visit (AST::TypeBoundWhereClauseItem &item)
{}
void
TopLevelScan::visit (AST::Method &method)
{}
void
TopLevelScan::visit (AST::ModuleBodied &module)
{}
void
TopLevelScan::visit (AST::ModuleNoBody &module)
{}
void
TopLevelScan::visit (AST::ExternCrate &crate)
{}
// void TopLevelScan::visit(UseTree& use_tree) {}
void
TopLevelScan::visit (AST::UseTreeGlob &use_tree)
{}
void
TopLevelScan::visit (AST::UseTreeList &use_tree)
{}
void
TopLevelScan::visit (AST::UseTreeRebind &use_tree)
{}
void
TopLevelScan::visit (AST::UseDeclaration &use_decl)
{}

void
TopLevelScan::visit (AST::Function &function)
{
  functions[function.function_name] = &function;
}

void
TopLevelScan::visit (AST::TypeAlias &type_alias)
{}
void
TopLevelScan::visit (AST::StructStruct &struct_item)
{}
void
TopLevelScan::visit (AST::TupleStruct &tuple_struct)
{}
void
TopLevelScan::visit (AST::EnumItem &item)
{}
void
TopLevelScan::visit (AST::EnumItemTuple &item)
{}
void
TopLevelScan::visit (AST::EnumItemStruct &item)
{}
void
TopLevelScan::visit (AST::EnumItemDiscriminant &item)
{}
void
TopLevelScan::visit (AST::Enum &enum_item)
{}
void
TopLevelScan::visit (AST::Union &union_item)
{}

void
TopLevelScan::visit (AST::ConstantItem &const_item)
{}

void
TopLevelScan::visit (AST::StaticItem &static_item)
{}
void
TopLevelScan::visit (AST::TraitItemFunc &item)
{}
void
TopLevelScan::visit (AST::TraitItemMethod &item)
{}
void
TopLevelScan::visit (AST::TraitItemConst &item)
{}
void
TopLevelScan::visit (AST::TraitItemType &item)
{}
void
TopLevelScan::visit (AST::Trait &trait)
{}
void
TopLevelScan::visit (AST::InherentImpl &impl)
{}
void
TopLevelScan::visit (AST::TraitImpl &impl)
{}
// void TopLevelScan::visit(ExternalItem& item) {}
void
TopLevelScan::visit (AST::ExternalStaticItem &item)
{}
void
TopLevelScan::visit (AST::ExternalFunctionItem &item)
{}
void
TopLevelScan::visit (AST::ExternBlock &block)
{}

// rust-macro.h
void
TopLevelScan::visit (AST::MacroMatchFragment &match)
{}
void
TopLevelScan::visit (AST::MacroMatchRepetition &match)
{}
void
TopLevelScan::visit (AST::MacroMatcher &matcher)
{}
void
TopLevelScan::visit (AST::MacroRulesDefinition &rules_def)
{}
void
TopLevelScan::visit (AST::MacroInvocation &macro_invoc)
{}
void
TopLevelScan::visit (AST::MetaItemPath &meta_item)
{}
void
TopLevelScan::visit (AST::MetaItemSeq &meta_item)
{}
void
TopLevelScan::visit (AST::MetaWord &meta_item)
{}
void
TopLevelScan::visit (AST::MetaNameValueStr &meta_item)
{}
void
TopLevelScan::visit (AST::MetaListPaths &meta_item)
{}
void
TopLevelScan::visit (AST::MetaListNameValueStr &meta_item)
{}

// rust-pattern.h
void
TopLevelScan::visit (AST::LiteralPattern &pattern)
{}

void
TopLevelScan::visit (AST::IdentifierPattern &pattern)
{}

void
TopLevelScan::visit (AST::WildcardPattern &pattern)
{}
// void TopLevelScan::visit(RangePatternBound& bound) {}
void
TopLevelScan::visit (AST::RangePatternBoundLiteral &bound)
{}
void
TopLevelScan::visit (AST::RangePatternBoundPath &bound)
{}
void
TopLevelScan::visit (AST::RangePatternBoundQualPath &bound)
{}
void
TopLevelScan::visit (AST::RangePattern &pattern)
{}
void
TopLevelScan::visit (AST::ReferencePattern &pattern)
{}
// void TopLevelScan::visit(StructPatternField& field) {}
void
TopLevelScan::visit (AST::StructPatternFieldTuplePat &field)
{}
void
TopLevelScan::visit (AST::StructPatternFieldIdentPat &field)
{}
void
TopLevelScan::visit (AST::StructPatternFieldIdent &field)
{}
void
TopLevelScan::visit (AST::StructPattern &pattern)
{}
// void TopLevelScan::visit(TupleStructItems& tuple_items) {}
void
TopLevelScan::visit (AST::TupleStructItemsNoRange &tuple_items)
{}
void
TopLevelScan::visit (AST::TupleStructItemsRange &tuple_items)
{}
void
TopLevelScan::visit (AST::TupleStructPattern &pattern)
{}
// void TopLevelScan::visit(TuplePatternItems& tuple_items) {}
void
TopLevelScan::visit (AST::TuplePatternItemsMultiple &tuple_items)
{}
void
TopLevelScan::visit (AST::TuplePatternItemsRanged &tuple_items)
{}
void
TopLevelScan::visit (AST::TuplePattern &pattern)
{}
void
TopLevelScan::visit (AST::GroupedPattern &pattern)
{}
void
TopLevelScan::visit (AST::SlicePattern &pattern)
{}

// rust-stmt.h
void
TopLevelScan::visit (AST::EmptyStmt &stmt)
{}

void
TopLevelScan::visit (AST::LetStmt &stmt)
{}

void
TopLevelScan::visit (AST::ExprStmtWithoutBlock &stmt)
{}

void
TopLevelScan::visit (AST::ExprStmtWithBlock &stmt)
{}

// rust-type.h
void
TopLevelScan::visit (AST::TraitBound &bound)
{}

void
TopLevelScan::visit (AST::ImplTraitType &type)
{}

void
TopLevelScan::visit (AST::TraitObjectType &type)
{}
void
TopLevelScan::visit (AST::ParenthesisedType &type)
{}
void
TopLevelScan::visit (AST::ImplTraitTypeOneBound &type)
{}
void
TopLevelScan::visit (AST::TraitObjectTypeOneBound &type)
{}
void
TopLevelScan::visit (AST::TupleType &type)
{}
void
TopLevelScan::visit (AST::NeverType &type)
{}
void
TopLevelScan::visit (AST::RawPointerType &type)
{}
void
TopLevelScan::visit (AST::ReferenceType &type)
{}
void
TopLevelScan::visit (AST::ArrayType &type)
{}
void
TopLevelScan::visit (AST::SliceType &type)
{}
void
TopLevelScan::visit (AST::InferredType &type)
{}
void
TopLevelScan::visit (AST::BareFunctionType &type)
{}

} // namespace Analysis
} // namespace Rust
