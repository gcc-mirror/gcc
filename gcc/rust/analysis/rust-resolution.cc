#include "rust-resolution.h"
#include "rust-diagnostics.h"

namespace Rust {
namespace Analysis {

TypeResolution::TypeResolution (AST::Crate &crate) : scope (), crate (crate)
{
  // push all builtin types
  // base is parse_path_ident_segment based up on segments
  /*  scope.Insert ("u8",
	      new AST::MaybeNamedParam (Identifier ("u8"),
					AST::MaybeNamedParam::IDENTIFIER,
					NULL, Location ()));*/
}

TypeResolution::~TypeResolution () {}

bool
TypeResolution::ResolveNamesAndTypes (AST::Crate &crate)
{
  TypeResolution resolver (crate);
  return resolver.go ();
}

bool
TypeResolution::go ()
{
  scope.Push ();
  for (auto &item : crate.items)
    {
      item->accept_vis (*this);
    }
  scope.Pop ();
  return true;
}

void
TypeResolution::visit (AST::Token &tok)
{}

void
TypeResolution::visit (AST::DelimTokenTree &delim_tok_tree)
{}

void
TypeResolution::visit (AST::AttrInputMetaItemContainer &input)
{}

void
TypeResolution::visit (AST::IdentifierExpr &ident_expr)
{
  printf ("IdentifierExpr %s\n", ident_expr.as_string ().c_str ());
}

void
TypeResolution::visit (AST::Lifetime &lifetime)
{}

void
TypeResolution::visit (AST::LifetimeParam &lifetime_param)
{}

void
TypeResolution::visit (AST::MacroInvocationSemi &macro)
{}

// rust-path.h
void
TypeResolution::visit (AST::PathInExpression &path)
{}
void
TypeResolution::visit (AST::TypePathSegment &segment)
{}
void
TypeResolution::visit (AST::TypePathSegmentGeneric &segment)
{}
void
TypeResolution::visit (AST::TypePathSegmentFunction &segment)
{}
void
TypeResolution::visit (AST::TypePath &path)
{}
void
TypeResolution::visit (AST::QualifiedPathInExpression &path)
{}
void
TypeResolution::visit (AST::QualifiedPathInType &path)
{}

// rust-expr.h
void
TypeResolution::visit (AST::LiteralExpr &expr)
{
  printf ("LiteralExpr: %s\n", expr.as_string ().c_str ());
  // figure out what this type is and push it onto the
}

void
TypeResolution::visit (AST::AttrInputLiteral &attr_input)
{}
void
TypeResolution::visit (AST::MetaItemLitExpr &meta_item)
{}
void
TypeResolution::visit (AST::MetaItemPathLit &meta_item)
{}
void
TypeResolution::visit (AST::BorrowExpr &expr)
{}
void
TypeResolution::visit (AST::DereferenceExpr &expr)
{}
void
TypeResolution::visit (AST::ErrorPropagationExpr &expr)
{}
void
TypeResolution::visit (AST::NegationExpr &expr)
{}
void
TypeResolution::visit (AST::ArithmeticOrLogicalExpr &expr)
{}
void
TypeResolution::visit (AST::ComparisonExpr &expr)
{}
void
TypeResolution::visit (AST::LazyBooleanExpr &expr)
{}
void
TypeResolution::visit (AST::TypeCastExpr &expr)
{}
void
TypeResolution::visit (AST::AssignmentExpr &expr)
{}
void
TypeResolution::visit (AST::CompoundAssignmentExpr &expr)
{}
void
TypeResolution::visit (AST::GroupedExpr &expr)
{}
// void TypeResolution::visit(ArrayElems& elems) {}
void
TypeResolution::visit (AST::ArrayElemsValues &elems)
{}
void
TypeResolution::visit (AST::ArrayElemsCopied &elems)
{}
void
TypeResolution::visit (AST::ArrayExpr &expr)
{}
void
TypeResolution::visit (AST::ArrayIndexExpr &expr)
{}
void
TypeResolution::visit (AST::TupleExpr &expr)
{}
void
TypeResolution::visit (AST::TupleIndexExpr &expr)
{}
void
TypeResolution::visit (AST::StructExprStruct &expr)
{}
// void TypeResolution::visit(StructExprField& field) {}
void
TypeResolution::visit (AST::StructExprFieldIdentifier &field)
{}
void
TypeResolution::visit (AST::StructExprFieldIdentifierValue &field)
{}
void
TypeResolution::visit (AST::StructExprFieldIndexValue &field)
{}
void
TypeResolution::visit (AST::StructExprStructFields &expr)
{}
void
TypeResolution::visit (AST::StructExprStructBase &expr)
{}
void
TypeResolution::visit (AST::StructExprTuple &expr)
{}
void
TypeResolution::visit (AST::StructExprUnit &expr)
{}
// void TypeResolution::visit(EnumExprField& field) {}
void
TypeResolution::visit (AST::EnumExprFieldIdentifier &field)
{}
void
TypeResolution::visit (AST::EnumExprFieldIdentifierValue &field)
{}
void
TypeResolution::visit (AST::EnumExprFieldIndexValue &field)
{}
void
TypeResolution::visit (AST::EnumExprStruct &expr)
{}
void
TypeResolution::visit (AST::EnumExprTuple &expr)
{}
void
TypeResolution::visit (AST::EnumExprFieldless &expr)
{}
void
TypeResolution::visit (AST::CallExpr &expr)
{}
void
TypeResolution::visit (AST::MethodCallExpr &expr)
{}
void
TypeResolution::visit (AST::FieldAccessExpr &expr)
{}
void
TypeResolution::visit (AST::ClosureExprInner &expr)
{}
void
TypeResolution::visit (AST::BlockExpr &expr)
{}
void
TypeResolution::visit (AST::ClosureExprInnerTyped &expr)
{}
void
TypeResolution::visit (AST::ContinueExpr &expr)
{}
void
TypeResolution::visit (AST::BreakExpr &expr)
{}
void
TypeResolution::visit (AST::RangeFromToExpr &expr)
{}
void
TypeResolution::visit (AST::RangeFromExpr &expr)
{}
void
TypeResolution::visit (AST::RangeToExpr &expr)
{}
void
TypeResolution::visit (AST::RangeFullExpr &expr)
{}
void
TypeResolution::visit (AST::RangeFromToInclExpr &expr)
{}
void
TypeResolution::visit (AST::RangeToInclExpr &expr)
{}
void
TypeResolution::visit (AST::ReturnExpr &expr)
{}
void
TypeResolution::visit (AST::UnsafeBlockExpr &expr)
{}
void
TypeResolution::visit (AST::LoopExpr &expr)
{}
void
TypeResolution::visit (AST::WhileLoopExpr &expr)
{}
void
TypeResolution::visit (AST::WhileLetLoopExpr &expr)
{}
void
TypeResolution::visit (AST::ForLoopExpr &expr)
{}
void
TypeResolution::visit (AST::IfExpr &expr)
{}
void
TypeResolution::visit (AST::IfExprConseqElse &expr)
{}
void
TypeResolution::visit (AST::IfExprConseqIf &expr)
{}
void
TypeResolution::visit (AST::IfExprConseqIfLet &expr)
{}
void
TypeResolution::visit (AST::IfLetExpr &expr)
{}
void
TypeResolution::visit (AST::IfLetExprConseqElse &expr)
{}
void
TypeResolution::visit (AST::IfLetExprConseqIf &expr)
{}
void
TypeResolution::visit (AST::IfLetExprConseqIfLet &expr)
{}
// void TypeResolution::visit(MatchCase& match_case) {}
void
TypeResolution::visit (AST::MatchCaseBlockExpr &match_case)
{}
void
TypeResolution::visit (AST::MatchCaseExpr &match_case)
{}
void
TypeResolution::visit (AST::MatchExpr &expr)
{}
void
TypeResolution::visit (AST::AwaitExpr &expr)
{}
void
TypeResolution::visit (AST::AsyncBlockExpr &expr)
{}

// rust-item.h
void
TypeResolution::visit (AST::TypeParam &param)
{}
// void TypeResolution::visit(WhereClauseItem& item) {}
void
TypeResolution::visit (AST::LifetimeWhereClauseItem &item)
{}
void
TypeResolution::visit (AST::TypeBoundWhereClauseItem &item)
{}
void
TypeResolution::visit (AST::Method &method)
{}
void
TypeResolution::visit (AST::ModuleBodied &module)
{}
void
TypeResolution::visit (AST::ModuleNoBody &module)
{}
void
TypeResolution::visit (AST::ExternCrate &crate)
{}
// void TypeResolution::visit(UseTree& use_tree) {}
void
TypeResolution::visit (AST::UseTreeGlob &use_tree)
{}
void
TypeResolution::visit (AST::UseTreeList &use_tree)
{}
void
TypeResolution::visit (AST::UseTreeRebind &use_tree)
{}
void
TypeResolution::visit (AST::UseDeclaration &use_decl)
{}

void
TypeResolution::visit (AST::Function &function)
{
  scope.Insert (function.function_name, function.return_type.get ());

  scope.Push ();
  printf ("INSIDE FUNCTION: %s\n", function.function_name.c_str ());

  for (auto &param : function.function_params)
    {
      printf ("FUNC PARAM: %s\n", param.as_string ().c_str ());
    }

  // ensure return types
  // TODO

  // walk the expression body
  for (auto &stmt : function.function_body->statements)
    {
      stmt->accept_vis (*this);
    }

  scope.Pop ();
}

void
TypeResolution::visit (AST::TypeAlias &type_alias)
{}
void
TypeResolution::visit (AST::StructStruct &struct_item)
{}
void
TypeResolution::visit (AST::TupleStruct &tuple_struct)
{}
void
TypeResolution::visit (AST::EnumItem &item)
{}
void
TypeResolution::visit (AST::EnumItemTuple &item)
{}
void
TypeResolution::visit (AST::EnumItemStruct &item)
{}
void
TypeResolution::visit (AST::EnumItemDiscriminant &item)
{}
void
TypeResolution::visit (AST::Enum &enum_item)
{}
void
TypeResolution::visit (AST::Union &union_item)
{}
void
TypeResolution::visit (AST::ConstantItem &const_item)
{}
void
TypeResolution::visit (AST::StaticItem &static_item)
{}
void
TypeResolution::visit (AST::TraitItemFunc &item)
{}
void
TypeResolution::visit (AST::TraitItemMethod &item)
{}
void
TypeResolution::visit (AST::TraitItemConst &item)
{}
void
TypeResolution::visit (AST::TraitItemType &item)
{}
void
TypeResolution::visit (AST::Trait &trait)
{}
void
TypeResolution::visit (AST::InherentImpl &impl)
{}
void
TypeResolution::visit (AST::TraitImpl &impl)
{}
// void TypeResolution::visit(ExternalItem& item) {}
void
TypeResolution::visit (AST::ExternalStaticItem &item)
{}
void
TypeResolution::visit (AST::ExternalFunctionItem &item)
{}
void
TypeResolution::visit (AST::ExternBlock &block)
{}

// rust-macro.h
void
TypeResolution::visit (AST::MacroMatchFragment &match)
{}
void
TypeResolution::visit (AST::MacroMatchRepetition &match)
{}
void
TypeResolution::visit (AST::MacroMatcher &matcher)
{}
void
TypeResolution::visit (AST::MacroRulesDefinition &rules_def)
{}
void
TypeResolution::visit (AST::MacroInvocation &macro_invoc)
{}
void
TypeResolution::visit (AST::MetaItemPath &meta_item)
{}
void
TypeResolution::visit (AST::MetaItemSeq &meta_item)
{}
void
TypeResolution::visit (AST::MetaWord &meta_item)
{}
void
TypeResolution::visit (AST::MetaNameValueStr &meta_item)
{}
void
TypeResolution::visit (AST::MetaListPaths &meta_item)
{}
void
TypeResolution::visit (AST::MetaListNameValueStr &meta_item)
{}

// rust-pattern.h
void
TypeResolution::visit (AST::LiteralPattern &pattern)
{
  printf ("LiteralPattern: %s\n", pattern.as_string ().c_str ());
}

void
TypeResolution::visit (AST::IdentifierPattern &pattern)
{
  printf ("IdentifierPattern: %s\n", pattern.as_string ().c_str ());
  letPatternBuffer.push_back (pattern);
}

void
TypeResolution::visit (AST::WildcardPattern &pattern)
{}
// void TypeResolution::visit(RangePatternBound& bound) {}
void
TypeResolution::visit (AST::RangePatternBoundLiteral &bound)
{}
void
TypeResolution::visit (AST::RangePatternBoundPath &bound)
{}
void
TypeResolution::visit (AST::RangePatternBoundQualPath &bound)
{}
void
TypeResolution::visit (AST::RangePattern &pattern)
{}
void
TypeResolution::visit (AST::ReferencePattern &pattern)
{}
// void TypeResolution::visit(StructPatternField& field) {}
void
TypeResolution::visit (AST::StructPatternFieldTuplePat &field)
{}
void
TypeResolution::visit (AST::StructPatternFieldIdentPat &field)
{}
void
TypeResolution::visit (AST::StructPatternFieldIdent &field)
{}
void
TypeResolution::visit (AST::StructPattern &pattern)
{}
// void TypeResolution::visit(TupleStructItems& tuple_items) {}
void
TypeResolution::visit (AST::TupleStructItemsNoRange &tuple_items)
{}
void
TypeResolution::visit (AST::TupleStructItemsRange &tuple_items)
{}
void
TypeResolution::visit (AST::TupleStructPattern &pattern)
{}
// void TypeResolution::visit(TuplePatternItems& tuple_items) {}
void
TypeResolution::visit (AST::TuplePatternItemsMultiple &tuple_items)
{}
void
TypeResolution::visit (AST::TuplePatternItemsRanged &tuple_items)
{}
void
TypeResolution::visit (AST::TuplePattern &pattern)
{}
void
TypeResolution::visit (AST::GroupedPattern &pattern)
{}
void
TypeResolution::visit (AST::SlicePattern &pattern)
{}

// rust-stmt.h
void
TypeResolution::visit (AST::EmptyStmt &stmt)
{}

void
TypeResolution::visit (AST::LetStmt &stmt)
{
  AST::Type *inferedType = NULL;
  if (stmt.has_type ())
    {
      inferedType = stmt.type.get ();
    }
  else if (stmt.has_init_expr ())
    {
      stmt.init_expr->accept_vis (*this);

      if (typeBuffer.empty ())
	{
	  rust_error_at (
	    stmt.init_expr->get_locus_slow (),
	    "unable to determine type for declaration from init expr");
	  return;
	}

      inferedType = typeBuffer.back ();
      typeBuffer.pop_back ();
    }
  else
    {
      rust_error_at (stmt.locus, "unable to determine type for declaration");
      return;
    }

  // TODO check we know what the type is

  // get all the names part of this declaration and add the types to the scope
  stmt.variables_pattern->accept_vis (*this);
  for (auto it = letPatternBuffer.begin (); it != letPatternBuffer.end (); it++)
    {
      scope.Insert (it->variable_ident, inferedType);
    }
  letPatternBuffer.clear ();
}

void
TypeResolution::visit (AST::ExprStmtWithoutBlock &stmt)
{
  printf ("ExprStmtWithoutBlock: %s\n", stmt.as_string ().c_str ());
  stmt.expr->accept_vis (*this);
}

void
TypeResolution::visit (AST::ExprStmtWithBlock &stmt)
{
  printf ("ExprStmtWithBlock: %s\n", stmt.as_string ().c_str ());
  stmt.expr->accept_vis (*this);
}

// rust-type.h
void
TypeResolution::visit (AST::TraitBound &bound)
{}

void
TypeResolution::visit (AST::ImplTraitType &type)
{}

void
TypeResolution::visit (AST::TraitObjectType &type)
{}
void
TypeResolution::visit (AST::ParenthesisedType &type)
{}
void
TypeResolution::visit (AST::ImplTraitTypeOneBound &type)
{}
void
TypeResolution::visit (AST::TraitObjectTypeOneBound &type)
{}
void
TypeResolution::visit (AST::TupleType &type)
{}
void
TypeResolution::visit (AST::NeverType &type)
{}
void
TypeResolution::visit (AST::RawPointerType &type)
{}
void
TypeResolution::visit (AST::ReferenceType &type)
{}
void
TypeResolution::visit (AST::ArrayType &type)
{}
void
TypeResolution::visit (AST::SliceType &type)
{}
void
TypeResolution::visit (AST::InferredType &type)
{}
void
TypeResolution::visit (AST::BareFunctionType &type)
{}

} // namespace Analysis
} // namespace Rust
