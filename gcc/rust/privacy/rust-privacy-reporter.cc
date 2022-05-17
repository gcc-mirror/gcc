#include "rust-privacy-reporter.h"
#include "rust-hir-expr.h"
#include "rust-hir-stmt.h"
#include "rust-hir-item.h"

namespace Rust {
namespace Privacy {

PrivacyReporter::PrivacyReporter (Analysis::Mappings &mappings,
				  Resolver::Resolver &resolver)
  : mappings (mappings), resolver (resolver),
    current_module (Optional<NodeId>::none ())
{}

void
PrivacyReporter::go (HIR::Crate &crate)
{
  for (auto &item : crate.items)
    item->accept_vis (*this);
}

static bool
is_child_module (Analysis::Mappings &mappings, NodeId parent,
		 NodeId possible_child)
{
  auto children = mappings.lookup_module_children (parent);

  if (!children)
    return false;

  // Visit all toplevel children
  for (auto &child : *children)
    if (child == possible_child)
      return true;

  // Now descend recursively in the child module tree
  for (auto &child : *children)
    if (is_child_module (mappings, child, possible_child))
      return true;

  return false;
}

// FIXME: This function needs a lot of refactoring
void
PrivacyReporter::check_for_privacy_violation (const NodeId &use_id,
					      const Location &locus)
{
  NodeId ref_node_id;

  // FIXME: Don't assert here - we might be dealing with a type
  rust_assert (resolver.lookup_resolved_name (use_id, &ref_node_id));

  ModuleVisibility vis;

  // FIXME: Can we really return here if the item has no visibility?
  if (!mappings.lookup_visibility (ref_node_id, vis))
    return;

  auto valid = true;

  switch (vis.get_kind ())
    {
    case ModuleVisibility::Public:
      break;
      case ModuleVisibility::Restricted: {
	// If we are in the crate, everything is restricted correctly, but we
	// can't get a module for it
	if (current_module.is_none ())
	  return;

	auto module = mappings.lookup_defid (vis.get_module_id ());
	rust_assert (module != nullptr);

	auto mod_node_id = module->get_mappings ().get_nodeid ();

	// We are in the module referenced by the pub(restricted) visibility.
	// This is valid
	if (mod_node_id == current_module.get ())
	  break;

	// FIXME: This needs a LOT of TLC: hinting about the definition, a
	// string to say if it's a module, function, type, etc...
	if (!is_child_module (mappings, mod_node_id, current_module.get ()))
	  valid = false;
      }
      break;
    case ModuleVisibility::Unknown:
      rust_unreachable ();
      break;
    }

  if (!valid)
    rust_error_at (locus, "definition is private in this context");
}

void
PrivacyReporter::visit (HIR::IdentifierExpr &ident_expr)
{}

void
PrivacyReporter::visit (HIR::Lifetime &lifetime)
{}

void
PrivacyReporter::visit (HIR::LifetimeParam &lifetime_param)
{}

void
PrivacyReporter::visit (HIR::PathInExpression &path)
{
  check_for_privacy_violation (path.get_mappings ().get_nodeid (),
			       path.get_locus ());
}

void
PrivacyReporter::visit (HIR::TypePathSegment &segment)
{}

void
PrivacyReporter::visit (HIR::TypePathSegmentGeneric &segment)
{}

void
PrivacyReporter::visit (HIR::TypePathSegmentFunction &segment)
{}

void
PrivacyReporter::visit (HIR::TypePath &path)
{}

void
PrivacyReporter::visit (HIR::QualifiedPathInExpression &path)
{}

void
PrivacyReporter::visit (HIR::QualifiedPathInType &path)
{}

void
PrivacyReporter::visit (HIR::LiteralExpr &expr)
{}

void
PrivacyReporter::visit (HIR::BorrowExpr &expr)
{}

void
PrivacyReporter::visit (HIR::DereferenceExpr &expr)
{}

void
PrivacyReporter::visit (HIR::ErrorPropagationExpr &expr)
{}

void
PrivacyReporter::visit (HIR::NegationExpr &expr)
{}

void
PrivacyReporter::visit (HIR::ArithmeticOrLogicalExpr &expr)
{}

void
PrivacyReporter::visit (HIR::ComparisonExpr &expr)
{}

void
PrivacyReporter::visit (HIR::LazyBooleanExpr &expr)
{}

void
PrivacyReporter::visit (HIR::TypeCastExpr &expr)
{}

void
PrivacyReporter::visit (HIR::AssignmentExpr &expr)
{}

void
PrivacyReporter::visit (HIR::CompoundAssignmentExpr &expr)
{}

void
PrivacyReporter::visit (HIR::GroupedExpr &expr)
{}

void
PrivacyReporter::visit (HIR::ArrayElemsValues &elems)
{}

void
PrivacyReporter::visit (HIR::ArrayElemsCopied &elems)
{}

void
PrivacyReporter::visit (HIR::ArrayExpr &expr)
{}

void
PrivacyReporter::visit (HIR::ArrayIndexExpr &expr)
{}

void
PrivacyReporter::visit (HIR::TupleExpr &expr)
{}

void
PrivacyReporter::visit (HIR::TupleIndexExpr &expr)
{}

void
PrivacyReporter::visit (HIR::StructExprStruct &expr)
{}

void
PrivacyReporter::visit (HIR::StructExprFieldIdentifier &field)
{}

void
PrivacyReporter::visit (HIR::StructExprFieldIdentifierValue &field)
{}

void
PrivacyReporter::visit (HIR::StructExprFieldIndexValue &field)
{}

void
PrivacyReporter::visit (HIR::StructExprStructFields &expr)
{}

void
PrivacyReporter::visit (HIR::StructExprStructBase &expr)
{}

void
PrivacyReporter::visit (HIR::CallExpr &expr)
{
  expr.get_fnexpr ()->accept_vis (*this);

  // rust_assert (mappings.lookup_visibility (definition_id, def_vis));
  // check_for_privacy_violation (def_vis, expr.get_locus ());
}

void
PrivacyReporter::visit (HIR::MethodCallExpr &expr)
{}

void
PrivacyReporter::visit (HIR::FieldAccessExpr &expr)
{}

void
PrivacyReporter::visit (HIR::ClosureExprInner &expr)
{}

void
PrivacyReporter::visit (HIR::BlockExpr &expr)
{
  for (auto &stmt : expr.get_statements ())
    stmt->accept_vis (*this);

  auto &last_expr = expr.get_final_expr ();
  if (last_expr)
    last_expr->accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::ClosureExprInnerTyped &expr)
{}

void
PrivacyReporter::visit (HIR::ContinueExpr &expr)
{}

void
PrivacyReporter::visit (HIR::BreakExpr &expr)
{}

void
PrivacyReporter::visit (HIR::RangeFromToExpr &expr)
{}

void
PrivacyReporter::visit (HIR::RangeFromExpr &expr)
{}

void
PrivacyReporter::visit (HIR::RangeToExpr &expr)
{}

void
PrivacyReporter::visit (HIR::RangeFullExpr &expr)
{}

void
PrivacyReporter::visit (HIR::RangeFromToInclExpr &expr)
{}

void
PrivacyReporter::visit (HIR::RangeToInclExpr &expr)
{}

void
PrivacyReporter::visit (HIR::ReturnExpr &expr)
{}

void
PrivacyReporter::visit (HIR::UnsafeBlockExpr &expr)
{}

void
PrivacyReporter::visit (HIR::LoopExpr &expr)
{}

void
PrivacyReporter::visit (HIR::WhileLoopExpr &expr)
{}

void
PrivacyReporter::visit (HIR::WhileLetLoopExpr &expr)
{}

void
PrivacyReporter::visit (HIR::ForLoopExpr &expr)
{}

void
PrivacyReporter::visit (HIR::IfExpr &expr)
{}

void
PrivacyReporter::visit (HIR::IfExprConseqElse &expr)
{}

void
PrivacyReporter::visit (HIR::IfExprConseqIf &expr)
{}

void
PrivacyReporter::visit (HIR::IfExprConseqIfLet &expr)
{}

void
PrivacyReporter::visit (HIR::IfLetExpr &expr)
{}

void
PrivacyReporter::visit (HIR::IfLetExprConseqElse &expr)
{}

void
PrivacyReporter::visit (HIR::IfLetExprConseqIf &expr)
{}

void
PrivacyReporter::visit (HIR::IfLetExprConseqIfLet &expr)
{}

void
PrivacyReporter::visit (HIR::MatchExpr &expr)
{}

void
PrivacyReporter::visit (HIR::AwaitExpr &expr)
{}

void
PrivacyReporter::visit (HIR::AsyncBlockExpr &expr)
{}

void
PrivacyReporter::visit (HIR::TypeParam &param)
{}

void
PrivacyReporter::visit (HIR::LifetimeWhereClauseItem &item)
{}

void
PrivacyReporter::visit (HIR::TypeBoundWhereClauseItem &item)
{}

void
PrivacyReporter::visit (HIR::Module &module)
{
  auto old_module = current_module;
  current_module
    = Optional<NodeId>::some (module.get_mappings ().get_nodeid ());

  for (auto &item : module.get_items ())
    item->accept_vis (*this);

  current_module = old_module;
}

void
PrivacyReporter::visit (HIR::ExternCrate &crate)
{}

void
PrivacyReporter::visit (HIR::UseTreeGlob &use_tree)
{}

void
PrivacyReporter::visit (HIR::UseTreeList &use_tree)
{}

void
PrivacyReporter::visit (HIR::UseTreeRebind &use_tree)
{}

void
PrivacyReporter::visit (HIR::UseDeclaration &use_decl)
{}

void
PrivacyReporter::visit (HIR::Function &function)
{
  function.get_definition ()->accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::TypeAlias &type_alias)
{}

void
PrivacyReporter::visit (HIR::StructStruct &struct_item)
{}

void
PrivacyReporter::visit (HIR::TupleStruct &tuple_struct)
{}

void
PrivacyReporter::visit (HIR::EnumItem &item)
{}

void
PrivacyReporter::visit (HIR::EnumItemTuple &item)
{}

void
PrivacyReporter::visit (HIR::EnumItemStruct &item)
{}

void
PrivacyReporter::visit (HIR::EnumItemDiscriminant &item)
{}

void
PrivacyReporter::visit (HIR::Enum &enum_item)
{}

void
PrivacyReporter::visit (HIR::Union &union_item)
{}

void
PrivacyReporter::visit (HIR::ConstantItem &const_item)
{}

void
PrivacyReporter::visit (HIR::StaticItem &static_item)
{}

void
PrivacyReporter::visit (HIR::TraitItemFunc &item)
{}

void
PrivacyReporter::visit (HIR::TraitItemConst &item)
{}

void
PrivacyReporter::visit (HIR::TraitItemType &item)
{}

void
PrivacyReporter::visit (HIR::Trait &trait)
{}

void
PrivacyReporter::visit (HIR::ImplBlock &impl)
{}

void
PrivacyReporter::visit (HIR::ExternalStaticItem &item)
{}

void
PrivacyReporter::visit (HIR::ExternalFunctionItem &item)
{}

void
PrivacyReporter::visit (HIR::ExternBlock &block)
{}

void
PrivacyReporter::visit (HIR::LiteralPattern &pattern)
{}

void
PrivacyReporter::visit (HIR::IdentifierPattern &pattern)
{}

void
PrivacyReporter::visit (HIR::WildcardPattern &pattern)
{}

void
PrivacyReporter::visit (HIR::RangePatternBoundLiteral &bound)
{}

void
PrivacyReporter::visit (HIR::RangePatternBoundPath &bound)
{}

void
PrivacyReporter::visit (HIR::RangePatternBoundQualPath &bound)
{}

void
PrivacyReporter::visit (HIR::RangePattern &pattern)
{}

void
PrivacyReporter::visit (HIR::ReferencePattern &pattern)
{}

void
PrivacyReporter::visit (HIR::StructPatternFieldTuplePat &field)
{}

void
PrivacyReporter::visit (HIR::StructPatternFieldIdentPat &field)
{}

void
PrivacyReporter::visit (HIR::StructPatternFieldIdent &field)
{}

void
PrivacyReporter::visit (HIR::StructPattern &pattern)
{}

void
PrivacyReporter::visit (HIR::TupleStructItemsNoRange &tuple_items)
{}

void
PrivacyReporter::visit (HIR::TupleStructItemsRange &tuple_items)
{}

void
PrivacyReporter::visit (HIR::TupleStructPattern &pattern)
{}

void
PrivacyReporter::visit (HIR::TuplePatternItemsMultiple &tuple_items)
{}

void
PrivacyReporter::visit (HIR::TuplePatternItemsRanged &tuple_items)
{}

void
PrivacyReporter::visit (HIR::TuplePattern &pattern)
{}

void
PrivacyReporter::visit (HIR::GroupedPattern &pattern)
{}

void
PrivacyReporter::visit (HIR::SlicePattern &pattern)
{}

void
PrivacyReporter::visit (HIR::EmptyStmt &stmt)
{}

void
PrivacyReporter::visit (HIR::LetStmt &stmt)
{}

void
PrivacyReporter::visit (HIR::ExprStmtWithoutBlock &stmt)
{
  stmt.get_expr ()->accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::ExprStmtWithBlock &stmt)
{}

void
PrivacyReporter::visit (HIR::TraitBound &bound)
{}

void
PrivacyReporter::visit (HIR::ImplTraitType &type)
{}

void
PrivacyReporter::visit (HIR::TraitObjectType &type)
{}

void
PrivacyReporter::visit (HIR::ParenthesisedType &type)
{}

void
PrivacyReporter::visit (HIR::ImplTraitTypeOneBound &type)
{}

void
PrivacyReporter::visit (HIR::TupleType &type)
{}

void
PrivacyReporter::visit (HIR::NeverType &type)
{}

void
PrivacyReporter::visit (HIR::RawPointerType &type)
{}

void
PrivacyReporter::visit (HIR::ReferenceType &type)
{}

void
PrivacyReporter::visit (HIR::ArrayType &type)
{}

void
PrivacyReporter::visit (HIR::SliceType &type)
{}

void
PrivacyReporter::visit (HIR::InferredType &type)
{}

void
PrivacyReporter::visit (HIR::BareFunctionType &type)
{}

} // namespace Privacy
} // namespace Rust
