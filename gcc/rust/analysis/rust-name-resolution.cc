#include "rust-name-resolution.h"
#include "rust-diagnostics.h"

/*
 * The principles of name resolution:
 * 1. If a name resolves to a binding then it should always resolve to that
 * binding, and if resolving a name produces an error, it should always produce
 * an error.
 * 2. Avoid errors due to the resolver being stuck.
 * 3. Names should be able to be used before they are declared, but the order of
 * declarations should be irrelevant.
 * 4. Macros should be hygiene and manually expandable.
 * 5. Glob imports should be manually expandable.
 */

/* The algorithm of name resolution
 * 1. When encouter items which bind a name, add the name to the binding table.
 * 2. When we find an import which can't be resolved, we add it to a work list.
 * 3. When we find a glob import, we have to record a 'back link', so that when
 *    a public name is added for the supplying module, we can add it for the
 *    importing module.
 * 4. Loop over the work list and try to lookup names.
 *    a. If a name has exactly one best binding then we use it (and record the
 *       binding on a list of resolved names).
 *    b. If there are zero then we put it back on the work list.
 *    c. If there is more than one binding, then we record an ambiguity error.
 *    d. When the work list no longer changes, then we are done.
 *    e. If the work list is empty, then expansion/import resolution succeeded.
 *       Otherwise there are names not found, or ambiguous names, then failed.
 * 5. When looking up names, we record the resolutions in the binding table.
 *    a. If the name a glob import, we add bindings for every accessible name
 *       currently known.
 * 6. To expand a macro, we try to resolve the macro's name.
 *    a. If that fails, we put it on the work list.
 *       Otherwise, we expand that macro by parsing the arguments,
 *       pattern matching, and doing hygienic expansion.
 *    b. We then parse the generated code in the same way as we parsed the
 *       original program. We add new names to the binding table, and expand any
 *       new macro uses.
 * 7. If we add names for a module which has back links, we must follow them and
 *    add these names to the importing module (if they are accessible).
 */

namespace Rust {
namespace Analysis {

NameResolution::NameResolution (AST::Crate &crate, TopLevelScan &toplevel)
  : Resolution (crate, toplevel), is_work_list_changed_ (false)

{}

NameResolution::~NameResolution () {}

bool
NameResolution::Resolve (AST::Crate &crate, TopLevelScan &toplevel)
{
  NameResolution resolver (crate, toplevel);
  return resolver.go ();
}

void
NameResolution::process_work_list ()
{}

void
NameResolution::expand_macros ()
{}

bool
NameResolution::go ()
{
  bool ret = true;

  do
    {
      for (auto &item : crate.items)
	{
	  item->accept_vis (*this);
	}
    }
  while (is_work_list_changed ());

  ret = work_list_.empty ();
  for (auto &item : work_list_)
    {
      std::cout << "Resolution error: " << item.as_string () << std::endl;
    }

  return ret;
}

void
NameResolution::visit (AST::Token &tok)
{}

void
NameResolution::visit (AST::DelimTokenTree &delim_tok_tree)
{}

void
NameResolution::visit (AST::AttrInputMetaItemContainer &input)
{}

void
NameResolution::visit (AST::IdentifierExpr &ident_expr)
{
  do
    {
      process_work_list ();
    }
  while (is_work_list_changed ());
  expand_macros ();
}

void
NameResolution::visit (AST::Lifetime &lifetime)
{}

void
NameResolution::visit (AST::LifetimeParam &lifetime_param)
{}

void
NameResolution::visit (AST::MacroInvocationSemi &macro)
{}

// rust-path.h
void
NameResolution::visit (AST::PathInExpression &path)
{}

void
NameResolution::visit (AST::TypePathSegment &segment)
{}
void
NameResolution::visit (AST::TypePathSegmentGeneric &segment)
{}

void
NameResolution::visit (AST::TypePathSegmentFunction &segment)
{}

void
NameResolution::visit (AST::TypePath &path)
{}

void
NameResolution::visit (AST::QualifiedPathInExpression &path)
{
  typeComparisonBuffer.push_back (path.as_string ());
}

void
NameResolution::visit (AST::QualifiedPathInType &path)
{}

// rust-expr.h
void
NameResolution::visit (AST::LiteralExpr &expr)
{}

void
NameResolution::visit (AST::AttrInputLiteral &attr_input)
{}

void
NameResolution::visit (AST::MetaItemLitExpr &meta_item)
{}

void
NameResolution::visit (AST::MetaItemPathLit &meta_item)
{}

void
NameResolution::visit (AST::BorrowExpr &expr)
{}
void
NameResolution::visit (AST::DereferenceExpr &expr)
{}
void
NameResolution::visit (AST::ErrorPropagationExpr &expr)
{}
void
NameResolution::visit (AST::NegationExpr &expr)
{}

void
NameResolution::visit (AST::ArithmeticOrLogicalExpr &expr)
{}

void
NameResolution::visit (AST::ComparisonExpr &expr)
{}

void
NameResolution::visit (AST::LazyBooleanExpr &expr)
{}

void
NameResolution::visit (AST::TypeCastExpr &expr)
{}

void
NameResolution::visit (AST::AssignmentExpr &expr)
{}

void
NameResolution::visit (AST::CompoundAssignmentExpr &expr)
{}

void
NameResolution::visit (AST::GroupedExpr &expr)
{}
// void NameResolution::visit(ArrayElems& elems) {}
void
NameResolution::visit (AST::ArrayElemsValues &elems)
{}
void
NameResolution::visit (AST::ArrayElemsCopied &elems)
{}
void
NameResolution::visit (AST::ArrayExpr &expr)
{}
void
NameResolution::visit (AST::ArrayIndexExpr &expr)
{}
void
NameResolution::visit (AST::TupleExpr &expr)
{}
void
NameResolution::visit (AST::TupleIndexExpr &expr)
{}
void
NameResolution::visit (AST::StructExprStruct &expr)
{}
// void NameResolution::visit(StructExprField& field) {}
void
NameResolution::visit (AST::StructExprFieldIdentifier &field)
{}
void
NameResolution::visit (AST::StructExprFieldIdentifierValue &field)
{}
void
NameResolution::visit (AST::StructExprFieldIndexValue &field)
{}
void
NameResolution::visit (AST::StructExprStructFields &expr)
{}
void
NameResolution::visit (AST::StructExprStructBase &expr)
{}
void
NameResolution::visit (AST::StructExprTuple &expr)
{}
void
NameResolution::visit (AST::StructExprUnit &expr)
{}
// void NameResolution::visit(EnumExprField& field) {}
void
NameResolution::visit (AST::EnumExprFieldIdentifier &field)
{}
void
NameResolution::visit (AST::EnumExprFieldIdentifierValue &field)
{}
void
NameResolution::visit (AST::EnumExprFieldIndexValue &field)
{}
void
NameResolution::visit (AST::EnumExprStruct &expr)
{}
void
NameResolution::visit (AST::EnumExprTuple &expr)
{}
void
NameResolution::visit (AST::EnumExprFieldless &expr)
{}

void
NameResolution::visit (AST::CallExpr &expr)
{}

void
NameResolution::visit (AST::MethodCallExpr &expr)
{}
void
NameResolution::visit (AST::FieldAccessExpr &expr)
{}
void
NameResolution::visit (AST::ClosureExprInner &expr)
{}
void
NameResolution::visit (AST::BlockExpr &expr)
{}
void
NameResolution::visit (AST::ClosureExprInnerTyped &expr)
{}
void
NameResolution::visit (AST::ContinueExpr &expr)
{}
void
NameResolution::visit (AST::BreakExpr &expr)
{}
void
NameResolution::visit (AST::RangeFromToExpr &expr)
{}
void
NameResolution::visit (AST::RangeFromExpr &expr)
{}
void
NameResolution::visit (AST::RangeToExpr &expr)
{}
void
NameResolution::visit (AST::RangeFullExpr &expr)
{}
void
NameResolution::visit (AST::RangeFromToInclExpr &expr)
{}
void
NameResolution::visit (AST::RangeToInclExpr &expr)
{}
void
NameResolution::visit (AST::ReturnExpr &expr)
{}
void
NameResolution::visit (AST::UnsafeBlockExpr &expr)
{}
void
NameResolution::visit (AST::LoopExpr &expr)
{}
void
NameResolution::visit (AST::WhileLoopExpr &expr)
{}
void
NameResolution::visit (AST::WhileLetLoopExpr &expr)
{}
void
NameResolution::visit (AST::ForLoopExpr &expr)
{}
void
NameResolution::visit (AST::IfExpr &expr)
{}
void
NameResolution::visit (AST::IfExprConseqElse &expr)
{}
void
NameResolution::visit (AST::IfExprConseqIf &expr)
{}
void
NameResolution::visit (AST::IfExprConseqIfLet &expr)
{}
void
NameResolution::visit (AST::IfLetExpr &expr)
{}
void
NameResolution::visit (AST::IfLetExprConseqElse &expr)
{}
void
NameResolution::visit (AST::IfLetExprConseqIf &expr)
{}
void
NameResolution::visit (AST::IfLetExprConseqIfLet &expr)
{}
// void NameResolution::visit(MatchCase& match_case) {}
/*void
NameResolution::visit (AST::MatchCaseBlockExpr &match_case)
{}*/
/*void
NameResolution::visit (AST::MatchCaseExpr &match_case)
{}*/
void
NameResolution::visit (AST::MatchExpr &expr)
{}
void
NameResolution::visit (AST::AwaitExpr &expr)
{}
void
NameResolution::visit (AST::AsyncBlockExpr &expr)
{}

// rust-item.h
void
NameResolution::visit (AST::TypeParam &param)
{}
// void NameResolution::visit(WhereClauseItem& item) {}
void
NameResolution::visit (AST::LifetimeWhereClauseItem &item)
{}
void
NameResolution::visit (AST::TypeBoundWhereClauseItem &item)
{}
void
NameResolution::visit (AST::Method &method)
{}
void
NameResolution::visit (AST::ModuleBodied &module)
{}
void
NameResolution::visit (AST::ModuleNoBody &module)
{}
void
NameResolution::visit (AST::ExternCrate &crate)
{}
// void NameResolution::visit(UseTree& use_tree) {}
void
NameResolution::visit (AST::UseTreeGlob &use_tree)
{}
void
NameResolution::visit (AST::UseTreeList &use_tree)
{}
void
NameResolution::visit (AST::UseTreeRebind &use_tree)
{}
void
NameResolution::visit (AST::UseDeclaration &use_decl)
{}

void
NameResolution::visit (AST::Function &function)
{}

void
NameResolution::visit (AST::TypeAlias &type_alias)
{}
void
NameResolution::visit (AST::StructStruct &struct_item)
{}
void
NameResolution::visit (AST::TupleStruct &tuple_struct)
{}
void
NameResolution::visit (AST::EnumItem &item)
{}
void
NameResolution::visit (AST::EnumItemTuple &item)
{}
void
NameResolution::visit (AST::EnumItemStruct &item)
{}
void
NameResolution::visit (AST::EnumItemDiscriminant &item)
{}
void
NameResolution::visit (AST::Enum &enum_item)
{}
void
NameResolution::visit (AST::Union &union_item)
{}

void
NameResolution::visit (AST::ConstantItem &const_item)
{}

void
NameResolution::visit (AST::StaticItem &static_item)
{}
void
NameResolution::visit (AST::TraitItemFunc &item)
{}
void
NameResolution::visit (AST::TraitItemMethod &item)
{}
void
NameResolution::visit (AST::TraitItemConst &item)
{}
void
NameResolution::visit (AST::TraitItemType &item)
{}
void
NameResolution::visit (AST::Trait &trait)
{}
void
NameResolution::visit (AST::InherentImpl &impl)
{}
void
NameResolution::visit (AST::TraitImpl &impl)
{}
// void NameResolution::visit(ExternalItem& item) {}
void
NameResolution::visit (AST::ExternalStaticItem &item)
{}
void
NameResolution::visit (AST::ExternalFunctionItem &item)
{}
void
NameResolution::visit (AST::ExternBlock &block)
{}

// rust-macro.h
void
NameResolution::visit (AST::MacroMatchFragment &match)
{}
void
NameResolution::visit (AST::MacroMatchRepetition &match)
{}
void
NameResolution::visit (AST::MacroMatcher &matcher)
{}

void
NameResolution::visit (AST::MacroRulesDefinition &rules_def)
{}

void
NameResolution::visit (AST::MacroInvocation &macro_invoc)
{}
void
NameResolution::visit (AST::MetaItemPath &meta_item)
{}
void
NameResolution::visit (AST::MetaItemSeq &meta_item)
{}
void
NameResolution::visit (AST::MetaWord &meta_item)
{}
void
NameResolution::visit (AST::MetaNameValueStr &meta_item)
{}
void
NameResolution::visit (AST::MetaListPaths &meta_item)
{}
void
NameResolution::visit (AST::MetaListNameValueStr &meta_item)
{}

// rust-pattern.h
void
NameResolution::visit (AST::LiteralPattern &pattern)
{}

void
NameResolution::visit (AST::IdentifierPattern &pattern)
{}

void
NameResolution::visit (AST::WildcardPattern &pattern)
{}
// void NameResolution::visit(RangePatternBound& bound) {}
void
NameResolution::visit (AST::RangePatternBoundLiteral &bound)
{}
void
NameResolution::visit (AST::RangePatternBoundPath &bound)
{}
void
NameResolution::visit (AST::RangePatternBoundQualPath &bound)
{}
void
NameResolution::visit (AST::RangePattern &pattern)
{}
void
NameResolution::visit (AST::ReferencePattern &pattern)
{}
// void NameResolution::visit(StructPatternField& field) {}
void
NameResolution::visit (AST::StructPatternFieldTuplePat &field)
{}
void
NameResolution::visit (AST::StructPatternFieldIdentPat &field)
{}
void
NameResolution::visit (AST::StructPatternFieldIdent &field)
{}
void
NameResolution::visit (AST::StructPattern &pattern)
{}
// void NameResolution::visit(TupleStructItems& tuple_items) {}
void
NameResolution::visit (AST::TupleStructItemsNoRange &tuple_items)
{}
void
NameResolution::visit (AST::TupleStructItemsRange &tuple_items)
{}
void
NameResolution::visit (AST::TupleStructPattern &pattern)
{}
// void NameResolution::visit(TuplePatternItems& tuple_items) {}
void
NameResolution::visit (AST::TuplePatternItemsMultiple &tuple_items)
{}
void
NameResolution::visit (AST::TuplePatternItemsRanged &tuple_items)
{}
void
NameResolution::visit (AST::TuplePattern &pattern)
{}
void
NameResolution::visit (AST::GroupedPattern &pattern)
{}
void
NameResolution::visit (AST::SlicePattern &pattern)
{}

// rust-stmt.h
void
NameResolution::visit (AST::EmptyStmt &stmt)
{}

void
NameResolution::visit (AST::LetStmt &stmt)
{}

void
NameResolution::visit (AST::ExprStmtWithoutBlock &stmt)
{}

void
NameResolution::visit (AST::ExprStmtWithBlock &stmt)
{}

// rust-type.h
void
NameResolution::visit (AST::TraitBound &bound)
{}

void
NameResolution::visit (AST::ImplTraitType &type)
{}

void
NameResolution::visit (AST::TraitObjectType &type)
{}
void
NameResolution::visit (AST::ParenthesisedType &type)
{}
void
NameResolution::visit (AST::ImplTraitTypeOneBound &type)
{}
void
NameResolution::visit (AST::TraitObjectTypeOneBound &type)
{}
void
NameResolution::visit (AST::TupleType &type)
{}
void
NameResolution::visit (AST::NeverType &type)
{}
void
NameResolution::visit (AST::RawPointerType &type)
{}
void
NameResolution::visit (AST::ReferenceType &type)
{}
void
NameResolution::visit (AST::ArrayType &type)
{}
void
NameResolution::visit (AST::SliceType &type)
{}
void
NameResolution::visit (AST::InferredType &type)
{}
void
NameResolution::visit (AST::BareFunctionType &type)
{}

} // namespace Analysis
} // namespace Rust
