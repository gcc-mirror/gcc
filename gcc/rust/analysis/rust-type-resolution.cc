#include "rust-type-resolution.h"
#include "rust-diagnostics.h"

#define ADD_BUILTIN_TYPE(_X, _S)                                               \
  do                                                                           \
    {                                                                          \
      AST::PathIdentSegment seg (_X);                                          \
      auto typePath = ::std::unique_ptr<AST::TypePathSegment> (                \
	new AST::TypePathSegment (::std::move (seg), false,                    \
				  Linemap::unknown_location ()));              \
      ::std::vector< ::std::unique_ptr<AST::TypePathSegment> > segs;           \
      segs.push_back (::std::move (typePath));                                 \
      auto bType = new AST::TypePath (::std::move (segs),                      \
				      Linemap::unknown_location (), false);    \
      _S.Insert (_X, bType);                                                   \
    }                                                                          \
  while (0)

namespace Rust {
namespace Analysis {

TypeResolution::TypeResolution (AST::Crate &crate, TopLevelScan &toplevel)
  : Resolution (crate, toplevel)
{
  functionScope.Push ();

  // push all builtin types - this is probably too basic for future needs
  ADD_BUILTIN_TYPE ("u8", typeScope);
  ADD_BUILTIN_TYPE ("u16", typeScope);
  ADD_BUILTIN_TYPE ("u32", typeScope);
  ADD_BUILTIN_TYPE ("u64", typeScope);

  ADD_BUILTIN_TYPE ("i8", typeScope);
  ADD_BUILTIN_TYPE ("i16", typeScope);
  ADD_BUILTIN_TYPE ("i32", typeScope);
  ADD_BUILTIN_TYPE ("i64", typeScope);

  ADD_BUILTIN_TYPE ("f32", typeScope);
  ADD_BUILTIN_TYPE ("f64", typeScope);

  ADD_BUILTIN_TYPE ("char", typeScope);
  ADD_BUILTIN_TYPE ("str", typeScope);
  ADD_BUILTIN_TYPE ("bool", typeScope);
}

TypeResolution::~TypeResolution () { functionScope.Pop (); }

bool
TypeResolution::Resolve (AST::Crate &crate, TopLevelScan &toplevel)
{
  TypeResolution resolver (crate, toplevel);
  return resolver.go ();
}

bool
TypeResolution::go ()
{
  for (auto &item : crate.items)
    item->accept_vis (*this);

  return true;
}

bool
TypeResolution::typesAreCompatible (AST::Type *lhs, AST::Type *rhs,
				    Location locus)
{
  auto before = typeComparisonBuffer.size ();
  lhs->accept_vis (*this);
  if (typeComparisonBuffer.size () <= before)
    {
      rust_error_at (locus, "failed to understand type for lhs");
      return false;
    }

  auto lhsTypeStr = typeComparisonBuffer.back ();
  typeComparisonBuffer.pop_back ();

  rhs->accept_vis (*this);
  if (typeComparisonBuffer.size () <= before)
    {
      rust_error_at (locus, "failed to understand type for rhs");
      return false;
    }

  auto rhsTypeStr = typeComparisonBuffer.back ();
  typeComparisonBuffer.pop_back ();

  // FIXME this needs to handle the cases of an i8 going into an i32 which is
  // compatible
  if (lhsTypeStr.compare (rhsTypeStr))
    {
      rust_error_at (locus, "E0308: expected: %s, found %s",
		     lhsTypeStr.c_str (), rhsTypeStr.c_str ());
      return false;
    }

  AST::Type *val = NULL;
  if (!typeScope.Lookup (lhsTypeStr, &val))
    {
      rust_error_at (locus, "unknown type");
      return false;
    }

  return true;
}

bool
TypeResolution::isTypeInScope (AST::Type *type, Location locus)
{
  auto before = typeComparisonBuffer.size ();
  type->accept_vis (*this);
  if (typeComparisonBuffer.size () <= before)
    {
      rust_error_at (locus, "unable to decipher type: %s",
		     type->as_string ().c_str ());
      return false;
    }

  auto t = typeComparisonBuffer.back ();
  typeComparisonBuffer.pop_back ();

  AST::Type *val = NULL;
  if (!typeScope.Lookup (t, &val))
    {
      rust_error_at (locus, "unknown type");
      return false;
    }

  return true;
}

AST::Function *
TypeResolution::lookupFndecl (AST::Expr *expr)
{
  size_t before = functionLookup.size ();
  expr->accept_vis (*this);
  if (functionLookup.size () > before)
    {
      auto fndecl = functionLookup.back ();
      functionLookup.pop_back ();
      return fndecl;
    }

  rust_error_at (expr->get_locus_slow (), "failed to lookup function");
  return NULL;
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
  AST::Type *type = NULL;
  bool ok = scope.Lookup (ident_expr.ident, &type);
  if (!ok)
    {
      rust_error_at (ident_expr.locus, "unknown identifier");
      return;
    }

  typeBuffer.push_back (type);
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
{
  // look up in the functionScope else lookup in the toplevel scan
  AST::Function *fndecl = NULL;
  if (functionScope.Lookup (path.as_string (), &fndecl))
    {
      functionLookup.push_back (fndecl);
      return;
    }

  fndecl = toplevel.lookupFunction (&path);
  if (fndecl != NULL)
    {
      functionLookup.push_back (fndecl);
      return;
    }
}

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
{
  // this may not be robust enough for type comparisons but lets try it for now
  typeComparisonBuffer.push_back (path.as_string ());
}

void
TypeResolution::visit (AST::QualifiedPathInExpression &path)
{
  typeComparisonBuffer.push_back (path.as_string ());
}

void
TypeResolution::visit (AST::QualifiedPathInType &path)
{
  typeComparisonBuffer.push_back (path.as_string ());
}

// rust-expr.h
void
TypeResolution::visit (AST::LiteralExpr &expr)
{
  std::string type;
  switch (expr.literal.get_lit_type ())
    {
    case AST::Literal::CHAR:
      type = "char";
      break;

    case AST::Literal::STRING:
    case AST::Literal::RAW_STRING:
      type = "str";
      break;

    case AST::Literal::BOOL:
      type = "bool";
      break;

    case AST::Literal::BYTE:
      type = "u8";
      break;

      // FIXME these are not always going to be the case
      // eg: suffix on the value can change the type
    case AST::Literal::FLOAT:
      type = "f32";
      break;

    case AST::Literal::INT:
      type = "i32";
      break;

    case AST::Literal::BYTE_STRING:
    case AST::Literal::RAW_BYTE_STRING:
      // FIXME
      break;
    }

  if (type.empty ())
    {
      rust_error_at (expr.locus, "unknown literal: %s",
		     expr.literal.as_string ().c_str ());
      return;
    }

  AST::Type *val = NULL;
  bool ok = typeScope.Lookup (type, &val);
  if (ok)
    typeBuffer.push_back (val);
  else
    rust_error_at (expr.locus, "unknown literal type: %s", type.c_str ());
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
{
  size_t before;
  before = typeBuffer.size ();
  expr.visit_lhs (*this);
  if (typeBuffer.size () <= before)
    {
      rust_error_at (expr.locus, "unable to determine lhs type");
      return;
    }

  auto lhsType = typeBuffer.back ();
  typeBuffer.pop_back ();

  before = typeBuffer.size ();
  expr.visit_rhs (*this);
  if (typeBuffer.size () <= before)
    {
      rust_error_at (expr.locus, "unable to determine rhs type");
      return;
    }

  auto rhsType = typeBuffer.back ();
  // not poping because we will be checking they match and the
  // scope will require knowledge of the type

  // do the lhsType and the rhsType match
  typesAreCompatible (lhsType, rhsType, expr.right_expr->get_locus_slow ());
}

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
{
  size_t before;
  before = typeBuffer.size ();
  expr.visit_lhs (*this);
  if (typeBuffer.size () <= before)
    {
      rust_error_at (expr.locus, "unable to determine lhs type");
      return;
    }

  auto lhsType = typeBuffer.back ();
  typeBuffer.pop_back ();

  before = typeBuffer.size ();
  expr.visit_rhs (*this);
  if (typeBuffer.size () <= before)
    {
      rust_error_at (expr.locus, "unable to determine rhs type");
      return;
    }

  auto rhsType = typeBuffer.back ();
  // not poping because we will be checking they match and the
  // scope will require knowledge of the type

  // do the lhsType and the rhsType match
  if (!typesAreCompatible (lhsType, rhsType,
			   expr.right_expr->get_locus_slow ()))
    return;

  // is the lhs mutable?
}

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
{
  // this look up should probably be moved to name resolution
  auto fndecl = lookupFndecl (expr.function.get ());
  if (fndecl == NULL)
    return;

  typeBuffer.push_back (fndecl->return_type.get ());
  expr.fndeclRef = fndecl;

  auto before = typeBuffer.size ();
  for (auto &item : expr.params)
    item->accept_vis (*this);

  auto numInferedParams = typeBuffer.size () - before;
  if (numInferedParams != expr.params.size ())
    {
      rust_error_at (expr.locus, "Failed to infer all parameters");
      return;
    }

  auto offs = numInferedParams - 1;
  for (auto it = fndecl->function_params.rbegin ();
       it != fndecl->function_params.rend (); ++it)
    {
      AST::Type *argument = typeBuffer.back ();
      typeBuffer.pop_back ();

      if (!typesAreCompatible (it->type.get (), argument,
			       expr.params[offs]->get_locus_slow ()))
	return;
      offs--;
    }
}

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
  // always emit the function with return type in the event of nil return type
  // its  a marker for a void function
  scope.Insert (function.function_name, function.return_type.get ());
  functionScope.Insert (function.function_name, &function);

  functionScope.Push ();
  scope.Push ();
  for (auto &param : function.function_params)
    {
      if (!isTypeInScope (param.type.get (), param.locus))
	return;

      auto before = letPatternBuffer.size ();
      param.param_name->accept_vis (*this);
      if (letPatternBuffer.size () <= before)
	{
	  rust_error_at (param.locus, "failed to analyse parameter name");
	  return;
	}

      auto paramName = letPatternBuffer.back ();
      letPatternBuffer.pop_back ();
      scope.Insert (paramName.variable_ident, param.type.get ());
    }

  // ensure the return type is resolved
  if (function.has_function_return_type ())
    {
      if (!isTypeInScope (function.return_type.get (), function.locus))
	return;
    }

  // walk the expression body
  for (auto &stmt : function.function_body->statements)
    {
      stmt->accept_vis (*this);
    }

  scope.Pop ();
  functionScope.Pop ();
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
{
  printf ("ConstantItem: %s\n", const_item.as_string ().c_str ());
}

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
  if (!stmt.has_init_expr () && !stmt.has_type ())
    {
      rust_error_at (stmt.locus,
		     "E0282: type annotations or init expression needed");
      return;
    }

  AST::Type *inferedType = NULL;
  if (stmt.has_init_expr ())
    {
      auto before = typeBuffer.size ();
      stmt.init_expr->accept_vis (*this);

      if (typeBuffer.size () <= before)
	{
	  rust_error_at (
	    stmt.init_expr->get_locus_slow (),
	    "unable to determine type for declaration from init expr");
	  return;
	}

      inferedType = typeBuffer.back ();
      typeBuffer.pop_back ();

      if (inferedType == NULL)
	{
	  rust_error_at (stmt.init_expr->get_locus_slow (),
			 "void type found for statement initialisation");
	  return;
	}
    }

  if (stmt.has_type () && stmt.has_init_expr ())
    {
      if (!typesAreCompatible (stmt.type.get (), inferedType,
			       stmt.init_expr->get_locus_slow ()))
	{
	  return;
	}
    }
  else if (stmt.has_type () && !stmt.has_init_expr ())
    {
      inferedType = stmt.type.get ();
    }

  // TODO check we know what the type is in the scope requires the builtins to
  // be defined at the constructor

  // ensure the decl has the type set for compilation later on
  if (!stmt.has_type ())
    {
      stmt.inferedType = inferedType;
    }

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
  stmt.expr->accept_vis (*this);
}

void
TypeResolution::visit (AST::ExprStmtWithBlock &stmt)
{}

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
