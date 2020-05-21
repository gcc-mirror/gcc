#include "rust-compile.h"
#include "rust-diagnostics.h"

namespace Rust {
namespace Compile {

Compilation::Compilation (AST::Crate &crate, ::Backend *backend)
  : crate (crate), backend (backend)
{}

Compilation::~Compilation () {}

bool
Compilation::Compile (AST::Crate &crate, ::Backend *backend)
{
  Compilation resolver (crate, backend);
  return resolver.go ();
}

bool
Compilation::go ()
{
  scope.Push ();
  for (auto &item : crate.items)
    {
      item->accept_vis (*this);
    }
  scope.Pop ();

  // Define all globally declared values.
  if (saw_errors ())
    return false;

  backend->write_global_definitions (type_decls, const_decls, func_decls,
				     var_decls);
  return true;
}

bool
Compilation::compileVarDecl (AST::LetStmt *stmt, std::vector<Bvariable *> &vars)
{
  AST::Type *type = stmt->has_type () ? stmt->type.get () : stmt->inferedType;
  translatedType = NULL;
  type->accept_vis (*this);
  if (translatedType == NULL)
    {
      rust_error_at (stmt->locus, "failed to compile type for var decl");
      return false;
    }

  stmt->variables_pattern->accept_vis (*this);
  for (auto &pattern : patternBuffer)
    {
      auto var = backend->local_variable (currentFndecl, pattern.variable_ident,
					  translatedType, NULL /*decl_var*/,
					  false /*address_taken*/, stmt->locus);
      vars.push_back (var);
    }
  patternBuffer.clear ();
  return true;
}

void
Compilation::visit (AST::Token &tok)
{}

void
Compilation::visit (AST::DelimTokenTree &delim_tok_tree)
{}

void
Compilation::visit (AST::AttrInputMetaItemContainer &input)
{}

void
Compilation::visit (AST::IdentifierExpr &ident_expr)
{}

void
Compilation::visit (AST::Lifetime &lifetime)
{}

void
Compilation::visit (AST::LifetimeParam &lifetime_param)
{}

void
Compilation::visit (AST::MacroInvocationSemi &macro)
{}

// rust-path.h
void
Compilation::visit (AST::PathInExpression &path)
{}
void
Compilation::visit (AST::TypePathSegment &segment)
{}
void
Compilation::visit (AST::TypePathSegmentGeneric &segment)
{}
void
Compilation::visit (AST::TypePathSegmentFunction &segment)
{}

void
Compilation::visit (AST::TypePath &path)
{
  if (path.segments.size () > 1)
    {
      rust_error_at (path.locus, "unable to compile multi segment types yet");
      return;
    }

  auto typeString = path.as_string ();
  if (typeString.compare ("i64") == 0)
    {
      translatedType = backend->integer_type (false, 64);
      return;
    }
  else if (typeString.compare ("i32") == 0)
    {
      translatedType = backend->integer_type (false, 32);
      return;
    }
  else if (typeString.compare ("i16") == 0)
    {
      translatedType = backend->integer_type (false, 16);
      return;
    }
  else if (typeString.compare ("i8") == 0)
    {
      translatedType = backend->integer_type (false, 8);
      return;
    }
  else if (typeString.compare ("u64") == 0)
    {
      translatedType = backend->integer_type (true, 64);
      return;
    }
  else if (typeString.compare ("u32") == 0)
    {
      translatedType = backend->integer_type (true, 32);
      return;
    }
  else if (typeString.compare ("u16") == 0)
    {
      translatedType = backend->integer_type (true, 16);
      return;
    }
  else if (typeString.compare ("u8") == 0)
    {
      translatedType = backend->integer_type (true, 8);
      return;
    }
  else if (typeString.compare ("bool") == 0)
    {
      translatedType = backend->bool_type ();
      return;
    }
}

void
Compilation::visit (AST::QualifiedPathInExpression &path)
{}
void
Compilation::visit (AST::QualifiedPathInType &path)
{}

// rust-expr.h
void
Compilation::visit (AST::LiteralExpr &expr)
{}
void
Compilation::visit (AST::AttrInputLiteral &attr_input)
{}
void
Compilation::visit (AST::MetaItemLitExpr &meta_item)
{}
void
Compilation::visit (AST::MetaItemPathLit &meta_item)
{}
void
Compilation::visit (AST::BorrowExpr &expr)
{}
void
Compilation::visit (AST::DereferenceExpr &expr)
{}
void
Compilation::visit (AST::ErrorPropagationExpr &expr)
{}
void
Compilation::visit (AST::NegationExpr &expr)
{}
void
Compilation::visit (AST::ArithmeticOrLogicalExpr &expr)
{}
void
Compilation::visit (AST::ComparisonExpr &expr)
{}
void
Compilation::visit (AST::LazyBooleanExpr &expr)
{}
void
Compilation::visit (AST::TypeCastExpr &expr)
{}
void
Compilation::visit (AST::AssignmentExpr &expr)
{}
void
Compilation::visit (AST::CompoundAssignmentExpr &expr)
{}
void
Compilation::visit (AST::GroupedExpr &expr)
{}
// void Compilation::visit(ArrayElems& elems) {}
void
Compilation::visit (AST::ArrayElemsValues &elems)
{}
void
Compilation::visit (AST::ArrayElemsCopied &elems)
{}
void
Compilation::visit (AST::ArrayExpr &expr)
{}
void
Compilation::visit (AST::ArrayIndexExpr &expr)
{}
void
Compilation::visit (AST::TupleExpr &expr)
{}
void
Compilation::visit (AST::TupleIndexExpr &expr)
{}
void
Compilation::visit (AST::StructExprStruct &expr)
{}
// void Compilation::visit(StructExprField& field) {}
void
Compilation::visit (AST::StructExprFieldIdentifier &field)
{}
void
Compilation::visit (AST::StructExprFieldIdentifierValue &field)
{}
void
Compilation::visit (AST::StructExprFieldIndexValue &field)
{}
void
Compilation::visit (AST::StructExprStructFields &expr)
{}
void
Compilation::visit (AST::StructExprStructBase &expr)
{}
void
Compilation::visit (AST::StructExprTuple &expr)
{}
void
Compilation::visit (AST::StructExprUnit &expr)
{}
// void Compilation::visit(EnumExprField& field) {}
void
Compilation::visit (AST::EnumExprFieldIdentifier &field)
{}
void
Compilation::visit (AST::EnumExprFieldIdentifierValue &field)
{}
void
Compilation::visit (AST::EnumExprFieldIndexValue &field)
{}
void
Compilation::visit (AST::EnumExprStruct &expr)
{}
void
Compilation::visit (AST::EnumExprTuple &expr)
{}
void
Compilation::visit (AST::EnumExprFieldless &expr)
{}
void
Compilation::visit (AST::CallExpr &expr)
{}
void
Compilation::visit (AST::MethodCallExpr &expr)
{}
void
Compilation::visit (AST::FieldAccessExpr &expr)
{}
void
Compilation::visit (AST::ClosureExprInner &expr)
{}
void
Compilation::visit (AST::BlockExpr &expr)
{}
void
Compilation::visit (AST::ClosureExprInnerTyped &expr)
{}
void
Compilation::visit (AST::ContinueExpr &expr)
{}
void
Compilation::visit (AST::BreakExpr &expr)
{}
void
Compilation::visit (AST::RangeFromToExpr &expr)
{}
void
Compilation::visit (AST::RangeFromExpr &expr)
{}
void
Compilation::visit (AST::RangeToExpr &expr)
{}
void
Compilation::visit (AST::RangeFullExpr &expr)
{}
void
Compilation::visit (AST::RangeFromToInclExpr &expr)
{}
void
Compilation::visit (AST::RangeToInclExpr &expr)
{}
void
Compilation::visit (AST::ReturnExpr &expr)
{}
void
Compilation::visit (AST::UnsafeBlockExpr &expr)
{}
void
Compilation::visit (AST::LoopExpr &expr)
{}
void
Compilation::visit (AST::WhileLoopExpr &expr)
{}
void
Compilation::visit (AST::WhileLetLoopExpr &expr)
{}
void
Compilation::visit (AST::ForLoopExpr &expr)
{}
void
Compilation::visit (AST::IfExpr &expr)
{}
void
Compilation::visit (AST::IfExprConseqElse &expr)
{}
void
Compilation::visit (AST::IfExprConseqIf &expr)
{}
void
Compilation::visit (AST::IfExprConseqIfLet &expr)
{}
void
Compilation::visit (AST::IfLetExpr &expr)
{}
void
Compilation::visit (AST::IfLetExprConseqElse &expr)
{}
void
Compilation::visit (AST::IfLetExprConseqIf &expr)
{}
void
Compilation::visit (AST::IfLetExprConseqIfLet &expr)
{}
// void Compilation::visit(MatchCase& match_case) {}
void
Compilation::visit (AST::MatchCaseBlockExpr &match_case)
{}
void
Compilation::visit (AST::MatchCaseExpr &match_case)
{}
void
Compilation::visit (AST::MatchExpr &expr)
{}
void
Compilation::visit (AST::AwaitExpr &expr)
{}
void
Compilation::visit (AST::AsyncBlockExpr &expr)
{}

// rust-item.h
void
Compilation::visit (AST::TypeParam &param)
{}
// void Compilation::visit(WhereClauseItem& item) {}
void
Compilation::visit (AST::LifetimeWhereClauseItem &item)
{}
void
Compilation::visit (AST::TypeBoundWhereClauseItem &item)
{}
void
Compilation::visit (AST::Method &method)
{}
void
Compilation::visit (AST::ModuleBodied &module)
{}
void
Compilation::visit (AST::ModuleNoBody &module)
{}
void
Compilation::visit (AST::ExternCrate &crate)
{}
// void Compilation::visit(UseTree& use_tree) {}
void
Compilation::visit (AST::UseTreeGlob &use_tree)
{}
void
Compilation::visit (AST::UseTreeList &use_tree)
{}
void
Compilation::visit (AST::UseTreeRebind &use_tree)
{}
void
Compilation::visit (AST::UseDeclaration &use_decl)
{}

void
Compilation::visit (AST::Function &function)
{
  scope.Insert (function.function_name, function.return_type.get ());

  scope.Push ();
  printf ("INSIDE FUNCTION: %s\n", function.function_name.c_str ());

  Backend::Btyped_identifier receiver;
  std::vector<Backend::Btyped_identifier> parameters;
  std::vector<Backend::Btyped_identifier> results;

  for (auto &param : function.function_params)
    {
      // translate the type
      translatedType = NULL;
      param.type->accept_vis (*this);
      if (translatedType == NULL)
	{
	  rust_error_at (param.locus, "failed to generate type for parameter");
	  return;
	}

      auto before = patternBuffer.size ();
      param.param_name->accept_vis (*this);
      if (patternBuffer.size () <= before)
	{
	  rust_error_at (param.locus, "failed to analyse parameter name");
	  return;
	}

      auto numParamsPerType = patternBuffer.size () - before;
      for (auto i = 0; i < numParamsPerType; i++)
	{
	  auto paramName = patternBuffer.back ();
	  patternBuffer.pop_back ();
	  scope.Insert (paramName.variable_ident, param.type.get ());

	  parameters.push_back (
	    Backend::Btyped_identifier (paramName.variable_ident,
					translatedType, param.locus));
	}
    }

  Btype *returnType = NULL;
  if (function.has_function_return_type ())
    {
      translatedType = NULL;
      function.return_type->accept_vis (*this);
      if (translatedType == NULL)
	{
	  rust_error_at (function.locus,
			 "failed to generate type for function");
	  return;
	}
    }

  Btype *fntype = backend->function_type (receiver, parameters, results,
					  returnType, function.locus);
  Bfunction *fndecl
    = backend->function (fntype, function.function_name, "" /* asm_name */,
			 0 /* flags */, function.locus);
  currentFndecl = fndecl;

  // setup the params
  std::vector<Bvariable *> param_vars;
  for (auto &param : parameters)
    {
      bool tree_addressable = false;
      backend->parameter_variable (fndecl, param.name, param.btype,
				   tree_addressable, param.location);
    }

  if (!backend->function_set_parameters (fndecl, param_vars))
    {
      rust_error_at (function.locus, "failed to setup parameter variables");
      return;
    }

  std::vector<Bvariable *> vars;
  for (auto &decl : function.locals)
    {
      if (!compileVarDecl (decl, vars))
	{
	  rust_error_at (decl->locus, "failed to compile var decl");
	  return;
	}
      // TODO add to scope
    }

  // is null for top level functions - nested functions will have an enclosing
  // scope
  Bblock *enclosingScope = NULL;
  Location start_location = function.locus;
  Location end_location;
  if (function.function_body->statements.size () > 0)
    {
      end_location
	= function.function_body->statements.back ()->get_locus_slow ();
    }

  auto code_block = backend->block (fndecl, enclosingScope, vars,
				    start_location, end_location);
  auto body = backend->block_statement (code_block);

  if (!backend->function_set_body (fndecl, body))
    {
      rust_error_at (function.locus, "failed to set body to function");
      return;
    }

  for (auto &stmt : function.function_body->statements)
    {
      stmt->accept_vis (*this);
    }

  func_decls.push_back (fndecl);
  scope.Pop ();
}

void
Compilation::visit (AST::TypeAlias &type_alias)
{}
void
Compilation::visit (AST::StructStruct &struct_item)
{}
void
Compilation::visit (AST::TupleStruct &tuple_struct)
{}
void
Compilation::visit (AST::EnumItem &item)
{}
void
Compilation::visit (AST::EnumItemTuple &item)
{}
void
Compilation::visit (AST::EnumItemStruct &item)
{}
void
Compilation::visit (AST::EnumItemDiscriminant &item)
{}
void
Compilation::visit (AST::Enum &enum_item)
{}
void
Compilation::visit (AST::Union &union_item)
{}
void
Compilation::visit (AST::ConstantItem &const_item)
{}
void
Compilation::visit (AST::StaticItem &static_item)
{}
void
Compilation::visit (AST::TraitItemFunc &item)
{}
void
Compilation::visit (AST::TraitItemMethod &item)
{}
void
Compilation::visit (AST::TraitItemConst &item)
{}
void
Compilation::visit (AST::TraitItemType &item)
{}
void
Compilation::visit (AST::Trait &trait)
{}
void
Compilation::visit (AST::InherentImpl &impl)
{}
void
Compilation::visit (AST::TraitImpl &impl)
{}
// void Compilation::visit(ExternalItem& item) {}
void
Compilation::visit (AST::ExternalStaticItem &item)
{}
void
Compilation::visit (AST::ExternalFunctionItem &item)
{}
void
Compilation::visit (AST::ExternBlock &block)
{}

// rust-macro.h
void
Compilation::visit (AST::MacroMatchFragment &match)
{}
void
Compilation::visit (AST::MacroMatchRepetition &match)
{}
void
Compilation::visit (AST::MacroMatcher &matcher)
{}
void
Compilation::visit (AST::MacroRulesDefinition &rules_def)
{}
void
Compilation::visit (AST::MacroInvocation &macro_invoc)
{}
void
Compilation::visit (AST::MetaItemPath &meta_item)
{}
void
Compilation::visit (AST::MetaItemSeq &meta_item)
{}
void
Compilation::visit (AST::MetaWord &meta_item)
{}
void
Compilation::visit (AST::MetaNameValueStr &meta_item)
{}
void
Compilation::visit (AST::MetaListPaths &meta_item)
{}
void
Compilation::visit (AST::MetaListNameValueStr &meta_item)
{}

// rust-pattern.h
void
Compilation::visit (AST::LiteralPattern &pattern)
{
  printf ("LiteralPattern: %s\n", pattern.as_string ().c_str ());
}

void
Compilation::visit (AST::IdentifierPattern &pattern)
{
  printf ("IdentifierPattern: %s\n", pattern.as_string ().c_str ());
  patternBuffer.push_back (pattern);
}

void
Compilation::visit (AST::WildcardPattern &pattern)
{}
// void Compilation::visit(RangePatternBound& bound) {}
void
Compilation::visit (AST::RangePatternBoundLiteral &bound)
{}
void
Compilation::visit (AST::RangePatternBoundPath &bound)
{}
void
Compilation::visit (AST::RangePatternBoundQualPath &bound)
{}
void
Compilation::visit (AST::RangePattern &pattern)
{}
void
Compilation::visit (AST::ReferencePattern &pattern)
{}
// void Compilation::visit(StructPatternField& field) {}
void
Compilation::visit (AST::StructPatternFieldTuplePat &field)
{}
void
Compilation::visit (AST::StructPatternFieldIdentPat &field)
{}
void
Compilation::visit (AST::StructPatternFieldIdent &field)
{}
void
Compilation::visit (AST::StructPattern &pattern)
{}
// void Compilation::visit(TupleStructItems& tuple_items) {}
void
Compilation::visit (AST::TupleStructItemsNoRange &tuple_items)
{}
void
Compilation::visit (AST::TupleStructItemsRange &tuple_items)
{}
void
Compilation::visit (AST::TupleStructPattern &pattern)
{}
// void Compilation::visit(TuplePatternItems& tuple_items) {}
void
Compilation::visit (AST::TuplePatternItemsMultiple &tuple_items)
{}
void
Compilation::visit (AST::TuplePatternItemsRanged &tuple_items)
{}
void
Compilation::visit (AST::TuplePattern &pattern)
{}
void
Compilation::visit (AST::GroupedPattern &pattern)
{}
void
Compilation::visit (AST::SlicePattern &pattern)
{}

// rust-stmt.h
void
Compilation::visit (AST::EmptyStmt &stmt)
{}
void

Compilation::visit (AST::LetStmt &stmt)
{
  printf ("Within LetStmt: %s\n", stmt.as_string ().c_str ());
}

void
Compilation::visit (AST::ExprStmtWithoutBlock &stmt)
{}
void
Compilation::visit (AST::ExprStmtWithBlock &stmt)
{}

// rust-type.h
void
Compilation::visit (AST::TraitBound &bound)
{}
void
Compilation::visit (AST::ImplTraitType &type)
{}
void
Compilation::visit (AST::TraitObjectType &type)
{}
void
Compilation::visit (AST::ParenthesisedType &type)
{}
void
Compilation::visit (AST::ImplTraitTypeOneBound &type)
{}
void
Compilation::visit (AST::TraitObjectTypeOneBound &type)
{}
void
Compilation::visit (AST::TupleType &type)
{}
void
Compilation::visit (AST::NeverType &type)
{}
void
Compilation::visit (AST::RawPointerType &type)
{}
void
Compilation::visit (AST::ReferenceType &type)
{}
void
Compilation::visit (AST::ArrayType &type)
{}
void
Compilation::visit (AST::SliceType &type)
{}
void
Compilation::visit (AST::InferredType &type)
{}
void
Compilation::visit (AST::BareFunctionType &type)
{}

} // namespace Compile
} // namespace Rust
