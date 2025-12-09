// Copyright (C) 2020-2025 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-ast-collector.h"
#include "rust-ast.h"
#include "rust-builtin-ast-nodes.h"
#include "rust-diagnostics.h"
#include "rust-expr.h"
#include "rust-item.h"
#include "rust-keyword-values.h"
#include "rust-path.h"
#include "rust-system.h"
#include "rust-token.h"

namespace Rust {
namespace AST {

std::vector<TokenPtr>
TokenCollector::collect_tokens () const
{
  std::vector<TokenPtr> result;
  for (auto item : tokens)
    {
      if (item.get_kind () == CollectItem::Kind::Token)
	{
	  result.emplace_back (item.get_token ());
	}
    }
  return result;
}

std::vector<CollectItem>
TokenCollector::collect () const
{
  return tokens;
}

void
TokenCollector::visit (AST::Crate &crate)
{
  visit_items_as_lines (crate.inner_attrs);
  visit_items_as_lines (crate.items);
}

void
TokenCollector::visit (AST::Item &item)
{
  item.accept_vis (*this);
}

void
TokenCollector::trailing_comma ()
{
  if (output_trailing_commas)
    {
      push (Rust::Token::make (COMMA, UNDEF_LOCATION));
    }
}

void
TokenCollector::newline ()
{
  tokens.emplace_back (CollectItem::Kind::Newline);
}

void
TokenCollector::indentation ()
{
  tokens.emplace_back (indent_level);
}

void
TokenCollector::increment_indentation ()
{
  indent_level++;
}

void
TokenCollector::decrement_indentation ()
{
  rust_assert (indent_level != 0);
  indent_level--;
}

void
TokenCollector::comment (std::string comment)
{
  tokens.emplace_back (CollectItem::make_comment (comment));
}

void
TokenCollector::describe_node (const std::string &node_name,
			       std::function<void ()> visitor)
{
  tokens.emplace_back (CollectItem::make_begin_node_description (node_name));

  visitor ();

  tokens.push_back (CollectItem::make_end_node_description (node_name));
}

void
TokenCollector::visit (Visitable &v)
{
  v.accept_vis (*this);
}

void
TokenCollector::visit (FunctionParam &param)
{
  describe_node (std::string ("FunctionParam"), [this, &param] () {
    visit_items_as_lines (param.get_outer_attrs ());
    if (!param.is_variadic ())
      {
	visit (param.get_pattern ());
	push (Rust::Token::make (COLON, UNDEF_LOCATION));
	visit (param.get_type ());
      }
    else
      {
	if (param.has_name ())
	  {
	    visit (param.get_pattern ());
	    push (Rust::Token::make (COLON, UNDEF_LOCATION));
	  }
	push (Rust::Token::make (ELLIPSIS, UNDEF_LOCATION));
      }
  });
}

void
TokenCollector::visit (VariadicParam &param)
{
  describe_node (std::string ("VariadicParam"), [this, &param] () {
    if (param.has_pattern ())
      {
	visit (param.get_pattern ());
	push (Rust::Token::make (COLON, UNDEF_LOCATION));
      }
    push (Rust::Token::make (ELLIPSIS, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (Attribute &attrib)
{
  describe_node (std::string ("Attribute"), [this, &attrib] () {
    push (Rust::Token::make (HASH, attrib.get_locus ()));
    if (attrib.is_inner_attribute ())
      push (Rust::Token::make (EXCLAM, UNDEF_LOCATION));
    push (Rust::Token::make (LEFT_SQUARE, UNDEF_LOCATION));
    visit (attrib.get_path ());

    if (attrib.has_attr_input ())
      {
	switch (attrib.get_attr_input ().get_attr_input_type ())
	  {
	  case AST::AttrInput::AttrInputType::LITERAL:
	    {
	      visit (
		static_cast<AttrInputLiteral &> (attrib.get_attr_input ()));
	      break;
	    }
	  case AST::AttrInput::AttrInputType::MACRO:
	    {
	      visit (static_cast<AttrInputMacro &> (attrib.get_attr_input ()));
	      break;
	    }
	  case AST::AttrInput::AttrInputType::META_ITEM:
	    {
	      visit (static_cast<AttrInputMetaItemContainer &> (
		attrib.get_attr_input ()));
	      break;
	    }
	  case AST::AttrInput::AttrInputType::TOKEN_TREE:
	    {
	      visit (static_cast<DelimTokenTree &> (attrib.get_attr_input ()));
	      break;
	    }
	  default:
	    rust_unreachable ();
	  }
      }
    push (Rust::Token::make (RIGHT_SQUARE, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (SimplePath &path)
{
  describe_node (std::string ("SimplePath"), [this, &path] () {
    if (path.has_opening_scope_resolution ())
      {
	push (Rust::Token::make (SCOPE_RESOLUTION, path.get_locus ()));
      }
    visit_items_joined_by_separator (path.get_segments (), SCOPE_RESOLUTION);
  });
}

void
TokenCollector::visit (SimplePathSegment &segment)
{
  describe_node (std::string ("SimplePathSegment"), [this, &segment] () {
    auto name = segment.get_segment_name ();
    if (segment.is_crate_path_seg ())
      {
	push (Rust::Token::make (CRATE, segment.get_locus ()));
      }
    else if (segment.is_super_path_seg ())
      {
	push (Rust::Token::make (SUPER, segment.get_locus ()));
      }
    else if (segment.is_lower_self_seg ())
      {
	push (Rust::Token::make (SELF, segment.get_locus ()));
      }
    else if (segment.is_big_self ())
      {
	push (Rust::Token::make (SELF_ALIAS, segment.get_locus ()));
      }
    else
      {
	push (Rust::Token::make_identifier (segment.get_locus (),
					    std::move (name)));
      }
  });
}

void
TokenCollector::visit (Visibility &vis)
{
  describe_node (std::string ("Visibility"), [this, &vis] () {
    switch (vis.get_vis_type ())
      {
      case Visibility::PUB:
	push (Rust::Token::make (PUB, vis.get_locus ()));
	break;
      case Visibility::PUB_CRATE:
	push (Rust::Token::make (PUB, vis.get_locus ()));
	push (Rust::Token::make (LEFT_PAREN, UNDEF_LOCATION));
	push (Rust::Token::make (CRATE, UNDEF_LOCATION));
	push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
	break;
      case Visibility::PUB_SELF:
	push (Rust::Token::make (PUB, vis.get_locus ()));
	push (Rust::Token::make (LEFT_PAREN, UNDEF_LOCATION));
	push (Rust::Token::make (SELF, UNDEF_LOCATION));
	push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
	break;
      case Visibility::PUB_SUPER:
	push (Rust::Token::make (PUB, vis.get_locus ()));
	push (Rust::Token::make (LEFT_PAREN, UNDEF_LOCATION));
	push (Rust::Token::make (SUPER, UNDEF_LOCATION));
	push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
	break;
      case Visibility::PUB_IN_PATH:
	push (Rust::Token::make (PUB, vis.get_locus ()));
	push (Rust::Token::make (LEFT_PAREN, UNDEF_LOCATION));
	push (Rust::Token::make (IN, UNDEF_LOCATION));
	visit (vis.get_path ());
	push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
	break;
      case Visibility::PRIV:
	break;
      }
  });
}

void
TokenCollector::visit (std::vector<std::unique_ptr<GenericParam>> &params)
{
  describe_node (std::string ("GenericParam"), [this, &params] () {
    push (Rust::Token::make (LEFT_ANGLE, UNDEF_LOCATION));
    visit_items_joined_by_separator (params, COMMA);
    push (Rust::Token::make (RIGHT_ANGLE, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (TupleField &field)
{
  describe_node (std::string ("TupleField"), [this, &field] () {
    for (auto attr : field.get_outer_attrs ())
      {
	visit (attr);
      }
    visit (field.get_visibility ());
    visit (field.get_field_type ());
  });
}

void
TokenCollector::visit (StructField &field)
{
  describe_node (std::string ("StructField"), [this, &field] () {
    for (auto attr : field.get_outer_attrs ())
      {
	visit (attr);
      }
    visit (field.get_visibility ());
    auto name = field.get_field_name ().as_string ();
    push (Rust::Token::make_identifier (field.get_locus (), std::move (name)));
    push (Rust::Token::make (COLON, UNDEF_LOCATION));
    visit (field.get_field_type ());
  });
}

void
TokenCollector::visit (std::vector<LifetimeParam> &for_lifetimes)
{
  describe_node (std::string ("LifetimeParam"), [this, &for_lifetimes] () {
    push (Rust::Token::make (FOR, UNDEF_LOCATION));
    push (Rust::Token::make (LEFT_ANGLE, UNDEF_LOCATION));
    visit_items_joined_by_separator (for_lifetimes, COMMA);
    push (Rust::Token::make (RIGHT_ANGLE, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (FunctionQualifiers &qualifiers)
{
  // Syntax:
  //    `const`? `async`? `unsafe`? (`extern` Abi?)?
  //    unsafe? (extern Abi?)?
  describe_node (std::string ("FunctionQualifiers"), [this, &qualifiers] () {
    if (qualifiers.is_async ())
      push (Rust::Token::make (ASYNC, qualifiers.get_locus ()));
    if (qualifiers.is_const ())
      push (Rust::Token::make (CONST, qualifiers.get_locus ()));
    if (qualifiers.is_unsafe ())
      push (Rust::Token::make (UNSAFE, qualifiers.get_locus ()));
    if (qualifiers.is_extern ())
      {
	push (Rust::Token::make (EXTERN_KW, qualifiers.get_locus ()));
	if (qualifiers.has_abi ())
	  {
	    push (Rust::Token::make_string (UNDEF_LOCATION,
					    qualifiers.get_extern_abi ()));
	  }
      }
  });
}

void
TokenCollector::visit (MaybeNamedParam &param)
{
  // Syntax:
  //     OuterAttribute* ( ( IDENTIFIER | _ ) : )? Type

  describe_node (std::string ("MaybeNamedParam"), [this, &param] () {
    for (auto attr : param.get_outer_attrs ())
      {
	visit (attr);
      }
    auto param_name = param.get_name ().as_string ();
    switch (param.get_param_kind ())
      {
      case MaybeNamedParam::UNNAMED:
	break;
      case MaybeNamedParam::IDENTIFIER:
	push (Rust::Token::make_identifier (UNDEF_LOCATION,
					    std::move (param_name)));
	push (Rust::Token::make (COLON, UNDEF_LOCATION));
	break;
      case MaybeNamedParam::WILDCARD:
	push (Rust::Token::make (UNDERSCORE, UNDEF_LOCATION));
	push (Rust::Token::make (COLON, UNDEF_LOCATION));
	break;
      }
    visit (param.get_type ());
  });
}

void
TokenCollector::visit (Token &tok)
{
  std::string data
    = tok.get_tok_ptr ()->should_have_str () ? tok.get_str () : "";
  switch (tok.get_id ())
    {
    case IDENTIFIER:
      push (Rust::Token::make_identifier (tok.get_locus (), std::move (data)));
      break;
    case INT_LITERAL:
      push (Rust::Token::make_int (tok.get_locus (), std::move (data),
				   tok.get_type_hint ()));
      break;
    case FLOAT_LITERAL:
      push (Rust::Token::make_float (tok.get_locus (), std::move (data),
				     tok.get_type_hint ()));
      break;
    case STRING_LITERAL:
      push (Rust::Token::make_string (tok.get_locus (), std::move (data)));
      break;
    case CHAR_LITERAL:
      push (Rust::Token::make_char (
	tok.get_locus (),
	// FIXME: This need to be fixed to properly support UTF-8
	static_cast<uint32_t> (data[0])));
      break;
    case BYTE_CHAR_LITERAL:
      push (Rust::Token::make_byte_char (tok.get_locus (), data[0]));
      break;
    case BYTE_STRING_LITERAL:
      push (Rust::Token::make_byte_string (tok.get_locus (), std::move (data)));
      break;
    case RAW_STRING_LITERAL:
      push (Rust::Token::make_raw_string (tok.get_locus (), std::move (data)));
      break;
    case INNER_DOC_COMMENT:
      push (Rust::Token::make_inner_doc_comment (tok.get_locus (),
						 std::move (data)));
      break;
    case OUTER_DOC_COMMENT:
      push (Rust::Token::make_outer_doc_comment (tok.get_locus (),
						 std::move (data)));
      break;
    case LIFETIME:
      push (Rust::Token::make_lifetime (tok.get_locus (), std::move (data)));
      break;
    default:
      push (Rust::Token::make (tok.get_id (), tok.get_locus ()));
    }
}

void
TokenCollector::visit (DelimTokenTree &delim_tok_tree)
{
  describe_node (std::string ("DelimTokenTree"), [this, &delim_tok_tree] () {
    for (auto &token : delim_tok_tree.to_token_stream ())
      {
	visit (token);
      }
  });
}

void
TokenCollector::visit (AttrInputMetaItemContainer &container)
{
  describe_node (std::string ("AttrInputMetaItemContainer"),
		 [this, &container] () {
		   for (auto &item : container.get_items ())
		     {
		       visit (item);
		     }
		 });
}

void
TokenCollector::visit (IdentifierExpr &ident_expr)
{
  describe_node (std::string ("IdentifierExpr"), [this, &ident_expr] () {
    auto ident = ident_expr.get_ident ().as_string ();
    push (Rust::Token::make_identifier (ident_expr.get_locus (),
					std::move (ident)));
  });
}

void
TokenCollector::visit (Lifetime &lifetime)
{
  // Syntax:
  // Lifetime :
  // 	LIFETIME_OR_LABEL
  // 	| 'static
  // 	| '_

  describe_node (std::string ("Lifetime"), [this, &lifetime] () {
    auto name = lifetime.get_lifetime_name ();
    switch (lifetime.get_lifetime_type ())
      {
      case Lifetime::LifetimeType::NAMED:
	push (
	  Rust::Token::make_lifetime (lifetime.get_locus (), std::move (name)));
	break;
      case Lifetime::LifetimeType::STATIC:
	push (Rust::Token::make_lifetime (lifetime.get_locus (),
					  Values::Keywords::STATIC_KW));
	break;
      case Lifetime::LifetimeType::WILDCARD:
	push (Rust::Token::make_lifetime (lifetime.get_locus (),
					  Values::Keywords::UNDERSCORE));
	break;
      }
  });
}

void
TokenCollector::visit (LifetimeParam &lifetime_param)
{
  // Syntax:
  //   LIFETIME_OR_LABEL ( : LifetimeBounds )?
  // LifetimeBounds :
  //   ( Lifetime + )* Lifetime?

  // TODO what to do with outer attr? They are not mentioned in the reference.
  describe_node (std::string ("LifetimeParam"), [this, &lifetime_param] () {
    visit_items_as_lines (lifetime_param.get_outer_attrs ());
    auto lifetime = lifetime_param.get_lifetime ();
    visit (lifetime);

    if (lifetime_param.has_lifetime_bounds ())
      {
	push (Rust::Token::make (COLON, UNDEF_LOCATION));
	for (auto &bound : lifetime_param.get_lifetime_bounds ())
	  {
	    visit (bound);
	  }
      }
  });
}

void
TokenCollector::visit (ConstGenericParam &param)
{
  // Syntax:
  // const IDENTIFIER : Type ( = Block | IDENTIFIER | -?LITERAL )?
  describe_node (std::string ("ConstGenericParam"), [this, &param] () {
    visit_items_as_lines (param.get_outer_attrs ());
    push (Rust::Token::make (CONST, param.get_locus ()));
    auto id = param.get_name ().as_string ();
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (id)));
    push (Rust::Token::make (COLON, UNDEF_LOCATION));
    if (param.has_type ())
      visit (param.get_type ());
    if (param.has_default_value ())
      {
	push (Rust::Token::make (EQUAL, UNDEF_LOCATION));
	visit (param.get_default_value_unchecked ());
      }
  });
}

void
TokenCollector::visit (PathExprSegment &segment)
{
  describe_node (std::string ("PathExprSegment"), [this, &segment] () {
    visit (segment.get_ident_segment ());
    if (segment.has_generic_args ())
      {
	auto generics = segment.get_generic_args ();
	push (Rust::Token::make (SCOPE_RESOLUTION, segment.get_locus ()));
	push (Rust::Token::make (LEFT_ANGLE, generics.get_locus ()));

	auto &lifetime_args = generics.get_lifetime_args ();
	auto &generic_args = generics.get_generic_args ();
	auto &binding_args = generics.get_binding_args ();

	visit_items_joined_by_separator (generic_args, COMMA);

	if (!lifetime_args.empty ()
	    && (!generic_args.empty () || !binding_args.empty ()))
	  {
	    push (Rust::Token::make (COMMA, UNDEF_LOCATION));
	  }

	visit_items_joined_by_separator (binding_args, COMMA);

	if (!generic_args.empty () && !binding_args.empty ())
	  {
	    push (Rust::Token::make (COMMA, UNDEF_LOCATION));
	  }

	visit_items_joined_by_separator (lifetime_args, COMMA);

	push (Rust::Token::make (RIGHT_ANGLE, UNDEF_LOCATION));
      }
  });
}

void
TokenCollector::visit (PathInExpression &path)
{
  describe_node (std::string ("PathInExpression"), [this, &path] () {
    if (path.is_lang_item ())
      {
	push (Rust::Token::make (TokenId::HASH, path.get_locus ()));
	push (Rust::Token::make (TokenId::LEFT_SQUARE, path.get_locus ()));
	push (Rust::Token::make_identifier (path.get_locus (), "lang"));
	push (Rust::Token::make (TokenId::EQUAL, path.get_locus ()));
	push (Rust::Token::make_string (
	  path.get_locus (), LangItem::ToString (path.get_lang_item ())));
	push (Rust::Token::make (TokenId::RIGHT_SQUARE, path.get_locus ()));

	return;
      }

    if (path.opening_scope_resolution ())
      push (Rust::Token::make (SCOPE_RESOLUTION, path.get_locus ()));

    visit_items_joined_by_separator (path.get_segments (), SCOPE_RESOLUTION);
  });
}

void
TokenCollector::visit (TypePathSegment &segment)
{
  // Syntax:
  //    PathIdentSegment
  describe_node (std::string ("TypePathSegment"), [this, &segment] () {
    auto locus = segment.is_lang_item ()
		   ? segment.get_locus ()
		   : segment.get_ident_segment ().get_locus ();
    auto segment_string = segment.is_lang_item ()
			    ? LangItem::PrettyString (segment.get_lang_item ())
			    : segment.get_ident_segment ().as_string ();
    push (Rust::Token::make_identifier (locus, std::move (segment_string)));
  });
}

void
TokenCollector::visit (TypePathSegmentGeneric &segment)
{
  // Syntax:
  //    PathIdentSegment `::`? (GenericArgs)?
  // GenericArgs :
  //    `<` `>`
  //    | `<` ( GenericArg `,` )* GenericArg `,`? `>`
  describe_node (std::string ("TypePathSegmentGeneric"), [this, &segment] () {
    auto ident_segment = segment.get_ident_segment ();
    auto id = ident_segment.as_string ();
    push (Rust::Token::make_identifier (ident_segment.get_locus (),
					std::move (id)));

    auto locus = segment.is_lang_item ()
		   ? segment.get_locus ()
		   : segment.get_ident_segment ().get_locus ();
    auto segment_string = segment.is_lang_item ()
			    ? LangItem::PrettyString (segment.get_lang_item ())
			    : segment.get_ident_segment ().as_string ();
    push (Rust::Token::make_identifier (locus, std::move (segment_string)));

    push (Rust::Token::make (LEFT_ANGLE, UNDEF_LOCATION));

    {
      auto &lifetime_args = segment.get_generic_args ().get_lifetime_args ();
      auto &generic_args = segment.get_generic_args ().get_generic_args ();
      auto &binding_args = segment.get_generic_args ().get_binding_args ();

      visit_items_joined_by_separator (lifetime_args, COMMA);
      if (!lifetime_args.empty ()
	  && (!generic_args.empty () || !binding_args.empty ()))
	push (Rust::Token::make (COMMA, UNDEF_LOCATION));
      visit_items_joined_by_separator (generic_args, COMMA);
      if (!generic_args.empty () && !binding_args.empty ())
	push (Rust::Token::make (COMMA, UNDEF_LOCATION));
      visit_items_joined_by_separator (binding_args, COMMA);
    }

    push (Rust::Token::make (RIGHT_ANGLE, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (GenericArgsBinding &binding)
{
  // Syntax:
  //    IDENTIFIER `=` Type
  describe_node (std::string ("GenericArgsBinding"), [this, &binding] () {
    auto identifier = binding.get_identifier ().as_string ();
    push (Rust::Token::make_identifier (binding.get_locus (),
					std::move (identifier)));

    push (Rust::Token::make (EQUAL, UNDEF_LOCATION));
    visit (binding.get_type ());
  });
}

void
TokenCollector::visit (GenericArg &arg)
{
  // `GenericArg` implements `accept_vis` but it is not useful for this case
  // as it ignores unresolved cases (`Kind::Either`).
  describe_node (std::string ("GenericArg"), [this, &arg] () {
    switch (arg.get_kind ())
      {
      case GenericArg::Kind::Const:
	visit (arg.get_expression ());
	break;
      case GenericArg::Kind::Type:
	visit (arg.get_type ());
	break;
      case GenericArg::Kind::Either:
	{
	  auto path = arg.get_path ();
	  push (
	    Rust::Token::make_identifier (UNDEF_LOCATION, std::move (path)));
	}
	break;
      }
  });
}

void
TokenCollector::visit (TypePathSegmentFunction &segment)
{
  // Syntax:
  //   PathIdentSegment `::`? (TypePathFn)?
  describe_node (std::string ("TypePathSegmentFunction"), [this, &segment] () {
    auto ident_segment = segment.get_ident_segment ();
    auto id = ident_segment.as_string ();
    push (Rust::Token::make_identifier (ident_segment.get_locus (),
					std::move (id)));

    if (segment.get_separating_scope_resolution ())
      push (Rust::Token::make (SCOPE_RESOLUTION, UNDEF_LOCATION));

    if (!segment.is_ident_only ())
      visit (segment.get_type_path_function ());
  });
}

void
TokenCollector::visit (TypePathFunction &type_path_fn)
{
  // Syntax:
  //   `(` TypePathFnInputs? `)` (`->` Type)?
  // TypePathFnInputs :
  //   Type (`,` Type)* `,`?
  describe_node (std::string ("TypePathFunction"), [this, &type_path_fn] () {
    push (Rust::Token::make (LEFT_PAREN, type_path_fn.get_locus ()));
    if (type_path_fn.has_inputs ())
      visit_items_joined_by_separator (type_path_fn.get_params (), COMMA);
    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));

    if (type_path_fn.has_return_type ())
      {
	push (Rust::Token::make (RETURN_TYPE, UNDEF_LOCATION));
	visit (type_path_fn.get_return_type ());
      }
  });
}

void
TokenCollector::visit (TypePath &path)
{
  // Syntax:
  //    `::`? TypePathSegment (`::` TypePathSegment)*
  describe_node (std::string ("TypePath"), [this, &path] () {
    if (path.has_opening_scope_resolution_op ())
      push (Rust::Token::make (SCOPE_RESOLUTION, path.get_locus ()));

    visit_items_joined_by_separator (path.get_segments (), SCOPE_RESOLUTION);
  });
}

void
TokenCollector::visit (PathIdentSegment &segment)
{
  describe_node (std::string ("PathIdentSegment"), [this, &segment] () {
    if (segment.is_super_path_seg ())
      {
	push (Rust::Token::make (SUPER, segment.get_locus ()));
      }
    else if (segment.is_crate_path_seg ())
      {
	push (Rust::Token::make (CRATE, segment.get_locus ()));
      }
    else if (segment.is_lower_self_seg ())
      {
	push (Rust::Token::make (SELF, segment.get_locus ()));
      }
    else if (segment.is_big_self_seg ())
      {
	push (Rust::Token::make (SELF_ALIAS, segment.get_locus ()));
      }
    else
      {
	auto id = segment.as_string ();
	push (
	  Rust::Token::make_identifier (segment.get_locus (), std::move (id)));
      }
  });
}

void
TokenCollector::visit (QualifiedPathInExpression &path)
{
  describe_node (std::string ("QualifiedPathInExpression"), [this, &path] () {
    visit (path.get_qualified_path_type ());
    for (auto &segment : path.get_segments ())
      {
	push (Rust::Token::make (SCOPE_RESOLUTION, UNDEF_LOCATION));
	visit (segment);
      }
  });
}

void
TokenCollector::visit (QualifiedPathType &path)
{
  describe_node (std::string ("QualifiedPathType"), [this, &path] () {
    push (Rust::Token::make (LEFT_ANGLE, path.get_locus ()));
    visit (path.get_type ());
    if (path.has_as_clause ())
      {
	push (Rust::Token::make (AS, UNDEF_LOCATION));
	visit (path.get_as_type_path ());
      }
    push (Rust::Token::make (RIGHT_ANGLE, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (QualifiedPathInType &path)
{
  describe_node (std::string ("QualifiedPathInType"), [this, &path] () {
    visit (path.get_qualified_path_type ());

    push (Rust::Token::make (SCOPE_RESOLUTION, UNDEF_LOCATION));
    visit (path.get_associated_segment ());
    for (auto &segment : path.get_segments ())
      {
	push (Rust::Token::make (SCOPE_RESOLUTION, UNDEF_LOCATION));
	visit (segment);
      }
  });
}

void
TokenCollector::visit (Literal &lit, location_t locus)
{
  auto value = lit.as_string ();
  switch (lit.get_lit_type ())
    {
    case Literal::LitType::CHAR:
      push (
	Rust::Token::make_char (locus,
				// TODO: Change this to support utf-8 properly
				Codepoint (static_cast<uint32_t> (value[0]))));
      break;
    case Literal::LitType::STRING:
      push (Rust::Token::make_string (locus, std::move (value)));
      break;
    case Literal::LitType::BYTE:
      push (Rust::Token::make_byte_char (locus, value[0]));
      break;
    case Literal::LitType::BYTE_STRING:
      push (Rust::Token::make_byte_string (locus, std::move (value)));
      break;
    case Literal::LitType::RAW_STRING:
      push (Rust::Token::make_raw_string (locus, std::move (value)));
      break;
    case Literal::LitType::INT:
      push (
	Rust::Token::make_int (locus, std::move (value), lit.get_type_hint ()));
      break;
    case Literal::LitType::FLOAT:
      push (Rust::Token::make_float (locus, std::move (value),
				     lit.get_type_hint ()));
      break;
    case Literal::LitType::BOOL:
      {
	if (value == Values::Keywords::FALSE_LITERAL)
	  push (Rust::Token::make (FALSE_LITERAL, locus));
	else if (value == Values::Keywords::TRUE_LITERAL)
	  push (Rust::Token::make (TRUE_LITERAL, locus));
	else
	  rust_unreachable (); // Not a boolean
	break;
      }
    case Literal::LitType::ERROR:
      rust_unreachable ();
      break;
    }
}

void
TokenCollector::visit (LiteralExpr &expr)
{
  describe_node (std::string ("LiteralExpr"), [this, &expr] () {
    auto lit = expr.get_literal ();
    visit (lit, expr.get_locus ());
  });
}

void
TokenCollector::visit (AttrInputLiteral &literal)
{
  describe_node (std::string ("AttrInputLiteral"), [this, &literal] () {
    push (Rust::Token::make (EQUAL, UNDEF_LOCATION));
    visit (literal.get_literal ());
  });
}

void
TokenCollector::visit (AttrInputMacro &macro)
{
  describe_node (std::string ("AttrInputMacro"), [this, &macro] () {
    push (Rust::Token::make (EQUAL, UNDEF_LOCATION));
    visit (macro.get_macro ());
  });
}

void
TokenCollector::visit (MetaItemLitExpr &item)
{
  describe_node (std::string ("MetaItemLitExpr"), [this, &item] () {
    auto lit = item.get_literal ();
    visit (lit);
  });
}

void
TokenCollector::visit (MetaItemPathExpr &item)
{
  describe_node (std::string ("MetaItemPathLit"), [this, &item] () {
    auto &path = item.get_path ();
    auto &expr = item.get_expr ();
    visit (path);
    push (Rust::Token::make (EQUAL, item.get_locus ()));
    visit (expr);
  });
}

void
TokenCollector::visit (BorrowExpr &expr)
{
  describe_node (std::string ("BorrowExpr"), [this, &expr] () {
    push (Rust::Token::make (AMP, expr.get_locus ()));
    if (expr.get_is_double_borrow ())
      push (Rust::Token::make (AMP, UNDEF_LOCATION));

    if (expr.is_raw_borrow ())
      {
	push (Rust::Token::make_identifier (expr.get_locus (),
					    Values::WeakKeywords::RAW));
	if (expr.get_is_mut ())
	  push (Rust::Token::make (MUT, UNDEF_LOCATION));
	else
	  push (Rust::Token::make (CONST, UNDEF_LOCATION));
      }
    else
      {
	if (expr.get_is_mut ())
	  push (Rust::Token::make (MUT, UNDEF_LOCATION));
      }

    if (expr.is_raw_borrow ())
      {
	push (Rust::Token::make_identifier (expr.get_locus (),
					    Values::WeakKeywords::RAW));
	if (expr.get_is_mut ())
	  push (Rust::Token::make (MUT, UNDEF_LOCATION));
	else
	  push (Rust::Token::make (CONST, UNDEF_LOCATION));
      }
    else
      {
	if (expr.get_is_mut ())
	  push (Rust::Token::make (MUT, UNDEF_LOCATION));
      }

    if (expr.has_borrow_expr ())
      visit (expr.get_borrowed_expr ());
  });
}

void
TokenCollector::visit (DereferenceExpr &expr)
{
  describe_node (std::string ("DereferenceExpr"), [this, &expr] () {
    push (Rust::Token::make (ASTERISK, expr.get_locus ()));
    visit (expr.get_dereferenced_expr ());
  });
}

void
TokenCollector::visit (ErrorPropagationExpr &expr)
{
  describe_node (std::string ("ErrorPropagationExpr"), [this, &expr] () {
    visit (expr.get_propagating_expr ());
    push (Rust::Token::make (QUESTION_MARK, expr.get_locus ()));
  });
}

void
TokenCollector::visit (NegationExpr &expr)
{
  describe_node (std::string ("NegationExpr"), [this, &expr] () {
    switch (expr.get_expr_type ())
      {
      case NegationOperator::NEGATE:
	push (Rust::Token::make (MINUS, expr.get_locus ()));
	break;
      case NegationOperator::NOT:
	push (Rust::Token::make (EXCLAM, expr.get_locus ()));
	break;
      }
    visit (expr.get_negated_expr ());
  });
}

void
TokenCollector::visit (ArithmeticOrLogicalExpr &expr)
{
  describe_node (std::string ("ArithmeticOrLogicalExpr"), [this, &expr] () {
    visit (expr.get_left_expr ());
    switch (expr.get_expr_type ())
      {
      case ArithmeticOrLogicalOperator::ADD:
	push (Rust::Token::make (PLUS, expr.get_locus ()));
	break;

      case ArithmeticOrLogicalOperator::SUBTRACT:
	push (Rust::Token::make (MINUS, expr.get_locus ()));
	break;

      case ArithmeticOrLogicalOperator::MULTIPLY:
	push (Rust::Token::make (ASTERISK, expr.get_locus ()));
	break;

      case ArithmeticOrLogicalOperator::DIVIDE:
	push (Rust::Token::make (DIV, expr.get_locus ()));
	break;

      case ArithmeticOrLogicalOperator::MODULUS:
	push (Rust::Token::make (PERCENT, expr.get_locus ()));
	break;

      case ArithmeticOrLogicalOperator::BITWISE_AND:
	push (Rust::Token::make (AMP, expr.get_locus ()));
	break;

      case ArithmeticOrLogicalOperator::BITWISE_OR:
	push (Rust::Token::make (PIPE, expr.get_locus ()));
	break;

      case ArithmeticOrLogicalOperator::BITWISE_XOR:
	push (Rust::Token::make (CARET, expr.get_locus ()));
	break;

      case ArithmeticOrLogicalOperator::LEFT_SHIFT:
	push (Rust::Token::make (LEFT_SHIFT, expr.get_locus ()));
	break;

      case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
	push (Rust::Token::make (RIGHT_SHIFT, expr.get_locus ()));
	break;
      }

    visit (expr.get_right_expr ());
  });
}

void
TokenCollector::visit (ComparisonExpr &expr)
{
  describe_node (std::string ("ComparisonExpr"), [this, &expr] () {
    visit (expr.get_left_expr ());

    switch (expr.get_expr_type ())
      {
      case ComparisonOperator::EQUAL:
	push (Rust::Token::make (EQUAL_EQUAL, expr.get_locus ()));
	break;
      case ComparisonOperator::NOT_EQUAL:
	push (Rust::Token::make (NOT_EQUAL, expr.get_locus ()));
	break;
      case ComparisonOperator::GREATER_THAN:
	push (Rust::Token::make (RIGHT_ANGLE, expr.get_locus ()));
	break;
      case ComparisonOperator::LESS_THAN:
	push (Rust::Token::make (LEFT_ANGLE, expr.get_locus ()));
	break;
      case ComparisonOperator::GREATER_OR_EQUAL:
	push (Rust::Token::make (GREATER_OR_EQUAL, expr.get_locus ()));
	break;

      case ComparisonOperator::LESS_OR_EQUAL:
	push (Rust::Token::make (LESS_OR_EQUAL, expr.get_locus ()));
	break;
      }
    visit (expr.get_right_expr ());
  });
}

void
TokenCollector::visit (LazyBooleanExpr &expr)
{
  describe_node (std::string ("LazyBooleanExpr"), [this, &expr] () {
    visit (expr.get_left_expr ());

    switch (expr.get_expr_type ())
      {
      case LazyBooleanOperator::LOGICAL_AND:
	push (Rust::Token::make (LOGICAL_AND, expr.get_locus ()));
	break;
      case LazyBooleanOperator::LOGICAL_OR:
	push (Rust::Token::make (OR, expr.get_locus ()));
	break;
      }

    visit (expr.get_right_expr ());
  });
}

void
TokenCollector::visit (TypeCastExpr &expr)
{
  describe_node (std::string ("TypeCastExpr"), [this, &expr] () {
    visit (expr.get_casted_expr ());
    push (Rust::Token::make (AS, expr.get_locus ()));
    visit (expr.get_type_to_cast_to ());
  });
}

void
TokenCollector::visit (AssignmentExpr &expr)
{
  describe_node (std::string ("AssignementExpr"), [this, &expr] () {
    expr.visit_lhs (*this);
    push (Rust::Token::make (EQUAL, expr.get_locus ()));
    expr.visit_rhs (*this);
  });
}

void
TokenCollector::visit (CompoundAssignmentExpr &expr)
{
  describe_node (std::string ("CompoundAssignmentExpr"), [this, &expr] () {
    visit (expr.get_left_expr ());

    switch (expr.get_expr_type ())
      {
      case CompoundAssignmentOperator::ADD:
	push (Rust::Token::make (PLUS_EQ, expr.get_locus ()));
	break;
      case CompoundAssignmentOperator::SUBTRACT:
	push (Rust::Token::make (MINUS_EQ, expr.get_locus ()));
	break;
      case CompoundAssignmentOperator::MULTIPLY:
	push (Rust::Token::make (ASTERISK_EQ, expr.get_locus ()));
	break;
      case CompoundAssignmentOperator::DIVIDE:
	push (Rust::Token::make (DIV_EQ, expr.get_locus ()));
	break;
      case CompoundAssignmentOperator::MODULUS:
	push (Rust::Token::make (PERCENT_EQ, expr.get_locus ()));
	break;
      case CompoundAssignmentOperator::BITWISE_AND:
	push (Rust::Token::make (AMP_EQ, expr.get_locus ()));
	break;
      case CompoundAssignmentOperator::BITWISE_OR:
	push (Rust::Token::make (PIPE_EQ, expr.get_locus ()));
	break;
      case CompoundAssignmentOperator::BITWISE_XOR:
	push (Rust::Token::make (CARET_EQ, expr.get_locus ()));
	break;
      case CompoundAssignmentOperator::LEFT_SHIFT:
	push (Rust::Token::make (LEFT_SHIFT_EQ, expr.get_locus ()));
	break;
      case CompoundAssignmentOperator::RIGHT_SHIFT:
	push (Rust::Token::make (RIGHT_SHIFT_EQ, expr.get_locus ()));
	break;
      }
    visit (expr.get_right_expr ());
  });
}

void
TokenCollector::visit (GroupedExpr &expr)
{
  describe_node (std::string ("GroupedExpr"), [this, &expr] () {
    push (Rust::Token::make (LEFT_PAREN, expr.get_locus ()));
    visit (expr.get_expr_in_parens ());
    push (Rust::Token::make (RIGHT_PAREN, expr.get_locus ()));
  });
}

void
TokenCollector::visit (ArrayElemsValues &elems)
{
  describe_node (std::string ("ArraysElemValues"), [this, &elems] () {
    visit_items_joined_by_separator (elems.get_values (), COMMA);
  });
}

void
TokenCollector::visit (ArrayElemsCopied &elems)
{
  describe_node (std::string ("ArrayElemsCopied"), [this, &elems] () {
    visit (elems.get_elem_to_copy ());
    push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
    visit (elems.get_num_copies ());
  });
}

void
TokenCollector::visit (ArrayExpr &expr)
{
  describe_node (std::string ("ArrayExpr"), [this, &expr] () {
    push (Rust::Token::make (LEFT_SQUARE, expr.get_locus ()));
    visit (expr.get_array_elems ());
    push (Rust::Token::make (RIGHT_SQUARE, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (ArrayIndexExpr &expr)
{
  describe_node (std::string ("ArrayIndexExpr"), [this, &expr] () {
    visit (expr.get_array_expr ());
    push (Rust::Token::make (LEFT_SQUARE, expr.get_locus ()));
    visit (expr.get_index_expr ());
    push (Rust::Token::make (RIGHT_SQUARE, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (TupleExpr &expr)
{
  describe_node (std::string ("TupleExpr"), [this, &expr] () {
    visit_items_as_lines (expr.get_outer_attrs ());
    push (Rust::Token::make (LEFT_PAREN, expr.get_locus ()));
    visit_items_joined_by_separator (expr.get_tuple_elems (), COMMA);
    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (TupleIndexExpr &expr)
{
  describe_node (std::string ("TupleIndexExpr"), [this, &expr] () {
    visit (expr.get_tuple_expr ());
    push (Rust::Token::make (DOT, expr.get_locus ()));
    push (Rust::Token::make_int (UNDEF_LOCATION,
				 std::to_string (expr.get_tuple_index ())));
  });
}

void
TokenCollector::visit (StructExprStruct &expr)
{
  describe_node (std::string ("StructExprStruct"),
		 [this, &expr] () { visit (expr.get_struct_name ()); });
}

void
TokenCollector::visit (StructExprFieldIdentifier &expr)
{
  describe_node (std::string ("StructExprFieldIdentifier"), [this, &expr] () {
    visit_items_as_lines (expr.get_outer_attrs ());
    auto id = expr.get_field_name ().as_string ();
    push (Rust::Token::make_identifier (expr.get_locus (), std::move (id)));
  });
}

void
TokenCollector::visit (StructExprFieldIdentifierValue &expr)
{
  describe_node (std::string ("StructExprFieldIdentifierValue"), [this,
								  &expr] () {
    visit_items_as_lines (expr.get_outer_attrs ());
    auto id = expr.get_field_name ();
    push (Rust::Token::make_identifier (expr.get_locus (), std::move (id)));
    push (Rust::Token::make (COLON, UNDEF_LOCATION));
    visit (expr.get_value ());
  });
}

void
TokenCollector::visit (StructExprFieldIndexValue &expr)
{
  describe_node (std::string ("StructExprFieldIndexValue"), [this, &expr] () {
    visit_items_as_lines (expr.get_outer_attrs ());
    push (Rust::Token::make_int (expr.get_locus (),
				 std::to_string (expr.get_index ())));
    push (Rust::Token::make (COLON, UNDEF_LOCATION));
    visit (expr.get_value ());
  });
}

void
TokenCollector::visit (StructBase &base)
{
  describe_node (std::string ("StructBase"), [this, &base] () {
    push (Rust::Token::make (DOT_DOT, UNDEF_LOCATION));
    visit (base.get_base_struct ());
  });
}

void
TokenCollector::visit (StructExprStructFields &expr)
{
  describe_node (std::string ("StructExprStructFields"), [this, &expr] () {
    visit (expr.get_struct_name ());
    push (Rust::Token::make (LEFT_CURLY, expr.get_locus ()));
    visit_items_joined_by_separator (expr.get_fields (), COMMA);
    if (expr.has_struct_base ())
      {
	push (Rust::Token::make (COMMA, UNDEF_LOCATION));
	visit (expr.get_struct_base ());
      }
    else
      {
	trailing_comma ();
      }
    push (Rust::Token::make (RIGHT_CURLY, expr.get_locus ()));
  });
}

void
TokenCollector::visit (StructExprStructBase &)
{
  // FIXME: Implement this node
  rust_unreachable ();
}

void
TokenCollector::visit (CallExpr &expr)
{
  describe_node (std::string ("CallExpr"), [this, &expr] () {
    visit (expr.get_function_expr ());

    push (Rust::Token::make (LEFT_PAREN, UNDEF_LOCATION));

    visit_items_joined_by_separator (expr.get_params (), COMMA);

    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (MethodCallExpr &expr)
{
  describe_node (std::string ("MethodCallExpr"), [this, &expr] () {
    visit (expr.get_receiver_expr ());
    push (Rust::Token::make (DOT, expr.get_locus ()));
    visit (expr.get_method_name ());
    push (Rust::Token::make (LEFT_PAREN, UNDEF_LOCATION));
    visit_items_joined_by_separator (expr.get_params (), COMMA);
    trailing_comma ();
    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (FieldAccessExpr &expr)
{
  describe_node (std::string ("FieldAccessExpr"), [this, &expr] () {
    visit (expr.get_receiver_expr ());
    push (Rust::Token::make (DOT, expr.get_locus ()));
    auto field_name = expr.get_field_name ().as_string ();
    push (
      Rust::Token::make_identifier (UNDEF_LOCATION, std::move (field_name)));
  });
}

void
TokenCollector::visit (ClosureParam &param)
{
  describe_node (std::string ("ClosureParam"), [this, &param] () {
    visit_items_as_lines (param.get_outer_attrs ());
    visit (param.get_pattern ());
    if (param.has_type_given ())
      {
	push (Rust::Token::make (COLON, param.get_locus ()));
	visit (param.get_type ());
      }
  });
}

void
TokenCollector::visit_closure_common (ClosureExpr &expr)
{
  describe_node (std::string ("ClosureExpr"), [this, &expr] () {
    if (expr.get_has_move ())
      {
	push (Rust::Token::make (MOVE, expr.get_locus ()));
      }
    push (Rust::Token::make (PIPE, UNDEF_LOCATION));
    visit_items_joined_by_separator (expr.get_params (), COMMA);
    push (Rust::Token::make (PIPE, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (ClosureExprInner &expr)
{
  describe_node (std::string ("ClosureExprInner"), [this, &expr] () {
    visit_closure_common (expr);
    visit (expr.get_definition_expr ());
  });
}

void
TokenCollector::visit (BlockExpr &expr)
{
  describe_node (std::string ("BlockExpr"), [this, &expr] () {
    visit_items_as_lines (expr.get_outer_attrs ());
    push (Rust::Token::make (LEFT_CURLY, expr.get_locus ()));
    newline ();
    increment_indentation ();
    visit_items_as_lines (expr.get_inner_attrs ());

    visit_items_as_lines (expr.get_statements (), {});

    if (expr.has_tail_expr ())
      {
	indentation ();
	visit (expr.get_tail_expr ());
	newline ();
      }

    decrement_indentation ();
    indentation ();
    push (Rust::Token::make (RIGHT_CURLY, expr.get_locus ()));
    newline ();
  });
}

void
TokenCollector::visit (AnonConst &expr)
{
  if (!expr.is_deferred ())
    {
      visit (expr.get_inner_expr ());
      return;
    }

  push (Rust::Token::make_string (expr.get_locus (), "_"));
}

void
TokenCollector::visit (ConstBlock &expr)
{
  push (Rust::Token::make (CONST, expr.get_locus ()));

  // The inner expression is already a block expr, so we don't need to add
  // curlies
  visit (expr.get_const_expr ());
}

void
TokenCollector::visit (ClosureExprInnerTyped &expr)
{
  describe_node (std::string ("ClosureExprInnerTyped"), [this, &expr] () {
    visit_closure_common (expr);
    push (Rust::Token::make (RETURN_TYPE, expr.get_locus ()));
    visit (expr.get_return_type ());

    visit (expr.get_definition_expr ());
  });
}

void
TokenCollector::visit (ContinueExpr &expr)
{
  describe_node (std::string ("ContinueExpr"), [this, &expr] () {
    push (Rust::Token::make (CONTINUE, expr.get_locus ()));
    if (expr.has_label ())
      visit (expr.get_label_unchecked ());
  });
}

void
TokenCollector::visit (BreakExpr &expr)
{
  describe_node (std::string ("BreakExpr"), [this, &expr] () {
    push (Rust::Token::make (BREAK, expr.get_locus ()));
    if (expr.has_label ())
      visit (expr.get_label_unchecked ());
    if (expr.has_break_expr ())
      visit (expr.get_break_expr ());
  });
}

void
TokenCollector::visit (RangeFromToExpr &expr)
{
  describe_node (std::string ("RangeFromToExpr"), [this, &expr] () {
    visit (expr.get_from_expr ());
    push (Rust::Token::make (DOT_DOT, expr.get_locus ()));
    visit (expr.get_to_expr ());
  });
}

void
TokenCollector::visit (RangeFromExpr &expr)
{
  describe_node (std::string ("RangeFromExpr"), [this, &expr] () {
    visit (expr.get_from_expr ());
    push (Rust::Token::make (DOT_DOT, expr.get_locus ()));
  });
}

void
TokenCollector::visit (RangeToExpr &expr)
{
  describe_node (std::string ("RangeToExpr"), [this, &expr] () {
    push (Rust::Token::make (DOT_DOT, expr.get_locus ()));
    visit (expr.get_to_expr ());
  });
}

void
TokenCollector::visit (RangeFullExpr &expr)
{
  describe_node (std::string ("RangeFullExpr"), [this, &expr] () {
    push (Rust::Token::make (DOT_DOT, expr.get_locus ()));
  });
}

void
TokenCollector::visit (RangeFromToInclExpr &expr)
{
  describe_node (std::string ("RangeFromToInclExpr"), [this, &expr] () {
    visit (expr.get_from_expr ());
    push (Rust::Token::make (DOT_DOT_EQ, expr.get_locus ()));
    visit (expr.get_to_expr ());
  });
}

void
TokenCollector::visit (RangeToInclExpr &expr)
{
  describe_node (std::string ("RangeToInclExpr"), [this, &expr] () {
    push (Rust::Token::make (DOT_DOT_EQ, expr.get_locus ()));
    visit (expr.get_to_expr ());
  });
}

void
TokenCollector::visit (BoxExpr &expr)
{
  describe_node (std::string ("BoxExpr"), [this, &expr] () {
    push (Rust::Token::make (BOX, expr.get_locus ()));
    visit (expr.get_boxed_expr ());
  });
}

void
TokenCollector::visit (ReturnExpr &expr)
{
  describe_node (std::string ("ReturnExpr"), [this, &expr] () {
    push (Rust::Token::make (RETURN_KW, expr.get_locus ()));
    if (expr.has_returned_expr ())
      visit (expr.get_returned_expr ());
  });
}

void
TokenCollector::visit (TryExpr &expr)
{
  push (Rust::Token::make (TRY, expr.get_locus ()));
  visit (expr.get_block_expr ());
}

void
TokenCollector::visit (UnsafeBlockExpr &expr)
{
  describe_node (std::string ("UnsafeBlockExpr"), [this, &expr] () {
    push (Rust::Token::make (UNSAFE, expr.get_locus ()));
    visit (expr.get_block_expr ());
  });
}

void
TokenCollector::visit (LoopLabel &label)
{
  describe_node (std::string ("LoopLabel"), [this, &label] () {
    visit (label.get_lifetime ());
    push (Rust::Token::make (COLON, label.get_locus ()));
  });
}

void
TokenCollector::visit_loop_common (BaseLoopExpr &expr)
{
  describe_node (std::string ("BaseLoopExpr"), [this, &expr] () {
    if (expr.has_loop_label ())
      visit (expr.get_loop_label ());
  });
}

void
TokenCollector::visit (LoopExpr &expr)
{
  describe_node (std::string ("LoopExpr"), [this, &expr] () {
    visit_loop_common (expr);
    push (Rust::Token::make (LOOP, expr.get_locus ()));
    visit (expr.get_loop_block ());
  });
}

void
TokenCollector::visit (WhileLoopExpr &expr)
{
  describe_node (std::string ("WhileLoopExpr"), [this, &expr] () {
    visit_loop_common (expr);
    push (Rust::Token::make (WHILE, expr.get_locus ()));
    visit (expr.get_predicate_expr ());
    visit (expr.get_loop_block ());
  });
}

void
TokenCollector::visit (WhileLetLoopExpr &expr)
{
  describe_node (std::string ("WhileLetLoopExpr"), [this, &expr] () {
    visit_loop_common (expr);
    push (Rust::Token::make (WHILE, expr.get_locus ()));
    push (Rust::Token::make (LET, UNDEF_LOCATION));
    // TODO: The reference mention only one Pattern
    for (auto &item : expr.get_patterns ())
      {
	visit (item);
      }
    push (Rust::Token::make (EQUAL, UNDEF_LOCATION));
    visit (expr.get_scrutinee_expr ());
    visit (expr.get_loop_block ());
  });
}

void
TokenCollector::visit (ForLoopExpr &expr)
{
  describe_node (std::string ("ForLoopExpr"), [this, &expr] () {
    visit_loop_common (expr);
    push (Rust::Token::make (FOR, expr.get_locus ()));
    visit (expr.get_pattern ());
    push (Rust::Token::make (IN, UNDEF_LOCATION));
    visit (expr.get_iterator_expr ());
    visit (expr.get_loop_block ());
  });
}

void
TokenCollector::visit (IfExpr &expr)
{
  describe_node (std::string ("IfExpr"), [this, &expr] () {
    push (Rust::Token::make (IF, expr.get_locus ()));

    visit (expr.get_condition_expr ());
    visit (expr.get_if_block ());
  });
}

void
TokenCollector::visit (IfExprConseqElse &expr)
{
  describe_node (std::string ("IfExprConseqElse"), [this, &expr] () {
    visit (static_cast<IfExpr &> (expr));
    indentation ();
    push (Rust::Token::make (ELSE, expr.get_locus ()));
    visit (expr.get_else_block ());
  });
}

void
TokenCollector::visit (IfLetExpr &expr)
{
  describe_node (std::string ("IfLetExpr"), [this, &expr] () {
    push (Rust::Token::make (IF, expr.get_locus ()));
    push (Rust::Token::make (LET, UNDEF_LOCATION));
    for (auto &pattern : expr.get_patterns ())
      {
	visit (pattern);
      }
    push (Rust::Token::make (EQUAL, UNDEF_LOCATION));
    visit (expr.get_value_expr ());
    visit (expr.get_if_block ());
  });
}

void
TokenCollector::visit (IfLetExprConseqElse &expr)
{
  describe_node (std::string ("IfLetExprConseqElse"), [this, &expr] () {
    visit (static_cast<IfLetExpr &> (expr));
    indentation ();
    push (Rust::Token::make (ELSE, expr.get_locus ()));
    visit (expr.get_else_block ());
  });
}

void
TokenCollector::visit (MatchArm &arm)
{
  describe_node (std::string ("MatchArm"), [this, &arm] () {
    visit_items_as_lines (arm.get_outer_attrs ());
    for (auto &pattern : arm.get_patterns ())
      {
	visit (pattern);
      }
    if (arm.has_match_arm_guard ())
      {
	push (Rust::Token::make (IF, UNDEF_LOCATION));
	visit (arm.get_guard_expr ());
      }
  });
}

void
TokenCollector::visit (MatchCase &match_case)
{
  describe_node (std::string ("MatchCase"), [this, &match_case] () {
    indentation ();
    visit (match_case.get_arm ());
    push (Rust::Token::make (MATCH_ARROW, UNDEF_LOCATION));
    visit (match_case.get_expr ());
    indentation ();
    push (Rust::Token::make (COMMA, UNDEF_LOCATION));
    newline ();
  });
}

void
TokenCollector::visit (MatchExpr &expr)
{
  describe_node (std::string ("MatchExpr"), [this, &expr] () {
    push (Rust::Token::make (MATCH_KW, expr.get_locus ()));
    visit (expr.get_scrutinee_expr ());
    push (Rust::Token::make (LEFT_CURLY, UNDEF_LOCATION));
    newline ();
    increment_indentation ();
    visit_items_as_lines (expr.get_inner_attrs ());
    for (auto &arm : expr.get_match_cases ())
      {
	visit (arm);
      }
    decrement_indentation ();
    indentation ();
    push (Rust::Token::make (RIGHT_CURLY, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (AwaitExpr &expr)
{
  describe_node (std::string ("AwaitExpr"), [this, &expr] () {
    visit (expr.get_awaited_expr ());
    push (Rust::Token::make (DOT, expr.get_locus ()));
    // TODO: Check status of await keyword (Context dependant ?)
    push (
      Rust::Token::make_identifier (UNDEF_LOCATION, Values::Keywords::AWAIT));
  });
}

void
TokenCollector::visit (AsyncBlockExpr &expr)
{
  describe_node (std::string ("AsyncBlockExpr"), [this, &expr] () {
    push (Rust::Token::make (ASYNC, expr.get_locus ()));
    if (expr.get_has_move ())
      push (Rust::Token::make (MOVE, UNDEF_LOCATION));
    visit (expr.get_block_expr ());
  });
}

void
TokenCollector::visit (InlineAsm &expr)
{
  push (Rust::Token::make_identifier (expr.get_locus (), "asm"));
  push (Rust::Token::make (EXCLAM, expr.get_locus ()));
  push (Rust::Token::make (LEFT_PAREN, expr.get_locus ()));

  for (auto &template_str : expr.get_template_strs ())
    push (Rust::Token::make_string (template_str.get_locus (),
				    std::move (template_str.symbol)));

  push (Rust::Token::make (COLON, expr.get_locus ()));

  for (auto &operand : expr.get_operands ())
    {
      using RegisterType = AST::InlineAsmOperand::RegisterType;
      switch (operand.get_register_type ())
	{
	case RegisterType::In:
	  {
	    visit (operand.get_in ().expr);
	    break;
	  }
	case RegisterType::Out:
	  {
	    visit (operand.get_out ().expr);
	    break;
	  }
	case RegisterType::InOut:
	  {
	    visit (operand.get_in_out ().expr);
	    break;
	  }
	case RegisterType::SplitInOut:
	  {
	    auto split = operand.get_split_in_out ();
	    visit (split.in_expr);
	    visit (split.out_expr);
	    break;
	  }
	case RegisterType::Const:
	  {
	    visit (operand.get_const ().anon_const.get_inner_expr ());
	    break;
	  }
	case RegisterType::Sym:
	  {
	    visit (operand.get_sym ().expr);
	    break;
	  }
	case RegisterType::Label:
	  {
	    visit (operand.get_label ().expr);
	    break;
	  }
	}
      push (Rust::Token::make (COMMA, expr.get_locus ()));
    }
  push (Rust::Token::make (COLON, expr.get_locus ()));

  for (auto &clobber : expr.get_clobber_abi ())
    {
      push (Rust::Token::make_string (expr.get_locus (),
				      std::move (clobber.symbol)));
      push (Rust::Token::make (COMMA, expr.get_locus ()));
    }
  push (Rust::Token::make (COLON, expr.get_locus ()));

  for (auto it = expr.named_args.begin (); it != expr.named_args.end (); ++it)
    {
      auto &arg = *it;
      push (
	Rust::Token::make_identifier (expr.get_locus (), arg.first.c_str ()));
      push (Rust::Token::make (EQUAL, expr.get_locus ()));
      push (Rust::Token::make_identifier (expr.get_locus (),
					  std::to_string (arg.second)));

      push (Rust::Token::make (COMMA, expr.get_locus ()));
    }

  push (Rust::Token::make (COLON, expr.get_locus ()));

  for (auto &option : expr.get_options ())
    {
      push (Rust::Token::make_identifier (
	expr.get_locus (), InlineAsm::option_to_string (option).c_str ()));
      push (Rust::Token::make (COMMA, expr.get_locus ()));
    }

  push (Rust::Token::make (RIGHT_PAREN, expr.get_locus ()));
}

void
TokenCollector::visit (LlvmInlineAsm &expr)
{
  push (Rust::Token::make_identifier (expr.get_locus (), "llvm_asm"));
  push (Rust::Token::make (EXCLAM, expr.get_locus ()));
  push (Rust::Token::make (LEFT_PAREN, expr.get_locus ()));
  for (auto &template_str : expr.get_templates ())
    push (Rust::Token::make_string (template_str.get_locus (),
				    std::move (template_str.symbol)));

  push (Rust::Token::make (COLON, expr.get_locus ()));
  for (auto output : expr.get_outputs ())
    {
      push (Rust::Token::make_string (expr.get_locus (),
				      std::move (output.constraint)));
      visit (output.expr);
      push (Rust::Token::make (COMMA, expr.get_locus ()));
    }

  push (Rust::Token::make (COLON, expr.get_locus ()));
  for (auto input : expr.get_inputs ())
    {
      push (Rust::Token::make_string (expr.get_locus (),
				      std::move (input.constraint)));
      visit (input.expr);
      push (Rust::Token::make (COMMA, expr.get_locus ()));
    }

  push (Rust::Token::make (COLON, expr.get_locus ()));
  for (auto &clobber : expr.get_clobbers ())
    {
      push (Rust::Token::make_string (expr.get_locus (),
				      std::move (clobber.symbol)));
      push (Rust::Token::make (COMMA, expr.get_locus ()));
    }
  push (Rust::Token::make (COLON, expr.get_locus ()));
  // Dump options

  push (Rust::Token::make (RIGHT_PAREN, expr.get_locus ()));
}

// rust-item.h

void
TokenCollector::visit (TypeParam &param)
{
  // Syntax:
  //    IDENTIFIER( : TypeParamBounds? )? ( = Type )?
  // TypeParamBounds :
  //    TypeParamBound ( + TypeParamBound )* +?
  describe_node (std::string ("TypeParam"), [this, &param] () {
    visit_items_as_lines (param.get_outer_attrs ());
    auto id = param.get_type_representation ().as_string ();
    push (Rust::Token::make_identifier (param.get_locus (), std::move (id)));
    if (param.has_type_param_bounds ())
      {
	push (Rust::Token::make (COLON, UNDEF_LOCATION));
	visit_items_joined_by_separator (param.get_type_param_bounds (), PLUS);
      }
    if (param.has_type ())
      {
	push (Rust::Token::make (EQUAL, UNDEF_LOCATION));
	visit (param.get_type ());
      }
  });
}

void
TokenCollector::visit (WhereClause &rule)
{
  // Syntax:
  // 	where ( WhereClauseItem , )* WhereClauseItem ?
  // WhereClauseItem :
  // 	LifetimeWhereClauseItem
  //  	| TypeBoundWhereClauseItem
  describe_node (std::string ("WhereClause"), [this, &rule] () {
    push (Rust::Token::make (WHERE, UNDEF_LOCATION));
    newline ();
    increment_indentation ();
    visit_items_joined_by_separator (rule.get_items (), COMMA);
    decrement_indentation ();
  });
}

void
TokenCollector::visit (LifetimeWhereClauseItem &item)
{
  // Syntax:
  // 	Lifetime : LifetimeBounds
  // LifetimeBounds :
  //   ( Lifetime + )* Lifetime?

  describe_node (std::string ("LifetimeWhereClauseItem"), [this, &item] () {
    visit (item.get_lifetime ());
    push (Rust::Token::make (COLON, UNDEF_LOCATION));
    visit_items_joined_by_separator (item.get_lifetime_bounds (), PLUS);
  });
}

void
TokenCollector::visit (TypeBoundWhereClauseItem &item)
{
  // Syntax:
  // 	ForLifetimes? Type : TypeParamBounds?
  // TypeParamBounds :
  // 	TypeParamBound ( + TypeParamBound )* +?
  // TypeParamBound :
  //    Lifetime | TraitBound

  describe_node (std::string ("TypeBoundWhereClauseItem"), [this, &item] () {
    if (item.has_for_lifetimes ())
      visit (item.get_for_lifetimes ());

    visit (item.get_type ());

    push (Rust::Token::make (COLON, UNDEF_LOCATION));
    visit_items_joined_by_separator (item.get_type_param_bounds (), PLUS);
  });
}

void
TokenCollector::visit (Module &module)
{
  //  Syntax:
  //	mod IDENTIFIER ;
  //     | mod IDENTIFIER {
  //	  InnerAttribute*
  //	  Item*
  //	}
  describe_node (std::string ("Module"), [this, &module] () {
    visit_items_as_lines (module.get_outer_attrs ());
    visit (module.get_visibility ());
    auto name = module.get_name ().as_string ();
    push (Rust::Token::make (MOD, module.get_locus ()));
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (name)));

    if (module.get_kind () == Module::UNLOADED)
      {
	push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
	newline ();
      }
    else /* Module::LOADED */
      {
	push (Rust::Token::make (LEFT_CURLY, UNDEF_LOCATION));
	newline ();
	increment_indentation ();

	visit_items_as_lines (module.get_inner_attrs ());
	visit_items_as_lines (module.get_items ());

	decrement_indentation ();

	push (Rust::Token::make (RIGHT_CURLY, UNDEF_LOCATION));
	newline ();
      }
  });
}

void
TokenCollector::visit (ExternCrate &crate)
{
  describe_node (std::string ("ExternCrate"), [this, &crate] () {
    visit_items_as_lines (crate.get_outer_attrs ());
    push (Rust::Token::make (EXTERN_KW, crate.get_locus ()));
    push (Rust::Token::make (CRATE, UNDEF_LOCATION));
    auto ref = crate.get_referenced_crate ();
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (ref)));
    if (crate.has_as_clause ())
      {
	auto as_clause = crate.get_as_clause ();
	push (Rust::Token::make (AS, UNDEF_LOCATION));
	push (
	  Rust::Token::make_identifier (UNDEF_LOCATION, std::move (as_clause)));
      }
    push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
    newline ();
  });
}

void
TokenCollector::visit (UseTreeGlob &use_tree)
{
  describe_node (std::string ("UseTreeGlob"), [this, &use_tree] () {
    switch (use_tree.get_glob_type ())
      {
      case UseTreeGlob::PathType::PATH_PREFIXED:
	{
	  auto path = use_tree.get_path ();
	  visit (path);
	  push (Rust::Token::make (SCOPE_RESOLUTION, UNDEF_LOCATION));
	}
	break;
      case UseTreeGlob::PathType::NO_PATH:
	push (Rust::Token::make (SCOPE_RESOLUTION, UNDEF_LOCATION));
	break;
      case UseTreeGlob::PathType::GLOBAL:
	break;
      }
    push (Rust::Token::make (ASTERISK, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (UseTreeList &use_tree)
{
  describe_node (std::string ("UseTreeList"), [this, &use_tree] () {
    switch (use_tree.get_path_type ())
      {
      case UseTreeList::PathType::PATH_PREFIXED:
	{
	  auto path = use_tree.get_path ();
	  visit (path);

	  push (Rust::Token::make (SCOPE_RESOLUTION, UNDEF_LOCATION));
	}
	break;
      case UseTreeList::PathType::NO_PATH:
	push (Rust::Token::make (SCOPE_RESOLUTION, UNDEF_LOCATION));
	break;
      case UseTreeList::PathType::GLOBAL:
	break;
      }

    push (Rust::Token::make (LEFT_CURLY, UNDEF_LOCATION));
    if (use_tree.has_trees ())
      {
	visit_items_joined_by_separator (use_tree.get_trees (), COMMA);
      }
    push (Rust::Token::make (RIGHT_CURLY, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (UseTreeRebind &use_tree)
{
  describe_node (std::string ("UseTreeRebind"), [this, &use_tree] () {
    auto path = use_tree.get_path ();
    visit (path);
    switch (use_tree.get_new_bind_type ())
      {
      case UseTreeRebind::NewBindType::IDENTIFIER:
	{
	  push (Rust::Token::make (AS, UNDEF_LOCATION));
	  auto id = use_tree.get_identifier ().as_string ();
	  push (Rust::Token::make_identifier (use_tree.get_locus (),
					      std::move (id)));
	}
	break;
      case UseTreeRebind::NewBindType::WILDCARD:
	push (Rust::Token::make (AS, UNDEF_LOCATION));
	push (Rust::Token::make (UNDERSCORE, use_tree.get_locus ()));
	break;
      case UseTreeRebind::NewBindType::NONE:
	break;
      }
  });
}

void
TokenCollector::visit (UseDeclaration &decl)
{
  describe_node (std::string ("UseDeclaration"), [this, &decl] () {
    visit_items_as_lines (decl.get_outer_attrs ());
    push (Rust::Token::make (USE, decl.get_locus ()));
    visit (*decl.get_tree ());
    push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
    newline ();
  });
}

void
TokenCollector::visit (Function &function)
{
  // Syntax:
  //   FunctionQualifiers fn IDENTIFIER GenericParams?
  //      ( FunctionParameters? )
  //      FunctionReturnType? WhereClause?
  //      ( BlockExpression | ; )
  describe_node (std::string ("Function"), [this, &function] () {
    visit_items_as_lines (function.get_outer_attrs ());

    visit (function.get_visibility ());
    auto qualifiers = function.get_qualifiers ();
    visit (qualifiers);

    push (Rust::Token::make (FN_KW, function.get_locus ()));
    auto name = function.get_function_name ().as_string ();
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (name)));
    if (function.has_generics ())
      visit (function.get_generic_params ());

    push (Rust::Token::make (LEFT_PAREN, UNDEF_LOCATION));

    visit_items_joined_by_separator (function.get_function_params ());
    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));

    if (function.has_return_type ())
      {
	push (Rust::Token::make (RETURN_TYPE, UNDEF_LOCATION));
	visit (function.get_return_type ());
      }

    if (function.has_where_clause ())
      visit (function.get_where_clause ());

    if (function.has_body ())
      visit (*function.get_definition ());
    else
      push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
    newline ();
  });
}

void
TokenCollector::visit (TypeAlias &type_alias)
{
  // Syntax:
  // Visibility? type IDENTIFIER GenericParams? WhereClause? = Type;

  // Note: Associated types are handled by `AST::TraitItemType`.
  describe_node (std::string ("TypeAlias"), [this, &type_alias] () {
    visit_items_as_lines (type_alias.get_outer_attrs ());
    if (type_alias.has_visibility ())
      visit (type_alias.get_visibility ());
    auto alias_name = type_alias.get_new_type_name ().as_string ();
    push (Rust::Token::make (TYPE, type_alias.get_locus ()));
    push (
      Rust::Token::make_identifier (UNDEF_LOCATION, std::move (alias_name)));

    if (type_alias.has_generics ())
      visit (type_alias.get_generic_params ());

    if (type_alias.has_where_clause ())
      visit (type_alias.get_where_clause ());

    push (Rust::Token::make (EQUAL, UNDEF_LOCATION));
    visit (type_alias.get_type_aliased ());
    push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (StructStruct &struct_item)
{
  describe_node (std::string ("StructStruct"), [this, &struct_item] () {
    visit_items_as_lines (struct_item.get_outer_attrs ());
    if (struct_item.has_visibility ())
      visit (struct_item.get_visibility ());
    auto struct_name = struct_item.get_identifier ().as_string ();
    push (Rust::Token::make (STRUCT_KW, struct_item.get_locus ()));
    push (
      Rust::Token::make_identifier (UNDEF_LOCATION, std::move (struct_name)));

    if (struct_item.has_generics ())
      visit (struct_item.get_generic_params ());
    if (struct_item.has_where_clause ())
      visit (struct_item.get_where_clause ());
    if (struct_item.is_unit_struct ())
      {
	push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
	newline ();
      }
    else
      visit_items_as_block (struct_item.get_fields (),
			    {Rust::Token::make (COMMA, UNDEF_LOCATION)});
  });
}

void
TokenCollector::visit (TupleStruct &tuple_struct)
{
  describe_node (std::string ("TupleStruct"), [this, &tuple_struct] () {
    visit_items_as_lines (tuple_struct.get_outer_attrs ());
    auto struct_name = tuple_struct.get_identifier ().as_string ();
    push (Rust::Token::make (STRUCT_KW, tuple_struct.get_locus ()));
    push (
      Rust::Token::make_identifier (UNDEF_LOCATION, std::move (struct_name)));
    if (tuple_struct.has_generics ())
      visit (tuple_struct.get_generic_params ());
    if (tuple_struct.has_where_clause ())
      visit (tuple_struct.get_where_clause ());

    push (Rust::Token::make (LEFT_PAREN, UNDEF_LOCATION));
    visit_items_joined_by_separator (tuple_struct.get_fields (), COMMA);
    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
    push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
    newline ();
  });
}

void
TokenCollector::visit (EnumItem &item)
{
  describe_node (std::string ("EnumItem"), [this, &item] () {
    visit_items_as_lines (item.get_outer_attrs ());
    auto id = item.get_identifier ().as_string ();
    push (Rust::Token::make_identifier (item.get_locus (), std::move (id)));
  });
}

void
TokenCollector::visit (EnumItemTuple &item)
{
  describe_node (std::string ("EnumItemTuple"), [this, &item] () {
    auto id = item.get_identifier ().as_string ();
    push (Rust::Token::make_identifier (item.get_locus (), std::move (id)));
    push (Rust::Token::make (LEFT_PAREN, UNDEF_LOCATION));
    visit_items_joined_by_separator (item.get_tuple_fields (), COMMA);
    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (EnumItemStruct &item)
{
  describe_node (std::string ("EnumItemStruct"), [this, &item] () {
    auto id = item.get_identifier ().as_string ();
    push (Rust::Token::make_identifier (item.get_locus (), std::move (id)));
    visit_items_as_block (item.get_struct_fields (),
			  {Rust::Token::make (COMMA, UNDEF_LOCATION)});
  });
}

void
TokenCollector::visit (EnumItemDiscriminant &item)
{
  describe_node (std::string ("EnumItemDiscriminant"), [this, &item] () {
    auto id = item.get_identifier ().as_string ();
    push (Rust::Token::make_identifier (item.get_locus (), std::move (id)));
    push (Rust::Token::make (EQUAL, UNDEF_LOCATION));
    visit (item.get_expr ());
  });
}

void
TokenCollector::visit (Enum &enumeration)
{
  describe_node (std::string ("Enum"), [this, &enumeration] () {
    visit_items_as_lines (enumeration.get_outer_attrs ());
    if (enumeration.has_visibility ())
      visit (enumeration.get_visibility ());
    push (Rust::Token::make (ENUM_KW, enumeration.get_locus ()));
    auto id = enumeration.get_identifier ().as_string ();
    push (
      Rust::Token::make_identifier (enumeration.get_locus (), std::move (id)));
    if (enumeration.has_generics ())
      visit (enumeration.get_generic_params ());
    if (enumeration.has_where_clause ())
      visit (enumeration.get_where_clause ());

    visit_items_as_block (enumeration.get_variants (),
			  {Rust::Token::make (COMMA, UNDEF_LOCATION)});
  });
}

void
TokenCollector::visit (Union &union_item)
{
  describe_node (std::string ("Union"), [this, &union_item] () {
    visit_items_as_lines (union_item.get_outer_attrs ());
    auto id = union_item.get_identifier ().as_string ();
    push (Rust::Token::make_identifier (union_item.get_locus (),
					Values::WeakKeywords::UNION));
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (id)));

    if (union_item.has_generics ())
      visit (union_item.get_generic_params ());

    if (union_item.has_where_clause ())
      visit (union_item.get_where_clause ());

    visit_items_as_block (union_item.get_variants (),
			  {Rust::Token::make (COMMA, UNDEF_LOCATION)});
  });
}

void
TokenCollector::visit (ConstantItem &item)
{
  describe_node (std::string ("ConstantItem"), [this, &item] () {
    visit_items_as_lines (item.get_outer_attrs ());
    push (Rust::Token::make (CONST, item.get_locus ()));
    if (item.is_unnamed ())
      {
	push (Rust::Token::make (UNDERSCORE, UNDEF_LOCATION));
      }
    else
      {
	push (Rust::Token::make_identifier (item.get_identifier ()));
      }
    push (Rust::Token::make (COLON, UNDEF_LOCATION));
    visit (item.get_type ());
    if (item.has_expr ())
      {
	push (Rust::Token::make (EQUAL, UNDEF_LOCATION));
	visit (item.get_expr ());
      }
    push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (StaticItem &item)
{
  describe_node (std::string ("StaticItem"), [this, &item] () {
    visit_items_as_lines (item.get_outer_attrs ());
    push (Rust::Token::make (STATIC_KW, item.get_locus ()));
    if (item.is_mutable ())
      push (Rust::Token::make (MUT, UNDEF_LOCATION));

    auto id = item.get_identifier ().as_string ();
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (id)));
    push (Rust::Token::make (COLON, UNDEF_LOCATION));

    visit (item.get_type ());

    if (item.has_expr ())
      {
	push (Rust::Token::make (EQUAL, UNDEF_LOCATION));
	visit (item.get_expr ());
      }
    push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit_function_common (std::unique_ptr<Type> &return_type,
				       std::unique_ptr<BlockExpr> &block)
{
  // FIXME: This should format the `<vis> fn <name> ( [args] )` as well
  if (return_type)
    {
      push (Rust::Token::make (RETURN_TYPE, UNDEF_LOCATION));
      visit (return_type);
    }

  if (block)
    {
      visit (block);
    }
  else
    {
      push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
      newline ();
    }
}

void
TokenCollector::visit (SelfParam &param)
{
  describe_node (std::string ("SelfParam"), [this, &param] () {
    if (param.get_has_ref ())
      {
	push (Rust::Token::make (AMP, UNDEF_LOCATION));
	if (param.has_lifetime ())
	  {
	    auto lifetime = param.get_lifetime ();
	    visit (lifetime);
	  }
	if (param.get_is_mut ())
	  push (Rust::Token::make (MUT, UNDEF_LOCATION));
      }
    push (Rust::Token::make (SELF, UNDEF_LOCATION));
    if (param.has_type ())
      {
	push (Rust::Token::make (COLON, UNDEF_LOCATION));
	visit (param.get_type ());
      }
  });
}

void
TokenCollector::visit (TraitItemType &item)
{
  describe_node (std::string ("TraitItemType"), [this, &item] () {
    visit_items_as_lines (item.get_outer_attrs ());
    auto id = item.get_identifier ().as_string ();
    indentation ();

    push (Rust::Token::make (TYPE, item.get_locus ()));
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (id)));
    push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
    newline ();
  });
}

void
TokenCollector::visit (Trait &trait)
{
  describe_node (std::string ("Trait"), [this, &trait] () {
    for (auto &attr : trait.get_outer_attrs ())
      {
	visit (attr);
	newline ();
	indentation ();
      }

    visit (trait.get_visibility ());

    auto id = trait.get_identifier ().as_string ();
    push (Rust::Token::make (TRAIT, trait.get_locus ()));
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (id)));

    visit (trait.get_generic_params ());

    visit_items_as_block (trait.get_trait_items (), {});
  });
}

void
TokenCollector::visit (InherentImpl &impl)
{
  describe_node (std::string ("InherentImpl"), [this, &impl] () {
    visit_items_as_lines (impl.get_outer_attrs ());
    push (Rust::Token::make (IMPL, impl.get_locus ()));
    visit (impl.get_generic_params ());

    visit (impl.get_type ());

    if (impl.has_where_clause ())
      visit (impl.get_where_clause ());

    // FIXME: Handle inner attributes

    visit_items_as_block (impl.get_impl_items (), {});
  });
}

void
TokenCollector::visit (TraitImpl &impl)
{
  describe_node (std::string ("TraitImpl"), [this, &impl] () {
    visit_items_as_lines (impl.get_outer_attrs ());
    push (Rust::Token::make (IMPL, impl.get_locus ()));
    visit (impl.get_generic_params ());
    if (impl.is_exclam ())
      push (Rust::Token::make (EXCLAM, UNDEF_LOCATION));
    visit (impl.get_trait_path ());
    push (Rust::Token::make (FOR, UNDEF_LOCATION));
    visit (impl.get_type ());

    if (impl.has_where_clause ())
      visit (impl.get_where_clause ());
  });
  visit_items_as_block (impl.get_impl_items ());
}

void
TokenCollector::visit (ExternalTypeItem &type)
{
  describe_node (std::string ("ExternalTypeItem"), [this, &type] () {
    visit (type.get_visibility ());

    auto id = type.get_identifier ().as_string ();

    push (Rust::Token::make (TYPE, UNDEF_LOCATION));
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (id)));
    push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (ExternalStaticItem &item)
{
  describe_node (std::string ("ExternalStaticItem"), [this, &item] () {
    auto id = item.get_identifier ().as_string ();
    visit_items_as_lines (item.get_outer_attrs ());
    if (item.has_visibility ())
      visit (item.get_visibility ());
    push (Rust::Token::make (STATIC_KW, item.get_locus ()));
    if (item.is_mut ())
      push (Rust::Token::make (MUT, UNDEF_LOCATION));
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (id)));
    push (Rust::Token::make (COLON, UNDEF_LOCATION));
    visit (item.get_type ());
    // TODO: No expr ? The "(= Expression)?" part from the reference seems
    // missing in the ast.
    push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (ExternBlock &block)
{
  describe_node (std::string ("ExternBlock"), [this, &block] () {
    visit_items_as_lines (block.get_outer_attrs ());
    push (Rust::Token::make (EXTERN_KW, block.get_locus ()));

    if (block.has_abi ())
      {
	auto abi = block.get_abi ();
	push (Rust::Token::make_string (UNDEF_LOCATION, std::move (abi)));
      }

    visit_items_as_block (block.get_extern_items (), {});
  });
}

static std::pair<TokenId, TokenId>
get_delimiters (DelimType delim)
{
  switch (delim)
    {
    case PARENS:
      return {LEFT_PAREN, RIGHT_PAREN};
    case SQUARE:
      return {LEFT_SQUARE, RIGHT_SQUARE};
    case CURLY:
      return {LEFT_CURLY, RIGHT_CURLY};
    default:
      rust_unreachable ();
    }
}

void
TokenCollector::visit (MacroMatchFragment &match)
{
  describe_node (std::string ("MacroMatchFragment"), [this, &match] () {
    auto id = match.get_ident ().as_string ();
    auto frag_spec = match.get_frag_spec ().as_string ();
    push (Rust::Token::make (DOLLAR_SIGN, UNDEF_LOCATION));
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (id)));
    push (Rust::Token::make (COLON, UNDEF_LOCATION));
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (frag_spec)));
  });
}

void
TokenCollector::visit (MacroMatchRepetition &repetition)
{
  describe_node (std::string ("MacroMatchRepetition"), [this, &repetition] () {
    push (Rust::Token::make (DOLLAR_SIGN, UNDEF_LOCATION));
    push (Rust::Token::make (LEFT_PAREN, UNDEF_LOCATION));

    for (auto &match : repetition.get_matches ())
      {
	visit (match);
      }

    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));

    if (repetition.has_sep ())
      {
	push (Rust::Token::make (repetition.get_sep ()->get_id (),
				 repetition.get_sep ()->get_locus ()));
      }
    switch (repetition.get_op ())
      {
      case MacroMatchRepetition::ANY:
	push (Rust::Token::make (ASTERISK, UNDEF_LOCATION));
	break;
      case MacroMatchRepetition::ONE_OR_MORE:
	push (Rust::Token::make (PLUS, UNDEF_LOCATION));
	break;
      case MacroMatchRepetition::ZERO_OR_ONE:
	push (Rust::Token::make (QUESTION_MARK, UNDEF_LOCATION));
	break;
      case MacroMatchRepetition::NONE:
	break;
      }
  });
}

void
TokenCollector::visit (MacroMatcher &matcher)
{
  describe_node (std::string ("MacroMatcher"), [this, &matcher] () {
    auto delimiters = get_delimiters (matcher.get_delim_type ());

    push (Rust::Token::make (delimiters.first, UNDEF_LOCATION));

    for (auto &item : matcher.get_matches ())
      {
	visit (item);
      }

    push (Rust::Token::make (delimiters.second, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (MacroRule &rule)
{
  describe_node (std::string ("MacroRule"), [this, &rule] () {
    visit (rule.get_matcher ());
    push (Rust::Token::make (MATCH_ARROW, rule.get_locus ()));
    visit (rule.get_transcriber ().get_token_tree ());
  });
}

void
TokenCollector::visit (MacroRulesDefinition &rules_def)
{
  describe_node (std::string ("MacroRulesDefinition"), [this, &rules_def] () {
    for (auto &outer_attr : rules_def.get_outer_attrs ())
      visit (outer_attr);

    auto rule_name = rules_def.get_rule_name ().as_string ();

    push (Rust::Token::make_identifier (rules_def.get_locus (),
					Values::WeakKeywords::MACRO_RULES));
    push (Rust::Token::make (EXCLAM, UNDEF_LOCATION));

    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (rule_name)));

    visit_items_as_block (rules_def.get_rules (),
			  {Rust::Token::make (SEMICOLON, UNDEF_LOCATION)});
  });
}

void
TokenCollector::visit (MacroInvocation &invocation)
{
  describe_node (std::string ("MacroInvocation"), [this, &invocation] () {
    auto data = invocation.get_invoc_data ();
    visit (data.get_path ());
    push (Rust::Token::make (EXCLAM, UNDEF_LOCATION));
    visit (data.get_delim_tok_tree ());
    if (invocation.has_semicolon ())
      push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (MetaItemPath &item)
{
  describe_node (std::string ("MetaItemPath"), [this, &item] () {
    auto path = item.to_path_item ();
    visit (path);
  });
}

void
TokenCollector::visit (MetaItemSeq &item)
{
  describe_node (std::string ("MetaItemSeq"), [this, &item] () {
    visit (item.get_path ());
    // TODO: Double check this, there is probably a mistake.
    push (Rust::Token::make (LEFT_PAREN, UNDEF_LOCATION));
    visit_items_joined_by_separator (item.get_seq (), COMMA);
    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (MetaWord &word)
{
  describe_node (std::string ("MetaWord"), [this, &word] () {
    auto id = word.get_ident ().as_string ();

    push (Rust::Token::make_identifier (word.get_locus (), std::move (id)));
  });
}

void
TokenCollector::visit (MetaNameValueStr &name)
{
  describe_node (std::string ("MetaNameValueStr"), [this, &name] () {
    auto pair = name.get_name_value_pair ();
    auto id = std::get<0> (pair).as_string ();
    auto value = std::get<1> (pair);

    push (Rust::Token::make_identifier (name.get_locus (), std::move (id)));
    push (Rust::Token::make (EQUAL, name.get_locus ()));
    push (Rust::Token::make (DOUBLE_QUOTE, UNDEF_LOCATION));
    push (Rust::Token::make_identifier (name.get_locus (), std::move (value)));
    push (Rust::Token::make (DOUBLE_QUOTE, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (MetaListPaths &list)
{
  describe_node (std::string ("MetaListPath"), [this, &list] () {
    auto id = list.get_ident ().as_string ();

    push (Rust::Token::make_identifier (list.get_locus (), std::move (id)));
    push (Rust::Token::make (LEFT_PAREN, UNDEF_LOCATION));

    visit_items_joined_by_separator (list.get_paths (), COMMA);

    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (MetaListNameValueStr &list)
{
  describe_node (std::string ("MetaListNameValueStr"), [this, &list] () {
    auto id = list.get_ident ().as_string ();

    push (Rust::Token::make_identifier (list.get_locus (), std::move (id)));
    push (Rust::Token::make (LEFT_PAREN, UNDEF_LOCATION));

    visit_items_joined_by_separator (list.get_values (), COMMA);

    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
  });
}

// rust-pattern.h
void
TokenCollector::visit (LiteralPattern &pattern)
{
  describe_node (std::string ("LiteralPattern"), [this, &pattern] () {
    visit (pattern.get_literal (), pattern.get_locus ());
  });
}

void
TokenCollector::visit (IdentifierPattern &pattern)
{
  describe_node (std::string ("IdentifierPattern"), [this, &pattern] () {
    if (pattern.get_is_ref ())
      {
	push (Rust::Token::make (REF, pattern.get_locus ()));
      }
    if (pattern.get_is_mut ())
      {
	push (Rust::Token::make (MUT, UNDEF_LOCATION));
      }

    auto id = pattern.get_ident ().as_string ();
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (id)));

    if (pattern.has_subpattern ())
      {
	push (Rust::Token::make (PATTERN_BIND, UNDEF_LOCATION));
	visit (pattern.get_subpattern ());
      }
  });
}

void
TokenCollector::visit (WildcardPattern &pattern)
{
  describe_node (std::string ("WildcardPattern"), [this, &pattern] () {
    push (Rust::Token::make (UNDERSCORE, pattern.get_locus ()));
  });
}

void
TokenCollector::visit (RestPattern &pattern)
{
  describe_node (std::string ("RestPattern"), [this, &pattern] () {
    push (Rust::Token::make (DOT_DOT, pattern.get_locus ()));
  });
}

// void TokenCollector::visit(RangePatternBound& ){}

void
TokenCollector::visit (RangePatternBoundLiteral &pattern)
{
  describe_node (std::string ("RangePatternBoundLiteral"), [this, &pattern] () {
    if (pattern.get_has_minus ())
      {
	push (Rust::Token::make (MINUS, pattern.get_locus ()));
      }
    auto literal = pattern.get_literal ();
    visit (literal);
  });
}

void
TokenCollector::visit (RangePatternBoundPath &pattern)
{
  describe_node (std::string ("RangePatternBoundPath"),
		 [this, &pattern] () { visit (pattern.get_path ()); });
}

void
TokenCollector::visit (RangePatternBoundQualPath &pattern)
{
  describe_node (std::string ("RangePatternBoundQualPath"),
		 [this, &pattern] () {
		   visit (pattern.get_qualified_path ());
		 });
}

void
TokenCollector::visit (RangePattern &pattern)
{
  describe_node (std::string ("RangePattern"), [this, &pattern] () {
    if (pattern.get_has_lower_bound () && pattern.get_has_upper_bound ())
      {
	visit (pattern.get_lower_bound ());
	if (pattern.get_has_ellipsis_syntax ())
	  push (Rust::Token::make (ELLIPSIS, pattern.get_locus ()));
	else
	  push (Rust::Token::make (DOT_DOT_EQ, pattern.get_locus ()));
	visit (pattern.get_upper_bound ());
      }
    else if (pattern.get_has_lower_bound ())
      {
	visit (pattern.get_lower_bound ());
	push (Rust::Token::make (DOT_DOT, pattern.get_locus ()));
      }
    else
      {
	push (Rust::Token::make (DOT_DOT_EQ, pattern.get_locus ()));
	visit (pattern.get_upper_bound ());
      }
  });
}

void
TokenCollector::visit (ReferencePattern &pattern)

{
  describe_node (std::string ("ReferencePattern"), [this, &pattern] () {
    if (pattern.is_double_reference ())
      {
	push (Rust::Token::make (LOGICAL_AND, pattern.get_locus ()));
      }
    else
      {
	push (Rust::Token::make (AMP, pattern.get_locus ()));
      }

    if (pattern.get_is_mut ())
      {
	push (Rust::Token::make (MUT, UNDEF_LOCATION));
      }

    visit (pattern.get_referenced_pattern ());
  });
}

// void TokenCollector::visit(StructPatternField& ){}

void
TokenCollector::visit (StructPatternFieldTuplePat &pattern)
{
  describe_node (std::string ("StructPatternFieldTuplePat"), [this,
							      &pattern] () {
    visit_items_as_lines (pattern.get_outer_attrs ());
    push (Rust::Token::make_int (pattern.get_locus (),
				 std::to_string (pattern.get_index ())));
    push (Rust::Token::make (COLON, pattern.get_locus ()));
    visit (pattern.get_index_pattern ());
  });
}

void
TokenCollector::visit (StructPatternFieldIdentPat &pattern)
{
  describe_node (std::string ("StructPatternFieldIdentPat"), [this,
							      &pattern] () {
    visit_items_as_lines (pattern.get_outer_attrs ());

    auto id = pattern.get_identifier ().as_string ();
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (id)));

    push (Rust::Token::make (COLON, pattern.get_locus ()));

    visit (pattern.get_ident_pattern ());
  });
}

void
TokenCollector::visit (StructPatternFieldIdent &pattern)
{
  describe_node (std::string ("StructPatternFieldIdent"), [this, &pattern] () {
    visit_items_as_lines (pattern.get_outer_attrs ());
    if (pattern.is_ref ())
      push (Rust::Token::make (REF, UNDEF_LOCATION));
    if (pattern.is_mut ())
      push (Rust::Token::make (MUT, UNDEF_LOCATION));

    auto id = pattern.get_identifier ().as_string ();
    push (Rust::Token::make_identifier (UNDEF_LOCATION, std::move (id)));
  });
}

void
TokenCollector::visit (StructPattern &pattern)
{
  describe_node (std::string ("StructPattern"), [this, &pattern] () {
    visit (pattern.get_path ());
    push (Rust::Token::make (LEFT_CURLY, pattern.get_locus ()));
    auto elems = pattern.get_struct_pattern_elems ();
    if (elems.has_struct_pattern_fields ())
      {
	visit_items_joined_by_separator (elems.get_struct_pattern_fields ());
	if (elems.has_rest ())
	  {
	    push (Rust::Token::make (COMMA, UNDEF_LOCATION));
	    visit_items_as_lines (elems.get_etc_outer_attrs ());
	  }
      }
    else
      {
	visit_items_as_lines (elems.get_etc_outer_attrs ());
      }

    push (Rust::Token::make (RIGHT_CURLY, UNDEF_LOCATION));
  });
}

// void TokenCollector::visit(TupleStructItems& ){}

void
TokenCollector::visit (TupleStructItemsNoRest &pattern)
{
  describe_node (std::string ("TupleStructItemsNoRange"), [this, &pattern] () {
    visit_items_joined_by_separator (pattern.get_patterns ());
  });
}

void
TokenCollector::visit (TupleStructItemsHasRest &pattern)
{
  describe_node (std::string ("TupleStructItemsRange"), [this, &pattern] () {
    for (auto &lower : pattern.get_lower_patterns ())
      {
	visit (lower);
      }
    push (Rust::Token::make (DOT_DOT, UNDEF_LOCATION));
    for (auto &upper : pattern.get_lower_patterns ())
      {
	visit (upper);
      }
  });
}

void
TokenCollector::visit (TupleStructPattern &pattern)
{
  describe_node (std::string ("TupleStructPattern"), [this, &pattern] () {
    visit (pattern.get_path ());
    push (Rust::Token::make (LEFT_PAREN, pattern.get_locus ()));
    visit (pattern.get_items ());
    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
  });
}

// void
// TokenCollector::visit (TuplePatternItems &)
// {}

void
TokenCollector::visit (TuplePatternItemsNoRest &pattern)
{
  describe_node (std::string ("TuplePatternItemsMultiple"), [this,
							     &pattern] () {
    visit_items_joined_by_separator (pattern.get_patterns (), COMMA);
  });
}

void
TokenCollector::visit (TuplePatternItemsHasRest &pattern)
{
  describe_node (std::string ("TuplePatternItemsRanged"), [this, &pattern] () {
    for (auto &lower : pattern.get_lower_patterns ())
      {
	visit (lower);
      }
    push (Rust::Token::make (DOT_DOT, UNDEF_LOCATION));
    for (auto &upper : pattern.get_lower_patterns ())
      {
	visit (upper);
      }
  });
}

void
TokenCollector::visit (TuplePattern &pattern)
{
  describe_node (std::string ("TuplePattern"), [this, &pattern] () {
    push (Rust::Token::make (LEFT_PAREN, pattern.get_locus ()));
    visit (pattern.get_items ());
    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (GroupedPattern &pattern)
{
  describe_node (std::string ("GroupedPattern"), [this, &pattern] () {
    push (Rust::Token::make (LEFT_PAREN, pattern.get_locus ()));
    visit (pattern.get_pattern_in_parens ());
    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (SlicePatternItemsNoRest &items)
{
  visit_items_joined_by_separator (items.get_patterns (), COMMA);
}

void
TokenCollector::visit (SlicePatternItemsHasRest &items)
{
  if (!items.get_lower_patterns ().empty ())
    {
      visit_items_joined_by_separator (items.get_lower_patterns (), COMMA);
      push (Rust::Token::make (COMMA, UNDEF_LOCATION));
    }

  push (Rust::Token::make (DOT_DOT, UNDEF_LOCATION));

  if (!items.get_upper_patterns ().empty ())
    {
      push (Rust::Token::make (COMMA, UNDEF_LOCATION));
      visit_items_joined_by_separator (items.get_upper_patterns (), COMMA);
    }
}

void
TokenCollector::visit (SlicePattern &pattern)
{
  describe_node (std::string ("SlicePattern"), [this, &pattern] () {
    push (Rust::Token::make (LEFT_SQUARE, pattern.get_locus ()));
    visit (pattern.get_items ());
    push (Rust::Token::make (RIGHT_SQUARE, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (AltPattern &pattern)
{
  describe_node (std::string ("AltPattern"), [this, &pattern] () {
    visit_items_joined_by_separator (pattern.get_alts (), PIPE);
  });
}

// rust-stmt.h
void
TokenCollector::visit (EmptyStmt &)
{}

void
TokenCollector::visit (LetStmt &stmt)
{
  describe_node (std::string ("LetStmt"), [this, &stmt] () {
    push (Rust::Token::make (LET, stmt.get_locus ()));
    auto &pattern = stmt.get_pattern ();
    visit (pattern);

    if (stmt.has_type ())
      {
	push (Rust::Token::make (COLON, UNDEF_LOCATION));
	visit (stmt.get_type ());
      }

    if (stmt.has_type ())
      {
	push (Rust::Token::make (COLON, UNDEF_LOCATION));
	visit (stmt.get_type ());
      }

    if (stmt.has_init_expr ())
      {
	push (Rust::Token::make (EQUAL, UNDEF_LOCATION));
	visit (stmt.get_init_expr ());
      }

    if (stmt.has_else_expr ())
      {
	push (Rust::Token::make (ELSE, UNDEF_LOCATION));
	visit (stmt.get_else_expr ());
      }

    push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (ExprStmt &stmt)
{
  describe_node (std::string ("ExprStmt"), [this, &stmt] () {
    visit (stmt.get_expr ());

    if (stmt.is_semicolon_followed ())
      push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
  });
}

// rust-type.h
void
TokenCollector::visit (TraitBound &bound)
{
  // Syntax:
  //      ?? ForLifetimes? TypePath
  //   | ( ?? ForLifetimes? TypePath )
  describe_node (std::string ("TraitBound"), [this, &bound] () {
    if (bound.has_opening_question_mark ())
      push (Rust::Token::make (QUESTION_MARK, bound.get_locus ()));

    if (bound.has_for_lifetimes ())
      visit (bound.get_for_lifetimes ());

    visit (bound.get_type_path ());
  });
}

void
TokenCollector::visit (ImplTraitType &type)
{
  // Syntax:
  //    impl TypeParamBounds
  // TypeParamBounds :
  //    TypeParamBound ( + TypeParamBound )* +?
  describe_node (std::string ("ImplTraitType"), [this, &type] () {
    push (Rust::Token::make (IMPL, type.get_locus ()));
    visit_items_joined_by_separator (type.get_type_param_bounds (), PLUS);
  });
}

void
TokenCollector::visit (TraitObjectType &type)
{
  // Syntax:
  //   dyn? TypeParamBounds
  // TypeParamBounds :
  //   TypeParamBound ( + TypeParamBound )* +?
  describe_node (std::string ("TraiObjectType"), [this, &type] () {
    if (type.is_dyn ())
      push (Rust::Token::make (DYN, type.get_locus ()));
    visit_items_joined_by_separator (type.get_type_param_bounds (), PLUS);
  });
}

void
TokenCollector::visit (ParenthesisedType &type)
{
  // Syntax:
  //    ( Type )
  describe_node (std::string ("ParenthesisedType"), [this, &type] () {
    push (Rust::Token::make (LEFT_PAREN, type.get_locus ()));
    visit (type.get_type_in_parens ());
    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (ImplTraitTypeOneBound &type)
{
  // Syntax:
  //    impl TraitBound

  describe_node (std::string ("ImplTraitTypeOneBound"), [this, &type] () {
    push (Rust::Token::make (IMPL, type.get_locus ()));
    visit (type.get_trait_bound ());
  });
}

void
TokenCollector::visit (TraitObjectTypeOneBound &type)
{
  // Syntax:
  //    dyn? TraitBound
  describe_node (std::string ("TraitObjectTypeOneBound"), [this, &type] () {
    if (type.is_dyn ())
      push (Rust::Token::make (DYN, type.get_locus ()));
    visit (type.get_trait_bound ());
  });
}

void
TokenCollector::visit (TupleType &type)
{
  // Syntax:
  //   ( )
  //   | ( ( Type , )+ Type? )

  describe_node (std::string ("TupleType"), [this, &type] () {
    push (Rust::Token::make (LEFT_PAREN, type.get_locus ()));
    visit_items_joined_by_separator (type.get_elems (), COMMA);
    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (NeverType &type)
{
  // Syntax:
  //  !

  describe_node (std::string ("NeverType"), [this, &type] () {
    push (Rust::Token::make (EXCLAM, type.get_locus ()));
  });
}

void
TokenCollector::visit (RawPointerType &type)
{
  // Syntax:
  //    * ( mut | const ) TypeNoBounds
  describe_node (std::string ("RawPointerType"), [this, &type] () {
    push (Rust::Token::make (ASTERISK, type.get_locus ()));
    if (type.get_pointer_type () == RawPointerType::MUT)
      push (Rust::Token::make (MUT, UNDEF_LOCATION));
    else /* RawPointerType::CONST */
      push (Rust::Token::make (CONST, UNDEF_LOCATION));

    visit (type.get_type_pointed_to ());
  });
}

void
TokenCollector::visit (ReferenceType &type)
{
  // Syntax:
  //    & Lifetime? mut? TypeNoBounds
  describe_node (std::string ("ReferenceType"), [this, &type] () {
    push (Rust::Token::make (AMP, type.get_locus ()));

    if (type.has_lifetime ())
      {
	visit (type.get_lifetime ());
      }

    if (type.get_has_mut ())
      push (Rust::Token::make (MUT, UNDEF_LOCATION));

    visit (type.get_type_referenced ());
  });
}

void
TokenCollector::visit (ArrayType &type)
{
  // Syntax:
  //    [type Type ; Expression ]
  describe_node (std::string ("ArrayType"), [this, &type] () {
    push (Rust::Token::make (LEFT_SQUARE, type.get_locus ()));
    visit (type.get_elem_type ());
    push (Rust::Token::make (SEMICOLON, UNDEF_LOCATION));
    visit (type.get_size_expr ());
    push (Rust::Token::make (RIGHT_SQUARE, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (SliceType &type)
{
  // Syntax:
  //    [type Type ]

  describe_node (std::string ("SliceType"), [this, &type] () {
    push (Rust::Token::make (LEFT_SQUARE, type.get_locus ()));
    visit (type.get_elem_type ());
    push (Rust::Token::make (RIGHT_SQUARE, UNDEF_LOCATION));
  });
}

void
TokenCollector::visit (InferredType &type)
{
  // Syntax:
  //    _
  describe_node (std::string ("InferredType"), [this, &type] () {
    push (Rust::Token::make (UNDERSCORE, type.get_locus ()));
  });
}

void
TokenCollector::visit (BareFunctionType &type)
{
  // Syntax:
  //    ForLifetimes? FunctionTypeQualifiers fn
  //      ( FunctionParametersMaybeNamedVariadic? )
  //      BareFunctionReturnType?
  //
  //    BareFunctionReturnType:
  //      -> TypeNoBounds
  //
  //    FunctionParametersMaybeNamedVariadic :
  //      MaybeNamedFunctionParameters |
  //      MaybeNamedFunctionParametersVariadic
  //
  //    MaybeNamedFunctionParameters :
  //      MaybeNamedParam ( , MaybeNamedParam )* ,?
  //
  //    MaybeNamedFunctionParametersVariadic :
  //      ( MaybeNamedParam , )* MaybeNamedParam , OuterAttribute* ...
  describe_node (std::string ("BareFunctionType"), [this, &type] () {
    if (type.has_for_lifetimes ())
      visit (type.get_for_lifetimes ());

    visit (type.get_function_qualifiers ());

    push (Rust::Token::make (FN_KW, type.get_locus ()));
    push (Rust::Token::make (LEFT_PAREN, UNDEF_LOCATION));

    visit_items_joined_by_separator (type.get_function_params (), COMMA);

    if (type.is_variadic ())
      {
	push (Rust::Token::make (COMMA, UNDEF_LOCATION));
	for (auto &item : type.get_variadic_attr ())
	  {
	    visit (item);
	  }
	push (Rust::Token::make (ELLIPSIS, UNDEF_LOCATION));
      }

    push (Rust::Token::make (RIGHT_PAREN, UNDEF_LOCATION));

    if (type.has_return_type ())
      {
	push (Rust::Token::make (RETURN_TYPE, UNDEF_LOCATION));
	visit (type.get_return_type ());
      }
  });
}

void
TokenCollector::visit (AST::FormatArgs &fmt)
{
  push (Rust::Token::make_identifier (fmt.get_locus (), "format_args"));
  push (Rust::Token::make (EXCLAM, fmt.get_locus ()));
  push (Rust::Token::make (LEFT_PAREN, fmt.get_locus ()));

  std::string reconstructed_template = "\"";
  const auto &template_pieces = fmt.get_template ();

  for (const auto &piece : template_pieces.get_pieces ())
    {
      if (piece.tag == Fmt::ffi::Piece::Tag::String)
	{
	  std::string literal = piece.string._0.to_string ();
	  for (char c : literal)
	    {
	      if (c == '"' || c == '\\')
		{
		  reconstructed_template += '\\';
		}
	      else if (c == '\n')
		{
		  reconstructed_template += "\\n";
		  continue;
		}
	      else if (c == '\r')
		{
		  reconstructed_template += "\\r";
		  continue;
		}
	      else if (c == '\t')
		{
		  reconstructed_template += "\\t";
		  continue;
		}
	      reconstructed_template += c;
	    }
	}
      else if (piece.tag == Fmt::ffi::Piece::Tag::NextArgument)
	{
	  reconstructed_template += "{";

	  const auto &argument = piece.next_argument._0;
	  const auto &position = argument.position;

	  switch (position.tag)
	    {
	    case Fmt::ffi::Position::Tag::ArgumentImplicitlyIs:
	      break;
	    case Fmt::ffi::Position::Tag::ArgumentIs:
	      reconstructed_template
		+= std::to_string (position.argument_is._0);
	      break;
	    case Fmt::ffi::Position::Tag::ArgumentNamed:
	      reconstructed_template += position.argument_named._0.to_string ();
	      break;
	    }

	  // Add format specifiers if any (like :?, :x, etc.)
	  const auto &format_spec = argument.format;

	  bool has_format_spec = false;
	  std::string format_part;

	  // For now, skipping the complex format specifications that
	  // use FFIOpt since FFIOpt::get_opt() has a bug.

	  // Alignment
	  if (format_spec.align != Fmt::ffi::Alignment::AlignUnknown)
	    {
	      has_format_spec = true;
	      switch (format_spec.align)
		{
		case Fmt::ffi::Alignment::AlignLeft:
		  format_part += "<";
		  break;
		case Fmt::ffi::Alignment::AlignRight:
		  format_part += ">";
		  break;
		case Fmt::ffi::Alignment::AlignCenter:
		  format_part += "^";
		  break;
		case Fmt::ffi::Alignment::AlignUnknown:
		  break;
		}
	    }

	  // Alternate flag
	  if (format_spec.alternate)
	    {
	      has_format_spec = true;
	      format_part += "#";
	    }

	  // Zero pad flag
	  if (format_spec.zero_pad)
	    {
	      has_format_spec = true;
	      format_part += "0";
	    }

	  // Width
	  if (format_spec.width.tag != Fmt::ffi::Count::Tag::CountImplied)
	    {
	      has_format_spec = true;
	      switch (format_spec.width.tag)
		{
		case Fmt::ffi::Count::Tag::CountIs:
		  format_part += std::to_string (format_spec.width.count_is._0);
		  break;
		case Fmt::ffi::Count::Tag::CountIsParam:
		  format_part
		    += std::to_string (format_spec.width.count_is_param._0)
		       + "$";
		  break;
		case Fmt::ffi::Count::Tag::CountIsName:
		  format_part
		    += format_spec.width.count_is_name._0.to_string () + "$";
		  break;
		case Fmt::ffi::Count::Tag::CountIsStar:
		  format_part += "*";
		  break;
		case Fmt::ffi::Count::Tag::CountImplied:
		  break;
		}
	    }

	  // Precision
	  if (format_spec.precision.tag != Fmt::ffi::Count::Tag::CountImplied)
	    {
	      has_format_spec = true;
	      format_part += ".";
	      switch (format_spec.precision.tag)
		{
		case Fmt::ffi::Count::Tag::CountIs:
		  format_part
		    += std::to_string (format_spec.precision.count_is._0);
		  break;
		case Fmt::ffi::Count::Tag::CountIsParam:
		  format_part
		    += std::to_string (format_spec.precision.count_is_param._0)
		       + "$";
		  break;
		case Fmt::ffi::Count::Tag::CountIsName:
		  format_part
		    += format_spec.precision.count_is_name._0.to_string ()
		       + "$";
		  break;
		case Fmt::ffi::Count::Tag::CountIsStar:
		  format_part += "*";
		  break;
		case Fmt::ffi::Count::Tag::CountImplied:
		  break;
		}
	    }

	  // Type/trait (like ?, x, X, etc.)
	  std::string type_str = format_spec.ty.to_string ();
	  if (!type_str.empty ())
	    {
	      has_format_spec = true;
	      format_part += type_str;
	    }

	  // Add the format specification if any
	  if (has_format_spec)
	    {
	      reconstructed_template += ":";
	      reconstructed_template += format_part;
	    }

	  reconstructed_template += "}";
	}
    }
  reconstructed_template += "\"";

  push (Rust::Token::make_string (fmt.get_locus (), reconstructed_template));

  // Visit format arguments if any exist
  auto &arguments = fmt.get_arguments ();
  if (!arguments.empty ())
    {
      push (Rust::Token::make (COMMA, fmt.get_locus ()));

      auto &args = arguments.get_args ();
      for (size_t i = 0; i < args.size (); ++i)
	{
	  if (i > 0)
	    {
	      push (Rust::Token::make (COMMA, fmt.get_locus ()));
	    }

	  auto kind = args[i].get_kind ();

	  // Handle named arguments: name = expr
	  if (kind.kind == FormatArgumentKind::Kind::Named)
	    {
	      auto ident = kind.get_ident ().as_string ();
	      push (Rust::Token::make_identifier (fmt.get_locus (),
						  std::move (ident)));
	      push (Rust::Token::make (EQUAL, fmt.get_locus ()));
	    }
	  // Note: Captured arguments are handled implicitly in the
	  // template reconstruction They don't need explicit "name ="
	  // syntax in the reconstructed macro call

	  auto &expr = args[i].get_expr ();
	  expr.accept_vis (*this);
	}
    }

  push (Rust::Token::make (RIGHT_PAREN, fmt.get_locus ()));
}

void
TokenCollector::visit (AST::OffsetOf &offset_of)
{
  auto loc = offset_of.get_locus ();

  push (Rust::Token::make_identifier (loc, "offset_of"));
  push (Rust::Token::make (EXCLAM, loc));
  push (Rust::Token::make (LEFT_PAREN, loc));

  visit (offset_of.get_type ());

  push (Rust::Token::make (COMMA, loc));

  push (Rust::Token::make_identifier (offset_of.get_field ()));

  push (Rust::Token::make (RIGHT_PAREN, loc));
}

} // namespace AST
} // namespace Rust
