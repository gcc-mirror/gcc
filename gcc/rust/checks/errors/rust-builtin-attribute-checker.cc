// Copyright (C) 2026 Free Software Foundation, Inc.

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

#include "rust-builtin-attribute-checker.h"
#include "optional.h"
#include "rust-attributes.h"
#include "rust-attribute-values.h"
#include "rust-session-manager.h"

namespace Rust {
namespace Analysis {

using Attrs = Values::Attributes;

void
check_inner_attribute (const AST::Attribute &attribute)
{
  auto result_opt = lookup_builtin (attribute);
  if (!result_opt.has_value ())
    return;
  auto result = result_opt.value ();

  if (Attributes::valid_outer_attribute (result.name))
    rust_error_at (attribute.get_locus (),
		   "attribute cannot be used at crate level");

  if (result.name == Values::Attributes::NEEDS_ALLOCATOR)
    {
      rust_warning_at (attribute.get_locus (), 0,
		       "%<#[%s]%> is not implemented yet and has no effect",
		       attribute.as_string ().c_str ());
    }
}

/**
 * Check that the string given to #[doc(alias = ...)] or #[doc(alias(...))] is
 * valid.
 *
 * This means no whitespace characters other than spaces and no quoting
 * characters.
 */
static void
check_doc_alias (const std::string &alias_input, const location_t locus)
{
  // FIXME: The locus here is for the whole attribute. Can we get the locus
  // of the alias input instead?
  for (auto c : alias_input)
    if ((ISSPACE (c) && c != ' ') || c == '\'' || c == '\"')
      {
	auto to_print = std::string (1, c);
	switch (c)
	  {
	  case '\n':
	    to_print = "\\n";
	    break;
	  case '\t':
	    to_print = "\\t";
	    break;
	  default:
	    break;
	  }
	rust_error_at (locus,
		       "invalid character used in %<#[doc(alias)]%> input: %qs",
		       to_print.c_str ());
      }

  if (alias_input.empty ())
    return;

  if (alias_input.front () == ' ' || alias_input.back () == ' ')
    rust_error_at (locus,
		   "%<#[doc(alias)]%> input cannot start or end with a space");
}

// This namespace contains handlers for the builtin attribute checker,
// those handlers must verify the attribute internal structure and emit the
// appropriate error message if the structure is incorrect.
//
// They DO NOT check the attribute validity on the parent item.
namespace handlers {

void
doc (const AST::Attribute &attribute)
{
  if (!attribute.has_attr_input ())
    {
      rust_error_at (
	attribute.get_locus (),
	"valid forms for the attribute are "
	"%<#[doc(hidden|inline|...)]%> and %<#[doc = \" string \"]%>");
      return;
    }

  switch (attribute.get_attr_input ().get_attr_input_type ())
    {
    case AST::AttrInput::LITERAL:
    case AST::AttrInput::META_ITEM:
    case AST::AttrInput::EXPR:
      break;
      // FIXME: Handle them as well

    case AST::AttrInput::TOKEN_TREE:
      {
	// FIXME: This doesn't check for #[doc(alias(...))]
	const auto &option = static_cast<const AST::DelimTokenTree &> (
	  attribute.get_attr_input ());
	auto *meta_item = option.parse_to_meta_item ();

	for (auto &item : meta_item->get_items ())
	  {
	    if (item->is_key_value_pair ())
	      {
		auto name_value
		  = static_cast<AST::MetaNameValueStr *> (item.get ())
		      ->get_name_value_pair ();

		// FIXME: Check for other stuff than #[doc(alias = ...)]
		if (name_value.first.as_string () == "alias")
		  check_doc_alias (name_value.second, attribute.get_locus ());
	      }
	  }
	break;
      }
    }
}

void
deprecated (const AST::Attribute &attribute)
{
  if (!attribute.has_attr_input ())
    return;

  const auto &input = attribute.get_attr_input ();

  if (input.get_attr_input_type () != AST::AttrInput::META_ITEM)
    return;

  auto &meta = static_cast<const AST::AttrInputMetaItemContainer &> (input);

  for (auto &current : meta.get_items ())
    {
      switch (current->get_kind ())
	{
	case AST::MetaItemInner::Kind::MetaItem:
	  {
	    auto *meta_item = static_cast<AST::MetaItem *> (current.get ());

	    switch (meta_item->get_item_kind ())
	      {
	      case AST::MetaItem::ItemKind::NameValueStr:
		{
		  auto *nv = static_cast<AST::MetaNameValueStr *> (meta_item);

		  const std::string key = nv->get_name ().as_string ();

		  if (key != "since" && key != "note")
		    {
		      rust_error_at (nv->get_locus (), "unknown meta item %qs",
				     key.c_str ());
		      rust_inform (nv->get_locus (),
				   "expected one of %<since%>, %<note%>");
		    }
		}
		break;

	      case AST::MetaItem::ItemKind::Path:
		{
		  // #[deprecated(a,a)]
		  auto *p = static_cast<AST::MetaItemPath *> (meta_item);

		  std::string ident = p->get_path ().as_string ();

		  rust_error_at (p->get_locus (), "unknown meta item %qs",
				 ident.c_str ());
		  rust_inform (p->get_locus (),
			       "expected one of %<since%>, %<note%>");
		}
		break;

	      case AST::MetaItem::ItemKind::Word:
		{
		  // #[deprecated("a")]
		  auto *w = static_cast<AST::MetaWord *> (meta_item);

		  rust_error_at (
		    w->get_locus (),
		    "item in %<deprecated%> must be a key/value pair");
		}
		break;

	      case AST::MetaItem::ItemKind::PathExpr:
		{
		  // #[deprecated(since=a)]
		  auto *px = static_cast<AST::MetaItemPathExpr *> (meta_item);

		  rust_error_at (
		    px->get_locus (),
		    "expected unsuffixed literal or identifier, found %qs",
		    px->get_expr ().as_string ().c_str ());
		}
		break;

	      case AST::MetaItem::ItemKind::Seq:
	      case AST::MetaItem::ItemKind::ListPaths:
	      case AST::MetaItem::ItemKind::ListNameValueStr:
	      default:
		gcc_unreachable ();
		break;
	      }
	  }
	  break;

	case AST::MetaItemInner::Kind::LitExpr:
	default:
	  gcc_unreachable ();
	  break;
	}
    }
}

void
link_section (const AST::Attribute &attribute)
{
  if (!attribute.has_attr_input ())
    {
      rust_error_at (attribute.get_locus (),
		     "malformed %<link_section%> attribute input");
      rust_inform (attribute.get_locus (),
		   "must be of the form: %<#[link_section = \"name\"]%>");
    }
}

void
export_name (const AST::Attribute &attribute)
{
  if (!attribute.has_attr_input ())
    {
      rust_error_at (attribute.get_locus (),
		     "malformed %<export_name%> attribute input");
      rust_inform (attribute.get_locus (),
		   "must be of the form: %<#[export_name = \"name\"]%>");
      return;
    }

  auto &attr_input = attribute.get_attr_input ();
  if (attr_input.get_attr_input_type ()
      == AST::AttrInput::AttrInputType::LITERAL)
    {
      auto &literal_expr
	= static_cast<AST::AttrInputLiteral &> (attr_input).get_literal ();
      auto lit_type = literal_expr.get_lit_type ();
      switch (lit_type)
	{
	case AST::Literal::LitType::STRING:
	case AST::Literal::LitType::RAW_STRING:
	case AST::Literal::LitType::BYTE_STRING:
	  return;
	default:
	  break;
	}
    }

  rust_error_at (attribute.get_locus (), "attribute must be a string literal");
}

void
lint (const AST::Attribute &attribute)
{
  if (!attribute.has_attr_input ())
    {
      auto name = attribute.get_path ().as_string ();
      rust_error_at (attribute.get_locus (), "malformed %qs attribute input",
		     name.c_str ());
      rust_inform (attribute.get_locus (),
		   "must be of the form: %<#[%s(lint1, lint2, ...)]%>",
		   name.c_str ());
    }
}

void
link_name (const AST::Attribute &attribute)
{
  if (!attribute.has_attr_input ())
    {
      rust_error_at (attribute.get_locus (),
		     "malformed %<link_name%> attribute input");
      rust_inform (attribute.get_locus (),
		   "must be of the form: %<#[link_name = \"name\"]%>");
    }
}

namespace {
void
check_crate_type (const AST::Attribute &attribute)
{
  if (!Session::get_instance ().options.is_proc_macro ())
    {
      auto name = attribute.get_path ().as_string ();

      rust_error_at (attribute.get_locus (),
		     "the %<#[%s]%> attribute is only usable with crates of "
		     "the %<proc-macro%> crate type",
		     name.c_str ());
    }
}
} // namespace

static void
proc_macro_derive (const AST::Attribute &attribute)
{
  if (!attribute.has_attr_input ())
    {
      auto name = attribute.get_path ().as_string ();
      rust_error_at (attribute.get_locus (), "malformed %qs attribute input",
		     name.c_str ());
      rust_inform (attribute.get_locus (),
		   "must be of the form: %<#[proc_macro_derive(TraitName, "
		   "/*opt*/ attributes(name1, name2, ...))]%>");
    }
  check_crate_type (attribute);
}

static void
proc_macro (const AST::Attribute &attribute)
{
  check_crate_type (attribute);
}

static void
target_feature (const AST::Attribute &attribute)
{
  if (!attribute.has_attr_input ())
    {
      rust_error_at (attribute.get_locus (),
		     "malformed %<target_feature%> attribute input");
      rust_inform (attribute.get_locus (),
		   "must be of the form: %<#[target_feature(enable = "
		   "\"name\")]%>");
    }
}

void
expect_no_input (const AST::Attribute &attribute)
{
  if (attribute.has_attr_input ())
    {
      std::string attr_name = attribute.get_path ().as_string ();
      rust_error_at (attribute.get_locus (), "malformed %<%s%> attribute input",
		     attr_name.c_str ());
      rust_inform (attribute.get_locus (), "must be of the form: %<#[%s]%>",
		   attr_name.c_str ());
    }
}

} // namespace handlers

const std::unordered_map<std::string, std::function<void (AST::Attribute &)>>
  attribute_checking_handlers = {
    {Attrs::DOC, handlers::doc},
    {Attrs::DEPRECATED, handlers::deprecated},
    {Attrs::LINK_SECTION, handlers::link_section},
    {Attrs::EXPORT_NAME, handlers::export_name},
    {Attrs::NO_MANGLE, handlers::expect_no_input},
    {Attrs::ALLOW, handlers::lint},
    {Attrs::DENY, handlers::lint},
    {Attrs::WARN, handlers::lint},
    {Attrs::FORBID, handlers::lint},
    {Attrs::LINK_NAME, handlers::link_name},
    {Attrs::PROC_MACRO_DERIVE, handlers::proc_macro_derive},
    {Attrs::PROC_MACRO, handlers::proc_macro},
    {Attrs::PROC_MACRO_ATTRIBUTE, handlers::proc_macro},
    {Attrs::TARGET_FEATURE, handlers::target_feature},
    {Attrs::RUSTC_STD_INTERNAL_SYMBOL, handlers::expect_no_input},
    {Attrs::RUSTC_ALLOCATOR, handlers::expect_no_input},
    {Attrs::RUSTC_ALLOCATOR_NOUNWIND, handlers::expect_no_input},
    {Attrs::GLOBAL_ALLOCATOR, handlers::expect_no_input},
    {Attrs::RUSTC_CONVERSION_SUGGESTION, handlers::expect_no_input},
};

tl::optional<std::function<void (AST::Attribute &)>>
lookup_handler (std::string attr_name)
{
  auto res = attribute_checking_handlers.find (attr_name);
  if (res != attribute_checking_handlers.cend ())
    return res->second;
  return tl::nullopt;
}

static void
check_no_mangle_function (const AST::Attribute &attribute,
			  const AST::Function &fun)
{
  if (!is_ascii_only (fun.get_function_name ().as_string ()))
    rust_error_at (fun.get_function_name ().get_locus (),
		   "the %<#[no_mangle]%> attribute requires ASCII identifier");
}

/**
 * Emit an error when an attribute is attached
 * to an incompatable item type. e.g.:
 *
 * #[cold]
 * struct A(u8, u8);
 *
 * Note that "#[derive]" is handled
 * explicitly in rust-derive.cc
 */
void
check_valid_attribute_for_item (const AST::Attribute &attr,
				const AST::Item &item)
{
  if (item.get_item_kind () != AST::Item::Kind::Function
      && (attr.get_path () == Values::Attributes::TARGET_FEATURE
	  || attr.get_path () == Values::Attributes::COLD
	  || attr.get_path () == Values::Attributes::INLINE))
    {
      rust_error_at (attr.get_locus (),
		     "the %<#[%s]%> attribute may only be applied to functions",
		     attr.get_path ().as_string ().c_str ());
    }
  else if (attr.get_path () == Values::Attributes::REPR
	   && item.get_item_kind () != AST::Item::Kind::Enum
	   && item.get_item_kind () != AST::Item::Kind::Union
	   && item.get_item_kind () != AST::Item::Kind::Struct)
    {
      rust_error_at (attr.get_locus (),
		     "the %<#[%s]%> attribute may only be applied "
		     "to structs, enums and unions",
		     attr.get_path ().as_string ().c_str ());
    }
  else if (attr.get_path () == Values::Attributes::GLOBAL_ALLOCATOR
	   && item.get_item_kind () != AST::Item::Kind::StaticItem)
    {
      rust_error_at (attr.get_locus (),
		     "the %<#[%s]%> attribute may only be applied "
		     "to static items",
		     attr.get_path ().as_string ().c_str ());
    }
  else if (attr.get_path () == Values::Attributes::RUSTC_CONVERSION_SUGGESTION)
    {
      auto attr_path = attr.get_path ().as_string ();
      rust_warning_at (attr.get_locus (), 0,
		       "%<#[%s]%> is not implemented yet and has no effect",
		       attr_path.c_str ());
      if (item.get_item_kind () != AST::Item::Kind::Function)
	{
	  rust_error_at (item.get_locus (),
			 "%<#[%s]%> can only be applied to functions",
			 attr_path.c_str ());
	}
    }
}

BuiltinAttributeChecker::BuiltinAttributeChecker () {}

void
BuiltinAttributeChecker::go (AST::Crate &crate)
{
  visit (crate);
}

void
BuiltinAttributeChecker::visit (AST::Crate &crate)
{
  for (auto &attr : crate.get_inner_attrs ())
    {
      check_inner_attribute (attr);
    }

  AST::DefaultASTVisitor::visit (crate);
}

void
BuiltinAttributeChecker::visit (AST::Attribute &attribute)
{
  lookup_handler (attribute.get_path ().as_string ()).map ([&] (auto handler) {
    handler (attribute);
  });
  AST::DefaultASTVisitor::visit (attribute);
}

void
BuiltinAttributeChecker::visit (AST::Module &module)
{
  default_outer_attribute_check (module);
}

void
BuiltinAttributeChecker::visit (AST::ExternCrate &extern_crate)
{
  default_outer_attribute_check (extern_crate);
}

void
BuiltinAttributeChecker::visit (AST::UseDeclaration &declaration)
{
  default_outer_attribute_check (declaration);
}

void
BuiltinAttributeChecker::visit (AST::Function &function)
{
  BuiltinAttrDefinition result;
  for (auto &attribute : function.get_outer_attrs ())
    {
      check_valid_attribute_for_item (attribute, function);

      auto result = lookup_builtin (attribute);
      if (!result)
	return;

      if (result->name == Attrs::TARGET_FEATURE)
	{
	  if (!function.get_qualifiers ().is_unsafe ())
	    {
	      rust_error_at (
		attribute.get_locus (),
		"the %<#[target_feature]%> attribute can only be applied "
		"to %<unsafe%> functions");
	    }
	}
      else if (result->name == Attrs::NO_MANGLE)
	{
	  check_no_mangle_function (attribute, function);
	}
    }

  AST::DefaultASTVisitor::visit (function);
}

void
BuiltinAttributeChecker::visit (AST::TypeAlias &alias)
{
  default_outer_attribute_check (alias);
}

void
BuiltinAttributeChecker::visit (AST::StructStruct &struct_item)
{
  default_outer_attribute_check (struct_item);
}

void
BuiltinAttributeChecker::visit (AST::TupleStruct &tuple_struct)
{
  default_outer_attribute_check (tuple_struct);
}

void
BuiltinAttributeChecker::visit (AST::Enum &enumeration)
{
  default_outer_attribute_check (enumeration);
}

void
BuiltinAttributeChecker::visit (AST::Union &u)
{
  default_outer_attribute_check (u);
}

void
BuiltinAttributeChecker::visit (AST::ConstantItem &item)
{
  default_outer_attribute_check (item);
}

void
BuiltinAttributeChecker::visit (AST::StaticItem &item)
{
  for (auto &attr : item.get_outer_attrs ())
    check_valid_attribute_for_item (attr, item);

  AST::DefaultASTVisitor::visit (item);
}

void
BuiltinAttributeChecker::visit (AST::Trait &trait)
{
  default_outer_attribute_check (trait);
}

void
BuiltinAttributeChecker::visit (AST::InherentImpl &impl)
{
  default_outer_attribute_check (impl);
}

void
BuiltinAttributeChecker::visit (AST::TraitImpl &impl)
{
  default_outer_attribute_check (impl);
}

void
BuiltinAttributeChecker::visit (AST::ExternBlock &block)
{
  default_outer_attribute_check (block);
}

} // namespace Analysis
} // namespace Rust
