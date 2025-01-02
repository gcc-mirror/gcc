/* General AST-related method implementations for Rust frontend.
   Copyright (C) 2009-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "rust-ast.h"
#include "optional.h"
#include "rust-builtin-ast-nodes.h"
#include "rust-system.h"
#include "rust-ast-full.h"
#include "rust-diagnostics.h"
#include "rust-ast-visitor.h"
#include "rust-macro.h"
#include "rust-session-manager.h"
#include "rust-lex.h"
#include "rust-parse.h"
#include "rust-operators.h"
#include "rust-dir-owner.h"
#include "rust-attribute-values.h"

/* Compilation unit used for various AST-related functions that would make
 * the headers too long if they were defined inline and don't receive any
 * benefits from being defined inline because they are virtual. Also used
 * for various other stuff. */

namespace Rust {
namespace AST {

SingleASTNode::SingleASTNode (SingleASTNode const &other)
{
  kind = other.kind;
  switch (kind)
    {
    case EXPRESSION:
      expr = other.expr->clone_expr ();
      break;

    case ITEM:
      item = other.item->clone_item ();
      break;

    case STMT:
      stmt = other.stmt->clone_stmt ();
      break;

    case EXTERN:
      external_item = other.external_item->clone_external_item ();
      break;

    case ASSOC_ITEM:
      assoc_item = other.assoc_item->clone_associated_item ();
      break;

    case TYPE:
      type = other.type->clone_type ();
      break;
    }
}

SingleASTNode
SingleASTNode::operator= (SingleASTNode const &other)
{
  kind = other.kind;
  switch (kind)
    {
    case EXPRESSION:
      expr = other.expr->clone_expr ();
      break;

    case ITEM:
      item = other.item->clone_item ();
      break;

    case STMT:
      stmt = other.stmt->clone_stmt ();
      break;

    case EXTERN:
      external_item = other.external_item->clone_external_item ();
      break;

    case ASSOC_ITEM:
      assoc_item = other.assoc_item->clone_associated_item ();
      break;

    case TYPE:
      type = other.type->clone_type ();
      break;
    }
  return *this;
}

void
SingleASTNode::accept_vis (ASTVisitor &vis)
{
  switch (kind)
    {
    case EXPRESSION:
      expr->accept_vis (vis);
      break;

    case ITEM:
      item->accept_vis (vis);
      break;

    case STMT:
      stmt->accept_vis (vis);
      break;

    case EXTERN:
      external_item->accept_vis (vis);
      break;

    case ASSOC_ITEM:
      assoc_item->accept_vis (vis);
      break;

    case TYPE:
      type->accept_vis (vis);
      break;
    }
}

bool
SingleASTNode::is_error ()
{
  switch (kind)
    {
    case EXPRESSION:
      return expr == nullptr;
    case ITEM:
      return item == nullptr;
    case STMT:
      return stmt == nullptr;
    case EXTERN:
      return external_item == nullptr;
    case ASSOC_ITEM:
      return assoc_item == nullptr;
    case TYPE:
      return type == nullptr;
    }

  rust_unreachable ();
  return true;
}

std::string
SingleASTNode::as_string () const
{
  switch (kind)
    {
    case EXPRESSION:
      return "Expr: " + expr->as_string ();
    case ITEM:
      return "Item: " + item->as_string ();
    case STMT:
      return "Stmt: " + stmt->as_string ();
    case EXTERN:
      return "External Item: " + external_item->as_string ();
    case ASSOC_ITEM:
      return "Associated Item: " + assoc_item->as_string ();
    case TYPE:
      return "Type: " + type->as_string ();
    }

  rust_unreachable ();
  return "";
}

std::string
Crate::as_string () const
{
  rust_debug ("beginning crate recursive as-string");

  std::string str ("Crate: ");

  // inner attributes
  str += append_attributes (inner_attrs, INNER);

  // items
  str += "\n items: ";
  if (items.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &item : items)
	{
	  // DEBUG: null pointer check
	  if (item == nullptr)
	    {
	      rust_debug ("something really terrible has gone wrong - "
			  "null pointer item in crate.");
	      return "NULL_POINTER_MARK";
	    }

	  str += "\n  " + item->as_string ();
	}
    }

  return str + "\n";
}

std::string
Attribute::as_string () const
{
  std::string path_str = path.as_string ();
  if (attr_input == nullptr)
    return path_str;
  else
    return path_str + attr_input->as_string ();
}

bool
Attribute::is_derive () const
{
  return has_attr_input () && get_path () == "derive";
}

/**
 * Returns a list of traits to derive from within a given attribute.
 *
 * @param attrs The attributes on the item to derive
 */
std::vector<std::reference_wrapper<AST::SimplePath>>
Attribute::get_traits_to_derive ()
{
  rust_assert (this->is_derive ());

  this->parse_attr_to_meta_item ();
  std::vector<std::reference_wrapper<AST::SimplePath>> result;
  auto &input = get_attr_input ();
  switch (input.get_attr_input_type ())
    {
      case AST::AttrInput::META_ITEM: {
	auto &meta = static_cast<AST::AttrInputMetaItemContainer &> (input);
	for (auto &current : meta.get_items ())
	  {
	    // HACK: Find a better way to achieve the downcast.
	    switch (current->get_kind ())
	      {
		case AST::MetaItemInner::Kind::MetaItem: {
		  // Let raw pointer go out of scope without freeing, it doesn't
		  // own the data anyway
		  auto meta_item
		    = static_cast<AST::MetaItem *> (current.get ());
		  switch (meta_item->get_item_kind ())
		    {
		      case AST::MetaItem::ItemKind::Path: {
			auto path
			  = static_cast<AST::MetaItemPath *> (meta_item);
			result.push_back (path->get_path ());
		      }
		      break;
		      case AST::MetaItem::ItemKind::Word: {
			auto word = static_cast<AST::MetaWord *> (meta_item);
			// Convert current word to path
			current
			  = make_unique<AST::MetaItemPath> (AST::MetaItemPath (
			    AST::SimplePath (word->get_ident ())));
			auto path
			  = static_cast<AST::MetaItemPath *> (current.get ());

			result.push_back (path->get_path ());
		      }
		      break;
		    case AST::MetaItem::ItemKind::ListPaths:
		    case AST::MetaItem::ItemKind::NameValueStr:
		    case AST::MetaItem::ItemKind::PathLit:
		    case AST::MetaItem::ItemKind::Seq:
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
      break;
    case AST::AttrInput::TOKEN_TREE:
    case AST::AttrInput::LITERAL:
    case AST::AttrInput::MACRO:
      rust_unreachable ();
      break;
    }
  return result;
}

// Copy constructor must deep copy attr_input as unique pointer
Attribute::Attribute (Attribute const &other)
  : path (other.path), locus (other.locus)
{
  // guard to protect from null pointer dereference
  if (other.attr_input != nullptr)
    attr_input = other.attr_input->clone_attr_input ();
}

// overload assignment operator to use custom clone method
Attribute &
Attribute::operator= (Attribute const &other)
{
  path = other.path;
  locus = other.locus;
  // guard to protect from null pointer dereference
  if (other.attr_input != nullptr)
    attr_input = other.attr_input->clone_attr_input ();
  else
    attr_input = nullptr;

  return *this;
}

std::string
DelimTokenTree::as_string () const
{
  std::string start_delim;
  std::string end_delim;
  switch (delim_type)
    {
    case PARENS:
      start_delim = "(";
      end_delim = ")";
      break;
    case SQUARE:
      start_delim = "[";
      end_delim = "]";
      break;
    case CURLY:
      start_delim = "{";
      end_delim = "}";
      break;
    default:
      rust_debug ("Invalid delimiter type, "
		  "Should be PARENS, SQUARE, or CURLY.");
      return "Invalid delimiter type";
    }
  std::string str = start_delim;
  if (!token_trees.empty ())
    {
      for (const auto &tree : token_trees)
	{
	  // DEBUG: null pointer check
	  if (tree == nullptr)
	    {
	      rust_debug (
		"something really terrible has gone wrong - null pointer "
		"token tree in delim token tree.");
	      return "NULL_POINTER_MARK";
	    }

	  str += tree->as_string ();
	}
    }
  str += end_delim;

  return str;
}

std::string
Token::as_string () const
{
  if (tok_ref->has_str ())
    {
      std::string str = tok_ref->get_str ();

      std::string quote = is_string_lit () ? "\"" : "";
      return quote + str + quote;
    }
  else
    {
      return tok_ref->get_token_description ();
    }
}

std::string
SimplePathSegment::as_string () const
{
  return segment_name;
}

const std::string
SimplePath::as_string () const
{
  std::string path;
  if (opening_scope_resolution)
    path = "::";

  // crappy hack because doing proper for loop would be more code
  bool first_time = true;
  for (const auto &segment : segments)
    {
      if (first_time)
	{
	  path += segment.as_string ();
	  first_time = false;
	}
      else
	{
	  path += "::" + segment.as_string ();
	}

      // DEBUG: remove later. Checks for path error.
      if (segment.is_error ())
	{
	  rust_debug ("segment in path is error - this should've been filtered "
		      "out. first segment "
		      "was '%s'",
		      segments.at (0).as_string ().c_str ());
	}
    }

  return path;
}

std::string
Visibility::as_string () const
{
  switch (vis_type)
    {
    case PRIV:
      return std::string ("");
    case PUB:
      return std::string ("pub");
    case PUB_CRATE:
      return std::string ("pub(crate)");
    case PUB_SELF:
      return std::string ("pub(self)");
    case PUB_SUPER:
      return std::string ("pub(super)");
    case PUB_IN_PATH:
      return std::string ("pub(in ") + in_path.as_string () + std::string (")");
    default:
      rust_unreachable ();
    }
}

// Creates a string that reflects the visibility stored.
std::string
VisItem::as_string () const
{
  // FIXME: can't do formatting on string to make identation occur.
  std::string str;

  if (!outer_attrs.empty ())
    {
      for (const auto &attr : outer_attrs)
	str += attr.as_string () + "\n";
    }

  if (has_visibility ())
    str += visibility.as_string () + " ";

  return str;
}

std::string
Module::as_string () const
{
  std::string str = VisItem::as_string () + "mod " + module_name.as_string ();

  // Return early if we're dealing with an unloaded module as their body resides
  // in a different file
  if (kind == ModuleKind::UNLOADED)
    return str + "\n no body (reference to external file)\n";

  // inner attributes
  str += append_attributes (inner_attrs, INNER);

  // items
  str += "\n items: ";

  // This can still happen if the module is loaded but empty, i.e. `mod foo {}`
  if (items.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &item : items)
	{
	  // DEBUG: null pointer check
	  if (item == nullptr)
	    {
	      rust_debug ("something really terrible has gone wrong - "
			  "null pointer item in crate.");
	      return "NULL_POINTER_MARK";
	    }

	  str += "\n  " + item->as_string ();
	}
    }

  return str + "\n";
}

std::string
StaticItem::as_string () const
{
  std::string str = VisItem::as_string ();

  str += indent_spaces (stay) + "static";

  if (has_mut)
    str += " mut";

  str += " " + name.as_string ();

  // DEBUG: null pointer check
  if (type == nullptr)
    {
      rust_debug ("something really terrible has gone wrong - null "
		  "pointer type in static item.");
      return "NULL_POINTER_MARK";
    }
  str += "\n" + indent_spaces (stay) + "Type: " + type->as_string ();

  // DEBUG: null pointer check
  if (expr == nullptr)
    {
      rust_debug ("something really terrible has gone wrong - null "
		  "pointer expr in static item.");
      return "NULL_POINTER_MARK";
    }
  str += "\n" + indent_spaces (stay) + "Expression: " + expr->as_string ();

  return str + "\n";
}

std::string
ExternCrate::as_string () const
{
  std::string str = VisItem::as_string ();

  str += "extern crate " + referenced_crate;

  if (has_as_clause ())
    str += " as " + as_clause_name;

  return str;
}

std::string
TupleStruct::as_string () const
{
  std::string str = VisItem::as_string ();

  str += "struct " + struct_name.as_string ();

  // generic params
  str += "\n Generic params: ";
  if (generic_params.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &param : generic_params)
	{
	  // DEBUG: null pointer check
	  if (param == nullptr)
	    {
	      rust_debug (
		"something really terrible has gone wrong - null pointer "
		"generic param in enum.");
	      return "NULL_POINTER_MARK";
	    }

	  str += "\n  " + param->as_string ();
	}
    }

  // tuple fields
  str += "\n Tuple fields: ";
  if (fields.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &field : fields)
	str += "\n  " + field.as_string ();
    }

  str += "\n Where clause: ";
  if (has_where_clause ())
    str += where_clause.as_string ();
  else
    str += "none";

  return str;
}

std::string
ConstantItem::as_string () const
{
  std::string str = VisItem::as_string ();

  str += "const " + identifier;

  // DEBUG: null pointer check
  if (type == nullptr)
    {
      rust_debug ("something really terrible has gone wrong - null "
		  "pointer type in const item.");
      return "NULL_POINTER_MARK";
    }
  str += "\n  Type: " + type->as_string ();

  // DEBUG: null pointer check
  if (const_expr == nullptr)
    {
      rust_debug ("something really terrible has gone wrong - null "
		  "pointer expr in const item.");
      return "NULL_POINTER_MARK";
    }
  str += "\n  Expression: " + const_expr->as_string ();

  return str + "\n";
}

std::string
InherentImpl::as_string () const
{
  std::string str = VisItem::as_string ();

  str += "impl ";

  // generic params
  str += "\n Generic params: ";
  if (generic_params.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &param : generic_params)
	{
	  // DEBUG: null pointer check
	  if (param == nullptr)
	    {
	      rust_debug (
		"something really terrible has gone wrong - null pointer "
		"generic param in inherent impl.");
	      return "NULL_POINTER_MARK";
	    }

	  str += "\n  " + param->as_string ();
	}
    }

  str += "\n Type: " + trait_type->as_string ();

  str += "\n Where clause: ";
  if (has_where_clause ())
    str += where_clause.as_string ();
  else
    str += "none";

  // inner attributes
  str += append_attributes (inner_attrs, INNER);

  // inherent impl items
  str += "\n Inherent impl items: ";
  if (!has_impl_items ())
    {
      str += "none";
    }
  else
    {
      for (const auto &item : impl_items)
	str += "\n  " + item->as_string ();
    }

  return str;
}

std::string
StructStruct::as_string () const
{
  std::string str = VisItem::as_string ();

  str += "struct " + struct_name.as_string ();

  // generic params
  str += "\n Generic params: ";
  if (generic_params.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &param : generic_params)
	{
	  // DEBUG: null pointer check
	  if (param == nullptr)
	    {
	      rust_debug (
		"something really terrible has gone wrong - null pointer "
		"generic param in enum.");
	      return "NULL_POINTER_MARK";
	    }

	  str += "\n  " + param->as_string ();
	}
    }

  str += "\n Where clause: ";
  if (has_where_clause ())
    str += where_clause.as_string ();
  else
    str += "none";

  // struct fields
  str += "\n Struct fields: ";
  if (is_unit)
    {
      str += "none (unit)";
    }
  else if (fields.empty ())
    {
      str += "none (non-unit)";
    }
  else
    {
      for (const auto &field : fields)
	str += "\n  " + field.as_string ();
    }

  return str;
}

std::string
UseDeclaration::as_string () const
{
  std::string str = VisItem::as_string ();

  // DEBUG: null pointer check
  if (use_tree == nullptr)
    {
      rust_debug (
	"something really terrible has gone wrong - null pointer use tree in "
	"use declaration.");
      return "NULL_POINTER_MARK";
    }

  str += "use " + use_tree->as_string ();

  return str;
}

std::string
UseTreeGlob::as_string () const
{
  switch (glob_type)
    {
    case NO_PATH:
      return "*";
    case GLOBAL:
      return "::*";
      case PATH_PREFIXED: {
	std::string path_str = path.as_string ();
	return path_str + "::*";
      }
    default:
      // some kind of error
      return "ERROR-PATH";
    }
  rust_unreachable ();
}

std::string
UseTreeList::as_string () const
{
  std::string path_str;
  switch (path_type)
    {
    case NO_PATH:
      path_str = "{";
      break;
    case GLOBAL:
      path_str = "::{";
      break;
      case PATH_PREFIXED: {
	path_str = path.as_string () + "::{";
	break;
      }
    default:
      // some kind of error
      return "ERROR-PATH-LIST";
    }

  if (has_trees ())
    {
      auto i = trees.begin ();
      auto e = trees.end ();

      // DEBUG: null pointer check
      if (*i == nullptr)
	{
	  rust_debug ("something really terrible has gone wrong - null pointer "
		      "tree in use tree list.");
	  return "NULL_POINTER_MARK";
	}

      for (; i != e; i++)
	{
	  path_str += (*i)->as_string ();
	  if (e != i + 1)
	    path_str += ", ";
	}
    }
  else
    {
      path_str += "none";
    }

  return path_str + "}";
}

std::string
UseTreeRebind::as_string () const
{
  std::string path_str = path.as_string ();

  switch (bind_type)
    {
    case NONE:
      // nothing to add, just path
      break;
    case IDENTIFIER:
      path_str += " as " + identifier.as_string ();
      break;
    case WILDCARD:
      path_str += " as _";
      break;
    default:
      // error
      return "ERROR-PATH-REBIND";
    }

  return path_str;
}

std::string
Enum::as_string () const
{
  std::string str = VisItem::as_string ();
  str += enum_name.as_string ();

  // generic params
  str += "\n Generic params: ";
  if (generic_params.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &param : generic_params)
	{
	  // DEBUG: null pointer check
	  if (param == nullptr)
	    {
	      rust_debug (
		"something really terrible has gone wrong - null pointer "
		"generic param in enum.");
	      return "NULL_POINTER_MARK";
	    }

	  str += "\n  " + param->as_string ();
	}
    }

  str += "\n Where clause: ";
  if (has_where_clause ())
    str += where_clause.as_string ();
  else
    str += "none";

  // items
  str += "\n Items: ";
  if (items.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &item : items)
	{
	  // DEBUG: null pointer check
	  if (item == nullptr)
	    {
	      rust_debug (
		"something really terrible has gone wrong - null pointer "
		"enum item in enum.");
	      return "NULL_POINTER_MARK";
	    }

	  str += "\n  " + item->as_string ();
	}
    }

  return str;
}

std::string
Trait::as_string () const
{
  std::string str = VisItem::as_string ();

  if (has_unsafe)
    str += "unsafe ";

  str += "trait " + name.as_string ();

  // generic params
  str += "\n Generic params: ";
  if (generic_params.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &param : generic_params)
	{
	  // DEBUG: null pointer check
	  if (param == nullptr)
	    {
	      rust_debug (
		"something really terrible has gone wrong - null pointer "
		"generic param in trait.");
	      return "NULL_POINTER_MARK";
	    }

	  str += "\n  " + param->as_string ();
	}
    }

  str += "\n Type param bounds: ";
  if (!has_type_param_bounds ())
    {
      str += "none";
    }
  else
    {
      for (const auto &bound : type_param_bounds)
	{
	  // DEBUG: null pointer check
	  if (bound == nullptr)
	    {
	      rust_debug (
		"something really terrible has gone wrong - null pointer "
		"type param bound in trait.");
	      return "NULL_POINTER_MARK";
	    }

	  str += "\n  " + bound->as_string ();
	}
    }

  str += "\n Where clause: ";
  if (!has_where_clause ())
    str += "none";
  else
    str += where_clause.as_string ();

  str += "\n Trait items: ";
  if (!has_trait_items ())
    {
      str += "none";
    }
  else
    {
      for (const auto &item : trait_items)
	{
	  // DEBUG: null pointer check
	  if (item == nullptr)
	    {
	      rust_debug (
		"something really terrible has gone wrong - null pointer "
		"trait item in trait.");
	      return "NULL_POINTER_MARK";
	    }

	  str += "\n  " + item->as_string ();
	}
    }

  return str;
}

std::string
Union::as_string () const
{
  std::string str = VisItem::as_string ();

  str += "union " + union_name.as_string ();

  // generic params
  str += "\n Generic params: ";
  if (generic_params.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &param : generic_params)
	{
	  // DEBUG: null pointer check
	  if (param == nullptr)
	    {
	      rust_debug (
		"something really terrible has gone wrong - null pointer "
		"generic param in union.");
	      return "NULL_POINTER_MARK";
	    }

	  str += "\n  " + param->as_string ();
	}
    }

  str += "\n Where clause: ";
  if (has_where_clause ())
    str += where_clause.as_string ();
  else
    str += "none";

  // struct fields
  str += "\n Struct fields (variants): ";
  if (variants.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &field : variants)
	str += "\n  " + field.as_string ();
    }

  return str;
}

Function::Function (Function const &other)
  : VisItem (other), ExternalItem (other.get_node_id ()),
    qualifiers (other.qualifiers), function_name (other.function_name),
    where_clause (other.where_clause), locus (other.locus),
    is_default (other.is_default),
    is_external_function (other.is_external_function)
{
  // guard to prevent null dereference (always required)
  if (other.return_type != nullptr)
    return_type = other.return_type->clone_type ();

  // guard to prevent null dereference (only required if error state)
  if (other.has_body ())
    function_body = other.function_body.value ()->clone_block_expr ();
  else
    function_body = tl::nullopt;

  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());

  function_params.reserve (other.function_params.size ());
  for (const auto &e : other.function_params)
    function_params.push_back (e->clone_param ());
}

Function &
Function::operator= (Function const &other)
{
  VisItem::operator= (other);
  function_name = other.function_name;
  qualifiers = other.qualifiers;
  where_clause = other.where_clause;
  // visibility = other.visibility->clone_visibility();
  // outer_attrs = other.outer_attrs;
  locus = other.locus;
  is_default = other.is_default;
  is_external_function = other.is_external_function;

  // guard to prevent null dereference (always required)
  if (other.return_type != nullptr)
    return_type = other.return_type->clone_type ();
  else
    return_type = nullptr;

  // guard to prevent null dereference (only required if error state)
  if (other.has_body ())
    function_body = other.function_body.value ()->clone_block_expr ();
  else
    function_body = tl::nullopt;

  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());

  function_params.reserve (other.function_params.size ());
  for (const auto &e : other.function_params)
    function_params.push_back (e->clone_param ());

  return *this;
}
std::string
Function::as_string () const
{
  std::string str = VisItem::as_string () + "\n";
  std::string qstr = qualifiers.as_string ();
  if ("" != qstr)
    str += qstr + " ";

  if (has_return_type ())
    {
      // DEBUG: null pointer check
      if (return_type == nullptr)
	{
	  rust_debug (
	    "something really terrible has gone wrong - null pointer return "
	    "type in function.");
	  return "NULL_POINTER_MARK";
	}

      str += return_type->as_string () + " ";
    }
  else
    {
      str += "void ";
    }

  str += function_name.as_string ();

  if (has_generics ())
    {
      str += "<";

      auto i = generic_params.begin ();
      auto e = generic_params.end ();

      // DEBUG: null pointer check
      if (i == e)
	{
	  rust_debug ("something really terrible has gone wrong - null pointer "
		      "generic param in function item.");
	  return "NULL_POINTER_MARK";
	}

      for (; i != e; i++)
	{
	  str += (*i)->as_string ();
	  if (e != i + 1)
	    str += ", ";
	}
      str += ">";
    }

  if (has_function_params ())
    {
      auto i = function_params.begin ();
      auto e = function_params.end ();
      str += "(";
      for (; i != e; i++)
	{
	  str += (*i)->as_string ();
	  if (e != i + 1)
	    str += ", ";
	}
      str += ")";
    }
  else
    {
      str += "()";
    }

  if (has_where_clause ())
    str += " where " + where_clause.as_string ();

  str += "\n";

  if (has_body ())
    str += function_body.value ()->as_string () + "\n";

  return str;
}

std::string
WhereClause::as_string () const
{
  // just print where clause items, don't mention "where" or "where clause"
  std::string str;

  if (where_clause_items.empty ())
    {
      str = "none";
    }
  else
    {
      for (const auto &item : where_clause_items)
	str += "\n  " + item->as_string ();
    }

  return str;
}

std::string
BlockExpr::as_string () const
{
  std::string istr = indent_spaces (enter);
  std::string str = istr + "BlockExpr:\n" + istr;

  // get outer attributes
  str += append_attributes (outer_attrs, OUTER);

  // inner attributes
  str += append_attributes (inner_attrs, INNER);

  // statements
  str += "\n" + indent_spaces (stay) + "statements: ";
  if (statements.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &stmt : statements)
	{
	  // DEBUG: null pointer check
	  if (stmt == nullptr)
	    {
	      rust_debug (
		"something really terrible has gone wrong - null pointer "
		"stmt in block expr.");
	      return "NULL_POINTER_MARK";
	    }

	  str += "\n" + indent_spaces (stay) + stmt->as_string ();
	}
    }

  // final expression
  str += "\n" + indent_spaces (stay) + "final expression: ";
  if (expr == nullptr)
    str += "none";
  else
    str += "\n" + expr->as_string ();

  str += "\n" + indent_spaces (out);
  return str;
}

std::string
TraitImpl::as_string () const
{
  std::string str = VisItem::as_string ();

  if (has_unsafe)
    str += "unsafe ";

  str += "impl ";

  // generic params
  str += "\n Generic params: ";
  if (!has_generics ())
    {
      str += "none";
    }
  else
    {
      for (const auto &param : generic_params)
	str += "\n  " + param->as_string ();
    }

  str += "\n Has exclam: ";
  if (has_exclam)
    str += "true";
  else
    str += "false";

  str += "\n TypePath (to trait): " + trait_path.as_string ();

  str += "\n Type (struct to impl on): " + trait_type->as_string ();

  str += "\n Where clause: ";
  if (!has_where_clause ())
    str += "none";
  else
    str += where_clause.as_string ();

  // inner attributes
  str += append_attributes (inner_attrs, INNER);

  str += "\n trait impl items: ";
  if (!has_impl_items ())
    {
      str += "none";
    }
  else
    {
      for (const auto &item : impl_items)
	str += "\n  " + item->as_string ();
    }

  return str;
}

std::string
TypeAlias::as_string () const
{
  std::string str = VisItem::as_string ();

  str += " " + new_type_name.as_string ();

  // generic params
  str += "\n Generic params: ";
  if (!has_generics ())
    {
      str += "none";
    }
  else
    {
      auto i = generic_params.begin ();
      auto e = generic_params.end ();

      for (; i != e; i++)
	{
	  str += (*i)->as_string ();
	  if (e != i + 1)
	    str += ", ";
	}
    }

  str += "\n Where clause: ";
  if (!has_where_clause ())
    str += "none";
  else
    str += where_clause.as_string ();

  str += "\n Type: " + existing_type->as_string ();

  return str;
}

std::string
ExternBlock::as_string () const
{
  std::string str = VisItem::as_string ();

  str += "extern ";
  if (has_abi ())
    str += "\"" + abi + "\" ";

  str += append_attributes (inner_attrs, INNER);

  str += "\n external items: ";
  if (!has_extern_items ())
    {
      str += "none";
    }
  else
    {
      for (const auto &item : extern_items)
	str += "\n  " + item->as_string ();
    }

  return str;
}

std::string
MacroRule::as_string () const
{
  std::string str ("Macro rule: ");

  str += "\n Matcher: \n  ";
  str += matcher.as_string ();

  str += "\n Transcriber: \n  ";
  str += transcriber.as_string ();

  return str;
}

std::string
MacroRulesDefinition::as_string () const
{
  std::string str;

  // get outer attrs
  str += append_attributes (outer_attrs, OUTER);

  // TODO: deal with macro_2_0
  str += "macro_rules!";

  str += rule_name.as_string ();

  str += "\n Macro rules: ";
  if (rules.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &rule : rules)
	str += "\n  " + rule.as_string ();
    }

  str += "\n Delim type: ";
  switch (delim_type)
    {
    case PARENS:
      str += "parentheses";
      break;
    case SQUARE:
      str += "square";
      break;
    case CURLY:
      str += "curly";
      break;
    default:
      return "ERROR_MARK_STRING - delim type in macro invocation";
    }

  return str;
}

std::string
MacroInvocation::as_string () const
{
  std::string str = "MacroInvocation: ";
  auto is_builtin = kind == InvocKind::Builtin;

  if (is_builtin)
    str += "[builtin] ";
  else
    str += "[regular] ";

  str += append_attributes (outer_attrs, OUTER);

  str += "\n " + invoc_data.as_string ();

  str += "\n has semicolon: ";
  str += has_semicolon () ? "true" : "false";

  if (is_builtin)
    {
      str += "[PENDING EAGER INVOCATIONS]: ";
      for (auto &pending : pending_eager_invocs)
	{
	  str += pending->as_string ();
	  str += "\n";
	}
    }

  return str;
}

std::string
MacroInvocData::as_string () const
{
  return path.as_string () + "!" + token_tree.as_string ();
}

std::string
ExprStmt::as_string () const
{
  std::string str = indent_spaces (enter) + "ExprStmt: \n";

  if (expr == nullptr)
    {
      str += "none (this should not happen and is an error)";
    }
  else
    {
      indent_spaces (enter);
      str += expr->as_string ();
      if (semicolon_followed)
	str += ";";
      indent_spaces (out);
    }

  indent_spaces (out);
  return str;
}

std::string
ClosureParam::as_string () const
{
  std::string str (pattern->as_string ());

  if (has_type_given ())
    str += " : " + type->as_string ();

  return str;
}

std::string
ClosureExpr::as_string () const
{
  std::string str = "ClosureExpr:";

  str += append_attributes (outer_attrs, OUTER);

  str += "\n Has move: ";
  if (has_move)
    str += "true";
  else
    str += "false";

  str += "\n Params: ";
  if (params.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &param : params)
	str += "\n  " + param.as_string ();
    }

  return str;
}

std::string
ClosureExprInnerTyped::as_string () const
{
  std::string str = ClosureExpr::as_string ();

  str += "\n Return type: " + return_type->as_string ();

  str += "\n Body: " + expr->as_string ();

  return str;
}

std::string
QualifiedPathType::as_string () const
{
  std::string str ("<");
  str += type_to_invoke_on->as_string ();

  if (has_as_clause ())
    str += " as " + trait_path.as_string ();

  return str + ">";
}

std::string
BorrowExpr::as_string () const
{
  /* TODO: find way to incorporate outer attrs - may have to represent in
   * different style (i.e. something more like BorrowExpr: \n outer attrs) */

  std::string str ("&");

  if (double_borrow)
    str += "&";

  if (is_mut)
    str += "mut ";

  str += main_or_left_expr->as_string ();

  return str;
}

std::string
ReturnExpr::as_string () const
{
  /* TODO: find way to incorporate outer attrs - may have to represent in
   * different style (i.e. something more like BorrowExpr: \n outer attrs) */

  std::string str ("return ");

  if (has_returned_expr ())
    str += return_expr->as_string ();

  return str;
}

std::string
RangeToExpr::as_string () const
{
  return ".." + to->as_string ();
}

std::string
ContinueExpr::as_string () const
{
  // TODO: rewrite format to allow outer attributes
  std::string str ("continue ");

  if (has_label ())
    str += label.as_string ();

  return str;
}

std::string
NegationExpr::as_string () const
{
  // TODO: rewrite formula to allow outer attributes
  std::string str;

  switch (expr_type)
    {
    case NegationOperator::NEGATE:
      str = "-";
      break;
    case NegationOperator::NOT:
      str = "!";
      break;
    default:
      return "ERROR_MARK_STRING - negation expr";
    }

  str += main_or_left_expr->as_string ();

  return str;
}

std::string
RangeFromExpr::as_string () const
{
  return from->as_string () + "..";
}

std::string
RangeFullExpr::as_string () const
{
  return "..";
}

std::string
ArrayIndexExpr::as_string () const
{
  // TODO: rewrite formula to allow outer attributes
  return array_expr->as_string () + "[" + index_expr->as_string () + "]";
}

std::string
AssignmentExpr::as_string () const
{
  std::string str ("AssignmentExpr: ");

  if (main_or_left_expr == nullptr || right_expr == nullptr)
    {
      str += "error (either or both expressions are null)";
    }
  else
    {
      // left expr
      str += "\n left: " + main_or_left_expr->as_string ();

      // right expr
      str += "\n right: " + right_expr->as_string ();
    }

  return str;
}

std::string
AsyncBlockExpr::as_string () const
{
  std::string str = "AsyncBlockExpr: ";

  // get outer attributes
  // str += "\n " + Expr::as_string ();
  str += append_attributes (outer_attrs, OUTER);

  str += "\n Has move: ";
  str += has_move ? "true" : "false";

  return str + "\n" + block_expr->as_string ();
}

std::string
ComparisonExpr::as_string () const
{
  // TODO: rewrite to better reflect non-literal expressions
  std::string str (main_or_left_expr->as_string ());

  switch (expr_type)
    {
    case ComparisonOperator::EQUAL:
      str += " == ";
      break;
    case ComparisonOperator::NOT_EQUAL:
      str += " != ";
      break;
    case ComparisonOperator::GREATER_THAN:
      str += " > ";
      break;
    case ComparisonOperator::LESS_THAN:
      str += " < ";
      break;
    case ComparisonOperator::GREATER_OR_EQUAL:
      str += " >= ";
      break;
    case ComparisonOperator::LESS_OR_EQUAL:
      str += " <= ";
      break;
    default:
      return "ERROR_MARK_STRING - comparison expr";
    }

  str += right_expr->as_string ();

  return str;
}

std::string
MethodCallExpr::as_string () const
{
  std::string str = "MethodCallExpr: ";

  str += append_attributes (outer_attrs, OUTER);

  str += "\n Object (receiver) expr: \n";
  str += receiver->as_string ();

  str += "\n Method path segment: \n";
  str += method_name.as_string ();

  str += "\n Call params:";
  if (params.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &param : params)
	{
	  if (param == nullptr)
	    return "ERROR_MARK_STRING - method call expr param is null";

	  str += "\n  " + param->as_string ();
	}
    }

  return str;
}

std::string
TupleIndexExpr::as_string () const
{
  // TODO: rewrite dump to better reflect non-literal exprs
  return tuple_expr->as_string () + "." + std::to_string (tuple_index);
}

std::string
DereferenceExpr::as_string () const
{
  // TODO: rewrite dump to better reflect non-literal exprs
  return "*" + main_or_left_expr->as_string ();
}

std::string
FieldAccessExpr::as_string () const
{
  // TODO: rewrite dump to better reflect non-literal exprs
  return receiver->as_string () + "." + field.as_string ();
}

std::string
LazyBooleanExpr::as_string () const
{
  // TODO: rewrite dump to better reflect non-literal exprs
  std::string str (main_or_left_expr->as_string ());

  switch (expr_type)
    {
    case LazyBooleanOperator::LOGICAL_OR:
      str += " || ";
      break;
    case LazyBooleanOperator::LOGICAL_AND:
      str += " && ";
      break;
    default:
      return "ERROR_MARK_STRING - lazy boolean expr out of bounds";
    }

  str += right_expr->as_string ();

  return str;
}

std::string
RangeFromToExpr::as_string () const
{
  // TODO: rewrite dump to better reflect non-literal exprs
  return from->as_string () + ".." + to->as_string ();
}

std::string
RangeToInclExpr::as_string () const
{
  // TODO: rewrite dump to better reflect non-literal exprs
  return "..=" + to->as_string ();
}

std::string
UnsafeBlockExpr::as_string () const
{
  std::string str = "UnsafeBlockExpr:" + indent_spaces (enter);

  // get outer attributes
  str += append_attributes (outer_attrs, OUTER);

  str += indent_spaces (stay) + expr->as_string () + "\n" + indent_spaces (out);

  return str;
}

std::string
ClosureExprInner::as_string () const
{
  std::string str = ClosureExpr::as_string ();

  str += "\n Expression: " + closure_inner->as_string ();

  return str;
}

std::string
IfExpr::as_string () const
{
  std::string str = "IfExpr: ";

  str += append_attributes (outer_attrs, OUTER);

  str += "\n Condition expr: " + condition->as_string ();

  str += "\n If block expr: " + if_block->as_string ();

  return str;
}

std::string
IfExprConseqElse::as_string () const
{
  std::string str = IfExpr::as_string ();

  str += "\n Else expr: " + else_block->as_string ();

  return str;
}

std::string
IfLetExpr::as_string () const
{
  std::string str = "IfLetExpr: ";

  str += append_attributes (outer_attrs, OUTER);

  str += "\n Condition match arm patterns: ";
  if (match_arm_patterns.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &pattern : match_arm_patterns)
	str += "\n  " + pattern->as_string ();
    }

  str += "\n Scrutinee expr: " + value->as_string ();

  str += "\n If let block expr: " + if_block->as_string ();

  return str;
}

std::string
IfLetExprConseqElse::as_string () const
{
  std::string str = IfLetExpr::as_string ();

  str += "\n Else expr: " + else_block->as_string ();

  return str;
}

std::string
RangeFromToInclExpr::as_string () const
{
  // TODO: rewrite to allow dumps with non-literal exprs
  return from->as_string () + "..=" + to->as_string ();
}

std::string
ErrorPropagationExpr::as_string () const
{
  // TODO: rewrite to allow dumps with non-literal exprs
  return main_or_left_expr->as_string () + "?";
}

std::string
CompoundAssignmentExpr::as_string () const
{
  std::string operator_str;
  operator_str.reserve (1);

  // get operator string
  switch (expr_type)
    {
    case CompoundAssignmentOperator::ADD:
      operator_str = "+";
      break;
    case CompoundAssignmentOperator::SUBTRACT:
      operator_str = "-";
      break;
    case CompoundAssignmentOperator::MULTIPLY:
      operator_str = "*";
      break;
    case CompoundAssignmentOperator::DIVIDE:
      operator_str = "/";
      break;
    case CompoundAssignmentOperator::MODULUS:
      operator_str = "%";
      break;
    case CompoundAssignmentOperator::BITWISE_AND:
      operator_str = "&";
      break;
    case CompoundAssignmentOperator::BITWISE_OR:
      operator_str = "|";
      break;
    case CompoundAssignmentOperator::BITWISE_XOR:
      operator_str = "^";
      break;
    case CompoundAssignmentOperator::LEFT_SHIFT:
      operator_str = "<<";
      break;
    case CompoundAssignmentOperator::RIGHT_SHIFT:
      operator_str = ">>";
      break;
    default:
      operator_str = "invalid operator. wtf";
      break;
    }

  operator_str += "=";

  std::string str ("CompoundAssignmentExpr: ");
  if (main_or_left_expr == nullptr || right_expr == nullptr)
    {
      str += "error. this is probably a parsing failure.";
    }
  else
    {
      str += "\n left: " + main_or_left_expr->as_string ();
      str += "\n right: " + right_expr->as_string ();
      str += "\n operator: " + operator_str;
    }

  return str;
}

std::string
ArithmeticOrLogicalExpr::as_string () const
{
  std::string operator_str;
  operator_str.reserve (1);

  // get operator string
  switch (expr_type)
    {
    case ArithmeticOrLogicalOperator::ADD:
      operator_str = "+";
      break;
    case ArithmeticOrLogicalOperator::SUBTRACT:
      operator_str = "-";
      break;
    case ArithmeticOrLogicalOperator::MULTIPLY:
      operator_str = "*";
      break;
    case ArithmeticOrLogicalOperator::DIVIDE:
      operator_str = "/";
      break;
    case ArithmeticOrLogicalOperator::MODULUS:
      operator_str = "%";
      break;
    case ArithmeticOrLogicalOperator::BITWISE_AND:
      operator_str = "&";
      break;
    case ArithmeticOrLogicalOperator::BITWISE_OR:
      operator_str = "|";
      break;
    case ArithmeticOrLogicalOperator::BITWISE_XOR:
      operator_str = "^";
      break;
    case ArithmeticOrLogicalOperator::LEFT_SHIFT:
      operator_str = "<<";
      break;
    case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
      operator_str = ">>";
      break;
    default:
      operator_str = "invalid operator. wtf";
      break;
    }

  std::string str ("ArithmeticOrLogicalExpr: ");
  if (main_or_left_expr == nullptr || right_expr == nullptr)
    {
      str += "error. this is probably a parsing failure.";
    }
  else
    {
      str += main_or_left_expr->as_string () + " ";
      str += operator_str + " ";
      str += right_expr->as_string ();
    }

  return str;
}

std::string
CallExpr::as_string () const
{
  std::string str = "CallExpr: ";

  str += append_attributes (outer_attrs, OUTER);

  str += "\n Function expr: ";
  str += function->as_string ();

  str += "\n Call params:";
  if (!has_params ())
    {
      str += "none";
    }
  else
    {
      for (const auto &param : params)
	{
	  if (param == nullptr)
	    return "ERROR_MARK_STRING - call expr param is null";

	  str += "\n  " + param->as_string ();
	}
    }

  return str;
}

std::string
WhileLoopExpr::as_string () const
{
  std::string str = "WhileLoopExpr: ";

  str += append_attributes (outer_attrs, OUTER);

  str += "\n Label: ";
  if (!has_loop_label ())
    str += "none";
  else
    str += loop_label.as_string ();

  str += "\n Conditional expr: " + condition->as_string ();

  str += "\n Loop block: " + loop_block->as_string ();

  return str;
}

std::string
WhileLetLoopExpr::as_string () const
{
  std::string str = "WhileLetLoopExpr: ";

  str += append_attributes (outer_attrs, OUTER);

  str += "\n Label: ";
  if (!has_loop_label ())
    str += "none";
  else
    str += loop_label.as_string ();

  str += "\n Match arm patterns: ";
  if (match_arm_patterns.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &pattern : match_arm_patterns)
	str += "\n  " + pattern->as_string ();
    }

  str += "\n Scrutinee expr: " + scrutinee->as_string ();

  str += "\n Loop block: " + loop_block->as_string ();

  return str;
}

std::string
LoopExpr::as_string () const
{
  std::string str = "LoopExpr: (infinite loop)";

  str += append_attributes (outer_attrs, OUTER);

  str += "\n Label: ";
  if (!has_loop_label ())
    str += "none";
  else
    str += loop_label.as_string ();

  str += "\n Loop block: " + loop_block->as_string ();

  return str;
}

std::string
ArrayExpr::as_string () const
{
  std::string str = "ArrayExpr:";

  str += append_attributes (outer_attrs, OUTER);

  // inner attributes
  str += append_attributes (inner_attrs, INNER);

  str += "\n Array elems: ";
  str += internal_elements->as_string ();

  return str;
}

std::string
AwaitExpr::as_string () const
{
  // TODO: rewrite dump to allow non-literal exprs
  return awaited_expr->as_string () + ".await";
}

std::string
BreakExpr::as_string () const
{
  // TODO: rewrite dump to allow outer attrs, non-literal exprs
  std::string str ("break ");

  if (has_label ())
    str += label.as_string () + " ";

  if (has_break_expr ())
    str += break_expr->as_string ();

  return str;
}

std::string
LoopLabel::as_string () const
{
  return label.as_string () + ": (label) ";
}

std::string
MatchArm::as_string () const
{
  // outer attributes
  std::string str = append_attributes (outer_attrs, OUTER);

  str += "\nPatterns: ";
  if (match_arm_patterns.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &pattern : match_arm_patterns)
	str += "\n " + pattern->as_string ();
    }

  str += "\nGuard expr: ";
  if (!has_match_arm_guard ())
    str += "none";
  else
    str += guard_expr->as_string ();

  return str;
}

std::string
MatchCase::as_string () const
{
  std::string str ("MatchCase: (match arm) ");

  str += "\n Match arm matcher: \n" + arm.as_string ();
  str += "\n Expr: " + expr->as_string ();

  return str;
}

std::string
MatchExpr::as_string () const
{
  std::string str ("MatchExpr:");

  str += append_attributes (outer_attrs, OUTER);

  str += "\n Scrutinee expr: " + branch_value->as_string ();

  // inner attributes
  str += append_attributes (inner_attrs, INNER);

  // match arms
  str += "\n Match arms: ";
  if (match_arms.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &arm : match_arms)
	str += "\n  " + arm.as_string ();
    }

  return str;
}

std::string
TupleExpr::as_string () const
{
  std::string str ("TupleExpr:");

  str += append_attributes (outer_attrs, OUTER);

  // inner attributes
  str += append_attributes (inner_attrs, INNER);

  str += "\n Tuple elements: ";
  if (tuple_elems.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &elem : tuple_elems)
	str += "\n  " + elem->as_string ();
    }

  return str;
}

std::string
FunctionParam::as_string () const
{
  // TODO: rewrite dump to allow non-literal types
  return param_name->as_string () + " : " + type->as_string ();
}

void
FunctionParam::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
SelfParam::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
VariadicParam::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

std::string
VariadicParam::as_string () const
{
  if (has_pattern ())
    return get_pattern ().as_string () + " : ...";
  else
    return "...";
}

std::string
FunctionQualifiers::as_string () const
{
  std::string str;

  if (is_async ())
    str += "async ";
  if (is_const ())
    str += "const ";
  if (is_unsafe ())
    str += "unsafe ";

  if (has_extern)
    {
      str += "extern";
      if (extern_abi != "")
	str += " \"" + extern_abi + "\"";
    }

  return str;
}

std::string
TraitBound::as_string () const
{
  std::string str ("TraitBound:");

  str += "\n Has opening question mark: ";
  if (opening_question_mark)
    str += "true";
  else
    str += "false";

  str += "\n For lifetimes: ";
  if (!has_for_lifetimes ())
    {
      str += "none";
    }
  else
    {
      for (const auto &lifetime : for_lifetimes)
	str += "\n  " + lifetime.as_string ();
    }

  str += "\n Type path: " + type_path.as_string ();

  return str;
}

std::string
MacroMatcher::as_string () const
{
  std::string str ("Macro matcher: ");

  str += "\n Delim type: ";

  switch (delim_type)
    {
    case PARENS:
      str += "parentheses";
      break;
    case SQUARE:
      str += "square";
      break;
    case CURLY:
      str += "curly";
      break;
    default:
      return "ERROR_MARK_STRING - macro matcher delim";
    }

  str += "\n Matches: ";

  if (matches.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &match : matches)
	str += "\n  " + match->as_string ();
    }

  return str;
}

std::string
LifetimeParam::as_string () const
{
  std::string str ("LifetimeParam: ");

  str += "\n Outer attribute: ";
  if (!has_outer_attribute ())
    str += "none";
  else
    str += outer_attr.as_string ();

  str += "\n Lifetime: " + lifetime.as_string ();

  str += "\n Lifetime bounds: ";
  if (!has_lifetime_bounds ())
    {
      str += "none";
    }
  else
    {
      for (const auto &bound : lifetime_bounds)
	str += "\n  " + bound.as_string ();
    }

  return str;
}

std::string
MacroMatchFragment::as_string () const
{
  return "$" + ident.as_string () + ": " + frag_spec.as_string ();
}

std::string
MacroMatchRepetition::as_string () const
{
  std::string str ("Macro match repetition: ");

  str += "\n Matches: ";
  if (matches.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &match : matches)
	str += "\n  " + match->as_string ();
    }

  str += "\n Sep: ";
  if (!has_sep ())
    str += "none";
  else
    str += sep->as_string ();

  str += "\n Op: ";
  switch (op)
    {
    case ANY:
      str += "*";
      break;
    case ONE_OR_MORE:
      str += "+";
      break;
    case ZERO_OR_ONE:
      str += "?";
      break;
    case NONE:
      str += "no op? shouldn't be allowed";
      break;
    default:
      return "ERROR_MARK_STRING - unknown op in macro match repetition";
    }

  return str;
}

std::string
Lifetime::as_string () const
{
  if (is_error ())
    return "error lifetime";

  switch (lifetime_type)
    {
    case NAMED:
      return "'" + lifetime_name;
    case STATIC:
      return "'static";
    case WILDCARD:
      return "'_";
    default:
      return "ERROR-MARK-STRING: lifetime type failure";
    }
}

std::string
TypeParam::as_string () const
{
  std::string str ("TypeParam: ");

  str += "\n Outer attribute: ";
  if (!has_outer_attribute ())
    str += "none";
  else
    str += outer_attr.as_string ();

  str += "\n Identifier: " + type_representation.as_string ();

  str += "\n Type param bounds: ";
  if (!has_type_param_bounds ())
    {
      str += "none";
    }
  else
    {
      for (const auto &bound : type_param_bounds)
	str += "\n  " + bound->as_string ();
    }

  str += "\n Type: ";
  if (!has_type ())
    str += "none";
  else
    str += type->as_string ();

  return str;
}

std::string
ForLoopExpr::as_string () const
{
  std::string str = "ForLoopExpr: ";

  str += append_attributes (outer_attrs, OUTER);

  str += "\n Label: ";
  if (!has_loop_label ())
    str += "none";
  else
    str += loop_label.as_string ();

  str += "\n Pattern: " + pattern->as_string ();

  str += "\n Iterator expr: " + iterator_expr->as_string ();

  str += "\n Loop block: " + loop_block->as_string ();

  return str;
}

std::string
LetStmt::as_string () const
{
  // TODO: rewrite to work with non-linearisable types and exprs
  std::string str = append_attributes (outer_attrs, OUTER);

  str += "\n" + indent_spaces (stay) + "let " + variables_pattern->as_string ();

  if (has_type ())
    str += " : " + type->as_string ();

  if (has_init_expr ())
    str += " = " + init_expr->as_string ();

  return str;
}

std::string
InferredType::as_string () const
{
  return "_ (inferred)";
}

std::string
TypeCastExpr::as_string () const
{
  // TODO: rewrite to work with non-linearisable exprs and types
  return main_or_left_expr->as_string () + " as "
	 + type_to_convert_to->as_string ();
}

std::string
ImplTraitType::as_string () const
{
  std::string str ("ImplTraitType: \n TypeParamBounds: ");

  if (type_param_bounds.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &bound : type_param_bounds)
	str += "\n  " + bound->as_string ();
    }

  return str;
}

std::string
ReferenceType::as_string () const
{
  // TODO: rewrite to work with non-linearisable types
  std::string str ("&");

  if (has_lifetime ())
    str += lifetime.as_string () + " ";

  if (has_mut)
    str += "mut ";

  str += type->as_string ();

  return str;
}

std::string
RawPointerType::as_string () const
{
  // TODO: rewrite to work with non-linearisable types
  std::string str ("*");

  switch (pointer_type)
    {
    case MUT:
      str += "mut ";
      break;
    case CONST:
      str += "const ";
      break;
    default:
      return "ERROR_MARK_STRING - unknown pointer type in raw pointer type";
    }

  str += type->as_string ();

  return str;
}

std::string
TraitObjectType::as_string () const
{
  std::string str ("TraitObjectType: \n Has dyn dispatch: ");

  if (has_dyn)
    str += "true";
  else
    str += "false";

  str += "\n TypeParamBounds: ";
  if (type_param_bounds.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &bound : type_param_bounds)
	str += "\n  " + bound->as_string ();
    }

  return str;
}

std::string
BareFunctionType::as_string () const
{
  std::string str ("BareFunctionType: \n For lifetimes: ");

  if (!has_for_lifetimes ())
    {
      str += "none";
    }
  else
    {
      for (const auto &for_lifetime : for_lifetimes)
	str += "\n  " + for_lifetime.as_string ();
    }

  str += "\n Qualifiers: " + function_qualifiers.as_string ();

  str += "\n Params: ";
  if (params.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &param : params)
	str += "\n  " + param.as_string ();
    }

  str += "\n Is variadic: ";
  if (_is_variadic)
    str += "true";
  else
    str += "false";

  str += "\n Return type: ";
  if (!has_return_type ())
    str += "none (void)";
  else
    str += return_type->as_string ();

  return str;
}

std::string
ImplTraitTypeOneBound::as_string () const
{
  std::string str ("ImplTraitTypeOneBound: \n TraitBound: ");

  return str + trait_bound.as_string ();
}

std::string
TraitObjectTypeOneBound::as_string () const
{
  std::string str ("TraitObjectTypeOneBound: \n Has dyn dispatch: ");

  if (has_dyn)
    str += "true";
  else
    str += "false";

  str += "\n TraitBound: " + trait_bound.as_string ();

  return str;
}

std::string
ArrayType::as_string () const
{
  // TODO: rewrite to work with non-linearisable types and exprs
  return "[" + elem_type->as_string () + "; " + size->as_string () + "]";
}

std::string
SliceType::as_string () const
{
  // TODO: rewrite to work with non-linearisable types
  return "[" + elem_type->as_string () + "]";
}

std::string
TupleType::as_string () const
{
  // TODO: rewrite to work with non-linearisable types
  std::string str ("(");

  if (!is_unit_type ())
    {
      auto i = elems.begin ();
      auto e = elems.end ();

      for (; i != e; i++)
	{
	  str += (*i)->as_string ();
	  if (e != i + 1)
	    str += ", ";
	}
    }

  str += ")";

  return str;
}

std::string
StructExpr::as_string () const
{
  std::string str = append_attributes (outer_attrs, OUTER);
  indent_spaces (enter);
  str += "\n" + indent_spaces (stay) + "StructExpr:";
  indent_spaces (enter);
  str += "\n" + indent_spaces (stay) + "PathInExpr:\n";
  str += indent_spaces (stay) + struct_name.as_string ();
  indent_spaces (out);
  indent_spaces (out);
  return str;
}

std::string
StructExprStruct::as_string () const
{
  // TODO: doesn't this require data from StructExpr?
  std::string str ("StructExprStruct (or subclass): ");

  str += "\n Path: " + get_struct_name ().as_string ();

  // inner attributes
  str += append_attributes (inner_attrs, INNER);

  return str;
}

std::string
StructBase::as_string () const
{
  if (base_struct != nullptr)
    return base_struct->as_string ();
  else
    return "ERROR_MARK_STRING - invalid struct base had as string applied";
}

std::string
StructExprFieldWithVal::as_string () const
{
  // used to get value string
  return value->as_string ();
}

std::string
StructExprFieldIdentifierValue::as_string () const
{
  // TODO: rewrite to work with non-linearisable exprs
  return field_name.as_string () + " : " + StructExprFieldWithVal::as_string ();
}

std::string
StructExprFieldIndexValue::as_string () const
{
  // TODO: rewrite to work with non-linearisable exprs
  return std::to_string (index) + " : " + StructExprFieldWithVal::as_string ();
}

std::string
StructExprStructFields::as_string () const
{
  std::string str = StructExprStruct::as_string ();

  str += "\n Fields: ";
  if (fields.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &field : fields)
	str += "\n  " + field->as_string ();
    }

  str += "\n Struct base: ";
  if (!has_struct_base ())
    str += "none";
  else
    str += struct_base.as_string ();

  return str;
}

std::string
EnumItem::as_string () const
{
  std::string str = VisItem::as_string ();
  str += variant_name.as_string ();

  return str;
}

std::string
EnumItemTuple::as_string () const
{
  std::string str = EnumItem::as_string ();

  // add tuple opening parens
  str += "(";

  // tuple fields
  if (has_tuple_fields ())
    {
      auto i = tuple_fields.begin ();
      auto e = tuple_fields.end ();

      for (; i != e; i++)
	{
	  str += (*i).as_string ();
	  if (e != i + 1)
	    str += ", ";
	}
    }

  // add tuple closing parens
  str += ")";

  return str;
}

std::string
TupleField::as_string () const
{
  // TODO: rewrite to work with non-linearisable exprs

  // outer attributes
  std::string str = append_attributes (outer_attrs, OUTER);

  if (has_visibility ())
    str += "\n" + visibility.as_string ();

  str += " " + field_type->as_string ();

  return str;
}

std::string
EnumItemStruct::as_string () const
{
  std::string str = EnumItem::as_string ();

  // add struct opening parens
  str += "{";

  // tuple fields
  if (has_struct_fields ())
    {
      auto i = struct_fields.begin ();
      auto e = struct_fields.end ();

      for (; i != e; i++)
	{
	  str += (*i).as_string ();
	  if (e != i + 1)
	    str += ", ";
	}
    }

  // add struct closing parens
  str += "}";

  return str;
}

std::string
StructField::as_string () const
{
  // TODO: rewrite to work with non-linearisable exprs
  // outer attributes
  std::string str = append_attributes (outer_attrs, OUTER);

  if (has_visibility ())
    str += "\n" + visibility.as_string ();

  str += " " + field_name.as_string () + " : " + field_type->as_string ();

  return str;
}

std::string
EnumItemDiscriminant::as_string () const
{
  // TODO: rewrite to work with non-linearisable exprs
  std::string str = EnumItem::as_string ();

  // add equal and expression
  str += " = " + expression->as_string ();

  return str;
}

std::string
ExternalTypeItem::as_string () const
{
  auto str = append_attributes (outer_attrs, OUTER);

  str += "type " + item_name.as_string () + ";";

  return str;
}

std::string
ExternalStaticItem::as_string () const
{
  // outer attributes
  std::string str = append_attributes (outer_attrs, OUTER);

  // start visibility on new line and with a space
  str += "\n" + visibility.as_string () + " ";

  str += "static ";

  if (has_mut)
    str += "mut ";

  // add name
  str += item_name.as_string ();

  // add type on new line
  str += "\n Type: " + item_type->as_string ();

  return str;
}

std::string
NamedFunctionParam::as_string () const
{
  std::string str = append_attributes (outer_attrs, OUTER);

  if (has_name ())
    str += "\n" + name;

  if (is_variadic ())
    str += "...";
  else
    str += "\n Type: " + param_type->as_string ();

  return str;
}

std::string
TraitItemConst::as_string () const
{
  // TODO: rewrite to work with non-linearisable exprs
  std::string str = append_attributes (outer_attrs, OUTER);

  str += "\nconst " + name.as_string () + " : " + type->as_string ();

  if (has_expression ())
    str += " = " + expr->as_string ();

  return str;
}

std::string
TraitItemType::as_string () const
{
  std::string str = append_attributes (outer_attrs, OUTER);

  str += "\ntype " + name.as_string ();

  str += "\n Type param bounds: ";
  if (!has_type_param_bounds ())
    {
      str += "none";
    }
  else
    {
      for (const auto &bound : type_param_bounds)
	{
	  // DEBUG: null pointer check
	  if (bound == nullptr)
	    {
	      rust_debug (
		"something really terrible has gone wrong - null pointer "
		"type param bound in trait item type.");
	      return "NULL_POINTER_MARK";
	    }

	  str += "\n  " + bound->as_string ();
	}
    }

  return str;
}

std::string
SelfParam::as_string () const
{
  // TODO: rewrite to allow non-linearisable types
  if (is_error ())
    {
      return "error";
    }
  else
    {
      if (has_type ())
	{
	  // type (i.e. not ref, no lifetime)
	  std::string str;

	  if (is_mut)
	    str += "mut ";

	  str += "self : ";

	  str += type->as_string ();

	  return str;
	}
      else if (has_lifetime ())
	{
	  // ref and lifetime
	  std::string str = "&" + lifetime.as_string () + " ";

	  if (is_mut)
	    str += "mut ";

	  str += "self";

	  return str;
	}
      else if (has_ref)
	{
	  // ref with no lifetime
	  std::string str = "&";

	  if (is_mut)
	    str += " mut ";

	  str += "self";

	  return str;
	}
      else
	{
	  // no ref, no type
	  std::string str;

	  if (is_mut)
	    str += "mut ";

	  str += "self";

	  return str;
	}
    }
}

std::string
ArrayElemsCopied::as_string () const
{
  // TODO: rewrite to allow non-linearisable exprs
  return elem_to_copy->as_string () + "; " + num_copies->as_string ();
}

std::string
LifetimeWhereClauseItem::as_string () const
{
  std::string str ("Lifetime: ");

  str += lifetime.as_string ();

  str += "\nLifetime bounds: ";

  for (const auto &bound : lifetime_bounds)
    str += "\n " + bound.as_string ();

  return str;
}

std::string
TypeBoundWhereClauseItem::as_string () const
{
  std::string str ("For lifetimes: ");

  if (!has_for_lifetimes ())
    {
      str += "none";
    }
  else
    {
      for (const auto &for_lifetime : for_lifetimes)
	str += "\n " + for_lifetime.as_string ();
    }

  str += "\nType: " + bound_type->as_string ();

  str += "\nType param bounds bounds: ";

  for (const auto &bound : type_param_bounds)
    {
      // debug null pointer check
      if (bound == nullptr)
	return "NULL_POINTER_MARK - type param bounds";

      str += "\n " + bound->as_string ();
    }

  return str;
}

std::string
ArrayElemsValues::as_string () const
{
  std::string str;

  for (const auto &expr : values)
    {
      // DEBUG: null pointer check
      if (expr == nullptr)
	{
	  rust_debug ("something really terrible has gone wrong - null pointer "
		      "expr in array elems values.");
	  return "NULL_POINTER_MARK";
	}

      str += "\n  " + expr->as_string ();
    }

  return str;
}

std::string
MaybeNamedParam::as_string () const
{
  // TODO: rewrite to allow using non-linearisable types in dump
  std::string str;

  switch (param_kind)
    {
    case UNNAMED:
      break;
    case IDENTIFIER:
      str = name.as_string () + " : ";
      break;
    case WILDCARD:
      str = "_ : ";
      break;
    default:
      return "ERROR_MARK_STRING - maybe named param unrecognised param kind";
    }

  str += param_type->as_string ();

  return str;
}

MetaItemInner::~MetaItemInner () = default;

std::unique_ptr<MetaNameValueStr>
MetaItemInner::to_meta_name_value_str () const
{
  if (is_key_value_pair ())
    {
      auto converted_item = static_cast<const MetaNameValueStr *> (this);
      return converted_item->to_meta_name_value_str ();
    }
  // TODO actually parse foo = bar
  return nullptr;
}

std::string
MetaItemSeq::as_string () const
{
  std::string path_str = path.as_string () + "(";

  auto i = seq.begin ();
  auto e = seq.end ();

  for (; i != e; i++)
    {
      path_str += (*i)->as_string ();
      if (e != i + 1)
	path_str += ", ";
    }

  return path_str + ")";
}

std::string
MetaListPaths::as_string () const
{
  std::string str = ident.as_string () + "(";

  auto i = paths.begin ();
  auto e = paths.end ();

  for (; i != e; i++)
    {
      str += (*i).as_string ();
      if (e != i + 1)
	str += ", ";
    }

  return str + ")";
}

std::string
MetaListNameValueStr::as_string () const
{
  std::string str = ident.as_string () + "(";

  auto i = strs.begin ();
  auto e = strs.end ();

  for (; i != e; i++)
    {
      str += (*i).as_string ();
      if (e != i + 1)
	str += ", ";
    }

  return str + ")";
}

std::string
AttrInputMetaItemContainer::as_string () const
{
  std::string str = "(";

  auto i = items.begin ();
  auto e = items.end ();

  for (; i != e; i++)
    {
      str += (*i)->as_string ();
      if (e != i + 1)
	str += ", ";
    }

  return str + ")";
}

std::string
AttrInputMacro::as_string () const
{
  return " = " + macro->as_string ();
}

/* Override that calls the function recursively on all items contained within
 * the module. */
void
Module::add_crate_name (std::vector<std::string> &names) const
{
  /* TODO: test whether module has been 'cfg'-ed out to determine whether to
   * exclude it from search */

  for (const auto &item : items)
    item->add_crate_name (names);
}

static bool
file_exists (const std::string path)
{
  // Simply check if the file exists
  // FIXME: This does not work on Windows
  return access (path.c_str (), F_OK) != -1;
}

static std::string
filename_from_path_attribute (std::vector<Attribute> &outer_attrs)
{
  // An out-of-line module cannot have inner attributes. Additionally, the
  // default name is specified as `""` so that the caller can detect the case
  // of "no path given" and use the default path logic (`name.rs` or
  // `name/mod.rs`).
  return extract_module_path ({}, outer_attrs, "");
}

void
Module::process_file_path ()
{
  rust_assert (kind == Module::ModuleKind::UNLOADED);
  rust_assert (module_file.empty ());

  // This corresponds to the path of the file 'including' the module. So the
  // file that contains the 'mod <file>;' directive
  std::string including_fpath (outer_filename);

  std::string expected_file_path = module_name.as_string () + ".rs";
  std::string expected_dir_path = "mod.rs";

  auto dir_slash_pos = including_fpath.rfind (file_separator);
  std::string current_directory_name;
  std::string including_fname;

  // If we haven't found a file_separator, then we may have to look for files in
  // the current directory ('.')
  if (dir_slash_pos == std::string::npos)
    {
      including_fname = std::move (including_fpath);
      including_fpath = std::string (".") + file_separator + including_fname;
      dir_slash_pos = 1;
    }
  else
    {
      including_fname = including_fpath.substr (dir_slash_pos + 1);
    }

  current_directory_name
    = including_fpath.substr (0, dir_slash_pos) + file_separator;

  auto path_string = filename_from_path_attribute (get_outer_attrs ());

  std::string including_subdir;
  if (path_string.empty () && module_scope.empty ()
      && get_file_subdir (including_fname, including_subdir))
    current_directory_name += including_subdir + file_separator;

  // Handle inline module declarations adding path components.
  for (auto const &name : module_scope)
    {
      current_directory_name.append (name);
      current_directory_name.append (file_separator);
    }

  if (!path_string.empty ())
    {
      module_file = current_directory_name + path_string;
      return;
    }

  // FIXME: We also have to search for
  // <directory>/<including_fname>/<module_name>.rs In rustc, this is done via
  // the concept of `DirOwnernship`, which is based on whether or not the
  // current file is titled `mod.rs`.

  // First, we search for <directory>/<module_name>.rs
  std::string file_mod_path = current_directory_name + expected_file_path;
  bool file_mod_found = file_exists (file_mod_path);

  // Then, search for <directory>/<module_name>/mod.rs
  std::string dir_mod_path = current_directory_name + module_name.as_string ()
			     + file_separator + expected_dir_path;
  bool dir_mod_found = file_exists (dir_mod_path);

  bool multiple_candidates_found = file_mod_found && dir_mod_found;
  bool no_candidates_found = !file_mod_found && !dir_mod_found;

  if (multiple_candidates_found)
    rust_error_at (locus,
		   "two candidates found for module %s: %s.rs and %s%smod.rs",
		   module_name.as_string ().c_str (),
		   module_name.as_string ().c_str (),
		   module_name.as_string ().c_str (), file_separator);

  if (no_candidates_found)
    rust_error_at (locus, "no candidate found for module %s",
		   module_name.as_string ().c_str ());

  if (no_candidates_found || multiple_candidates_found)
    return;

  module_file = std::move (file_mod_found ? file_mod_path : dir_mod_path);
}

void
Module::load_items ()
{
  process_file_path ();

  // We will already have errored out appropriately in the process_file_path ()
  // method
  if (module_file.empty ())
    return;

  RAIIFile file_wrap (module_file.c_str ());
  Linemap *linemap = Session::get_instance ().linemap;
  if (!file_wrap.ok ())
    {
      rust_error_at (get_locus (), "cannot open module file %s: %m",
		     module_file.c_str ());
      return;
    }

  rust_debug ("Attempting to parse file %s", module_file.c_str ());

  Lexer lex (module_file.c_str (), std::move (file_wrap), linemap);
  Parser<Lexer> parser (lex);

  // we need to parse any possible inner attributes for this module
  inner_attrs = parser.parse_inner_attributes ();
  auto parsed_items = parser.parse_items ();
  for (const auto &error : parser.get_errors ())
    error.emit ();

  items = std::move (parsed_items);
  kind = ModuleKind::LOADED;
}

void
Attribute::parse_attr_to_meta_item ()
{
  // only parse if has attribute input and not already parsed
  if (!has_attr_input () || is_parsed_to_meta_item ())
    return;

  auto res = attr_input->parse_to_meta_item ();
  std::unique_ptr<AttrInput> converted_input (res);

  if (converted_input != nullptr)
    attr_input = std::move (converted_input);
}

AttrInputMetaItemContainer *
DelimTokenTree::parse_to_meta_item () const
{
  // must have token trees
  if (token_trees.empty ())
    return nullptr;

  /* assume top-level delim token tree in attribute - convert all nested ones
   * to token stream */
  std::vector<std::unique_ptr<Token>> token_stream = to_token_stream ();

  AttributeParser parser (std::move (token_stream));
  std::vector<std::unique_ptr<MetaItemInner>> meta_items (
    parser.parse_meta_item_seq ());

  return new AttrInputMetaItemContainer (std::move (meta_items));
}

std::unique_ptr<MetaItemInner>
AttributeParser::parse_meta_item_inner ()
{
  // if first tok not identifier, not a "special" case one
  if (peek_token ()->get_id () != IDENTIFIER)
    {
      switch (peek_token ()->get_id ())
	{
	case CHAR_LITERAL:
	case STRING_LITERAL:
	case BYTE_CHAR_LITERAL:
	case BYTE_STRING_LITERAL:
	case INT_LITERAL:
	case FLOAT_LITERAL:
	case TRUE_LITERAL:
	case FALSE_LITERAL:
	  return parse_meta_item_lit ();

	case SUPER:
	case SELF:
	case CRATE:
	case DOLLAR_SIGN:
	case SCOPE_RESOLUTION:
	  return parse_path_meta_item ();

	default:
	  rust_error_at (peek_token ()->get_locus (),
			 "unrecognised token '%s' in meta item",
			 get_token_description (peek_token ()->get_id ()));
	  return nullptr;
	}
    }

  // else, check for path
  if (peek_token (1)->get_id () == SCOPE_RESOLUTION)
    {
      // path
      return parse_path_meta_item ();
    }

  auto ident = peek_token ()->as_string ();
  auto ident_locus = peek_token ()->get_locus ();

  if (is_end_meta_item_tok (peek_token (1)->get_id ()))
    {
      // meta word syntax
      skip_token ();
      return std::unique_ptr<MetaWord> (new MetaWord (ident, ident_locus));
    }

  if (peek_token (1)->get_id () == EQUAL)
    {
      // maybe meta name value str syntax - check next 2 tokens
      if (peek_token (2)->get_id () == STRING_LITERAL
	  && is_end_meta_item_tok (peek_token (3)->get_id ()))
	{
	  // meta name value str syntax
	  auto &value_tok = peek_token (2);
	  auto value = value_tok->as_string ();
	  auto locus = value_tok->get_locus ();

	  skip_token (2);

	  // remove the quotes from the string value
	  std::string raw_value = unquote_string (std::move (value));

	  return std::unique_ptr<MetaNameValueStr> (
	    new MetaNameValueStr (ident, ident_locus, std::move (raw_value),
				  locus));
	}
      else
	{
	  // just interpret as path-based meta item
	  return parse_path_meta_item ();
	}
    }

  if (peek_token (1)->get_id () != LEFT_PAREN)
    {
      rust_error_at (peek_token (1)->get_locus (),
		     "unexpected token '%s' after identifier in attribute",
		     get_token_description (peek_token (1)->get_id ()));
      return nullptr;
    }

  // is it one of those special cases like not?
  if (peek_token ()->get_id () == IDENTIFIER)
    {
      return parse_path_meta_item ();
    }

  auto meta_items = parse_meta_item_seq ();

  // pass for meta name value str
  std::vector<MetaNameValueStr> meta_name_value_str_items;
  for (const auto &item : meta_items)
    {
      std::unique_ptr<MetaNameValueStr> converted_item
	= item->to_meta_name_value_str ();
      if (converted_item == nullptr)
	{
	  meta_name_value_str_items.clear ();
	  break;
	}
      meta_name_value_str_items.push_back (std::move (*converted_item));
    }
  // if valid, return this
  if (!meta_name_value_str_items.empty ())
    {
      return std::unique_ptr<MetaListNameValueStr> (
	new MetaListNameValueStr (ident, ident_locus,
				  std::move (meta_name_value_str_items)));
    }

  // // pass for meta list idents
  // std::vector<Identifier> ident_items;
  // for (const auto &item : meta_items)
  //   {
  //     std::unique_ptr<Identifier> converted_ident (item->to_ident_item ());
  //     if (converted_ident == nullptr)
  //       {
  //         ident_items.clear ();
  //         break;
  //       }
  //     ident_items.push_back (std::move (*converted_ident));
  //   }
  // // if valid return this
  // if (!ident_items.empty ())
  //   {
  //     return std::unique_ptr<MetaListIdents> (
  //       new MetaListIdents (std::move (ident), std::move (ident_items)));
  //   }
  // // as currently no meta list ident, currently no path. may change in future

  // pass for meta list paths
  std::vector<SimplePath> path_items;
  for (const auto &item : meta_items)
    {
      SimplePath converted_path (item->to_path_item ());
      if (converted_path.is_empty ())
	{
	  path_items.clear ();
	  break;
	}
      path_items.push_back (std::move (converted_path));
    }
  if (!path_items.empty ())
    {
      return std::unique_ptr<MetaListPaths> (
	new MetaListPaths (ident, ident_locus, std::move (path_items)));
    }

  rust_error_at (UNKNOWN_LOCATION, "failed to parse any meta item inner");
  return nullptr;
}

bool
AttributeParser::is_end_meta_item_tok (TokenId id) const
{
  return id == COMMA || id == RIGHT_PAREN;
}

std::unique_ptr<MetaItem>
AttributeParser::parse_path_meta_item ()
{
  SimplePath path = parse_simple_path ();
  if (path.is_empty ())
    {
      rust_error_at (peek_token ()->get_locus (),
		     "failed to parse simple path in attribute");
      return nullptr;
    }

  switch (peek_token ()->get_id ())
    {
      case LEFT_PAREN: {
	std::vector<std::unique_ptr<MetaItemInner>> meta_items
	  = parse_meta_item_seq ();

	return std::unique_ptr<MetaItemSeq> (
	  new MetaItemSeq (std::move (path), std::move (meta_items)));
      }
      case EQUAL: {
	skip_token ();

	location_t locus = peek_token ()->get_locus ();
	Literal lit = parse_literal ();
	if (lit.is_error ())
	  {
	    rust_error_at (peek_token ()->get_locus (),
			   "failed to parse literal in attribute");
	    return nullptr;
	  }
	LiteralExpr expr (std::move (lit), {}, locus);
	// stream_pos++;
	/* shouldn't be required anymore due to parsing literal actually
	 * skipping the token */
	return std::unique_ptr<MetaItemPathLit> (
	  new MetaItemPathLit (std::move (path), std::move (expr)));
      }
    case COMMA:
      // just simple path
      return std::unique_ptr<MetaItemPath> (
	new MetaItemPath (std::move (path)));
    default:
      rust_error_at (peek_token ()->get_locus (),
		     "unrecognised token '%s' in meta item",
		     get_token_description (peek_token ()->get_id ()));
      return nullptr;
    }
}

/* Parses a parenthesised sequence of meta item inners. Parentheses are
 * required here. */
std::vector<std::unique_ptr<MetaItemInner>>
AttributeParser::parse_meta_item_seq ()
{
  int vec_length = token_stream.size ();
  std::vector<std::unique_ptr<MetaItemInner>> meta_items;

  if (peek_token ()->get_id () != LEFT_PAREN)
    {
      rust_error_at (peek_token ()->get_locus (),
		     "missing left paren in delim token tree");
      return {};
    }
  skip_token ();

  while (stream_pos < vec_length && peek_token ()->get_id () != RIGHT_PAREN)
    {
      std::unique_ptr<MetaItemInner> inner = parse_meta_item_inner ();
      if (inner == nullptr)
	{
	  rust_error_at (peek_token ()->get_locus (),
			 "failed to parse inner meta item in attribute");
	  return {};
	}
      meta_items.push_back (std::move (inner));

      if (peek_token ()->get_id () != COMMA)
	break;

      skip_token ();
    }

  if (peek_token ()->get_id () != RIGHT_PAREN)
    {
      rust_error_at (peek_token ()->get_locus (),
		     "missing right paren in delim token tree");
      return {};
    }
  skip_token ();

  return meta_items;
}

/* Collects any nested token trees into a flat token stream, suitable for
 * parsing. */
std::vector<std::unique_ptr<Token>>
DelimTokenTree::to_token_stream () const
{
  std::vector<std::unique_ptr<Token>> tokens;
  for (const auto &tree : token_trees)
    {
      std::vector<std::unique_ptr<Token>> stream = tree->to_token_stream ();

      tokens.insert (tokens.end (), std::make_move_iterator (stream.begin ()),
		     std::make_move_iterator (stream.end ()));
    }

  tokens.shrink_to_fit ();
  return tokens;
}

Literal
AttributeParser::parse_literal ()
{
  const std::unique_ptr<Token> &tok = peek_token ();
  switch (tok->get_id ())
    {
    case CHAR_LITERAL:
      skip_token ();
      return Literal (tok->as_string (), Literal::CHAR, tok->get_type_hint ());
    case STRING_LITERAL:
      skip_token ();
      return Literal (tok->as_string (), Literal::STRING,
		      tok->get_type_hint ());
    case BYTE_CHAR_LITERAL:
      skip_token ();
      return Literal (tok->as_string (), Literal::BYTE, tok->get_type_hint ());
    case BYTE_STRING_LITERAL:
      skip_token ();
      return Literal (tok->as_string (), Literal::BYTE_STRING,
		      tok->get_type_hint ());
    case INT_LITERAL:
      skip_token ();
      return Literal (tok->as_string (), Literal::INT, tok->get_type_hint ());
    case FLOAT_LITERAL:
      skip_token ();
      return Literal (tok->as_string (), Literal::FLOAT, tok->get_type_hint ());
    case TRUE_LITERAL:
      skip_token ();
      return Literal ("true", Literal::BOOL, tok->get_type_hint ());
    case FALSE_LITERAL:
      skip_token ();
      return Literal ("false", Literal::BOOL, tok->get_type_hint ());
    default:
      rust_error_at (tok->get_locus (), "expected literal - found '%s'",
		     get_token_description (tok->get_id ()));
      return Literal::create_error ();
    }
}

SimplePath
AttributeParser::parse_simple_path ()
{
  bool has_opening_scope_res = false;
  if (peek_token ()->get_id () == SCOPE_RESOLUTION)
    {
      has_opening_scope_res = true;
      skip_token ();
    }

  std::vector<SimplePathSegment> segments;

  SimplePathSegment segment = parse_simple_path_segment ();
  if (segment.is_error ())
    {
      rust_error_at (
	peek_token ()->get_locus (),
	"failed to parse simple path segment in attribute simple path");
      return SimplePath::create_empty ();
    }
  segments.push_back (std::move (segment));

  while (peek_token ()->get_id () == SCOPE_RESOLUTION)
    {
      skip_token ();

      SimplePathSegment segment = parse_simple_path_segment ();
      if (segment.is_error ())
	{
	  rust_error_at (
	    peek_token ()->get_locus (),
	    "failed to parse simple path segment in attribute simple path");
	  return SimplePath::create_empty ();
	}
      segments.push_back (std::move (segment));
    }
  segments.shrink_to_fit ();

  return SimplePath (std::move (segments), has_opening_scope_res);
}

SimplePathSegment
AttributeParser::parse_simple_path_segment ()
{
  const std::unique_ptr<Token> &tok = peek_token ();
  switch (tok->get_id ())
    {
    case IDENTIFIER:
      skip_token ();
      return SimplePathSegment (tok->as_string (), tok->get_locus ());
    case SUPER:
      skip_token ();
      return SimplePathSegment ("super", tok->get_locus ());
    case SELF:
      skip_token ();
      return SimplePathSegment ("self", tok->get_locus ());
    case CRATE:
      skip_token ();
      return SimplePathSegment ("crate", tok->get_locus ());
    case DOLLAR_SIGN:
      if (peek_token (1)->get_id () == CRATE)
	{
	  skip_token (1);
	  return SimplePathSegment ("$crate", tok->get_locus ());
	}
      gcc_fallthrough ();
    default:
      rust_error_at (tok->get_locus (),
		     "unexpected token '%s' in simple path segment",
		     get_token_description (tok->get_id ()));
      return SimplePathSegment::create_error ();
    }
}

std::unique_ptr<MetaItemLitExpr>
AttributeParser::parse_meta_item_lit ()
{
  location_t locus = peek_token ()->get_locus ();
  LiteralExpr lit_expr (parse_literal (), {}, locus);
  return std::unique_ptr<MetaItemLitExpr> (
    new MetaItemLitExpr (std::move (lit_expr)));
}

bool
AttrInputMetaItemContainer::check_cfg_predicate (const Session &session) const
{
  if (items.empty ())
    return false;

  for (const auto &inner_item : items)
    {
      if (!inner_item->check_cfg_predicate (session))
	return false;
    }

  return true;
}

bool
MetaItemLitExpr::check_cfg_predicate (const Session &) const
{
  /* as far as I can tell, a literal expr can never be a valid cfg body, so
   * false */
  return false;
}

bool
MetaListNameValueStr::check_cfg_predicate (const Session &session) const
{
  if (ident.as_string () == "all")
    {
      for (const auto &str : strs)
	{
	  if (!str.check_cfg_predicate (session))
	    return false;
	}
      return true;
    }
  else if (ident.as_string () == "any")
    {
      for (const auto &str : strs)
	{
	  if (str.check_cfg_predicate (session))
	    return true;
	}
      return false;
    }
  else if (ident.as_string () == "not")
    {
      if (strs.size () != 1)
	{
	  /* HACK: convert vector platform-dependent size_type to string to
	   * use in printf */
	  rust_error_at (UNKNOWN_LOCATION,
			 "cfg predicate could not be checked for "
			 "MetaListNameValueStr with ident of "
			 "'not' because there are '%s' elements, not '1'",
			 std::to_string (strs.size ()).c_str ());
	  return false;
	}

      return !strs[0].check_cfg_predicate (session);
    }
  else
    {
      rust_error_at (UNKNOWN_LOCATION,
		     "cfg predicate could not be checked for "
		     "MetaListNameValueStr with ident of "
		     "'%s' - ident must be 'all' or 'any'",
		     ident.as_string ().c_str ());
      return false;
    }
}

bool
MetaListPaths::check_cfg_predicate (const Session &session) const
{
  if (ident.as_string () == "all")
    {
      for (const auto &path : paths)
	{
	  if (!check_path_exists_in_cfg (session, path))
	    return false;
	}
      return true;
    }
  else if (ident.as_string () == "any")
    {
      for (const auto &path : paths)
	{
	  if (check_path_exists_in_cfg (session, path))
	    return true;
	}
      return false;
    }
  else if (ident.as_string () == "not")
    {
      if (paths.size () != 1)
	{
	  // HACK: convert vector platform-dependent size_type to string to
	  // use in printf
	  rust_error_at (UNKNOWN_LOCATION,
			 "cfg predicate could not be checked for MetaListPaths "
			 "with ident of 'not' "
			 "because there are '%s' elements, not '1'",
			 std::to_string (paths.size ()).c_str ());
	  return false;
	}

      return !check_path_exists_in_cfg (session, paths[0]);
    }
  else
    {
      rust_error_at (UNKNOWN_LOCATION,
		     "cfg predicate could not be checked for "
		     "MetaListNameValueStr with ident of "
		     "'%s' - ident must be 'all' or 'any'",
		     ident.as_string ().c_str ());
      return false;
    }
}

bool
MetaListPaths::check_path_exists_in_cfg (const Session &session,
					 const SimplePath &path) const
{
  return session.options.target_data.has_key (path.as_string ());
}

bool
MetaItemSeq::check_cfg_predicate (const Session &session) const
{
  if (path.as_string () == "all")
    {
      for (const auto &item : seq)
	{
	  if (!item->check_cfg_predicate (session))
	    return false;
	}
      return true;
    }
  else if (path.as_string () == "any")
    {
      for (const auto &item : seq)
	{
	  if (item->check_cfg_predicate (session))
	    return true;
	}
      return false;
    }
  else if (path.as_string () == "not")
    {
      if (seq.size () != 1)
	{
	  /* HACK: convert vector platform-dependent size_type to string to
	   * use in printf */
	  rust_error_at (UNKNOWN_LOCATION,
			 "cfg predicate could not be checked for MetaItemSeq "
			 "with ident of 'not' "
			 "because there are '%s' elements, not '1'",
			 std::to_string (seq.size ()).c_str ());
	  return false;
	}

      return !seq[0]->check_cfg_predicate (session);
    }
  else
    {
      rust_error_at (
	UNKNOWN_LOCATION,
	"cfg predicate could not be checked for MetaItemSeq with path of "
	"'%s' - path must be 'all' or 'any'",
	path.as_string ().c_str ());
      return false;
    }
}

bool
MetaWord::check_cfg_predicate (const Session &session) const
{
  return session.options.target_data.has_key (ident.as_string ());
}

bool
MetaItemPath::check_cfg_predicate (const Session &session) const
{
  /* Strictly speaking, this should always be false, but maybe do check
   * relating to SimplePath being identifier. Currently, it would return true
   * if path as identifier existed, and if the path in string form existed
   * (though this shouldn't occur). */
  return session.options.target_data.has_key (path.as_string ());
}

bool
MetaNameValueStr::check_cfg_predicate (const Session &session) const
{
  // DEBUG
  rust_debug (
    "checked key-value pair for cfg: '%s', '%s' - is%s in target data",
    ident.as_string ().c_str (), str.c_str (),
    session.options.target_data.has_key_value_pair (ident.as_string (), str)
      ? ""
      : " not");

  return session.options.target_data.has_key_value_pair (ident.as_string (),
							 str);
}

bool
MetaItemPathLit::check_cfg_predicate (const Session &session) const
{
  return session.options.target_data.has_key_value_pair (path.as_string (),
							 lit.as_string ());
}

std::vector<std::unique_ptr<Token>>
Token::to_token_stream () const
{
  /* initialisation list doesn't work as it needs copy constructor, so have to
   * do this */
  std::vector<std::unique_ptr<Token>> dummy_vector;
  dummy_vector.reserve (1);
  dummy_vector.push_back (std::unique_ptr<Token> (clone_token_impl ()));
  return dummy_vector;
}

Attribute
MetaNameValueStr::to_attribute () const
{
  LiteralExpr lit_expr (str, Literal::LitType::STRING,
			PrimitiveCoreType::CORETYPE_UNKNOWN, {}, str_locus);
  // FIXME: What location do we put here? Is the literal above supposed to have
  // an empty location as well?
  // Should MetaNameValueStr keep a location?
  return Attribute (SimplePath::from_str (ident.as_string (), ident_locus),
		    std::unique_ptr<AttrInputLiteral> (
		      new AttrInputLiteral (std::move (lit_expr))));
}

Attribute
MetaItemPath::to_attribute () const
{
  return Attribute (path, nullptr);
}

Attribute
MetaItemSeq::to_attribute () const
{
  std::vector<std::unique_ptr<MetaItemInner>> new_seq;
  new_seq.reserve (seq.size ());
  for (const auto &e : seq)
    new_seq.push_back (e->clone_meta_item_inner ());

  std::unique_ptr<AttrInputMetaItemContainer> new_seq_container (
    new AttrInputMetaItemContainer (std::move (new_seq)));
  return Attribute (path, std::move (new_seq_container));
}

Attribute
MetaWord::to_attribute () const
{
  return Attribute (SimplePath::from_str (ident.as_string (), ident_locus),
		    nullptr);
}

Attribute
MetaListPaths::to_attribute () const
{
  /* probably one of the most annoying conversions - have to lose specificity by
   * turning it into just AttrInputMetaItemContainer (i.e. paths-only nature is
   * no longer known). If conversions back are required, might have to do a
   * "check all are paths" pass or something. */

  std::vector<std::unique_ptr<MetaItemInner>> new_seq;
  new_seq.reserve (paths.size ());
  for (const auto &e : paths)
    new_seq.push_back (std::unique_ptr<MetaItemPath> (new MetaItemPath (e)));

  std::unique_ptr<AttrInputMetaItemContainer> new_seq_container (
    new AttrInputMetaItemContainer (std::move (new_seq)));
  return Attribute (SimplePath::from_str (ident.as_string (), ident_locus),
		    std::move (new_seq_container));
}

Attribute
MetaListNameValueStr::to_attribute () const
{
  std::vector<std::unique_ptr<MetaItemInner>> new_seq;
  new_seq.reserve (strs.size ());
  for (const auto &e : strs)
    new_seq.push_back (
      std::unique_ptr<MetaNameValueStr> (new MetaNameValueStr (e)));

  std::unique_ptr<AttrInputMetaItemContainer> new_seq_container (
    new AttrInputMetaItemContainer (std::move (new_seq)));
  return Attribute (SimplePath::from_str (ident.as_string (), ident_locus),
		    std::move (new_seq_container));
}

Attribute
MetaItemPathLit::to_attribute () const
{
  return Attribute (path, std::unique_ptr<AttrInputLiteral> (
			    new AttrInputLiteral (lit)));
}

std::vector<Attribute>
AttrInputMetaItemContainer::separate_cfg_attrs () const
{
  rust_assert (!items.empty ());

  if (items.size () == 1)
    return {};

  std::vector<Attribute> attrs;
  attrs.reserve (items.size () - 1);

  for (auto it = items.begin () + 1; it != items.end (); ++it)
    {
      Attribute attr = (*it)->to_attribute ();
      if (attr.is_empty ())
	{
	  /* TODO should this be an error that causes us to chuck out
	   * everything? */
	  continue;
	}
      attrs.push_back (std::move (attr));
    }

  attrs.shrink_to_fit ();
  return attrs;
}

bool
Attribute::check_cfg_predicate (const Session &session) const
{
  /* assume that cfg predicate actually can exist, i.e. attribute has cfg or
   * cfg_attr path */
  if (!has_attr_input ()
      || (path.as_string () != Values::Attributes::CFG
	  && path.as_string () != Values::Attributes::CFG_ATTR))
    {
      // DEBUG message
      rust_debug (
	"tried to check cfg predicate on attr that either has no input "
	"or invalid path. attr: '%s'",
	as_string ().c_str ());

      return false;
    }

  // assume that it has already been parsed
  if (!is_parsed_to_meta_item ())
    return false;

  auto &meta_item = static_cast<AttrInputMetaItemContainer &> (*attr_input);
  return meta_item.get_items ().front ()->check_cfg_predicate (session);
}

std::vector<Attribute>
Attribute::separate_cfg_attrs () const
{
  if (!has_attr_input () || path.as_string () != Values::Attributes::CFG_ATTR)
    return {};

  // assume that it has already been parsed
  if (!is_parsed_to_meta_item ())
    return {};

  return attr_input->separate_cfg_attrs ();
}

bool
Attribute::is_parsed_to_meta_item () const
{
  return has_attr_input () && attr_input->is_meta_item ();
}

void
BlockExpr::normalize_tail_expr ()
{
  if (!expr)
    {
      // HACK: try to turn the last statement into a tail expression
      if (!statements.empty () && statements.back ()->is_expr ())
	{
	  // Watch out: This reference become invalid when the vector is
	  // modified.
	  auto &stmt = static_cast<ExprStmt &> (*statements.back ());

	  if (!stmt.is_semicolon_followed ())
	    {
	      expr = std::move (stmt.take_expr ());
	      statements.pop_back ();
	    }
	}
    }
}

// needed here because "rust-expr.h" doesn't include "rust-macro.h"
AttrInputMacro::AttrInputMacro (const AttrInputMacro &oth)
  : macro (oth.macro->clone_macro_invocation_impl ())
{}

void
AttrInputMacro::operator= (const AttrInputMacro &oth)
{
  macro = std::unique_ptr<MacroInvocation> (
    oth.macro->clone_macro_invocation_impl ());
}

/* Visitor implementations - these are short but inlining can't happen anyway
 * due to virtual functions and I didn't want to make the ast header includes
 * any longer than they already are. */

void
Token::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
DelimTokenTree::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
IdentifierExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
Lifetime::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
LifetimeParam::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
LiteralExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
AttrInputLiteral::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
AttrInputMacro::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MetaItemLitExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MetaItemPathLit::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
BorrowExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
DereferenceExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ErrorPropagationExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
NegationExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ArithmeticOrLogicalExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ComparisonExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
LazyBooleanExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TypeCastExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
AssignmentExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
CompoundAssignmentExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ArrayElemsValues::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ArrayElemsCopied::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ArrayExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ArrayIndexExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TupleExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TupleIndexExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprStruct::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprFieldIdentifier::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprFieldIdentifierValue::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprFieldIndexValue::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprStructFields::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprStructBase::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
CallExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MethodCallExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
FieldAccessExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ClosureExprInner::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
BlockExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ClosureExprInnerTyped::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ContinueExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
BreakExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
RangeFromToExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
RangeFromExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
RangeToExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
RangeFullExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
RangeFromToInclExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
RangeToInclExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ReturnExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
UnsafeBlockExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
LoopExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
WhileLoopExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
WhileLetLoopExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ForLoopExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
IfExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
IfExprConseqElse::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
IfLetExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
IfLetExprConseqElse::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MatchExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
AwaitExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
AsyncBlockExpr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TypeParam::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
LifetimeWhereClauseItem::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TypeBoundWhereClauseItem::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
Module::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ExternCrate::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
UseTreeGlob::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
UseTreeList::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
UseTreeRebind::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
UseDeclaration::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
Function::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TypeAlias::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
StructStruct::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TupleStruct::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
EnumItem::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
EnumItemTuple::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
EnumItemStruct::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
EnumItemDiscriminant::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
Enum::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
Union::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ConstantItem::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
StaticItem::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TraitItemConst::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TraitItemType::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
Trait::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
InherentImpl::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TraitImpl::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ExternalTypeItem::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ExternalStaticItem::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ExternBlock::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MacroMatchFragment::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MacroMatchRepetition::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MacroMatcher::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MacroRulesDefinition::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MacroInvocation::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
EmptyStmt::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
LetStmt::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ExprStmt::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TraitBound::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ImplTraitType::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TraitObjectType::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ParenthesisedType::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ImplTraitTypeOneBound::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TraitObjectTypeOneBound::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
TupleType::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
NeverType::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
RawPointerType::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ReferenceType::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
ArrayType::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
SliceType::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
InferredType::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
BareFunctionType::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MetaItemSeq::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MetaItemPath::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MetaListPaths::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MetaNameValueStr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MetaListNameValueStr::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
AttrInputMetaItemContainer::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
MetaWord::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

void
FormatArgs::accept_vis (ASTVisitor &vis)
{
  vis.visit (*this);
}

std::string
FormatArgs::as_string () const
{
  // FIXME(Arthur): Improve
  return "FormatArgs";
}

location_t
FormatArgs::get_locus () const
{
  return loc;
}

bool
FormatArgs::is_expr_without_block () const
{
  return false;
}

void
FormatArgs::mark_for_strip ()
{
  marked_for_strip = true;
}

bool
FormatArgs::is_marked_for_strip () const
{
  return marked_for_strip;
}

std::vector<Attribute> &
FormatArgs::get_outer_attrs ()
{
  rust_unreachable ();
}

void FormatArgs::set_outer_attrs (std::vector<Attribute>)
{
  rust_unreachable ();
}

Expr *
FormatArgs::clone_expr_impl () const
{
  std::cerr << "[ARTHUR] cloning FormatArgs! " << std::endl;

  return new FormatArgs (*this);
}

} // namespace AST

std::ostream &
operator<< (std::ostream &os, Identifier const &i)
{
  return os << i.as_string ();
}

} // namespace Rust
