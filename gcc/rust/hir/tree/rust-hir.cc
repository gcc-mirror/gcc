// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-ast-full.h"
#include "rust-hir-full.h"
#include "rust-hir-visitor.h"
#include "rust-diagnostics.h"

/* Compilation unit used for various HIR-related functions that would make
 * the headers too long if they were defined inline and don't receive any
 * benefits from being defined inline because they are virtual. Also used
 * for various other stuff. */

namespace Rust {
namespace HIR {

enum indent_mode
{
  enter,
  out,
  stay
};

std::string
indent_spaces (enum indent_mode mode)
{
  static int indent = 0;
  std::string str = "";
  if (out == mode)
    indent--;
  for (int i = 0; i < indent; i++)
    str += " ";
  if (enter == mode)
    indent++;

  return str;
}

// Gets a string in a certain delim type.
std::string
get_string_in_delims (std::string str_input, AST::DelimType delim_type)
{
  switch (delim_type)
    {
    case AST::DelimType::PARENS:
      return "(" + str_input + ")";
    case AST::DelimType::SQUARE:
      return "[" + str_input + "]";
    case AST::DelimType::CURLY:
      return "{" + str_input + "}";
    default:
      return "ERROR-MARK-STRING (delims)";
    }
  gcc_unreachable ();
}

std::string
Crate::as_string () const
{
  std::string str ("HIR::Crate: ");

  // inner attributes
  str += "\n inner attributes: ";
  if (inner_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "inner attribute" syntax -
       * just the body */
      for (const auto &attr : inner_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

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
	      return "nullptr_POINTER_MARK";
	    }

	  str += "\n  " + item->as_string ();
	}
    }

  return str + "\n::" + get_mappings ().as_string () + "\n";
}

std::string
Visibility::as_string () const
{
  switch (vis_type)
    {
    case PRIVATE:
      return std::string ("private");
    case PUBLIC:
      return std::string ("pub");
    case RESTRICTED:
      return std::string ("pub(in ") + path.get_mappings ().as_string ()
	     + std::string (")");
    default:
      gcc_unreachable ();
    }
}

// Creates a string that reflects the visibility stored.
std::string
VisItem::as_string () const
{
  // FIXME: can't do formatting on string to make identation occur.
  std::string str = Item::as_string ();

  if (has_visibility ())
    {
      str = visibility.as_string () + " ";
    }

  return str;
}

// Creates a string that reflects the outer attributes stored.
std::string
Item::as_string () const
{
  std::string str;

  if (!outer_attrs.empty ())
    {
      for (const auto &attr : outer_attrs)
	{
	  str += attr.as_string () + "\n";
	}
    }

  return str;
}

std::string
Module::as_string () const
{
  // get module string for "[vis] mod [name]"
  std::string str = VisItem::as_string () + "mod " + module_name;

  // inner attributes
  str += "\n inner attributes: ";
  if (inner_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "inner attribute" syntax -
       * just the body */
      for (const auto &attr : inner_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

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
	      return "nullptr_POINTER_MARK";
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

  if (is_mut ())
    {
      str += " mut";
    }

  str += name;

  // DEBUG: null pointer check
  if (type == nullptr)
    {
      rust_debug ("something really terrible has gone wrong - null "
		  "pointer type in static item.");
      return "nullptr_POINTER_MARK";
    }
  str += "\n" + indent_spaces (stay) + "Type: " + type->as_string ();

  // DEBUG: null pointer check
  if (expr == nullptr)
    {
      rust_debug ("something really terrible has gone wrong - null "
		  "pointer expr in static item.");
      return "nullptr_POINTER_MARK";
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
    {
      str += " as " + as_clause_name;
    }

  return str;
}

std::string
TupleStruct::as_string () const
{
  std::string str = VisItem::as_string ();

  str += "struct " + struct_name;

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
	      return "nullptr_POINTER_MARK";
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
	{
	  str += "\n  " + field.as_string ();
	}
    }

  str += "\n Where clause: ";
  if (has_where_clause ())
    {
      str += where_clause.as_string ();
    }
  else
    {
      str += "none";
    }

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
      return "nullptr_POINTER_MARK";
    }
  str += "\n  Type: " + type->as_string ();

  // DEBUG: null pointer check
  if (const_expr == nullptr)
    {
      rust_debug ("something really terrible has gone wrong - null "
		  "pointer expr in const item.");
      return "nullptr_POINTER_MARK";
    }
  str += "\n  Expression: " + const_expr->as_string ();

  return str + "\n";
}

std::string
ImplBlock::as_string () const
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
		"generic param in impl.");
	      return "nullptr_POINTER_MARK";
	    }

	  str += "\n  " + param->as_string ();
	}
    }

  str += "\n Type: " + impl_type->as_string ();

  str += "\n Where clause: ";
  if (has_where_clause ())
    {
      str += where_clause.as_string ();
    }
  else
    {
      str += "none";
    }

  // inner attributes
  str += "\n inner attributes: ";
  if (inner_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "inner attribute" syntax -
       * just the body */
      for (const auto &attr : inner_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

  str += "\n impl items: ";
  if (!has_impl_items ())
    {
      str += "none";
    }
  else
    {
      for (const auto &item : impl_items)
	{
	  str += "\n  " + item->as_string ();
	}
    }

  return str;
}

std::string
StructStruct::as_string () const
{
  std::string str = VisItem::as_string ();

  str += "struct " + struct_name;

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
	      return "nullptr_POINTER_MARK";
	    }

	  str += "\n  " + param->as_string ();
	}
    }

  str += "\n Where clause: ";
  if (has_where_clause ())
    {
      str += where_clause.as_string ();
    }
  else
    {
      str += "none";
    }

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
	{
	  str += "\n  - " + field.as_string ();
	}
      str += "\n";
    }

  return str + "::" + get_mappings ().as_string () + "\n";
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
      return "nullptr_POINTER_MARK";
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
  gcc_unreachable ();
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
	  return "nullptr_POINTER_MARK";
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
      path_str += " as " + identifier;
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
  str += enum_name;

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
	      return "nullptr_POINTER_MARK";
	    }

	  str += "\n  " + param->as_string ();
	}
    }

  str += "\n Where clause: ";
  if (has_where_clause ())
    {
      str += where_clause.as_string ();
    }
  else
    {
      str += "none";
    }

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
	      return "nullptr_POINTER_MARK";
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

  if (unsafety == Unsafety::Unsafe)
    {
      str += "unsafe ";
    }

  str += "trait " + name;

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
	      return "nullptr_POINTER_MARK";
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
	      return "nullptr_POINTER_MARK";
	    }

	  str += "\n  " + bound->as_string ();
	}
    }

  str += "\n Where clause: ";
  if (!has_where_clause ())
    {
      str += "none";
    }
  else
    {
      str += where_clause.as_string ();
    }

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
	      return "nullptr_POINTER_MARK";
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

  str += "union " + union_name;

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
	      return "nullptr_POINTER_MARK";
	    }

	  str += "\n  " + param->as_string ();
	}
    }

  str += "\n Where clause: ";
  if (has_where_clause ())
    {
      str += where_clause.as_string ();
    }
  else
    {
      str += "none";
    }

  // struct fields
  str += "\n Struct fields (variants): ";
  if (variants.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &field : variants)
	{
	  str += "\n  " + field.as_string ();
	}
    }

  return str;
}

std::string
Function::as_string () const
{
  std::string str = VisItem::as_string () + "\n";
  std::string qstr = qualifiers.as_string ();
  if ("" != qstr)
    str += qstr + " ";

  if (has_function_return_type ())
    {
      // DEBUG: null pointer check
      if (return_type == nullptr)
	{
	  rust_debug (
	    "something really terrible has gone wrong - null pointer return "
	    "type in function.");
	  return "nullptr_POINTER_MARK";
	}

      str += return_type->as_string () + " ";
    }
  else
    {
      str += "void ";
    }

  str += function_name;

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
	  return "nullptr_POINTER_MARK";
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
	  str += (*i).as_string ();
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
    {
      str += " where " + where_clause.as_string ();
    }

  str += "\n";

  // DEBUG: null pointer check
  if (function_body == nullptr)
    {
      rust_debug (
	"something really terrible has gone wrong - null pointer function "
	"body in function.");
      return "nullptr_POINTER_MARK";
    }
  return str + function_body->as_string () + "::" + get_mappings ().as_string ()
	 + "\n";
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
	{
	  str += "\n  " + item->as_string ();
	}
    }

  return str;
}

std::string
BlockExpr::as_string () const
{
  std::string istr = indent_spaces (enter);
  std::string str = istr + "BlockExpr:\n" + istr;
  // get outer attributes
  str += "{\n" + indent_spaces (stay) + Expr::as_string ();

  // inner attributes
  str += "\n" + indent_spaces (stay) + "inner attributes: ";
  if (inner_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "inner attribute" syntax -
       * just the body */
      for (const auto &attr : inner_attrs)
	{
	  str += "\n" + indent_spaces (stay) + attr.as_string ();
	}
    }

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
	      return "nullptr_POINTER_MARK";
	    }

	  str += "\n" + indent_spaces (stay) + stmt->as_string ();
	}
    }

  // final expression
  str += "\n" + indent_spaces (stay) + "final expression: ";
  if (expr == nullptr)
    {
      str += "none";
    }
  else
    {
      str += "\n" + expr->as_string ();
    }

  str += "\n" + indent_spaces (out) + "}";
  return str;
}

std::string
TypeAlias::as_string () const
{
  std::string str = VisItem::as_string ();

  str += " " + new_type_name;

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
    {
      str += "none";
    }
  else
    {
      str += where_clause.as_string ();
    }

  str += "\n Type: " + existing_type->as_string ();

  return str;
}

std::string
ExternBlock::as_string () const
{
  std::string str = VisItem::as_string ();

  str += "extern ";
  str += "\"" + get_string_from_abi (abi) + "\" ";

  // inner attributes
  str += "\n inner attributes: ";
  if (inner_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "inner attribute" syntax -
       * just the body */
      for (const auto &attr : inner_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

  str += "\n external items: ";
  if (!has_extern_items ())
    {
      str += "none";
    }
  else
    {
      for (const auto &item : extern_items)
	{
	  str += "\n  " + item->as_string ();
	}
    }

  return str;
}

std::string
PathInExpression::as_string () const
{
  std::string str;

  if (has_opening_scope_resolution)
    {
      str = "::";
    }

  return str + PathPattern::as_string () + "::" + get_mappings ().as_string ();
}

std::string
ExprStmtWithBlock::as_string () const
{
  std::string str = indent_spaces (enter) + "ExprStmtWithBlock: \n";

  if (expr == nullptr)
    {
      str += "none (this should not happen and is an error)";
    }
  else
    {
      indent_spaces (enter);
      str += expr->as_string ();
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
    {
      str += " : " + type->as_string ();
    }

  return str;
}

std::string
ClosureExpr::as_string () const
{
  std::string str ("ClosureExpr:\n Has move: ");
  if (has_move)
    {
      str += "true";
    }
  else
    {
      str += "false";
    }

  str += "\n Params: ";
  if (params.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &param : params)
	{
	  str += "\n  " + param.as_string ();
	}
    }

  str += "\n Return type: "
	 + (has_return_type () ? return_type->as_string () : "none");

  str += "\n Body: " + expr->as_string ();

  return str;
}

std::string
PathPattern::as_string () const
{
  std::string str;

  for (const auto &segment : segments)
    {
      str += segment.as_string () + "::";
    }

  // basically a hack - remove last two characters of string (remove final ::)
  str.erase (str.length () - 2);

  return str;
}

std::string
QualifiedPathType::as_string () const
{
  std::string str ("<");
  str += type->as_string ();

  if (has_as_clause ())
    {
      str += " as " + trait->as_string ();
    }

  return str + ">";
}

std::string
QualifiedPathInExpression::as_string () const
{
  return path_type.as_string () + "::" + PathPattern::as_string ();
}

std::string
BorrowExpr::as_string () const
{
  std::string str ("&");

  if (double_borrow)
    {
      str += "&";
    }

  if (is_mut ())
    {
      str += "mut ";
    }

  str += main_or_left_expr->as_string ();

  return str;
}

std::string
ReturnExpr::as_string () const
{
  std::string str ("return ");

  if (has_return_expr ())
    {
      str += return_expr->as_string ();
    }

  return str + "::" + get_mappings ().as_string ();
}

std::string
GroupedExpr::as_string () const
{
  std::string str ("Grouped expr:");

  // inner attributes
  str += "\n inner attributes: ";
  if (inner_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "inner attribute" syntax -
       * just the body */
      for (const auto &attr : inner_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

  str += "\n Expr in parens: " + expr_in_parens->as_string ();

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
  std::string str ("continue ");

  if (has_label ())
    {
      str += label.as_string ();
    }

  return str;
}

std::string
NegationExpr::as_string () const
{
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
  return array_expr->as_string () + "[" + index_expr->as_string () + "]";
}

std::string
AssignmentExpr::as_string () const
{
  return main_or_left_expr->as_string () + " = " + right_expr->as_string ()
	 + "::" + get_mappings ().as_string ();
}

std::string
CompoundAssignmentExpr::as_string () const
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
      gcc_unreachable ();
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
AsyncBlockExpr::as_string () const
{
  std::string str = "AsyncBlockExpr: ";

  // get outer attributes
  str += "\n " + Expr::as_string ();

  str += "\n Has move: ";
  str += has_move ? "true" : "false";

  return str + "\n" + block_expr->as_string ();
}

std::string
ComparisonExpr::as_string () const
{
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
  std::string str ("MethodCallExpr: \n Object (receiver) expr: ");

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
	    {
	      return "ERROR_MARK_STRING - method call expr param is null";
	    }

	  str += "\n  " + param->as_string ();
	}
    }

  return str;
}

std::string
TupleIndexExpr::as_string () const
{
  return tuple_expr->as_string () + "." + std::to_string (tuple_index);
}

std::string
DereferenceExpr::as_string () const
{
  return "*" + main_or_left_expr->as_string ();
}

std::string
FieldAccessExpr::as_string () const
{
  return receiver->as_string () + "." + field;
}

std::string
LazyBooleanExpr::as_string () const
{
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
  return from->as_string () + ".." + to->as_string ();
}

std::string
RangeToInclExpr::as_string () const
{
  return "..=" + to->as_string ();
}

std::string
UnsafeBlockExpr::as_string () const
{
  std::string istr = indent_spaces (enter);
  std::string str = istr + "UnsafeBlockExpr:";
  str += istr + "{";

  // get outer attributes
  str += "\n" + indent_spaces (stay) + Expr::as_string ();

  return str + "\n" + indent_spaces (out) + "}\n" + expr->as_string ();
}

std::string
IfExpr::as_string () const
{
  std::string str ("IfExpr: ");

  str += "\n Condition expr: " + condition->as_string ();

  str += "\n If block expr: " + if_block->as_string ();

  return str;
}

std::string
IfExprConseqElse::as_string () const
{
  std::string str = IfExpr::as_string ();

  str += "\n Else block expr: " + else_block->as_string ();

  return str;
}

std::string
IfExprConseqIf::as_string () const
{
  std::string str = IfExpr::as_string ();

  str += "\n Else if expr: \n  " + conseq_if_expr->as_string ();

  return str;
}

std::string
IfExprConseqIfLet::as_string () const
{
  std::string str = IfExpr::as_string ();

  str += "\n Else if let expr: \n  " + if_let_expr->as_string ();

  return str;
}

std::string
IfLetExpr::as_string () const
{
  std::string str ("IfLetExpr: ");

  str += "\n Condition match arm patterns: ";
  if (match_arm_patterns.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &pattern : match_arm_patterns)
	{
	  str += "\n  " + pattern->as_string ();
	}
    }

  str += "\n Scrutinee expr: " + value->as_string ();

  str += "\n If let block expr: " + if_block->as_string ();

  return str;
}

std::string
IfLetExprConseqElse::as_string () const
{
  std::string str = IfLetExpr::as_string ();

  str += "\n Else block expr: " + else_block->as_string ();

  return str;
}

std::string
IfLetExprConseqIf::as_string () const
{
  std::string str = IfLetExpr::as_string ();

  str += "\n Else if expr: \n  " + if_expr->as_string ();

  return str;
}

std::string
IfLetExprConseqIfLet::as_string () const
{
  std::string str = IfLetExpr::as_string ();

  str += "\n Else if let expr: \n  " + if_let_expr->as_string ();

  return str;
}

std::string
RangeFromToInclExpr::as_string () const
{
  return from->as_string () + "..=" + to->as_string ();
}

std::string
ErrorPropagationExpr::as_string () const
{
  return main_or_left_expr->as_string () + "?";
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
      gcc_unreachable ();
      break;
    }

  std::string str = main_or_left_expr->as_string () + " ";
  str += operator_str + " ";
  str += right_expr->as_string ();

  return "( " + str + " (" + get_mappings ().as_string () + "))";
}

std::string
CallExpr::as_string () const
{
  std::string str = function->as_string () + "(";
  if (!has_params ())
    str += "none";
  else
    {
      for (const auto &param : params)
	{
	  if (param == nullptr)
	    {
	      return "ERROR_MARK_STRING - call expr param is null";
	    }

	  str += param->as_string () + ",";
	}
    }
  return str + ")" + "::" + get_mappings ().as_string ();
}

std::string
WhileLoopExpr::as_string () const
{
  std::string str ("WhileLoopExpr: ");

  str += "\n Label: ";
  if (!has_loop_label ())
    {
      str += "none";
    }
  else
    {
      str += loop_label.as_string ();
    }

  str += "\n Conditional expr: " + condition->as_string ();

  str += "\n Loop block: " + loop_block->as_string ();

  return str;
}

std::string
WhileLetLoopExpr::as_string () const
{
  std::string str ("WhileLetLoopExpr: ");

  str += "\n Label: ";
  if (!has_loop_label ())
    {
      str += "none";
    }
  else
    {
      str += loop_label.as_string ();
    }

  str += "\n Match arm patterns: ";
  if (match_arm_patterns.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &pattern : match_arm_patterns)
	{
	  str += "\n  " + pattern->as_string ();
	}
    }

  str += "\n Scrutinee expr: " + condition->as_string ();

  str += "\n Loop block: " + loop_block->as_string ();

  return str;
}

std::string
LoopExpr::as_string () const
{
  std::string str ("LoopExpr: (infinite loop)");

  str += "\n Label: ";
  if (!has_loop_label ())
    {
      str += "none";
    }
  else
    {
      str += loop_label.as_string ();
    }

  str += "\n Loop block: " + loop_block->as_string ();

  return str;
}

std::string
ArrayExpr::as_string () const
{
  std::string str ("ArrayExpr:");

  // inner attributes
  str += "\n inner attributes: ";
  if (inner_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "inner attribute" syntax -
       * just the body */
      for (const auto &attr : inner_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

  str += "\n Array elems: ";
  if (!has_array_elems ())
    {
      str += "none";
    }
  else
    {
      str += internal_elements->as_string ();
    }

  return str;
}

std::string
AwaitExpr::as_string () const
{
  return awaited_expr->as_string () + ".await";
}

std::string
BreakExpr::as_string () const
{
  std::string str ("break ");

  if (has_label ())
    {
      str += label.as_string () + " ";
    }

  if (has_break_expr ())
    {
      str += break_expr->as_string ();
    }

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
  std::string str = "Outer attributes: ";
  if (outer_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "outer attribute" syntax -
       * just the body */
      for (const auto &attr : outer_attrs)
	{
	  str += "\n " + attr.as_string ();
	}
    }

  str += "\nPatterns: ";
  if (match_arm_patterns.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &pattern : match_arm_patterns)
	{
	  str += "\n " + pattern->as_string ();
	}
    }

  str += "\nGuard expr: ";
  if (!has_match_arm_guard ())
    {
      str += "none";
    }
  else
    {
      str += guard_expr->as_string ();
    }

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

/*std::string
MatchCaseBlockExpr::as_string () const
{
  std::string str = MatchCase::as_string ();

  str += "\n Block expr: " + block_expr->as_string ();

  return str;
}

std::string
MatchCaseExpr::as_string () const
{
  std::string str = MatchCase::as_string ();

  str += "\n Expr: " + expr->as_string ();

  return str;
}*/

std::string
MatchExpr::as_string () const
{
  std::string str ("MatchExpr:");

  str += "\n Scrutinee expr: " + branch_value->as_string ();

  // inner attributes
  str += "\n inner attributes: ";
  if (inner_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "inner attribute" syntax -
       * just the body */
      for (const auto &attr : inner_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

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

  // inner attributes
  str += "\n inner attributes: ";
  if (inner_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "inner attribute" syntax -
       * just the body */
      for (const auto &attr : inner_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

  str += "\n Tuple elements: ";
  if (tuple_elems.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &elem : tuple_elems)
	{
	  str += "\n  " + elem->as_string ();
	}
    }

  return str;
}

std::string
ExprStmtWithoutBlock::as_string () const
{
  std::string str ("ExprStmtWithoutBlock:\n");
  indent_spaces (enter);
  str += indent_spaces (stay);

  if (expr == nullptr)
    {
      str += "none (this shouldn't happen and is probably an error)";
    }
  else
    {
      str += expr->as_string ();
    }
  indent_spaces (out);

  return str;
}

std::string
FunctionParam::as_string () const
{
  return param_name->as_string () + " : " + type->as_string ();
}

std::string
FunctionQualifiers::as_string () const
{
  std::string str;

  switch (const_status)
    {
    case NONE:
      // do nothing
      break;
    case CONST_FN:
      str += "const ";
      break;
    case ASYNC_FN:
      str += "async ";
      break;
    default:
      return "ERROR_MARK_STRING: async-const status failure";
    }

  if (unsafety == Unsafety::Unsafe)
    {
      str += "unsafe ";
    }

  if (has_extern)
    {
      str += "extern";
      str += " \"" + get_string_from_abi (abi) + "\"";
    }

  return str;
}

std::string
TraitBound::as_string () const
{
  std::string str ("TraitBound:");

  str += "\n Has opening question mark: ";
  if (opening_question_mark)
    {
      str += "true";
    }
  else
    {
      str += "false";
    }

  str += "\n For lifetimes: ";
  if (!has_for_lifetimes ())
    {
      str += "none";
    }
  else
    {
      for (const auto &lifetime : for_lifetimes)
	{
	  str += "\n  " + lifetime.as_string ();
	}
    }

  str += "\n Type path: " + type_path.as_string ();

  return str;
}

std::string
LifetimeParam::as_string () const
{
  std::string str ("LifetimeParam: ");

  str += "\n Outer attribute: ";
  if (!has_outer_attribute ())
    {
      str += "none";
    }
  else
    {
      str += outer_attr.as_string ();
    }

  str += "\n Lifetime: " + lifetime.as_string ();

  str += "\n Lifetime bounds: ";
  if (!has_lifetime_bounds ())
    {
      str += "none";
    }
  else
    {
      for (const auto &bound : lifetime_bounds)
	{
	  str += "\n  " + bound.as_string ();
	}
    }

  return str;
}

std::string
QualifiedPathInType::as_string () const
{
  std::string str = path_type.as_string ();

  for (const auto &segment : segments)
    {
      str += "::" + segment->as_string ();
    }

  return str;
}

std::string
Lifetime::as_string () const
{
  if (is_error ())
    {
      return "error lifetime";
    }

  switch (lifetime_type)
    {
    case AST::Lifetime::LifetimeType::NAMED:
      return "'" + lifetime_name;
    case AST::Lifetime::LifetimeType::STATIC:
      return "'static";
    case AST::Lifetime::LifetimeType::WILDCARD:
      return "'_";
    default:
      return "ERROR-MARK-STRING: lifetime type failure";
    }
}

std::string
TypePath::as_string () const
{
  std::string str;

  if (has_opening_scope_resolution)
    {
      str = "::";
    }

  for (const auto &segment : segments)
    {
      str += segment->as_string () + "::";
    }

  // kinda hack - remove last 2 '::' characters
  str.erase (str.length () - 2);

  return str;
}

std::string
TypeParam::as_string () const
{
  std::string str ("TypeParam: ");

  str += "\n Outer attribute: ";
  if (!has_outer_attribute ())
    {
      str += "none";
    }
  else
    {
      str += outer_attr.as_string ();
    }

  str += "\n Identifier: " + type_representation;

  str += "\n Type param bounds: ";
  if (!has_type_param_bounds ())
    {
      str += "none";
    }
  else
    {
      for (const auto &bound : type_param_bounds)
	{
	  str += "\n  " + bound->as_string ();
	}
    }

  str += "\n Type: ";
  if (!has_type ())
    {
      str += "none";
    }
  else
    {
      str += type->as_string ();
    }

  return str;
}

AST::SimplePath
PathPattern::convert_to_simple_path (bool with_opening_scope_resolution) const
{
  if (!has_segments ())
    {
      return AST::SimplePath::create_empty ();
    }

  // create vector of reserved size (to minimise reallocations)
  std::vector<AST::SimplePathSegment> simple_segments;
  simple_segments.reserve (segments.size ());

  for (const auto &segment : segments)
    {
      // return empty path if doesn't meet simple path segment requirements
      if (segment.has_generic_args () || segment.as_string () == "Self")
	{
	  return AST::SimplePath::create_empty ();
	}

      // create segment and add to vector
      std::string segment_str = segment.as_string ();
      simple_segments.push_back (
	AST::SimplePathSegment (std::move (segment_str), segment.get_locus ()));
    }

  // kind of a HACK to get locus depending on opening scope resolution
  Location locus = Linemap::unknown_location ();
  if (with_opening_scope_resolution)
    {
      locus = simple_segments[0].get_locus () - 2; // minus 2 chars for ::
    }
  else
    {
      locus = simple_segments[0].get_locus ();
    }

  return AST::SimplePath (std::move (simple_segments),
			  with_opening_scope_resolution, locus);
}

AST::SimplePath
TypePath::as_simple_path () const
{
  if (segments.empty ())
    {
      return AST::SimplePath::create_empty ();
    }

  // create vector of reserved size (to minimise reallocations)
  std::vector<AST::SimplePathSegment> simple_segments;
  simple_segments.reserve (segments.size ());

  for (const auto &segment : segments)
    {
      // return empty path if doesn't meet simple path segment requirements
      if (segment == nullptr || segment->is_error ()
	  || !segment->is_ident_only () || segment->as_string () == "Self")
	{
	  return AST::SimplePath::create_empty ();
	}

      // create segment and add to vector
      std::string segment_str = segment->as_string ();
      simple_segments.push_back (
	AST::SimplePathSegment (std::move (segment_str),
				segment->get_locus ()));
    }

  return AST::SimplePath (std::move (simple_segments),
			  has_opening_scope_resolution, locus);
}

std::string
PathExprSegment::as_string () const
{
  std::string ident_str = segment_name.as_string ();
  if (has_generic_args ())
    {
      ident_str += "::<" + generic_args.as_string () + ">";
    }

  return ident_str;
}

std::string
GenericArgs::as_string () const
{
  std::string args;

  // lifetime args
  if (!lifetime_args.empty ())
    {
      auto i = lifetime_args.begin ();
      auto e = lifetime_args.end ();

      for (; i != e; i++)
	{
	  args += (*i).as_string ();
	  if (e != i + 1)
	    args += ", ";
	}
    }

  // type args
  if (!type_args.empty ())
    {
      auto i = type_args.begin ();
      auto e = type_args.end ();

      for (; i != e; i++)
	{
	  args += (*i)->as_string ();
	  if (e != i + 1)
	    args += ", ";
	}
    }

  // binding args
  if (!binding_args.empty ())
    {
      auto i = binding_args.begin ();
      auto e = binding_args.end ();

      for (; i != e; i++)
	{
	  args += (*i).as_string ();
	  if (e != i + 1)
	    args += ", ";
	}
    }

  return args;
}

std::string
GenericArgsBinding::as_string () const
{
  return identifier + " = " + type->as_string ();
}

std::string
ForLoopExpr::as_string () const
{
  std::string str ("ForLoopExpr: ");

  str += "\n Label: ";
  if (!has_loop_label ())
    {
      str += "none";
    }
  else
    {
      str += loop_label.as_string ();
    }

  str += "\n Pattern: " + pattern->as_string ();

  str += "\n Iterator expr: " + iterator_expr->as_string ();

  str += "\n Loop block: " + loop_block->as_string ();

  return str;
}

std::string
RangePattern::as_string () const
{
  if (has_ellipsis_syntax)
    {
      return lower->as_string () + "..." + upper->as_string ();
    }
  else
    {
      return lower->as_string () + "..=" + upper->as_string ();
    }
}

std::string
RangePatternBoundLiteral::as_string () const
{
  std::string str;

  if (has_minus)
    {
      str += "-";
    }

  str += literal.as_string ();

  return str;
}

std::string
SlicePattern::as_string () const
{
  std::string str ("SlicePattern: ");

  for (const auto &pattern : items)
    {
      str += "\n " + pattern->as_string ();
    }

  return str;
}

std::string
TuplePatternItemsMultiple::as_string () const
{
  std::string str;

  for (const auto &pattern : patterns)
    {
      str += "\n " + pattern->as_string ();
    }

  return str;
}

std::string
TuplePatternItemsRanged::as_string () const
{
  std::string str;

  str += "\n Lower patterns: ";
  if (lower_patterns.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &lower : lower_patterns)
	{
	  str += "\n  " + lower->as_string ();
	}
    }

  str += "\n Upper patterns: ";
  if (upper_patterns.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &upper : upper_patterns)
	{
	  str += "\n  " + upper->as_string ();
	}
    }

  return str;
}

std::string
TuplePattern::as_string () const
{
  return "TuplePattern: " + items->as_string ();
}

std::string
StructPatternField::as_string () const
{
  // outer attributes
  std::string str ("Outer attributes: ");
  if (outer_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "outer attribute" syntax -
       * just the body */
      for (const auto &attr : outer_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

  return str;
}

std::string
StructPatternFieldIdent::as_string () const
{
  std::string str = StructPatternField::as_string ();

  str += "\n";

  if (has_ref)
    {
      str += "ref ";
    }

  if (is_mut ())
    {
      str += "mut ";
    }

  str += ident;

  return str;
}

std::string
StructPatternFieldTuplePat::as_string () const
{
  std::string str = StructPatternField::as_string ();

  str += "\n";

  str += std::to_string (index) + " : " + tuple_pattern->as_string ();

  return str;
}

std::string
StructPatternFieldIdentPat::as_string () const
{
  std::string str = StructPatternField::as_string ();

  str += "\n";

  str += ident + " : " + ident_pattern->as_string ();

  return str;
}

std::string
StructPatternElements::as_string () const
{
  std::string str ("\n  Fields: ");

  if (!has_struct_pattern_fields ())
    {
      str += "none";
    }
  else
    {
      for (const auto &field : fields)
	{
	  str += "\n   " + field->as_string ();
	}
    }

  return str;
}

std::string
StructPattern::as_string () const
{
  std::string str ("StructPattern: \n Path: ");

  str += path.as_string ();

  str += "\n Struct pattern elems: ";
  if (!has_struct_pattern_elems ())
    {
      str += "none";
    }
  else
    {
      str += elems.as_string ();
    }

  return str;
}

std::string
LiteralPattern::as_string () const
{
  return lit.as_string ();
}

std::string
ReferencePattern::as_string () const
{
  std::string str ("&");

  if (is_mut ())
    {
      str += "mut ";
    }

  str += pattern->as_string ();

  return str;
}

std::string
IdentifierPattern::as_string () const
{
  std::string str;

  if (is_ref)
    {
      str += "ref ";
    }

  if (is_mut ())
    {
      str += "mut ";
    }

  str += variable_ident;

  if (has_pattern_to_bind ())
    {
      str += " @ " + to_bind->as_string ();
    }

  return str;
}

std::string
TupleStructItemsNoRange::as_string () const
{
  std::string str;

  for (const auto &pattern : patterns)
    {
      str += "\n  " + pattern->as_string ();
    }

  return str;
}

std::string
TupleStructItemsRange::as_string () const
{
  std::string str ("\n  Lower patterns: ");

  if (lower_patterns.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &lower : lower_patterns)
	{
	  str += "\n   " + lower->as_string ();
	}
    }

  str += "\n  Upper patterns: ";
  if (upper_patterns.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &upper : upper_patterns)
	{
	  str += "\n   " + upper->as_string ();
	}
    }

  return str;
}

std::string
TupleStructPattern::as_string () const
{
  std::string str ("TupleStructPattern: \n Path: ");

  str += path.as_string ();

  str += "\n Tuple struct items: " + items->as_string ();

  return str;
}

std::string
LetStmt::as_string () const
{
  // outer attributes
  std::string str = "Outer attributes: ";
  if (outer_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "outer attribute" syntax -
       * just the body */
      indent_spaces (enter);
      for (const auto &attr : outer_attrs)
	{
	  str += "\n" + indent_spaces (stay) + attr.as_string ();
	}
      indent_spaces (out);
    }

  str += "\n" + indent_spaces (stay) + "let " + variables_pattern->as_string ();

  if (has_type ())
    {
      str += " : " + type->as_string ();
    }

  if (has_init_expr ())
    {
      str += " = " + init_expr->as_string ();
    }

  return str;
}

// Used to get outer attributes for expressions.
std::string
Expr::as_string () const
{
  // outer attributes
  std::string str = "outer attributes: ";
  if (outer_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "outer attribute" syntax -
       * just the body */
      for (const auto &attr : outer_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

  return str;
}

// hopefully definition here will prevent circular dependency issue
TraitBound *
TypePath::to_trait_bound (bool in_parens) const
{
  // create clone FIXME is this required? or is copy constructor automatically
  // called?
  TypePath copy (*this);
  return new TraitBound (mappings, std::move (copy), copy.get_locus (),
			 in_parens);
}

std::string
InferredType::as_string () const
{
  return "_ (inferred) " + get_mappings ().as_string ();
}

std::string
TypeCastExpr::as_string () const
{
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
	{
	  str += "\n  " + bound->as_string ();
	}
    }

  return str;
}

std::string
ReferenceType::as_string () const
{
  std::string str ("&");

  if (has_lifetime ())
    {
      str += lifetime.as_string () + " ";
    }

  if (is_mut ())
    {
      str += "mut ";
    }

  str += type->as_string ();

  return str;
}

std::string
RawPointerType::as_string () const
{
  return std::string ("*") + (is_mut () ? "mut " : "const ")
	 + type->as_string ();
}

std::string
TraitObjectType::as_string () const
{
  std::string str ("TraitObjectType: \n Has dyn dispatch: ");

  if (has_dyn)
    {
      str += "true";
    }
  else
    {
      str += "false";
    }

  str += "\n TypeParamBounds: ";
  if (type_param_bounds.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &bound : type_param_bounds)
	{
	  str += "\n  " + bound->as_string ();
	}
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
	{
	  str += "\n  " + for_lifetime.as_string ();
	}
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
	{
	  str += "\n  " + param.as_string ();
	}
    }

  str += "\n Is variadic: ";
  if (is_variadic)
    {
      str += "true";
    }
  else
    {
      str += "false";
    }

  str += "\n Return type: ";
  if (!has_return_type ())
    {
      str += "none (void)";
    }
  else
    {
      str += return_type->as_string ();
    }

  return str;
}

std::string
ImplTraitTypeOneBound::as_string () const
{
  std::string str ("ImplTraitTypeOneBound: \n TraitBound: ");

  return str + trait_bound.as_string ();
}

std::string
TypePathSegmentGeneric::as_string () const
{
  return TypePathSegment::as_string () + "<" + generic_args.as_string () + ">";
}

std::string
TypePathFunction::as_string () const
{
  std::string str ("(");

  if (has_inputs ())
    {
      auto i = inputs.begin ();
      auto e = inputs.end ();

      for (; i != e; i++)
	{
	  str += (*i)->as_string ();
	  if (e != i + 1)
	    str += ", ";
	}
    }

  str += ")";

  if (has_return_type ())
    {
      str += " -> " + return_type->as_string ();
    }

  return str;
}

std::string
TypePathSegmentFunction::as_string () const
{
  return TypePathSegment::as_string () + function_path.as_string ();
}

std::string
ArrayType::as_string () const
{
  return "[" + elem_type->as_string () + "; " + size->as_string () + "]";
}

std::string
SliceType::as_string () const
{
  return "[" + elem_type->as_string () + "]";
}

std::string
TupleType::as_string () const
{
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
  std::string str = ExprWithoutBlock::as_string ();
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
  std::string str ("StructExprStruct (or subclass): ");

  str += "\n Path: " + struct_name.as_string ();

  // inner attributes
  str += "\n inner attributes: ";
  if (inner_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "inner attribute" syntax -
       * just the body */
      for (const auto &attr : inner_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

  return str;
}

std::string
StructBase::as_string () const
{
  if (base_struct != nullptr)
    {
      return base_struct->as_string ();
    }
  else
    {
      return "ERROR_MARK_STRING - invalid struct base had as string applied";
    }
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
  return field_name + " : " + StructExprFieldWithVal::as_string ();
}

std::string
StructExprFieldIndexValue::as_string () const
{
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
	{
	  str += "\n  " + field->as_string ();
	}
    }

  str += "\n Struct base: ";
  if (!has_struct_base ())
    {
      str += "none";
    }
  else
    {
      str += struct_base->as_string ();
    }

  return str;
}

std::string
EnumItem::as_string () const
{
  std::string str = Item::as_string ();
  str += variant_name;
  str += " ";
  switch (get_enum_item_kind ())
    {
    case Named:
      str += "[Named variant]";
      break;
    case Tuple:
      str += "[Tuple variant]";
      break;
    case Struct:
      str += "[Struct variant]";
      break;
    case Discriminant:
      str += "[Discriminant variant]";
      break;
    }

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
  // outer attributes
  std::string str = "outer attributes: ";
  if (outer_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "outer attribute" syntax -
       * just the body */
      for (const auto &attr : outer_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

  if (has_visibility ())
    {
      str += "\n" + visibility.as_string ();
    }

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
  // outer attributes
  std::string str = "outer attributes: ";
  if (outer_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "outer attribute" syntax -
       * just the body */
      for (const auto &attr : outer_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

  if (has_visibility ())
    {
      str += "\n" + visibility.as_string ();
    }

  str += " " + field_name + " : " + field_type->as_string ();

  return str;
}

std::string
EnumItemDiscriminant::as_string () const
{
  std::string str = EnumItem::as_string ();

  // add equal and expression
  str += " = " + expression->as_string ();

  return str;
}

std::string
ExternalItem::as_string () const
{
  // outer attributes
  std::string str = "outer attributes: ";
  if (outer_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "outer attribute" syntax -
       * just the body */
      for (const auto &attr : outer_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

  // start visibility on new line and with a space
  str += "\n" + visibility.as_string () + " ";

  return str;
}

std::string
ExternalStaticItem::as_string () const
{
  std::string str = ExternalItem::as_string ();

  str += "static ";

  if (is_mut ())
    {
      str += "mut ";
    }

  // add name
  str += get_item_name ();

  // add type on new line
  str += "\n Type: " + item_type->as_string ();

  return str;
}

std::string
ExternalFunctionItem::as_string () const
{
  std::string str = ExternalItem::as_string ();

  str += "fn ";

  // add name
  str += get_item_name ();

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
		"generic param in external function item.");
	      return "nullptr_POINTER_MARK";
	    }

	  str += "\n  " + param->as_string ();
	}
    }

  // function params
  str += "\n Function params: ";
  if (function_params.empty ())
    {
      str += "none";
    }
  else
    {
      for (const auto &param : function_params)
	{
	  str += "\n  " + param.as_string ();
	}
      if (has_variadics)
	{
	  str += "\n  .. (variadic)";
	}
    }

  // add type on new line)
  str += "\n (return) Type: "
	 + (has_return_type () ? return_type->as_string () : "()");

  // where clause
  str += "\n Where clause: ";
  if (has_where_clause ())
    {
      str += where_clause.as_string ();
    }
  else
    {
      str += "none";
    }

  return str;
}

std::string
NamedFunctionParam::as_string () const
{
  std::string str = name;

  str += "\n Type: " + param_type->as_string ();

  return str;
}

/*std::string TraitItem::as_string() const {
    // outer attributes
    std::string str = "outer attributes: ";
    if (outer_attrs.empty()) {
	str += "none";
    } else {
	// note that this does not print them with "outer attribute" syntax -
just the body for (const auto& attr : outer_attrs) { str += "\n  " +
attr.as_string();
	}
    }

    return str;
}*/

std::string
TraitItemFunc::as_string () const
{
  std::string str = "outer attributes: ";
  if (outer_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "outer attribute" syntax -
       * just the body */
      for (const auto &attr : outer_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

  str += "\n" + decl.as_string ();

  str += "\n Definition (block expr): ";
  if (has_definition ())
    {
      str += block_expr->as_string ();
    }
  else
    {
      str += "none";
    }

  return str;
}

std::string
TraitFunctionDecl::as_string () const
{
  std::string str = qualifiers.as_string () + "fn " + function_name;

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
		"generic param in trait function decl.");
	      return "nullptr_POINTER_MARK";
	    }

	  str += "\n  " + param->as_string ();
	}
    }

  str += "\n Function params: ";
  if (is_method ())
    {
      str += self.as_string () + (has_params () ? ", " : "");
    }

  if (has_params ())
    {
      for (const auto &param : function_params)
	{
	  str += "\n  " + param.as_string ();
	}
    }
  else if (!is_method ())
    {
      str += "none";
    }

  str += "\n Return type: ";
  if (has_return_type ())
    {
      str += return_type->as_string ();
    }
  else
    {
      str += "none (void)";
    }

  str += "\n Where clause: ";
  if (has_where_clause ())
    {
      str += where_clause.as_string ();
    }
  else
    {
      str += "none";
    }

  return str;
}

std::string
TraitItemConst::as_string () const
{
  std::string str = "outer attributes: ";
  if (outer_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "outer attribute" syntax -
       * just the body */
      for (const auto &attr : outer_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

  str += "\nconst " + name + " : " + type->as_string ();

  if (has_expression ())
    {
      str += " = " + expr->as_string ();
    }

  return str;
}

std::string
TraitItemType::as_string () const
{
  std::string str = "outer attributes: ";
  if (outer_attrs.empty ())
    {
      str += "none";
    }
  else
    {
      /* note that this does not print them with "outer attribute" syntax -
       * just the body */
      for (const auto &attr : outer_attrs)
	{
	  str += "\n  " + attr.as_string ();
	}
    }

  str += "\ntype " + name;

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
	      return "nullptr_POINTER_MARK";
	    }

	  str += "\n  " + bound->as_string ();
	}
    }

  return str;
}

std::string
SelfParam::as_string () const
{
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

	  if (is_mut ())
	    {
	      str += "mut ";
	    }

	  str += "self : ";

	  str += type->as_string ();

	  return str;
	}
      else if (has_lifetime ())
	{
	  // ref and lifetime
	  std::string str = "&" + lifetime.as_string () + " ";

	  if (is_mut ())
	    {
	      str += "mut ";
	    }

	  str += "self";

	  return str;
	}
      else if (is_ref ())
	{
	  // ref with no lifetime
	  std::string str = "&";

	  if (is_mut ())
	    {
	      str += " mut ";
	    }

	  str += "self";

	  return str;
	}
      else
	{
	  // no ref, no type
	  std::string str;

	  if (is_mut ())
	    {
	      str += "mut ";
	    }

	  str += "self";

	  return str;
	}
    }
}

std::string
ArrayElemsCopied::as_string () const
{
  return elem_to_copy->as_string () + "; " + num_copies->as_string ();
}

std::string
LifetimeWhereClauseItem::as_string () const
{
  std::string str ("Lifetime: ");

  str += lifetime.as_string ();

  str += "\nLifetime bounds: ";

  for (const auto &bound : lifetime_bounds)
    {
      str += "\n " + bound.as_string ();
    }

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
	{
	  str += "\n " + for_lifetime.as_string ();
	}
    }

  str += "\nType: " + bound_type->as_string ();

  str += "\nType param bounds bounds: ";

  for (const auto &bound : type_param_bounds)
    {
      // debug null pointer check
      if (bound == nullptr)
	{
	  return "nullptr_POINTER_MARK - type param bounds";
	}

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
	  return "nullptr_POINTER_MARK";
	}

      str += "\n  " + expr->as_string ();
    }

  return str;
}

std::string
MaybeNamedParam::as_string () const
{
  std::string str;

  switch (param_kind)
    {
    case UNNAMED:
      break;
    case IDENTIFIER:
      str = name + " : ";
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

/* All accept_vis method below */

void
Lifetime::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
LifetimeParam::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
PathInExpression::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}
void
PathInExpression::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
TypePathSegment::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TypePathSegmentGeneric::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TypePathSegmentFunction::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TypePath::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
QualifiedPathInExpression::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}
void
QualifiedPathInExpression::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
QualifiedPathInType::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
LiteralExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
LiteralExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
BorrowExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
BorrowExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
DereferenceExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
DereferenceExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
ErrorPropagationExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ErrorPropagationExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
NegationExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
NegationExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
ArithmeticOrLogicalExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ArithmeticOrLogicalExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
ComparisonExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ComparisonExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
LazyBooleanExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
LazyBooleanExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
TypeCastExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TypeCastExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
AssignmentExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
AssignmentExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
CompoundAssignmentExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
CompoundAssignmentExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
GroupedExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
GroupedExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
ArrayElemsValues::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ArrayElemsCopied::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ArrayExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ArrayIndexExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TupleExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TupleIndexExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprStruct::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprFieldIndexValue::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprStructFields::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprStructBase::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
CallExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
MethodCallExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
FieldAccessExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ClosureExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
BlockExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ContinueExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
BreakExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
RangeFromToExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
RangeFromExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
RangeToExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
RangeFullExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
RangeFromToInclExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
RangeToInclExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ReturnExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
UnsafeBlockExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
LoopExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
WhileLoopExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
WhileLetLoopExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ForLoopExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
IfExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
IfExprConseqElse::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
IfExprConseqIf::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
IfExprConseqIfLet::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
IfLetExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
IfLetExprConseqElse::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
IfLetExprConseqIf::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
IfLetExprConseqIfLet::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
MatchExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
AwaitExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
AsyncBlockExpr::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TypeParam::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
LifetimeWhereClauseItem::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TypeBoundWhereClauseItem::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
Module::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
Module::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
Module::accept_vis (HIRVisItemVisitor &vis)
{
  vis.visit (*this);
}

void
ExternCrate::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
UseTreeGlob::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
UseTreeList::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
UseTreeRebind::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
UseDeclaration::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
Function::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TypeAlias::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
StructStruct::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TupleStruct::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
EnumItem::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
EnumItemTuple::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
EnumItemStruct::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
EnumItemDiscriminant::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
Enum::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
Union::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ConstantItem::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
StaticItem::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TraitItemFunc::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TraitItemConst::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TraitItemType::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
Trait::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ImplBlock::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ExternalStaticItem::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ExternalFunctionItem::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ExternBlock::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
LiteralPattern::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
IdentifierPattern::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
WildcardPattern::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
RangePatternBoundLiteral::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
RangePatternBoundPath::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
RangePatternBoundQualPath::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
RangePattern::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ReferencePattern::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
StructPatternFieldTuplePat::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
StructPatternFieldIdentPat::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
StructPatternFieldIdent::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
StructPattern::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TupleStructItemsNoRange::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TupleStructItemsRange::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TupleStructPattern::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TuplePatternItemsMultiple::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TuplePatternItemsRanged::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TuplePattern::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
SlicePattern::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
EmptyStmt::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
LetStmt::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ExprStmtWithoutBlock::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ExprStmtWithBlock::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TraitBound::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ImplTraitType::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TraitObjectType::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ParenthesisedType::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ImplTraitTypeOneBound::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
TupleType::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
NeverType::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
RawPointerType::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ReferenceType::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
ArrayType::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
SliceType::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
InferredType::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
BareFunctionType::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
NeverType::accept_vis (HIRTypeVisitor &vis)
{
  vis.visit (*this);
}

void
ParenthesisedType::accept_vis (HIRTypeVisitor &vis)
{
  vis.visit (*this);
}

void
EmptyStmt::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
WildcardPattern::accept_vis (HIRPatternVisitor &vis)
{
  vis.visit (*this);
}

void
TraitItemType::accept_vis (HIRTraitItemVisitor &vis)
{
  vis.visit (*this);
}

void
TraitItemConst::accept_vis (HIRTraitItemVisitor &vis)
{
  vis.visit (*this);
}

void
TraitItemFunc::accept_vis (HIRTraitItemVisitor &vis)
{
  vis.visit (*this);
}

void
ExternalFunctionItem::accept_vis (HIRExternalItemVisitor &vis)
{
  vis.visit (*this);
}

void
ExternalStaticItem::accept_vis (HIRExternalItemVisitor &vis)
{
  vis.visit (*this);
}

void
EnumItemDiscriminant::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
EnumItemStruct::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
EnumItemTuple::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
EnumItem::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprStructFields::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprFieldIndexValue::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprFieldIdentifierValue::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprFieldIdentifierValue::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprFieldIdentifier::accept_vis (HIRFullVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprFieldIdentifier::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
StructExprStruct::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
TupleType::accept_vis (HIRTypeVisitor &vis)
{
  vis.visit (*this);
}

void
SliceType::accept_vis (HIRTypeVisitor &vis)
{
  vis.visit (*this);
}

void
ArrayType::accept_vis (HIRTypeVisitor &vis)
{
  vis.visit (*this);
}

void
ImplTraitTypeOneBound::accept_vis (HIRTypeVisitor &vis)
{
  vis.visit (*this);
}

void
BareFunctionType::accept_vis (HIRTypeVisitor &vis)
{
  vis.visit (*this);
}

void
TraitObjectType::accept_vis (HIRTypeVisitor &vis)
{
  vis.visit (*this);
}

void
RawPointerType::accept_vis (HIRTypeVisitor &vis)
{
  vis.visit (*this);
}

void
ReferenceType::accept_vis (HIRTypeVisitor &vis)
{
  vis.visit (*this);
}

void
ImplTraitType::accept_vis (HIRTypeVisitor &vis)
{
  vis.visit (*this);
}

void
InferredType::accept_vis (HIRTypeVisitor &vis)
{
  vis.visit (*this);
}

void
LetStmt::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
TupleStructPattern::accept_vis (HIRPatternVisitor &vis)
{
  vis.visit (*this);
}

void
IdentifierPattern::accept_vis (HIRPatternVisitor &vis)
{
  vis.visit (*this);
}

void
ReferencePattern::accept_vis (HIRPatternVisitor &vis)
{
  vis.visit (*this);
}

void
LiteralPattern::accept_vis (HIRPatternVisitor &vis)
{
  vis.visit (*this);
}

void
StructPattern::accept_vis (HIRPatternVisitor &vis)
{
  vis.visit (*this);
}

void
TuplePattern::accept_vis (HIRPatternVisitor &vis)
{
  vis.visit (*this);
}

void
SlicePattern::accept_vis (HIRPatternVisitor &vis)
{
  vis.visit (*this);
}

void
RangePattern::accept_vis (HIRPatternVisitor &vis)
{
  vis.visit (*this);
}

void
ForLoopExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
TypePath::accept_vis (HIRTypeVisitor &vis)
{
  vis.visit (*this);
}

void
QualifiedPathInType::accept_vis (HIRTypeVisitor &vis)
{
  vis.visit (*this);
}

void
ExprStmtWithoutBlock::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
TupleExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
MatchExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
BreakExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
AwaitExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
ArrayExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
LoopExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
WhileLetLoopExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
WhileLoopExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
CallExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
RangeFromToInclExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
IfLetExprConseqIfLet::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
IfLetExprConseqIf::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
IfLetExprConseqElse::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
IfLetExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
IfExprConseqIfLet::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
IfExprConseqIf::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
IfExprConseqElse::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
IfExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
ClosureExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
UnsafeBlockExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
RangeToInclExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
RangeFromToExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
FieldAccessExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
TupleIndexExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
MethodCallExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
AsyncBlockExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
ArrayIndexExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
RangeFullExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
RangeFromExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
ContinueExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
RangeToExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
ReturnExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
QualifiedPathInExpression::accept_vis (HIRPatternVisitor &vis)
{
  vis.visit (*this);
}

void
ExprStmtWithBlock::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
PathInExpression::accept_vis (HIRPatternVisitor &vis)
{
  vis.visit (*this);
}

void
ExternBlock::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
ExternBlock::accept_vis (HIRVisItemVisitor &vis)
{
  vis.visit (*this);
}

void
TypeAlias::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
TypeAlias::accept_vis (HIRVisItemVisitor &vis)
{
  vis.visit (*this);
}

void
TypeAlias::accept_vis (HIRImplVisitor &vis)
{
  vis.visit (*this);
}

void
BlockExpr::accept_vis (HIRExpressionVisitor &vis)
{
  vis.visit (*this);
}

void
Function::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
Function::accept_vis (HIRVisItemVisitor &vis)
{
  vis.visit (*this);
}

void
Function::accept_vis (HIRImplVisitor &vis)
{
  vis.visit (*this);
}

void
Union::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
Union::accept_vis (HIRVisItemVisitor &vis)
{
  vis.visit (*this);
}

void
Trait::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
Trait::accept_vis (HIRVisItemVisitor &vis)
{
  vis.visit (*this);
}

void
Enum::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
Enum::accept_vis (HIRVisItemVisitor &vis)
{
  vis.visit (*this);
}

void
UseDeclaration::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
UseDeclaration::accept_vis (HIRVisItemVisitor &vis)
{
  vis.visit (*this);
}

void
StructStruct::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
StructStruct::accept_vis (HIRVisItemVisitor &vis)
{
  vis.visit (*this);
}

void
ImplBlock::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
ImplBlock::accept_vis (HIRVisItemVisitor &vis)
{
  vis.visit (*this);
}

void
ConstantItem::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
ConstantItem::accept_vis (HIRVisItemVisitor &vis)
{
  vis.visit (*this);
}

void
ConstantItem::accept_vis (HIRImplVisitor &vis)
{
  vis.visit (*this);
}

void
TupleStruct::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
TupleStruct::accept_vis (HIRVisItemVisitor &vis)
{
  vis.visit (*this);
}

void
ExternCrate::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
ExternCrate::accept_vis (HIRVisItemVisitor &vis)
{
  vis.visit (*this);
}

void
StaticItem::accept_vis (HIRStmtVisitor &vis)
{
  vis.visit (*this);
}

void
StaticItem::accept_vis (HIRVisItemVisitor &vis)
{
  vis.visit (*this);
}

std::string
ConstGenericParam::as_string () const
{
  auto result = "ConstGenericParam: " + name + " : " + type->as_string ();

  if (default_expression)
    result += " = " + default_expression->as_string ();

  return result;
}

void
ConstGenericParam::accept_vis (HIRFullVisitor &)
{}

} // namespace HIR
} // namespace Rust
