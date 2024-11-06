/* Some code common to C and ObjC front ends.
   Copyright (C) 2001-2024 Free Software Foundation, Inc.

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "c-tree.h"
#include "intl.h"
#include "c-family/c-pretty-print.h"
#include "tree-pretty-print.h"
#include "tree-pretty-print-markup.h"
#include "gimple-pretty-print.h"
#include "langhooks.h"
#include "c-objc-common.h"
#include "c-family/c-type-mismatch.h"
#include "stringpool.h"
#include "attribs.h"
#include "dwarf2.h"
#include "make-unique.h"

static bool c_tree_printer (pretty_printer *, text_info *, const char *,
			    int, bool, bool, bool, bool *, pp_token_list &);

/* Info for C language features which can be queried through
   __has_{feature,extension}.  */

struct c_feature_info
{
  const char *ident;
  const int *enable_flag;
};

static const c_feature_info c_feature_table[] =
{
  { "c_alignas", &flag_isoc11 },
  { "c_alignof", &flag_isoc11 },
  { "c_atomic", &flag_isoc11 },
  { "c_generic_selections", &flag_isoc11 },
  { "c_static_assert", &flag_isoc11 },
  { "c_thread_local", &flag_isoc11 },
  { "cxx_binary_literals", &flag_isoc23 }
};

/* Register features specific to the C language.  */

void
c_register_features ()
{
  for (unsigned i = 0; i < ARRAY_SIZE (c_feature_table); i++)
    {
      const c_feature_info *info = c_feature_table + i;
      const bool feat_p = !info->enable_flag || *info->enable_flag;
      c_common_register_feature (info->ident, feat_p);
    }
}

bool
c_missing_noreturn_ok_p (tree decl)
{
  /* A missing noreturn is ok for the `main' function.  */
  if (!MAIN_NAME_P (DECL_ASSEMBLER_NAME (decl)))
    return false;

  return flag_hosted
    || TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (decl))) == integer_type_node;
}

/* Called from check_global_declaration.  */

bool
c_warn_unused_global_decl (const_tree decl)
{
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (decl))
    return false;
  if (DECL_IN_SYSTEM_HEADER (decl))
    return false;

  return true;
}

/* Initialization common to C and Objective-C front ends.  */
bool
c_objc_common_init (void)
{
  c_init_decl_processing ();

  return c_common_init ();
}

/* Decide whether it's worth saying that TYPE is also known as some other
   type.  Return the other type if so, otherwise return TYPE.  */

static tree
get_aka_type (tree type)
{
  if (type == error_mark_node)
    return type;

  tree result;
  if (typedef_variant_p (type))
    {
      /* Saying that "foo" is also known as "struct foo" or
	 "struct <anonymous>" is unlikely to be useful, since users of
	 structure-like types would already know that they're structures.
	 The same applies to unions and enums; in general, printing the
	 tag is only useful if it has a different name.  */
      tree orig_type = DECL_ORIGINAL_TYPE (TYPE_NAME (type));
      tree_code code = TREE_CODE (orig_type);
      tree orig_id = TYPE_IDENTIFIER (orig_type);
      if ((code == RECORD_TYPE || code == UNION_TYPE || code == ENUMERAL_TYPE)
	  && (!orig_id || TYPE_IDENTIFIER (type) == orig_id))
	return type;

      if (!user_facing_original_type_p (type))
	return type;

      result = get_aka_type (orig_type);
    }
  else if (TREE_CODE (type) == ENUMERAL_TYPE)
    return type;
  else
    {
      tree canonical = TYPE_CANONICAL (type);
      if (canonical && TREE_CODE (type) != TREE_CODE (canonical))
	return canonical;

      /* Recursive calls might choose a middle ground between TYPE
	 (which has no typedefs stripped) and CANONICAL (which has
	 all typedefs stripped).  So try to reuse TYPE or CANONICAL if
	 convenient, but be prepared to create a new type if necessary.  */
      switch (TREE_CODE (type))
	{
	case POINTER_TYPE:
	case REFERENCE_TYPE:
	  {
	    tree target_type = get_aka_type (TREE_TYPE (type));

	    if (target_type == TREE_TYPE (type))
	      return type;

	    if (canonical && target_type == TREE_TYPE (canonical))
	      return canonical;

	    result = (TREE_CODE (type) == POINTER_TYPE
		      ? build_pointer_type (target_type)
		      : build_reference_type (target_type));
	    break;
	  }

	case ARRAY_TYPE:
	  {
	    tree element_type = get_aka_type (TREE_TYPE (type));
	    tree index_type = (TYPE_DOMAIN (type)
			       ? get_aka_type (TYPE_DOMAIN (type))
			       : NULL_TREE);

	    if (element_type == TREE_TYPE (type)
		&& index_type == TYPE_DOMAIN (type))
	      return type;

	    if (canonical
		&& element_type == TREE_TYPE (canonical)
		&& index_type == TYPE_DOMAIN (canonical))
	      return canonical;

	    result = build_array_type (element_type, index_type,
				       TYPE_TYPELESS_STORAGE (type));
	    break;
	  }

	case FUNCTION_TYPE:
	  {
	    tree return_type = get_aka_type (TREE_TYPE (type));

	    tree args = TYPE_ARG_TYPES (type);
	    if (args == error_mark_node)
	      return type;

	    auto_vec<tree, 32> arg_types;
	    bool type_ok_p = true;
	    while (args && args != void_list_node)
	      {
		tree arg_type = get_aka_type (TREE_VALUE (args));
		arg_types.safe_push (arg_type);
		type_ok_p &= (arg_type == TREE_VALUE (args));
		args = TREE_CHAIN (args);
	      }

	    if (type_ok_p && return_type == TREE_TYPE (type))
	      return type;

	    unsigned int i;
	    tree arg_type;
	    FOR_EACH_VEC_ELT_REVERSE (arg_types, i, arg_type)
	      args = tree_cons (NULL_TREE, arg_type, args);
	    result = build_function_type (return_type, args);
	    break;
	  }

	default:
	  return canonical ? canonical : type;
	}
    }
  return build_type_attribute_qual_variant (result, TYPE_ATTRIBUTES (type),
					    TYPE_QUALS (type));
}

/* Print T to CPP.  */

static void
print_type (c_pretty_printer *cpp, tree t, bool *quoted,
	    const char *highlight_color = nullptr)
{
  if (t == error_mark_node)
    {
      pp_string (cpp, _("{erroneous}"));
      return;
    }

  if (!pp_show_highlight_colors (cpp))
    highlight_color = nullptr;

  gcc_assert (TYPE_P (t));
  struct obstack *ob = pp_buffer (cpp)->m_obstack;
  char *p = (char *) obstack_base (ob);
  /* Remember the end of the initial dump.  */
  int len = obstack_object_size (ob);

  tree name = TYPE_NAME (t);
  if (name && TREE_CODE (name) == TYPE_DECL && DECL_NAME (name))
    pp_identifier (cpp, lang_hooks.decl_printable_name (name, 2));
  else
    cpp->type_id (t);

  /* If we're printing a type that involves typedefs, also print the
     stripped version.  But sometimes the stripped version looks
     exactly the same, so we don't want it after all.  To avoid
     printing it in that case, we play ugly obstack games.  */
  tree aka_type = get_aka_type (t);
  if (aka_type != t)
    {
      const bool show_color = pp_show_color (cpp);
      c_pretty_printer cpp2;
      /* Print the stripped version into a temporary printer.  */
      cpp2.type_id (aka_type);
      struct obstack *ob2 = pp_buffer (&cpp2)->m_obstack;
      /* Get the stripped version from the temporary printer.  */
      const char *aka = (char *) obstack_base (ob2);
      int aka_len = obstack_object_size (ob2);
      int type1_len = obstack_object_size (ob) - len;

      /* If they are identical, bail out.  */
      if (aka_len == type1_len && memcmp (p + len, aka, aka_len) == 0)
	return;

      /* They're not, print the stripped version now.  */
      if (*quoted)
	pp_end_quote (cpp, show_color);
      pp_c_whitespace (cpp);
      pp_left_brace (cpp);
      pp_c_ws_string (cpp, _("aka"));
      pp_c_whitespace (cpp);
      pp_string (cpp, colorize_stop (show_color));
      if (*quoted)
	pp_begin_quote (cpp, show_color);
      if (highlight_color)
	pp_string (cpp, colorize_start (show_color, highlight_color));
      cpp->type_id (aka_type);
      if (*quoted)
	pp_end_quote (cpp, show_color);
      pp_right_brace (cpp);
      /* No further closing quotes are needed.  */
      *quoted = false;
    }
}

/* Implementation of pp_markup::element_quoted_type::print_type
   for C/ObjC.  */

void
pp_markup::element_quoted_type::print_type (pp_markup::context &ctxt)
{
  auto pp = ctxt.m_pp.clone ();
  c_pretty_printer *cpp = (c_pretty_printer *)pp.get ();
  cpp->set_padding (pp_none);
  ::print_type (cpp, m_type, &ctxt.m_quoted, m_highlight_color);
  pp_string (&ctxt.m_pp, pp_formatted_text (cpp));
}

/* Called during diagnostic message formatting process to print a
   source-level entity onto BUFFER.  The meaning of the format specifiers
   is as follows:
   %D: a general decl,
   %E: an identifier or expression,
   %F: a function declaration,
   %T: a type.
   %V: a list of type qualifiers from a tree.
   %v: an explicit list of type qualifiers
   %#v: an explicit list of type qualifiers of a function type.

   Please notice when called, the `%' part was already skipped by the
   diagnostic machinery.  */
static bool
c_tree_printer (pretty_printer *pp, text_info *text, const char *spec,
		int precision, bool wide, bool set_locus, bool hash,
		bool *quoted, pp_token_list &)
{
  tree t = NULL_TREE;
  // FIXME: the next cast should be a dynamic_cast, when it is permitted.
  c_pretty_printer *cpp = (c_pretty_printer *) pp;
  pp->set_padding (pp_none);

  if (precision != 0 || wide)
    return false;

  if (*spec != 'v')
    {
      t = va_arg (*text->m_args_ptr, tree);
      if (set_locus)
	text->set_location (0, DECL_SOURCE_LOCATION (t),
			    SHOW_RANGE_WITH_CARET);
    }

  switch (*spec)
    {
    case 'D':
      if (VAR_P (t) && DECL_HAS_DEBUG_EXPR_P (t))
	{
	  t = DECL_DEBUG_EXPR (t);
	  if (!DECL_P (t))
	    {
	      cpp->expression (t);
	      return true;
	    }
	}
      /* FALLTHRU */

    case 'F':
      if (DECL_NAME (t))
	{
	  pp_identifier (cpp, lang_hooks.decl_printable_name (t, 2));
	  return true;
	}
      break;

    case 'T':
      print_type (cpp, t, quoted);
      return true;

    case 'E':
      if (TREE_CODE (t) == IDENTIFIER_NODE)
	pp_identifier (cpp, IDENTIFIER_POINTER (t));
      else
	cpp->expression (t);
      return true;

    case 'V':
      pp_c_type_qualifier_list (cpp, t);
      return true;

    case 'v':
      pp_c_cv_qualifiers (cpp, va_arg (*text->m_args_ptr, int), hash);
      return true;

    default:
      return false;
    }

  pp_string (cpp, _("({anonymous})"));
  return true;
}

/* C-specific implementation of range_label::get_text () vfunc for
   range_label_for_type_mismatch.  */

label_text
range_label_for_type_mismatch::get_text (unsigned /*range_idx*/) const
{
  if (m_labelled_type == NULL_TREE)
    return label_text::borrow (NULL);

  c_pretty_printer cpp;
  bool quoted = false;
  print_type (&cpp, m_labelled_type, &quoted);
  return label_text::take (xstrdup (pp_formatted_text (&cpp)));
}


/* In C and ObjC, all decls have "C" linkage.  */
bool
has_c_linkage (const_tree decl ATTRIBUTE_UNUSED)
{
  return true;
}

void
c_initialize_diagnostics (diagnostic_context *context)
{
  context->set_pretty_printer (::make_unique<c_pretty_printer> ());
  c_common_diagnostics_set_defaults (context);
  context->set_format_decoder (&c_tree_printer);
}

int
c_types_compatible_p (tree x, tree y)
{
  return comptypes (TYPE_MAIN_VARIANT (x), TYPE_MAIN_VARIANT (y));
}

/* Determine if the type is a variably modified type for the backend.  */

bool
c_var_mod_p (tree x, tree fn ATTRIBUTE_UNUSED)
{
  return C_TYPE_VARIABLY_MODIFIED (x);
}

/* Special routine to get the alias set of T for C.  */

alias_set_type
c_get_alias_set (tree t)
{
  return c_common_get_alias_set (t);
}

/* In C there are no invisible parameters like in C++ (this, in-charge, VTT,
   etc.).  */

int
maybe_adjust_arg_pos_for_attribute (const_tree)
{
  return 0;
}

/* In C, no expression is dependent.  */

bool
instantiation_dependent_expression_p (tree)
{
  return false;
}

/* Return -1 if dwarf ATTR shouldn't be added for TYPE, or the attribute
   value otherwise.  */
int
c_type_dwarf_attribute (const_tree type, int attr)
{
  if (type == NULL_TREE)
    return -1;

  switch (attr)
    {
    case DW_AT_export_symbols:
      if (RECORD_OR_UNION_TYPE_P (type) && TYPE_NAME (type) == NULL_TREE)
	return 1;
      break;

    default:
      break;
    }

  return -1;
}
