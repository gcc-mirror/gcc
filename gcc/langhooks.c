/* Default language-specific hooks.
   Copyright (C) 2001-2017 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva  <aoliva@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "timevar.h"
#include "stringpool.h"
#include "diagnostic.h"
#include "intl.h"
#include "toplev.h"
#include "attribs.h"
#include "gimplify.h"
#include "langhooks.h"
#include "tree-diagnostic.h"
#include "output.h"
#include "timevar.h"

/* Do nothing; in many cases the default hook.  */

void
lhd_do_nothing (void)
{
}

/* Do nothing (tree).  */

void
lhd_do_nothing_t (tree ARG_UNUSED (t))
{
}

/* Pass through (tree).  */
tree
lhd_pass_through_t (tree t)
{
  return t;
}

/* Do nothing (int, int, int).  Return NULL_TREE.  */

tree
lhd_do_nothing_iii_return_null_tree (int ARG_UNUSED (i),
				     int ARG_UNUSED (j),
				     int ARG_UNUSED (k))
{
  return NULL_TREE;
}

/* Do nothing (function).  */

void
lhd_do_nothing_f (struct function * ARG_UNUSED (f))
{
}

/* Do nothing (return NULL_TREE).  */

tree
lhd_return_null_tree (tree ARG_UNUSED (t))
{
  return NULL_TREE;
}

/* Do nothing (return NULL_TREE).  */

tree
lhd_return_null_const_tree (const_tree ARG_UNUSED (t))
{
  return NULL_TREE;
}

/* The default post options hook.  */

bool
lhd_post_options (const char ** ARG_UNUSED (pfilename))
{
  /* Excess precision other than "fast" requires front-end
     support.  */
  flag_excess_precision_cmdline = EXCESS_PRECISION_FAST;
  return false;
}

/* Called from by print-tree.c.  */

void
lhd_print_tree_nothing (FILE * ARG_UNUSED (file),
			tree ARG_UNUSED (node),
			int ARG_UNUSED (indent))
{
}

/* Called from check_global_declaration.  */

bool
lhd_warn_unused_global_decl (const_tree decl)
{
  /* This is what used to exist in check_global_declaration.  Probably
     not many of these actually apply to non-C languages.  */

  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (decl))
    return false;
  if (VAR_P (decl) && TREE_READONLY (decl))
    return false;
  if (DECL_IN_SYSTEM_HEADER (decl))
    return false;

  return true;
}

/* Set the DECL_ASSEMBLER_NAME for DECL.  */
void
lhd_set_decl_assembler_name (tree decl)
{
  tree id;

  /* set_decl_assembler_name may be called on TYPE_DECL to record ODR
     name for C++ types.  By default types have no ODR names.  */
  if (TREE_CODE (decl) == TYPE_DECL)
    return;

  /* The language-independent code should never use the
     DECL_ASSEMBLER_NAME for lots of DECLs.  Only FUNCTION_DECLs and
     VAR_DECLs for variables with static storage duration need a real
     DECL_ASSEMBLER_NAME.  */
  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL
	      || (VAR_P (decl)
		  && (TREE_STATIC (decl)
		      || DECL_EXTERNAL (decl)
		      || TREE_PUBLIC (decl))));

  /* By default, assume the name to use in assembly code is the same
     as that used in the source language.  (That's correct for C, and
     GCC used to set DECL_ASSEMBLER_NAME to the same value as
     DECL_NAME in build_decl, so this choice provides backwards
     compatibility with existing front-ends.  This assumption is wrapped
     in a target hook, to allow for target-specific modification of the
     identifier.

     Can't use just the variable's own name for a variable whose scope
     is less than the whole compilation.  Concatenate a distinguishing
     number - we use the DECL_UID.  */

  if (TREE_PUBLIC (decl) || DECL_FILE_SCOPE_P (decl))
    id = targetm.mangle_decl_assembler_name (decl, DECL_NAME (decl));
  else
    {
      const char *name = IDENTIFIER_POINTER (DECL_NAME (decl));
      char *label;

      ASM_FORMAT_PRIVATE_NAME (label, name, DECL_UID (decl));
      id = get_identifier (label);
    }
  SET_DECL_ASSEMBLER_NAME (decl, id);

}

/* Type promotion for variable arguments.  */
tree
lhd_type_promotes_to (tree ARG_UNUSED (type))
{
  gcc_unreachable ();
}

/* Registration of machine- or os-specific builtin types.  */
void
lhd_register_builtin_type (tree ARG_UNUSED (type),
			   const char * ARG_UNUSED (name))
{
}

/* Invalid use of an incomplete type.  */
void
lhd_incomplete_type_error (location_t ARG_UNUSED (loc),
			   const_tree ARG_UNUSED (value), const_tree type)
{
  gcc_assert (TREE_CODE (type) == ERROR_MARK);
  return;
}

/* Provide a default routine for alias sets that always returns -1.  This
   is used by languages that don't need to do anything special.  */

alias_set_type
lhd_get_alias_set (tree ARG_UNUSED (t))
{
  return -1;
}

/* This is the default decl_printable_name function.  */

const char *
lhd_decl_printable_name (tree decl, int ARG_UNUSED (verbosity))
{
  gcc_assert (decl && DECL_NAME (decl));
  return IDENTIFIER_POINTER (DECL_NAME (decl));
}

/* This is the default dwarf_name function.  */

const char *
lhd_dwarf_name (tree t, int verbosity)
{
  gcc_assert (DECL_P (t));

  return lang_hooks.decl_printable_name (t, verbosity);
}

/* This compares two types for equivalence ("compatible" in C-based languages).
   This routine should only return 1 if it is sure.  It should not be used
   in contexts where erroneously returning 0 causes problems.  */

int
lhd_types_compatible_p (tree x, tree y)
{
  return TYPE_MAIN_VARIANT (x) == TYPE_MAIN_VARIANT (y);
}

/* lang_hooks.tree_dump.dump_tree:  Dump language-specific parts of tree
   nodes.  Returns nonzero if it does not want the usual dumping of the
   second argument.  */

bool
lhd_tree_dump_dump_tree (void *di ATTRIBUTE_UNUSED, tree t ATTRIBUTE_UNUSED)
{
  return false;
}

/* lang_hooks.tree_dump.type_qual:  Determine type qualifiers in a
   language-specific way.  */

int
lhd_tree_dump_type_quals (const_tree t)
{
  return TYPE_QUALS (t);
}

/* lang_hooks.gimplify_expr re-writes *EXPR_P into GIMPLE form.  */

int
lhd_gimplify_expr (tree *expr_p ATTRIBUTE_UNUSED,
		   gimple_seq *pre_p ATTRIBUTE_UNUSED,
		   gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  return GS_UNHANDLED;
}

/* lang_hooks.tree_size: Determine the size of a tree with code C,
   which is a language-specific tree code in category tcc_constant,
   tcc_exceptional or tcc_type.  The default expects never to be called.  */
size_t
lhd_tree_size (enum tree_code c ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* Return true if decl, which is a function decl, may be called by a
   sibcall.  */

bool
lhd_decl_ok_for_sibcall (const_tree decl ATTRIBUTE_UNUSED)
{
  return true;
}

/* Generic global declaration processing.  This is meant to be called
   by the front-ends at the end of parsing.  C/C++ do their own thing,
   but other front-ends may call this.  */

void
global_decl_processing (void)
{
  tree globals, decl, *vec;
  int len, i;

  timevar_stop (TV_PHASE_PARSING);
  timevar_start (TV_PHASE_DEFERRED);
  /* Really define vars that have had only a tentative definition.
     Really output inline functions that must actually be callable
     and have not been output so far.  */

  globals = lang_hooks.decls.getdecls ();
  len = list_length (globals);
  vec = XNEWVEC (tree, len);

  /* Process the decls in reverse order--earliest first.
     Put them into VEC from back to front, then take out from front.  */

  for (i = 0, decl = globals; i < len; i++, decl = DECL_CHAIN (decl))
    vec[len - i - 1] = decl;

  wrapup_global_declarations (vec, len);
  timevar_stop (TV_PHASE_DEFERRED);

  timevar_start (TV_PHASE_PARSING);
  free (vec);
}

/* Called to perform language-specific initialization of CTX.  */
void
lhd_initialize_diagnostics (diagnostic_context *ctx ATTRIBUTE_UNUSED)
{
}

/* Called to register dumps.  */
void
lhd_register_dumps (gcc::dump_manager *)
{
}

/* Called to perform language-specific options initialization.  */
void
lhd_init_options (unsigned int decoded_options_count ATTRIBUTE_UNUSED,
		  struct cl_decoded_option *decoded_options ATTRIBUTE_UNUSED)
{
}

/* By default, always complain about options for the wrong language.  */
bool
lhd_complain_wrong_lang_p (const struct cl_option *option ATTRIBUTE_UNUSED)
{
  return true;
}

/* By default, no language-specific options are valid.  */
bool
lhd_handle_option (size_t code ATTRIBUTE_UNUSED,
		   const char *arg ATTRIBUTE_UNUSED,
		   int value ATTRIBUTE_UNUSED, int kind ATTRIBUTE_UNUSED,
		   location_t loc ATTRIBUTE_UNUSED,
		   const struct cl_option_handlers *handlers ATTRIBUTE_UNUSED)
{
  return false;
}

/* The default function to print out name of current function that caused
   an error.  */
void
lhd_print_error_function (diagnostic_context *context, const char *file,
			  diagnostic_info *diagnostic)
{
  if (diagnostic_last_function_changed (context, diagnostic))
    {
      const char *old_prefix = context->printer->prefix;
      tree abstract_origin = diagnostic_abstract_origin (diagnostic);
      char *new_prefix = (file && abstract_origin == NULL)
			 ? file_name_as_prefix (context, file) : NULL;

      pp_set_prefix (context->printer, new_prefix);

      if (current_function_decl == NULL)
	pp_printf (context->printer, _("At top level:"));
      else
	{
	  tree fndecl, ao;

	  if (abstract_origin)
	    {
	      ao = BLOCK_ABSTRACT_ORIGIN (abstract_origin);
	      while (TREE_CODE (ao) == BLOCK
		     && BLOCK_ABSTRACT_ORIGIN (ao)
		     && BLOCK_ABSTRACT_ORIGIN (ao) != ao)
		ao = BLOCK_ABSTRACT_ORIGIN (ao);
	      gcc_assert (TREE_CODE (ao) == FUNCTION_DECL);
	      fndecl = ao;
	    }
	  else
	    fndecl = current_function_decl;

	  if (TREE_CODE (TREE_TYPE (fndecl)) == METHOD_TYPE)
	    pp_printf
	      (context->printer, _("In member function %qs"),
	       identifier_to_locale (lang_hooks.decl_printable_name (fndecl, 2)));
	  else
	    pp_printf
	      (context->printer, _("In function %qs"),
	       identifier_to_locale (lang_hooks.decl_printable_name (fndecl, 2)));

	  while (abstract_origin)
	    {
	      location_t *locus;
	      tree block = abstract_origin;

	      locus = &BLOCK_SOURCE_LOCATION (block);
	      fndecl = NULL;
	      block = BLOCK_SUPERCONTEXT (block);
	      while (block && TREE_CODE (block) == BLOCK
		     && BLOCK_ABSTRACT_ORIGIN (block))
		{
		  ao = BLOCK_ABSTRACT_ORIGIN (block);

		  while (TREE_CODE (ao) == BLOCK
			 && BLOCK_ABSTRACT_ORIGIN (ao)
			 && BLOCK_ABSTRACT_ORIGIN (ao) != ao)
		    ao = BLOCK_ABSTRACT_ORIGIN (ao);

		  if (TREE_CODE (ao) == FUNCTION_DECL)
		    {
		      fndecl = ao;
		      break;
		    }
		  else if (TREE_CODE (ao) != BLOCK)
		    break;

		  block = BLOCK_SUPERCONTEXT (block);
		}
	      if (fndecl)
		abstract_origin = block;
	      else
		{
		  while (block && TREE_CODE (block) == BLOCK)
		    block = BLOCK_SUPERCONTEXT (block);

		  if (block && TREE_CODE (block) == FUNCTION_DECL)
		    fndecl = block;
		  abstract_origin = NULL;
		}
	      if (fndecl)
		{
		  expanded_location s = expand_location (*locus);
		  pp_comma (context->printer);
		  pp_newline (context->printer);
		  if (s.file != NULL)
		    {
		      if (context->show_column)
			pp_printf (context->printer,
				   _("    inlined from %qs at %r%s:%d:%d%R"),
				   identifier_to_locale (lang_hooks.decl_printable_name (fndecl, 2)),
				   "locus", s.file, s.line, s.column);
		      else
			pp_printf (context->printer,
				   _("    inlined from %qs at %r%s:%d%R"),
				   identifier_to_locale (lang_hooks.decl_printable_name (fndecl, 2)),
				   "locus", s.file, s.line);

		    }
		  else
		    pp_printf (context->printer, _("    inlined from %qs"),
			       identifier_to_locale (lang_hooks.decl_printable_name (fndecl, 2)));
		}
	    }
	  pp_colon (context->printer);
	}

      diagnostic_set_last_function (context, diagnostic);
      pp_newline_and_flush (context->printer);
      context->printer->prefix = old_prefix;
      free ((char*) new_prefix);
    }
}

tree
lhd_make_node (enum tree_code code)
{
  return make_node (code);
}

/* Default implementation of LANG_HOOKS_TYPE_FOR_SIZE.
   Return an integer type with PRECISION bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
lhd_type_for_size (unsigned precision, int unsignedp)
{
  int i;

  if (precision == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (precision == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (precision == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (precision == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (precision == TYPE_PRECISION (long_long_integer_type_node))
    return unsignedp
	   ? long_long_unsigned_type_node
	   : long_long_integer_type_node;

  for (i = 0; i < NUM_INT_N_ENTS; i ++)
    if (int_n_enabled_p[i]
	&& precision == int_n_data[i].bitsize)
      return (unsignedp ? int_n_trees[i].unsigned_type
	      : int_n_trees[i].signed_type);

  if (precision <= TYPE_PRECISION (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if (precision <= TYPE_PRECISION (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

  if (precision <= TYPE_PRECISION (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

  if (precision <= TYPE_PRECISION (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

  if (precision <= TYPE_PRECISION (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;

  return NULL_TREE;
}

HOST_WIDE_INT
lhd_to_target_charset (HOST_WIDE_INT c)
{
  return c;
}

tree
lhd_expr_to_decl (tree expr, bool *tc ATTRIBUTE_UNUSED, bool *se ATTRIBUTE_UNUSED)
{
  return expr;
}

/* Return sharing kind if OpenMP sharing attribute of DECL is
   predetermined, OMP_CLAUSE_DEFAULT_UNSPECIFIED otherwise.  */

enum omp_clause_default_kind
lhd_omp_predetermined_sharing (tree decl ATTRIBUTE_UNUSED)
{
  if (DECL_ARTIFICIAL (decl))
    return OMP_CLAUSE_DEFAULT_SHARED;
  return OMP_CLAUSE_DEFAULT_UNSPECIFIED;
}

/* Generate code to copy SRC to DST.  */

tree
lhd_omp_assignment (tree clause ATTRIBUTE_UNUSED, tree dst, tree src)
{
  return build2 (MODIFY_EXPR, TREE_TYPE (dst), dst, src);
}

/* Finalize clause C.  */

void
lhd_omp_finish_clause (tree, gimple_seq *)
{
}

/* Return true if DECL is a scalar variable (for the purpose of
   implicit firstprivatization).  */

bool
lhd_omp_scalar_p (tree decl)
{
  tree type = TREE_TYPE (decl);
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);
  if (TREE_CODE (type) == COMPLEX_TYPE)
    type = TREE_TYPE (type);
  if (INTEGRAL_TYPE_P (type)
      || SCALAR_FLOAT_TYPE_P (type)
      || TREE_CODE (type) == POINTER_TYPE)
    return true;
  return false;
}

/* Register language specific type size variables as potentially OpenMP
   firstprivate variables.  */

void
lhd_omp_firstprivatize_type_sizes (struct gimplify_omp_ctx *c ATTRIBUTE_UNUSED,
				   tree t ATTRIBUTE_UNUSED)
{
}

/* Return true if TYPE is an OpenMP mappable type.  */

bool
lhd_omp_mappable_type (tree type)
{
  /* Mappable type has to be complete.  */
  if (type == error_mark_node || !COMPLETE_TYPE_P (type))
    return false;
  return true;
}

/* Common function for add_builtin_function and
   add_builtin_function_ext_scope.  */
static tree
add_builtin_function_common (const char *name,
			     tree type,
			     int function_code,
			     enum built_in_class cl,
			     const char *library_name,
			     tree attrs,
			     tree (*hook) (tree))
{
  tree   id = get_identifier (name);
  tree decl = build_decl (BUILTINS_LOCATION, FUNCTION_DECL, id, type);

  TREE_PUBLIC (decl)         = 1;
  DECL_EXTERNAL (decl)       = 1;
  DECL_BUILT_IN_CLASS (decl) = cl;

  DECL_FUNCTION_CODE (decl)  = (enum built_in_function) function_code;

  /* DECL_FUNCTION_CODE is a bitfield; verify that the value fits.  */
  gcc_assert (DECL_FUNCTION_CODE (decl) == function_code);

  if (library_name)
    {
      tree libname = get_identifier (library_name);

      libname = targetm.mangle_decl_assembler_name (decl, libname);
      SET_DECL_ASSEMBLER_NAME (decl, libname);
    }

  /* Possibly apply some default attributes to this built-in function.  */
  if (attrs)
    decl_attributes (&decl, attrs, ATTR_FLAG_BUILT_IN);
  else
    decl_attributes (&decl, NULL_TREE, 0);

  return hook (decl);

}

/* Create a builtin function.  */

tree
add_builtin_function (const char *name,
		      tree type,
		      int function_code,
		      enum built_in_class cl,
		      const char *library_name,
		      tree attrs)
{
  return add_builtin_function_common (name, type, function_code, cl,
				      library_name, attrs,
				      lang_hooks.builtin_function);
}

/* Like add_builtin_function, but make sure the scope is the external scope.
   This is used to delay putting in back end builtin functions until the ISA
   that defines the builtin is declared via function specific target options,
   which can save memory for machines like the x86_64 that have multiple ISAs.
   If this points to the same function as builtin_function, the backend must
   add all of the builtins at program initialization time.  */

tree
add_builtin_function_ext_scope (const char *name,
				tree type,
				int function_code,
				enum built_in_class cl,
				const char *library_name,
				tree attrs)
{
  return add_builtin_function_common (name, type, function_code, cl,
				      library_name, attrs,
				      lang_hooks.builtin_function_ext_scope);
}

tree
lhd_builtin_function (tree decl)
{
  lang_hooks.decls.pushdecl (decl);
  return decl;
}

/* Create a builtin type.  */

tree
add_builtin_type (const char *name, tree type)
{
  tree   id = get_identifier (name);
  tree decl = build_decl (BUILTINS_LOCATION, TYPE_DECL, id, type);
  return lang_hooks.decls.pushdecl (decl);
}

/* LTO hooks.  */

/* Used to save and restore any previously active section.  */
static section *saved_section;


/* Begin a new LTO output section named NAME.  This default implementation
   saves the old section and emits assembly code to switch to the new
   section.  */

void
lhd_begin_section (const char *name)
{
  section *section;

  /* Save the old section so we can restore it in lto_end_asm_section.  */
  gcc_assert (!saved_section);
  saved_section = in_section;
  if (!saved_section)
    saved_section = text_section;

  /* Create a new section and switch to it.  */
  section = get_section (name, SECTION_DEBUG | SECTION_EXCLUDE, NULL);
  switch_to_section (section);
}


/* Write DATA of length LEN to the current LTO output section.  This default
   implementation just calls assemble_string.  */

void
lhd_append_data (const void *data, size_t len, void *)
{
  if (data)
    {
      timevar_push (TV_IPA_LTO_OUTPUT);
      assemble_string ((const char *)data, len);
      timevar_pop (TV_IPA_LTO_OUTPUT);
    }
}


/* Finish the current LTO output section.  This default implementation emits
   assembly code to switch to any section previously saved by
   lhd_begin_section.  */

void
lhd_end_section (void)
{
  if (saved_section)
    {
      switch_to_section (saved_section);
      saved_section = NULL;
    }
}

/* Default implementation of enum_underlying_base_type using type_for_size.  */

tree
lhd_enum_underlying_base_type (const_tree enum_type)
{
  return lang_hooks.types.type_for_size (TYPE_PRECISION (enum_type),
					 TYPE_UNSIGNED (enum_type));
}

/* Default implementation of LANG_HOOKS_GET_SUBSTRING_LOCATION.  */

const char *
lhd_get_substring_location (const substring_loc &, location_t *)
{
  return "unimplemented";
}

/* Default implementation of LANG_HOOKS_DECL_DWARF_ATTRIBUTE.  Don't add
   any attributes.  */

int
lhd_decl_dwarf_attribute (const_tree, int)
{
  return -1;
}

/* Default implementation of LANG_HOOKS_TYPE_DWARF_ATTRIBUTE.  Don't add
   any attributes.  */

int
lhd_type_dwarf_attribute (const_tree, int)
{
  return -1;
}

/* Default implementation of LANG_HOOKS_UNIT_SIZE_WITHOUT_REUSABLE_PADDING.
   Just return TYPE_SIZE_UNIT unadjusted.  */

tree
lhd_unit_size_without_reusable_padding (tree t)
{
  return TYPE_SIZE_UNIT (t);
}

/* Returns true if the current lang_hooks represents the GNU C frontend.  */

bool
lang_GNU_C (void)
{
  return (strncmp (lang_hooks.name, "GNU C", 5) == 0
	  && (lang_hooks.name[5] == '\0' || ISDIGIT (lang_hooks.name[5])));
}

/* Returns true if the current lang_hooks represents the GNU C++ frontend.  */

bool
lang_GNU_CXX (void)
{
  return strncmp (lang_hooks.name, "GNU C++", 7) == 0;
}

/* Returns true if the current lang_hooks represents the GNU Fortran frontend.  */

bool
lang_GNU_Fortran (void)
{
  return strncmp (lang_hooks.name, "GNU Fortran", 11) == 0;
}

/* Returns true if the current lang_hooks represents the GNU Objective-C
   frontend.  */

bool
lang_GNU_OBJC (void)
{
  return strncmp (lang_hooks.name, "GNU Objective-C", 15) == 0;
}
