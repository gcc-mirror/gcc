/* Default language-specific hooks.
   Copyright 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
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
#include "intl.h"
#include "tm.h"
#include "toplev.h"
#include "tree.h"
#include "tree-inline.h"
#include "gimple.h"
#include "rtl.h"
#include "insn-config.h"
#include "integrate.h"
#include "flags.h"
#include "langhooks.h"
#include "target.h"
#include "langhooks-def.h"
#include "ggc.h"
#include "diagnostic.h"

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

/* Do nothing (int).  */

void
lhd_do_nothing_i (int ARG_UNUSED (i))
{
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
lhd_return_null_tree_v (void)
{
  return NULL_TREE;
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
  return false;
}

/* Called from by print-tree.c.  */

void
lhd_print_tree_nothing (FILE * ARG_UNUSED (file),
			tree ARG_UNUSED (node),
			int ARG_UNUSED (indent))
{
}

/* Called from staticp.  */

tree
lhd_staticp (tree ARG_UNUSED (exp))
{
  return NULL;
}

/* Called from check_global_declarations.  */

bool
lhd_warn_unused_global_decl (const_tree decl)
{
  /* This is what used to exist in check_global_declarations.  Probably
     not many of these actually apply to non-C languages.  */

  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (decl))
    return false;
  if (TREE_CODE (decl) == VAR_DECL && TREE_READONLY (decl))
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

  /* The language-independent code should never use the
     DECL_ASSEMBLER_NAME for lots of DECLs.  Only FUNCTION_DECLs and
     VAR_DECLs for variables with static storage duration need a real
     DECL_ASSEMBLER_NAME.  */
  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL
	      || (TREE_CODE (decl) == VAR_DECL
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

  if (TREE_PUBLIC (decl) || DECL_CONTEXT (decl) == NULL_TREE)
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
lhd_incomplete_type_error (const_tree ARG_UNUSED (value), const_tree type)
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

/* This is the default expand_expr function.  */

rtx
lhd_expand_expr (tree ARG_UNUSED (t), rtx ARG_UNUSED (r),
		 enum machine_mode ARG_UNUSED (mm),
		 int ARG_UNUSED (em),
		 rtx * ARG_UNUSED (a))
{
  gcc_unreachable ();
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

/* lang_hooks.expr_size: Determine the size of the value of an expression T
   in a language-specific way.  Returns a tree for the size in bytes.  */

tree
lhd_expr_size (const_tree exp)
{
  if (DECL_P (exp)
      && DECL_SIZE_UNIT (exp) != 0)
    return DECL_SIZE_UNIT (exp);
  else
    return size_in_bytes (TREE_TYPE (exp));
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
   which is a language-specific tree code in category tcc_constant or
   tcc_exceptional.  The default expects never to be called.  */
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

/* Return the COMDAT group into which DECL should be placed.  */

const char *
lhd_comdat_group (tree decl)
{
  return IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
}

/* lang_hooks.decls.final_write_globals: perform final processing on
   global variables.  */
void
write_global_declarations (void)
{
  /* Really define vars that have had only a tentative definition.
     Really output inline functions that must actually be callable
     and have not been output so far.  */

  tree globals = lang_hooks.decls.getdecls ();
  int len = list_length (globals);
  tree *vec = XNEWVEC (tree, len);
  int i;
  tree decl;

  /* Process the decls in reverse order--earliest first.
     Put them into VEC from back to front, then take out from front.  */

  for (i = 0, decl = globals; i < len; i++, decl = TREE_CHAIN (decl))
    vec[len - i - 1] = decl;

  wrapup_global_declarations (vec, len);
  check_global_declarations (vec, len);
  emit_debug_global_declarations (vec, len);

  /* Clean up.  */
  free (vec);
}

/* Called to perform language-specific initialization of CTX.  */
void
lhd_initialize_diagnostics (struct diagnostic_context *ctx ATTRIBUTE_UNUSED)
{
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
      tree abstract_origin = diagnostic->abstract_origin;
      char *new_prefix = (file && abstract_origin == NULL)
			 ? file_name_as_prefix (file) : NULL;

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
	       lang_hooks.decl_printable_name (fndecl, 2));
	  else
	    pp_printf
	      (context->printer, _("In function %qs"),
	       lang_hooks.decl_printable_name (fndecl, 2));

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
		  pp_character (context->printer, ',');
		  pp_newline (context->printer);
		  if (s.file != NULL)
		    {
		      if (flag_show_column && s.column != 0)
			pp_printf (context->printer,
				   _("    inlined from %qs at %s:%d:%d"),
				   lang_hooks.decl_printable_name (fndecl, 2),
				   s.file, s.line, s.column);
		      else
			pp_printf (context->printer,
				   _("    inlined from %qs at %s:%d"),
				   lang_hooks.decl_printable_name (fndecl, 2),
				   s.file, s.line);

		    }
		  else
		    pp_printf (context->printer, _("    inlined from %qs"),
			       lang_hooks.decl_printable_name (fndecl, 2));
		}
	    }
	  pp_character (context->printer, ':');
	}

      diagnostic_set_last_function (context, diagnostic);
      pp_flush (context->printer);
      context->printer->prefix = old_prefix;
      free ((char*) new_prefix);
    }
}

tree
lhd_callgraph_analyze_expr (tree *tp ATTRIBUTE_UNUSED,
			    int *walk_subtrees ATTRIBUTE_UNUSED)
{
  return NULL;
}

tree
lhd_make_node (enum tree_code code)
{
  return make_node (code);
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

/* Register language specific type size variables as potentially OpenMP
   firstprivate variables.  */

void
lhd_omp_firstprivatize_type_sizes (struct gimplify_omp_ctx *c ATTRIBUTE_UNUSED,
				   tree t ATTRIBUTE_UNUSED)
{
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
  tree decl = build_decl (FUNCTION_DECL, id, type);

  TREE_PUBLIC (decl)         = 1;
  DECL_EXTERNAL (decl)       = 1;
  DECL_BUILT_IN_CLASS (decl) = cl;

  DECL_FUNCTION_CODE (decl)  = -1;
  gcc_assert (DECL_FUNCTION_CODE (decl) >= function_code);
  DECL_FUNCTION_CODE (decl)  = function_code;

  if (library_name)
    {
      tree libname = get_identifier (library_name);
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
