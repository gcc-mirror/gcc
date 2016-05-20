/* Pretty formatting of GENERIC trees in C syntax.
   Copyright (C) 2001-2016 Free Software Foundation, Inc.
   Adapted from c-pretty-print.c by Diego Novillo <dnovillo@redhat.com>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "predict.h"
#include "cgraph.h"
#include "tree-pretty-print.h"
#include "stor-layout.h"
#include "langhooks.h"
#include "tree-iterator.h"
#include "dumpfile.h"
#include "internal-fn.h"
#include "gomp-constants.h"

/* Local functions, macros and variables.  */
static const char *op_symbol (const_tree);
static void pretty_print_string (pretty_printer *, const char*);
static void newline_and_indent (pretty_printer *, int);
static void maybe_init_pretty_print (FILE *);
static void print_struct_decl (pretty_printer *, const_tree, int, int);
static void do_niy (pretty_printer *, const_tree, int);

#define INDENT(SPACE) do { \
  int i; for (i = 0; i<SPACE; i++) pp_space (pp); } while (0)

#define NIY do_niy (pp, node, flags)

static pretty_printer *tree_pp;

/* Try to print something for an unknown tree code.  */

static void
do_niy (pretty_printer *pp, const_tree node, int flags)
{
  int i, len;

  pp_string (pp, "<<< Unknown tree: ");
  pp_string (pp, get_tree_code_name (TREE_CODE (node)));

  if (EXPR_P (node))
    {
      len = TREE_OPERAND_LENGTH (node);
      for (i = 0; i < len; ++i)
	{
	  newline_and_indent (pp, 2);
	  dump_generic_node (pp, TREE_OPERAND (node, i), 2, flags, false);
	}
    }

  pp_string (pp, " >>>");
}

/* Debugging function to print out a generic expression.  */

DEBUG_FUNCTION void
debug_generic_expr (tree t)
{
  print_generic_expr (stderr, t, TDF_VOPS|TDF_MEMSYMS);
  fprintf (stderr, "\n");
}

/* Debugging function to print out a generic statement.  */

DEBUG_FUNCTION void
debug_generic_stmt (tree t)
{
  print_generic_stmt (stderr, t, TDF_VOPS|TDF_MEMSYMS);
  fprintf (stderr, "\n");
}

/* Debugging function to print out a chain of trees .  */

DEBUG_FUNCTION void
debug_tree_chain (tree t)
{
  hash_set<tree> seen;

  while (t)
    {
      print_generic_expr (stderr, t, TDF_VOPS|TDF_MEMSYMS|TDF_UID);
      fprintf (stderr, " ");
      t = TREE_CHAIN (t);
      if (seen.add (t))
	{
	  fprintf (stderr, "... [cycled back to ");
	  print_generic_expr (stderr, t, TDF_VOPS|TDF_MEMSYMS|TDF_UID);
	  fprintf (stderr, "]");
	  break;
	}
    }
  fprintf (stderr, "\n");
}

/* Prints declaration DECL to the FILE with details specified by FLAGS.  */
void
print_generic_decl (FILE *file, tree decl, int flags)
{
  maybe_init_pretty_print (file);
  print_declaration (tree_pp, decl, 2, flags);
  pp_write_text_to_stream (tree_pp);
}

/* Print tree T, and its successors, on file FILE.  FLAGS specifies details
   to show in the dump.  See TDF_* in dumpfile.h.  */

void
print_generic_stmt (FILE *file, tree t, int flags)
{
  maybe_init_pretty_print (file);
  dump_generic_node (tree_pp, t, 0, flags, true);
  pp_newline_and_flush (tree_pp);
}

/* Print tree T, and its successors, on file FILE.  FLAGS specifies details
   to show in the dump.  See TDF_* in dumpfile.h.  The output is indented by
   INDENT spaces.  */

void
print_generic_stmt_indented (FILE *file, tree t, int flags, int indent)
{
  int i;

  maybe_init_pretty_print (file);

  for (i = 0; i < indent; i++)
    pp_space (tree_pp);
  dump_generic_node (tree_pp, t, indent, flags, true);
  pp_newline_and_flush (tree_pp);
}

/* Print a single expression T on file FILE.  FLAGS specifies details to show
   in the dump.  See TDF_* in dumpfile.h.  */

void
print_generic_expr (FILE *file, tree t, int flags)
{
  maybe_init_pretty_print (file);
  dump_generic_node (tree_pp, t, 0, flags, false);
  pp_flush (tree_pp);
}

/* Dump NAME, an IDENTIFIER_POINTER, sanitized so that D<num> sequences
   in it are replaced with Dxxxx, as long as they are at the start or
   preceded by $ and at the end or followed by $.  See make_fancy_name
   in tree-sra.c.  */

static void
dump_fancy_name (pretty_printer *pp, tree name)
{
  int cnt = 0;
  int length = IDENTIFIER_LENGTH (name);
  const char *n = IDENTIFIER_POINTER (name);
  do
    {
      n = strchr (n, 'D');
      if (n == NULL)
	break;
      if (ISDIGIT (n[1])
	  && (n == IDENTIFIER_POINTER (name) || n[-1] == '$'))
	{
	  int l = 2;
	  while (ISDIGIT (n[l]))
	    l++;
	  if (n[l] == '\0' || n[l] == '$')
	    {
	      cnt++;
	      length += 5 - l;
	    }
	  n += l;
	}
      else
	n++;
    }
  while (1);
  if (cnt == 0)
    {
      pp_tree_identifier (pp, name);
      return;
    }

  char *str = XNEWVEC (char, length + 1);
  char *p = str;
  const char *q;
  q = n = IDENTIFIER_POINTER (name);
  do
    {
      q = strchr (q, 'D');
      if (q == NULL)
	break;
      if (ISDIGIT (q[1])
	  && (q == IDENTIFIER_POINTER (name) || q[-1] == '$'))
	{
	  int l = 2;
	  while (ISDIGIT (q[l]))
	    l++;
	  if (q[l] == '\0' || q[l] == '$')
	    {
	      memcpy (p, n, q - n);
	      memcpy (p + (q - n), "Dxxxx", 5);
	      p += (q - n) + 5;
	      n = q + l;
	    }
	  q += l;
	}
      else
	q++;
    }
  while (1);
  memcpy (p, n, IDENTIFIER_LENGTH (name) - (n - IDENTIFIER_POINTER (name)));
  str[length] = '\0';
  if (pp_translate_identifiers (pp))
    {
      const char *text = identifier_to_locale (str);
      pp_append_text (pp, text, text + strlen (text));
    }
  else
    pp_append_text (pp, str, str + length);
  XDELETEVEC (str);
}

/* Dump the name of a _DECL node and its DECL_UID if TDF_UID is set
   in FLAGS.  */

static void
dump_decl_name (pretty_printer *pp, tree node, int flags)
{
  if (DECL_NAME (node))
    {
      if ((flags & TDF_ASMNAME) && DECL_ASSEMBLER_NAME_SET_P (node))
	pp_tree_identifier (pp, DECL_ASSEMBLER_NAME (node));
      /* For DECL_NAMELESS names look for embedded uids in the
	 names and sanitize them for TDF_NOUID.  */
      else if ((flags & TDF_NOUID) && DECL_NAMELESS (node))
	dump_fancy_name (pp, DECL_NAME (node));
      else
	pp_tree_identifier (pp, DECL_NAME (node));
    }
  if ((flags & TDF_UID) || DECL_NAME (node) == NULL_TREE)
    {
      if (TREE_CODE (node) == LABEL_DECL && LABEL_DECL_UID (node) != -1)
	pp_printf (pp, "L.%d", (int) LABEL_DECL_UID (node));
      else if (TREE_CODE (node) == DEBUG_EXPR_DECL)
	{
	  if (flags & TDF_NOUID)
	    pp_string (pp, "D#xxxx");
	  else
	    pp_printf (pp, "D#%i", DEBUG_TEMP_UID (node));
	}
      else
	{
	  char c = TREE_CODE (node) == CONST_DECL ? 'C' : 'D';
	  if (flags & TDF_NOUID)
	    pp_printf (pp, "%c.xxxx", c);
	  else
	    pp_printf (pp, "%c.%u", c, DECL_UID (node));
	}
    }
  if ((flags & TDF_ALIAS) && DECL_PT_UID (node) != DECL_UID (node))
    {
      if (flags & TDF_NOUID)
	pp_printf (pp, "ptD.xxxx");
      else
	pp_printf (pp, "ptD.%u", DECL_PT_UID (node));
    }
}

/* Like the above, but used for pretty printing function calls.  */

static void
dump_function_name (pretty_printer *pp, tree node, int flags)
{
  if (CONVERT_EXPR_P (node))
    node = TREE_OPERAND (node, 0);
  if (DECL_NAME (node) && (flags & TDF_ASMNAME) == 0)
    pp_string (pp, lang_hooks.decl_printable_name (node, 1));
  else
    dump_decl_name (pp, node, flags);
}

/* Dump a function declaration.  NODE is the FUNCTION_TYPE.  PP, SPC and
   FLAGS are as in dump_generic_node.  */

static void
dump_function_declaration (pretty_printer *pp, tree node,
			   int spc, int flags)
{
  bool wrote_arg = false;
  tree arg;

  pp_space (pp);
  pp_left_paren (pp);

  /* Print the argument types.  */
  arg = TYPE_ARG_TYPES (node);
  while (arg && arg != void_list_node && arg != error_mark_node)
    {
      if (wrote_arg)
	{
	  pp_comma (pp);
	  pp_space (pp);
	}
      wrote_arg = true;
      dump_generic_node (pp, TREE_VALUE (arg), spc, flags, false);
      arg = TREE_CHAIN (arg);
    }

  /* Drop the trailing void_type_node if we had any previous argument.  */
  if (arg == void_list_node && !wrote_arg)
    pp_string (pp, "void");
  /* Properly dump vararg function types.  */
  else if (!arg && wrote_arg)
    pp_string (pp, ", ...");
  /* Avoid printing any arg for unprototyped functions.  */

  pp_right_paren (pp);
}

/* Dump the domain associated with an array.  */

static void
dump_array_domain (pretty_printer *pp, tree domain, int spc, int flags)
{
  pp_left_bracket (pp);
  if (domain)
    {
      tree min = TYPE_MIN_VALUE (domain);
      tree max = TYPE_MAX_VALUE (domain);

      if (min && max
	  && integer_zerop (min)
	  && tree_fits_shwi_p (max))
	pp_wide_integer (pp, tree_to_shwi (max) + 1);
      else
	{
	  if (min)
	    dump_generic_node (pp, min, spc, flags, false);
	  pp_colon (pp);
	  if (max)
	    dump_generic_node (pp, max, spc, flags, false);
	}
    }
  else
    pp_string (pp, "<unknown>");
  pp_right_bracket (pp);
}


/* Dump OpenMP clause CLAUSE.  PP, CLAUSE, SPC and FLAGS are as in
   dump_generic_node.  */

static void
dump_omp_clause (pretty_printer *pp, tree clause, int spc, int flags)
{
  const char *name;

  switch (OMP_CLAUSE_CODE (clause))
    {
    case OMP_CLAUSE_PRIVATE:
      name = "private";
      goto print_remap;
    case OMP_CLAUSE_SHARED:
      name = "shared";
      goto print_remap;
    case OMP_CLAUSE_FIRSTPRIVATE:
      name = "firstprivate";
      goto print_remap;
    case OMP_CLAUSE_LASTPRIVATE:
      name = "lastprivate";
      goto print_remap;
    case OMP_CLAUSE_COPYIN:
      name = "copyin";
      goto print_remap;
    case OMP_CLAUSE_COPYPRIVATE:
      name = "copyprivate";
      goto print_remap;
    case OMP_CLAUSE_UNIFORM:
      name = "uniform";
      goto print_remap;
    case OMP_CLAUSE_USE_DEVICE_PTR:
      name = "use_device_ptr";
      goto print_remap;
    case OMP_CLAUSE_IS_DEVICE_PTR:
      name = "is_device_ptr";
      goto print_remap;
    case OMP_CLAUSE__LOOPTEMP_:
      name = "_looptemp_";
      goto print_remap;
    case OMP_CLAUSE_DEVICE_RESIDENT:
      name = "device_resident";
      goto print_remap;
    case OMP_CLAUSE_TO_DECLARE:
      name = "to";
      goto print_remap;
    case OMP_CLAUSE_LINK:
      name = "link";
      goto print_remap;
  print_remap:
      pp_string (pp, name);
      pp_left_paren (pp);
      dump_generic_node (pp, OMP_CLAUSE_DECL (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_REDUCTION:
      pp_string (pp, "reduction(");
      if (OMP_CLAUSE_REDUCTION_CODE (clause) != ERROR_MARK)
	{
	  pp_string (pp,
		     op_symbol_code (OMP_CLAUSE_REDUCTION_CODE (clause)));
	  pp_colon (pp);
	}
      dump_generic_node (pp, OMP_CLAUSE_DECL (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_IF:
      pp_string (pp, "if(");
      switch (OMP_CLAUSE_IF_MODIFIER (clause))
	{
	case ERROR_MARK: break;
	case OMP_PARALLEL: pp_string (pp, "parallel:"); break;
	case OMP_TASK: pp_string (pp, "task:"); break;
	case OMP_TASKLOOP: pp_string (pp, "taskloop:"); break;
	case OMP_TARGET_DATA: pp_string (pp, "target data:"); break;
	case OMP_TARGET: pp_string (pp, "target:"); break;
	case OMP_TARGET_UPDATE: pp_string (pp, "target update:"); break;
	case OMP_TARGET_ENTER_DATA:
	  pp_string (pp, "target enter data:"); break;
	case OMP_TARGET_EXIT_DATA: pp_string (pp, "target exit data:"); break;
	default: gcc_unreachable ();
	}
      dump_generic_node (pp, OMP_CLAUSE_IF_EXPR (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_NUM_THREADS:
      pp_string (pp, "num_threads(");
      dump_generic_node (pp, OMP_CLAUSE_NUM_THREADS_EXPR (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE__CILK_FOR_COUNT_:
      pp_string (pp, "_Cilk_for_count_(");
      dump_generic_node (pp, OMP_CLAUSE_OPERAND (clause, 0),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_NOWAIT:
      pp_string (pp, "nowait");
      break;
    case OMP_CLAUSE_ORDERED:
      pp_string (pp, "ordered");
      if (OMP_CLAUSE_ORDERED_EXPR (clause))
	{
	  pp_left_paren (pp);
	  dump_generic_node (pp, OMP_CLAUSE_ORDERED_EXPR (clause),
			     spc, flags, false);
	  pp_right_paren (pp);
	}
      break;

    case OMP_CLAUSE_DEFAULT:
      pp_string (pp, "default(");
      switch (OMP_CLAUSE_DEFAULT_KIND (clause))
	{
	case OMP_CLAUSE_DEFAULT_UNSPECIFIED:
	  break;
	case OMP_CLAUSE_DEFAULT_SHARED:
	  pp_string (pp, "shared");
	  break;
	case OMP_CLAUSE_DEFAULT_NONE:
	  pp_string (pp, "none");
	  break;
	case OMP_CLAUSE_DEFAULT_PRIVATE:
	  pp_string (pp, "private");
	  break;
	case OMP_CLAUSE_DEFAULT_FIRSTPRIVATE:
	  pp_string (pp, "firstprivate");
	  break;
	default:
	  gcc_unreachable ();
	}
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_SCHEDULE:
      pp_string (pp, "schedule(");
      if (OMP_CLAUSE_SCHEDULE_KIND (clause)
	  & (OMP_CLAUSE_SCHEDULE_MONOTONIC
	     | OMP_CLAUSE_SCHEDULE_NONMONOTONIC))
	{
	  if (OMP_CLAUSE_SCHEDULE_KIND (clause)
	      & OMP_CLAUSE_SCHEDULE_MONOTONIC)
	    pp_string (pp, "monotonic");
	  else
	    pp_string (pp, "nonmonotonic");
	  if (OMP_CLAUSE_SCHEDULE_SIMD (clause))
	    pp_comma (pp);
	  else
	    pp_colon (pp);
	}
      if (OMP_CLAUSE_SCHEDULE_SIMD (clause))
	pp_string (pp, "simd:");

      switch (OMP_CLAUSE_SCHEDULE_KIND (clause) & OMP_CLAUSE_SCHEDULE_MASK)
	{
	case OMP_CLAUSE_SCHEDULE_STATIC:
	  pp_string (pp, "static");
	  break;
	case OMP_CLAUSE_SCHEDULE_DYNAMIC:
	  pp_string (pp, "dynamic");
	  break;
	case OMP_CLAUSE_SCHEDULE_GUIDED:
	  pp_string (pp, "guided");
	  break;
	case OMP_CLAUSE_SCHEDULE_RUNTIME:
	  pp_string (pp, "runtime");
	  break;
	case OMP_CLAUSE_SCHEDULE_AUTO:
	  pp_string (pp, "auto");
	  break;
	case OMP_CLAUSE_SCHEDULE_CILKFOR:
	  pp_string (pp, "cilk-for grain");
	  break;
	default:
	  gcc_unreachable ();
	}
      if (OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (clause))
	{
	  pp_comma (pp);
	  dump_generic_node (pp, OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (clause),
			     spc, flags, false);
	}
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_UNTIED:
      pp_string (pp, "untied");
      break;

    case OMP_CLAUSE_COLLAPSE:
      pp_string (pp, "collapse(");
      dump_generic_node (pp, OMP_CLAUSE_COLLAPSE_EXPR (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_FINAL:
      pp_string (pp, "final(");
      dump_generic_node (pp, OMP_CLAUSE_FINAL_EXPR (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_MERGEABLE:
      pp_string (pp, "mergeable");
      break;

    case OMP_CLAUSE_LINEAR:
      pp_string (pp, "linear(");
      switch (OMP_CLAUSE_LINEAR_KIND (clause))
	{
	case OMP_CLAUSE_LINEAR_DEFAULT:
	  break;
	case OMP_CLAUSE_LINEAR_REF:
	  pp_string (pp, "ref(");
	  break;
	case OMP_CLAUSE_LINEAR_VAL:
	  pp_string (pp, "val(");
	  break;
	case OMP_CLAUSE_LINEAR_UVAL:
	  pp_string (pp, "uval(");
	  break;
	default:
	  gcc_unreachable ();
	}
      dump_generic_node (pp, OMP_CLAUSE_DECL (clause),
			 spc, flags, false);
      if (OMP_CLAUSE_LINEAR_KIND (clause) != OMP_CLAUSE_LINEAR_DEFAULT)
	pp_right_paren (pp);
      pp_colon (pp);
      dump_generic_node (pp, OMP_CLAUSE_LINEAR_STEP (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_ALIGNED:
      pp_string (pp, "aligned(");
      dump_generic_node (pp, OMP_CLAUSE_DECL (clause),
			 spc, flags, false);
      if (OMP_CLAUSE_ALIGNED_ALIGNMENT (clause))
	{
	  pp_colon (pp);
	  dump_generic_node (pp, OMP_CLAUSE_ALIGNED_ALIGNMENT (clause),
			     spc, flags, false);
	}
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_DEPEND:
      pp_string (pp, "depend(");
      switch (OMP_CLAUSE_DEPEND_KIND (clause))
	{
	case OMP_CLAUSE_DEPEND_IN:
	  pp_string (pp, "in");
	  break;
	case OMP_CLAUSE_DEPEND_OUT:
	  pp_string (pp, "out");
	  break;
	case OMP_CLAUSE_DEPEND_INOUT:
	  pp_string (pp, "inout");
	  break;
	case OMP_CLAUSE_DEPEND_SOURCE:
	  pp_string (pp, "source)");
	  return;
	case OMP_CLAUSE_DEPEND_SINK:
	  pp_string (pp, "sink:");
	  for (tree t = OMP_CLAUSE_DECL (clause); t; t = TREE_CHAIN (t))
	    if (TREE_CODE (t) == TREE_LIST)
	      {
		dump_generic_node (pp, TREE_VALUE (t), spc, flags, false);
		if (TREE_PURPOSE (t) != integer_zero_node)
		  {
		    if (OMP_CLAUSE_DEPEND_SINK_NEGATIVE (t))
		      pp_minus (pp);
		    else
		      pp_plus (pp);
		    dump_generic_node (pp, TREE_PURPOSE (t), spc, flags,
				       false);
		  }
		if (TREE_CHAIN (t))
		  pp_comma (pp);
	      }
	    else
	      gcc_unreachable ();
	  pp_right_paren (pp);
	  return;
	default:
	  gcc_unreachable ();
	}
      pp_colon (pp);
      dump_generic_node (pp, OMP_CLAUSE_DECL (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_MAP:
      pp_string (pp, "map(");
      switch (OMP_CLAUSE_MAP_KIND (clause))
	{
	case GOMP_MAP_ALLOC:
	case GOMP_MAP_POINTER:
	  pp_string (pp, "alloc");
	  break;
	case GOMP_MAP_TO:
	case GOMP_MAP_TO_PSET:
	  pp_string (pp, "to");
	  break;
	case GOMP_MAP_FROM:
	  pp_string (pp, "from");
	  break;
	case GOMP_MAP_TOFROM:
	  pp_string (pp, "tofrom");
	  break;
	case GOMP_MAP_FORCE_ALLOC:
	  pp_string (pp, "force_alloc");
	  break;
	case GOMP_MAP_FORCE_TO:
	  pp_string (pp, "force_to");
	  break;
	case GOMP_MAP_FORCE_FROM:
	  pp_string (pp, "force_from");
	  break;
	case GOMP_MAP_FORCE_TOFROM:
	  pp_string (pp, "force_tofrom");
	  break;
	case GOMP_MAP_FORCE_PRESENT:
	  pp_string (pp, "force_present");
	  break;
	case GOMP_MAP_DELETE:
	  pp_string (pp, "delete");
	  break;
	case GOMP_MAP_FORCE_DEVICEPTR:
	  pp_string (pp, "force_deviceptr");
	  break;
	case GOMP_MAP_ALWAYS_TO:
	  pp_string (pp, "always,to");
	  break;
	case GOMP_MAP_ALWAYS_FROM:
	  pp_string (pp, "always,from");
	  break;
	case GOMP_MAP_ALWAYS_TOFROM:
	  pp_string (pp, "always,tofrom");
	  break;
	case GOMP_MAP_RELEASE:
	  pp_string (pp, "release");
	  break;
	case GOMP_MAP_FIRSTPRIVATE_POINTER:
	  pp_string (pp, "firstprivate");
	  break;
	case GOMP_MAP_FIRSTPRIVATE_REFERENCE:
	  pp_string (pp, "firstprivate ref");
	  break;
	case GOMP_MAP_STRUCT:
	  pp_string (pp, "struct");
	  break;
	case GOMP_MAP_ALWAYS_POINTER:
	  pp_string (pp, "always_pointer");
	  break;
	case GOMP_MAP_DEVICE_RESIDENT:
	  pp_string (pp, "device_resident");
	  break;
	case GOMP_MAP_LINK:
	  pp_string (pp, "link");
	  break;
	default:
	  gcc_unreachable ();
	}
      pp_colon (pp);
      dump_generic_node (pp, OMP_CLAUSE_DECL (clause),
			 spc, flags, false);
     print_clause_size:
      if (OMP_CLAUSE_SIZE (clause))
	{
	  switch (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_MAP
		  ? OMP_CLAUSE_MAP_KIND (clause) : GOMP_MAP_TO)
	    {
	    case GOMP_MAP_POINTER:
	    case GOMP_MAP_FIRSTPRIVATE_POINTER:
	    case GOMP_MAP_FIRSTPRIVATE_REFERENCE:
	    case GOMP_MAP_ALWAYS_POINTER:
	      pp_string (pp, " [pointer assign, bias: ");
	      break;
	    case GOMP_MAP_TO_PSET:
	      pp_string (pp, " [pointer set, len: ");
	      break;
	    default:
	      pp_string (pp, " [len: ");
	      break;
	    }
	  dump_generic_node (pp, OMP_CLAUSE_SIZE (clause),
			     spc, flags, false);
	  pp_right_bracket (pp);
	}
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_FROM:
      pp_string (pp, "from(");
      dump_generic_node (pp, OMP_CLAUSE_DECL (clause),
			 spc, flags, false);
      goto print_clause_size;

    case OMP_CLAUSE_TO:
      pp_string (pp, "to(");
      dump_generic_node (pp, OMP_CLAUSE_DECL (clause),
			 spc, flags, false);
      goto print_clause_size;

    case OMP_CLAUSE__CACHE_:
      pp_string (pp, "(");
      dump_generic_node (pp, OMP_CLAUSE_DECL (clause),
			 spc, flags, false);
      goto print_clause_size;

    case OMP_CLAUSE_NUM_TEAMS:
      pp_string (pp, "num_teams(");
      dump_generic_node (pp, OMP_CLAUSE_NUM_TEAMS_EXPR (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_THREAD_LIMIT:
      pp_string (pp, "thread_limit(");
      dump_generic_node (pp, OMP_CLAUSE_THREAD_LIMIT_EXPR (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_DEVICE:
      pp_string (pp, "device(");
      dump_generic_node (pp, OMP_CLAUSE_DEVICE_ID (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_DIST_SCHEDULE:
      pp_string (pp, "dist_schedule(static");
      if (OMP_CLAUSE_DIST_SCHEDULE_CHUNK_EXPR (clause))
	{
	  pp_comma (pp);
	  dump_generic_node (pp,
			     OMP_CLAUSE_DIST_SCHEDULE_CHUNK_EXPR (clause),
			     spc, flags, false);
	}
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_PROC_BIND:
      pp_string (pp, "proc_bind(");
      switch (OMP_CLAUSE_PROC_BIND_KIND (clause))
	{
	case OMP_CLAUSE_PROC_BIND_MASTER:
	  pp_string (pp, "master");
	  break;
	case OMP_CLAUSE_PROC_BIND_CLOSE:
	  pp_string (pp, "close");
	  break;
	case OMP_CLAUSE_PROC_BIND_SPREAD:
	  pp_string (pp, "spread");
	  break;
	default:
	  gcc_unreachable ();
	}
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_SAFELEN:
      pp_string (pp, "safelen(");
      dump_generic_node (pp, OMP_CLAUSE_SAFELEN_EXPR (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_SIMDLEN:
      pp_string (pp, "simdlen(");
      dump_generic_node (pp, OMP_CLAUSE_SIMDLEN_EXPR (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_PRIORITY:
      pp_string (pp, "priority(");
      dump_generic_node (pp, OMP_CLAUSE_PRIORITY_EXPR (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_GRAINSIZE:
      pp_string (pp, "grainsize(");
      dump_generic_node (pp, OMP_CLAUSE_GRAINSIZE_EXPR (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_NUM_TASKS:
      pp_string (pp, "num_tasks(");
      dump_generic_node (pp, OMP_CLAUSE_NUM_TASKS_EXPR (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_HINT:
      pp_string (pp, "hint(");
      dump_generic_node (pp, OMP_CLAUSE_HINT_EXPR (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_DEFAULTMAP:
      pp_string (pp, "defaultmap(tofrom:scalar)");
      break;

    case OMP_CLAUSE__SIMDUID_:
      pp_string (pp, "_simduid_(");
      dump_generic_node (pp, OMP_CLAUSE__SIMDUID__DECL (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE_GANG:
      pp_string (pp, "gang");
      if (OMP_CLAUSE_GANG_EXPR (clause) != NULL_TREE)
	{
	  pp_string (pp, "(num: ");
	  dump_generic_node (pp, OMP_CLAUSE_GANG_EXPR (clause),
			     spc, flags, false);
	}
      if (OMP_CLAUSE_GANG_STATIC_EXPR (clause) != NULL_TREE)
	{
	  if (OMP_CLAUSE_GANG_EXPR (clause) == NULL_TREE)
	    pp_left_paren (pp);
	  else
	    pp_space (pp);
	  pp_string (pp, "static:");
	  if (OMP_CLAUSE_GANG_STATIC_EXPR (clause)
	      == integer_minus_one_node)
	    pp_character (pp, '*');
	  else
	    dump_generic_node (pp, OMP_CLAUSE_GANG_STATIC_EXPR (clause),
			       spc, flags, false);
	}
      if (OMP_CLAUSE_GANG_EXPR (clause) != NULL_TREE
	  || OMP_CLAUSE_GANG_STATIC_EXPR (clause) != NULL_TREE)
	pp_right_paren (pp);
      break;

    case OMP_CLAUSE_ASYNC:
      pp_string (pp, "async");
      if (OMP_CLAUSE_ASYNC_EXPR (clause))
        {
          pp_character(pp, '(');
          dump_generic_node (pp, OMP_CLAUSE_ASYNC_EXPR (clause),
                             spc, flags, false);
          pp_character(pp, ')');
        }
      break;

    case OMP_CLAUSE_AUTO:
    case OMP_CLAUSE_SEQ:
      pp_string (pp, omp_clause_code_name[OMP_CLAUSE_CODE (clause)]);
      break;

    case OMP_CLAUSE_WAIT:
      pp_string (pp, "wait(");
      dump_generic_node (pp, OMP_CLAUSE_WAIT_EXPR (clause),
			 spc, flags, false);
      pp_character(pp, ')');
      break;

    case OMP_CLAUSE_WORKER:
      pp_string (pp, "worker");
      if (OMP_CLAUSE_WORKER_EXPR (clause) != NULL_TREE)
	{
	  pp_left_paren (pp);
	  dump_generic_node (pp, OMP_CLAUSE_WORKER_EXPR (clause),
			     spc, flags, false);
	  pp_right_paren (pp);
	}
      break;

    case OMP_CLAUSE_VECTOR:
      pp_string (pp, "vector");
      if (OMP_CLAUSE_VECTOR_EXPR (clause) != NULL_TREE)
	{
	  pp_left_paren (pp);
	  dump_generic_node (pp, OMP_CLAUSE_VECTOR_EXPR (clause),
			     spc, flags, false);
	  pp_right_paren (pp);
	}
      break;

    case OMP_CLAUSE_NUM_GANGS:
      pp_string (pp, "num_gangs(");
      dump_generic_node (pp, OMP_CLAUSE_NUM_GANGS_EXPR (clause),
                         spc, flags, false);
      pp_character (pp, ')');
      break;

    case OMP_CLAUSE_NUM_WORKERS:
      pp_string (pp, "num_workers(");
      dump_generic_node (pp, OMP_CLAUSE_NUM_WORKERS_EXPR (clause),
                         spc, flags, false);
      pp_character (pp, ')');
      break;

    case OMP_CLAUSE_VECTOR_LENGTH:
      pp_string (pp, "vector_length(");
      dump_generic_node (pp, OMP_CLAUSE_VECTOR_LENGTH_EXPR (clause),
                         spc, flags, false);
      pp_character (pp, ')');
      break;

    case OMP_CLAUSE_INBRANCH:
      pp_string (pp, "inbranch");
      break;
    case OMP_CLAUSE_NOTINBRANCH:
      pp_string (pp, "notinbranch");
      break;
    case OMP_CLAUSE_FOR:
      pp_string (pp, "for");
      break;
    case OMP_CLAUSE_PARALLEL:
      pp_string (pp, "parallel");
      break;
    case OMP_CLAUSE_SECTIONS:
      pp_string (pp, "sections");
      break;
    case OMP_CLAUSE_TASKGROUP:
      pp_string (pp, "taskgroup");
      break;
    case OMP_CLAUSE_NOGROUP:
      pp_string (pp, "nogroup");
      break;
    case OMP_CLAUSE_THREADS:
      pp_string (pp, "threads");
      break;
    case OMP_CLAUSE_SIMD:
      pp_string (pp, "simd");
      break;
    case OMP_CLAUSE_INDEPENDENT:
      pp_string (pp, "independent");
      break;
    case OMP_CLAUSE_TILE:
      pp_string (pp, "tile(");
      dump_generic_node (pp, OMP_CLAUSE_TILE_LIST (clause),
			 spc, flags, false);
      pp_right_paren (pp);
      break;

    case OMP_CLAUSE__GRIDDIM_:
      pp_string (pp, "_griddim_(");
      pp_unsigned_wide_integer (pp, OMP_CLAUSE__GRIDDIM__DIMENSION (clause));
      pp_colon (pp);
      dump_generic_node (pp, OMP_CLAUSE__GRIDDIM__SIZE (clause), spc, flags,
			 false);
      pp_comma (pp);
      dump_generic_node (pp, OMP_CLAUSE__GRIDDIM__GROUP (clause), spc, flags,
			 false);
      pp_right_paren (pp);
      break;

    default:
      /* Should never happen.  */
      dump_generic_node (pp, clause, spc, flags, false);
      break;
    }
}


/* Dump the list of OpenMP clauses.  PP, SPC and FLAGS are as in
   dump_generic_node.  */

void
dump_omp_clauses (pretty_printer *pp, tree clause, int spc, int flags)
{
  if (clause == NULL)
    return;

  pp_space (pp);
  while (1)
    {
      dump_omp_clause (pp, clause, spc, flags);
      clause = OMP_CLAUSE_CHAIN (clause);
      if (clause == NULL)
	return;
      pp_space (pp);
    }
}


/* Dump location LOC to PP.  */

void
dump_location (pretty_printer *pp, location_t loc)
{
  expanded_location xloc = expand_location (loc);

  pp_left_bracket (pp);
  if (xloc.file)
    {
      pp_string (pp, xloc.file);
      pp_string (pp, ":");
    }
  pp_decimal_int (pp, xloc.line);
  pp_colon (pp);
  pp_decimal_int (pp, xloc.column);
  pp_string (pp, "] ");
}


/* Dump lexical block BLOCK.  PP, SPC and FLAGS are as in
   dump_generic_node.  */

static void
dump_block_node (pretty_printer *pp, tree block, int spc, int flags)
{
  tree t;

  pp_printf (pp, "BLOCK #%d ", BLOCK_NUMBER (block));

  if (flags & TDF_ADDRESS)
    pp_printf (pp, "[%p] ", (void *) block);

  if (BLOCK_ABSTRACT (block))
    pp_string (pp, "[abstract] ");

  if (TREE_ASM_WRITTEN (block))
    pp_string (pp, "[written] ");

  if (flags & TDF_SLIM)
    return;

  if (BLOCK_SOURCE_LOCATION (block))
    dump_location (pp, BLOCK_SOURCE_LOCATION (block));

  newline_and_indent (pp, spc + 2);

  if (BLOCK_SUPERCONTEXT (block))
    {
      pp_string (pp, "SUPERCONTEXT: ");
      dump_generic_node (pp, BLOCK_SUPERCONTEXT (block), 0,
			 flags | TDF_SLIM, false);
      newline_and_indent (pp, spc + 2);
    }

  if (BLOCK_SUBBLOCKS (block))
    {
      pp_string (pp, "SUBBLOCKS: ");
      for (t = BLOCK_SUBBLOCKS (block); t; t = BLOCK_CHAIN (t))
	{
	  dump_generic_node (pp, t, 0, flags | TDF_SLIM, false);
	  pp_space (pp);
	}
      newline_and_indent (pp, spc + 2);
    }

  if (BLOCK_CHAIN (block))
    {
      pp_string (pp, "SIBLINGS: ");
      for (t = BLOCK_CHAIN (block); t; t = BLOCK_CHAIN (t))
	{
	  dump_generic_node (pp, t, 0, flags | TDF_SLIM, false);
	  pp_space (pp);
	}
      newline_and_indent (pp, spc + 2);
    }

  if (BLOCK_VARS (block))
    {
      pp_string (pp, "VARS: ");
      for (t = BLOCK_VARS (block); t; t = TREE_CHAIN (t))
	{
	  dump_generic_node (pp, t, 0, flags, false);
	  pp_space (pp);
	}
      newline_and_indent (pp, spc + 2);
    }

  if (vec_safe_length (BLOCK_NONLOCALIZED_VARS (block)) > 0)
    {
      unsigned i;
      vec<tree, va_gc> *nlv = BLOCK_NONLOCALIZED_VARS (block);

      pp_string (pp, "NONLOCALIZED_VARS: ");
      FOR_EACH_VEC_ELT (*nlv, i, t)
	{
	  dump_generic_node (pp, t, 0, flags, false);
	  pp_space (pp);
	}
      newline_and_indent (pp, spc + 2);
    }

  if (BLOCK_ABSTRACT_ORIGIN (block))
    {
      pp_string (pp, "ABSTRACT_ORIGIN: ");
      dump_generic_node (pp, BLOCK_ABSTRACT_ORIGIN (block), 0,
			 flags | TDF_SLIM, false);
      newline_and_indent (pp, spc + 2);
    }

  if (BLOCK_FRAGMENT_ORIGIN (block))
    {
      pp_string (pp, "FRAGMENT_ORIGIN: ");
      dump_generic_node (pp, BLOCK_FRAGMENT_ORIGIN (block), 0,
			 flags | TDF_SLIM, false);
      newline_and_indent (pp, spc + 2);
    }

  if (BLOCK_FRAGMENT_CHAIN (block))
    {
      pp_string (pp, "FRAGMENT_CHAIN: ");
      for (t = BLOCK_FRAGMENT_CHAIN (block); t; t = BLOCK_FRAGMENT_CHAIN (t))
	{
	  dump_generic_node (pp, t, 0, flags | TDF_SLIM, false);
	  pp_space (pp);
	}
      newline_and_indent (pp, spc + 2);
    }
}


/* Dump the node NODE on the pretty_printer PP, SPC spaces of
   indent.  FLAGS specifies details to show in the dump (see TDF_* in
   dumpfile.h).  If IS_STMT is true, the object printed is considered
   to be a statement and it is terminated by ';' if appropriate.  */

int
dump_generic_node (pretty_printer *pp, tree node, int spc, int flags,
		   bool is_stmt)
{
  tree type;
  tree op0, op1;
  const char *str;
  bool is_expr;
  enum tree_code code;

  if (node == NULL_TREE)
    return spc;

  is_expr = EXPR_P (node);

  if (is_stmt && (flags & TDF_STMTADDR))
    pp_printf (pp, "<&%p> ", (void *)node);

  if ((flags & TDF_LINENO) && EXPR_HAS_LOCATION (node))
    dump_location (pp, EXPR_LOCATION (node));

  code = TREE_CODE (node);
  switch (code)
    {
    case ERROR_MARK:
      pp_string (pp, "<<< error >>>");
      break;

    case IDENTIFIER_NODE:
      pp_tree_identifier (pp, node);
      break;

    case TREE_LIST:
      while (node && node != error_mark_node)
	{
	  if (TREE_PURPOSE (node))
	    {
	      dump_generic_node (pp, TREE_PURPOSE (node), spc, flags, false);
	      pp_space (pp);
	    }
	  dump_generic_node (pp, TREE_VALUE (node), spc, flags, false);
	  node = TREE_CHAIN (node);
	  if (node && TREE_CODE (node) == TREE_LIST)
	    {
	      pp_comma (pp);
	      pp_space (pp);
	    }
	}
      break;

    case TREE_BINFO:
      dump_generic_node (pp, BINFO_TYPE (node), spc, flags, false);
      break;

    case TREE_VEC:
      {
	size_t i;
	if (TREE_VEC_LENGTH (node) > 0)
	  {
	    size_t len = TREE_VEC_LENGTH (node);
	    for (i = 0; i < len - 1; i++)
	      {
		dump_generic_node (pp, TREE_VEC_ELT (node, i), spc, flags,
				   false);
		pp_comma (pp);
		pp_space (pp);
	      }
	    dump_generic_node (pp, TREE_VEC_ELT (node, len - 1), spc,
			       flags, false);
	  }
      }
      break;

    case VOID_TYPE:
    case POINTER_BOUNDS_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
      {
	unsigned int quals = TYPE_QUALS (node);
	enum tree_code_class tclass;

	if (quals & TYPE_QUAL_ATOMIC)
	  pp_string (pp, "atomic ");
	if (quals & TYPE_QUAL_CONST)
	  pp_string (pp, "const ");
	else if (quals & TYPE_QUAL_VOLATILE)
	  pp_string (pp, "volatile ");
	else if (quals & TYPE_QUAL_RESTRICT)
	  pp_string (pp, "restrict ");

	if (!ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (node)))
	  {
	    pp_string (pp, "<address-space-");
	    pp_decimal_int (pp, TYPE_ADDR_SPACE (node));
	    pp_string (pp, "> ");
	  }

	tclass = TREE_CODE_CLASS (TREE_CODE (node));

	if (tclass == tcc_declaration)
	  {
	    if (DECL_NAME (node))
	      dump_decl_name (pp, node, flags);
	    else
              pp_string (pp, "<unnamed type decl>");
	  }
	else if (tclass == tcc_type)
	  {
	    if (TYPE_NAME (node))
	      {
		if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
		  pp_tree_identifier (pp, TYPE_NAME (node));
		else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
			 && DECL_NAME (TYPE_NAME (node)))
		  dump_decl_name (pp, TYPE_NAME (node), flags);
		else
		  pp_string (pp, "<unnamed type>");
	      }
	    else if (TREE_CODE (node) == VECTOR_TYPE)
	      {
		pp_string (pp, "vector");
		pp_left_paren (pp);
		pp_wide_integer (pp, TYPE_VECTOR_SUBPARTS (node));
		pp_string (pp, ") ");
		dump_generic_node (pp, TREE_TYPE (node), spc, flags, false);
	      }
	    else if (TREE_CODE (node) == INTEGER_TYPE)
	      {
		if (TYPE_PRECISION (node) == CHAR_TYPE_SIZE)
		  pp_string (pp, (TYPE_UNSIGNED (node)
				      ? "unsigned char"
				      : "signed char"));
		else if (TYPE_PRECISION (node) == SHORT_TYPE_SIZE)
		  pp_string (pp, (TYPE_UNSIGNED (node)
				      ? "unsigned short"
				      : "signed short"));
		else if (TYPE_PRECISION (node) == INT_TYPE_SIZE)
		  pp_string (pp, (TYPE_UNSIGNED (node)
				      ? "unsigned int"
				      : "signed int"));
		else if (TYPE_PRECISION (node) == LONG_TYPE_SIZE)
		  pp_string (pp, (TYPE_UNSIGNED (node)
				      ? "unsigned long"
				      : "signed long"));
		else if (TYPE_PRECISION (node) == LONG_LONG_TYPE_SIZE)
		  pp_string (pp, (TYPE_UNSIGNED (node)
				      ? "unsigned long long"
				      : "signed long long"));
		else if (TYPE_PRECISION (node) >= CHAR_TYPE_SIZE
			 && exact_log2 (TYPE_PRECISION (node)) != -1)
		  {
		    pp_string (pp, (TYPE_UNSIGNED (node) ? "uint" : "int"));
		    pp_decimal_int (pp, TYPE_PRECISION (node));
		    pp_string (pp, "_t");
		  }
		else
		  {
		    pp_string (pp, (TYPE_UNSIGNED (node)
					? "<unnamed-unsigned:"
					: "<unnamed-signed:"));
		    pp_decimal_int (pp, TYPE_PRECISION (node));
		    pp_greater (pp);
		  }
	      }
	    else if (TREE_CODE (node) == COMPLEX_TYPE)
	      {
		pp_string (pp, "__complex__ ");
		dump_generic_node (pp, TREE_TYPE (node), spc, flags, false);
	      }
	    else if (TREE_CODE (node) == REAL_TYPE)
	      {
		pp_string (pp, "<float:");
		pp_decimal_int (pp, TYPE_PRECISION (node));
		pp_greater (pp);
	      }
	    else if (TREE_CODE (node) == FIXED_POINT_TYPE)
	      {
		pp_string (pp, "<fixed-point-");
		pp_string (pp, TYPE_SATURATING (node) ? "sat:" : "nonsat:");
		pp_decimal_int (pp, TYPE_PRECISION (node));
		pp_greater (pp);
	      }
	    else if (TREE_CODE (node) == VOID_TYPE)
	      pp_string (pp, "void");
	    else
              pp_string (pp, "<unnamed type>");
	  }
	break;
      }

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      str = (TREE_CODE (node) == POINTER_TYPE ? "*" : "&");

      if (TREE_TYPE (node) == NULL)
        {
	  pp_string (pp, str);
          pp_string (pp, "<null type>");
        }
      else if (TREE_CODE (TREE_TYPE (node)) == FUNCTION_TYPE)
        {
	  tree fnode = TREE_TYPE (node);

	  dump_generic_node (pp, TREE_TYPE (fnode), spc, flags, false);
	  pp_space (pp);
	  pp_left_paren (pp);
	  pp_string (pp, str);
	  if (TYPE_NAME (node) && DECL_NAME (TYPE_NAME (node)))
	    dump_decl_name (pp, TYPE_NAME (node), flags);
	  else if (flags & TDF_NOUID)
	    pp_printf (pp, "<Txxxx>");
	  else
	    pp_printf (pp, "<T%x>", TYPE_UID (node));

	  pp_right_paren (pp);
	  dump_function_declaration (pp, fnode, spc, flags);
	}
      else
        {
	  unsigned int quals = TYPE_QUALS (node);

          dump_generic_node (pp, TREE_TYPE (node), spc, flags, false);
	  pp_space (pp);
	  pp_string (pp, str);

	  if (quals & TYPE_QUAL_CONST)
	    pp_string (pp, " const");
	  if (quals & TYPE_QUAL_VOLATILE)
	    pp_string (pp, " volatile");
	  if (quals & TYPE_QUAL_RESTRICT)
	    pp_string (pp, " restrict");

	  if (!ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (node)))
	    {
	      pp_string (pp, " <address-space-");
	      pp_decimal_int (pp, TYPE_ADDR_SPACE (node));
	      pp_greater (pp);
	    }

	  if (TYPE_REF_CAN_ALIAS_ALL (node))
	    pp_string (pp, " {ref-all}");
	}
      break;

    case OFFSET_TYPE:
      NIY;
      break;

    case MEM_REF:
      {
	if (integer_zerop (TREE_OPERAND (node, 1))
	    /* Dump the types of INTEGER_CSTs explicitly, for we can't
	       infer them and MEM_ATTR caching will share MEM_REFs
	       with differently-typed op0s.  */
	    && TREE_CODE (TREE_OPERAND (node, 0)) != INTEGER_CST
	    /* Released SSA_NAMES have no TREE_TYPE.  */
	    && TREE_TYPE (TREE_OPERAND (node, 0)) != NULL_TREE
	    /* Same pointer types, but ignoring POINTER_TYPE vs.
	       REFERENCE_TYPE.  */
	    && (TREE_TYPE (TREE_TYPE (TREE_OPERAND (node, 0)))
		== TREE_TYPE (TREE_TYPE (TREE_OPERAND (node, 1))))
	    && (TYPE_MODE (TREE_TYPE (TREE_OPERAND (node, 0)))
		== TYPE_MODE (TREE_TYPE (TREE_OPERAND (node, 1))))
	    && (TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (TREE_OPERAND (node, 0)))
		== TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (TREE_OPERAND (node, 1))))
	    /* Same value types ignoring qualifiers.  */
	    && (TYPE_MAIN_VARIANT (TREE_TYPE (node))
		== TYPE_MAIN_VARIANT
		    (TREE_TYPE (TREE_TYPE (TREE_OPERAND (node, 1)))))
	    && (!(flags & TDF_ALIAS)
		|| MR_DEPENDENCE_CLIQUE (node) == 0))
	  {
	    if (TREE_CODE (TREE_OPERAND (node, 0)) != ADDR_EXPR)
	      {
		pp_star (pp);
		dump_generic_node (pp, TREE_OPERAND (node, 0),
				   spc, flags, false);
	      }
	    else
	      dump_generic_node (pp,
				 TREE_OPERAND (TREE_OPERAND (node, 0), 0),
				 spc, flags, false);
	  }
	else
	  {
	    tree ptype;

	    pp_string (pp, "MEM[");
	    pp_left_paren (pp);
	    ptype = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_OPERAND (node, 1)));
	    dump_generic_node (pp, ptype,
			       spc, flags | TDF_SLIM, false);
	    pp_right_paren (pp);
	    dump_generic_node (pp, TREE_OPERAND (node, 0),
			       spc, flags, false);
	    if (!integer_zerop (TREE_OPERAND (node, 1)))
	      {
		pp_string (pp, " + ");
		dump_generic_node (pp, TREE_OPERAND (node, 1),
				   spc, flags, false);
	      }
	    if ((flags & TDF_ALIAS)
		&& MR_DEPENDENCE_CLIQUE (node) != 0)
	      {
		pp_string (pp, " clique ");
		pp_unsigned_wide_integer (pp, MR_DEPENDENCE_CLIQUE (node));
		pp_string (pp, " base ");
		pp_unsigned_wide_integer (pp, MR_DEPENDENCE_BASE (node));
	      }
	    pp_right_bracket (pp);
	  }
	break;
      }

    case TARGET_MEM_REF:
      {
	const char *sep = "";
	tree tmp;

	pp_string (pp, "MEM[");

	if (TREE_CODE (TMR_BASE (node)) == ADDR_EXPR)
	  {
	    pp_string (pp, sep);
	    sep = ", ";
	    pp_string (pp, "symbol: ");
	    dump_generic_node (pp, TREE_OPERAND (TMR_BASE (node), 0),
			       spc, flags, false);
	  }
	else
	  {
	    pp_string (pp, sep);
	    sep = ", ";
	    pp_string (pp, "base: ");
	    dump_generic_node (pp, TMR_BASE (node), spc, flags, false);
	  }
	tmp = TMR_INDEX2 (node);
	if (tmp)
	  {
	    pp_string (pp, sep);
	    sep = ", ";
	    pp_string (pp, "base: ");
	    dump_generic_node (pp, tmp, spc, flags, false);
	  }
	tmp = TMR_INDEX (node);
	if (tmp)
	  {
	    pp_string (pp, sep);
	    sep = ", ";
	    pp_string (pp, "index: ");
	    dump_generic_node (pp, tmp, spc, flags, false);
	  }
	tmp = TMR_STEP (node);
	if (tmp)
	  {
	    pp_string (pp, sep);
	    sep = ", ";
	    pp_string (pp, "step: ");
	    dump_generic_node (pp, tmp, spc, flags, false);
	  }
	tmp = TMR_OFFSET (node);
	if (tmp)
	  {
	    pp_string (pp, sep);
	    sep = ", ";
	    pp_string (pp, "offset: ");
	    dump_generic_node (pp, tmp, spc, flags, false);
	  }
	pp_right_bracket (pp);
      }
      break;

    case ARRAY_TYPE:
      {
	tree tmp;

	/* Print the innermost component type.  */
	for (tmp = TREE_TYPE (node); TREE_CODE (tmp) == ARRAY_TYPE;
	     tmp = TREE_TYPE (tmp))
	  ;
	dump_generic_node (pp, tmp, spc, flags, false);

	/* Print the dimensions.  */
	for (tmp = node; TREE_CODE (tmp) == ARRAY_TYPE; tmp = TREE_TYPE (tmp))
	  dump_array_domain (pp, TYPE_DOMAIN (tmp), spc, flags);
	break;
      }

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	unsigned int quals = TYPE_QUALS (node);

	if (quals & TYPE_QUAL_ATOMIC)
	  pp_string (pp, "atomic ");
	if (quals & TYPE_QUAL_CONST)
	  pp_string (pp, "const ");
	if (quals & TYPE_QUAL_VOLATILE)
	  pp_string (pp, "volatile ");

        /* Print the name of the structure.  */
        if (TREE_CODE (node) == RECORD_TYPE)
	  pp_string (pp, "struct ");
        else if (TREE_CODE (node) == UNION_TYPE)
	  pp_string (pp, "union ");

        if (TYPE_NAME (node))
	  dump_generic_node (pp, TYPE_NAME (node), spc, flags, false);
	else if (!(flags & TDF_SLIM))
	  /* FIXME: If we eliminate the 'else' above and attempt
	     to show the fields for named types, we may get stuck
	     following a cycle of pointers to structs.  The alleged
	     self-reference check in print_struct_decl will not detect
	     cycles involving more than one pointer or struct type.  */
	  print_struct_decl (pp, node, spc, flags);
        break;
      }

    case LANG_TYPE:
      NIY;
      break;

    case INTEGER_CST:
      if (TREE_CODE (TREE_TYPE (node)) == POINTER_TYPE)
	{
	  /* In the case of a pointer, one may want to divide by the
	     size of the pointed-to type.  Unfortunately, this not
	     straightforward.  The C front-end maps expressions

	     (int *) 5
	     int *p; (p + 5)

	     in such a way that the two INTEGER_CST nodes for "5" have
	     different values but identical types.  In the latter
	     case, the 5 is multiplied by sizeof (int) in c-common.c
	     (pointer_int_sum) to convert it to a byte address, and
	     yet the type of the node is left unchanged.  Argh.  What
	     is consistent though is that the number value corresponds
	     to bytes (UNITS) offset.

             NB: Neither of the following divisors can be trivially
             used to recover the original literal:

             TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (node)))
	     TYPE_PRECISION (TREE_TYPE (TREE_TYPE (node)))  */
	  pp_wide_integer (pp, TREE_INT_CST_LOW (node));
	  pp_string (pp, "B"); /* pseudo-unit */
	}
      else if (tree_fits_shwi_p (node))
	pp_wide_integer (pp, tree_to_shwi (node));
      else if (tree_fits_uhwi_p (node))
	pp_unsigned_wide_integer (pp, tree_to_uhwi (node));
      else
	{
	  wide_int val = node;

	  if (wi::neg_p (val, TYPE_SIGN (TREE_TYPE (node))))
	    {
	      pp_minus (pp);
	      val = -val;
	    }
	  print_hex (val, pp_buffer (pp)->digit_buffer);
	  pp_string (pp, pp_buffer (pp)->digit_buffer);
	}
      if (TREE_OVERFLOW (node))
	pp_string (pp, "(OVF)");
      break;

    case REAL_CST:
      /* Code copied from print_node.  */
      {
	REAL_VALUE_TYPE d;
	if (TREE_OVERFLOW (node))
	  pp_string (pp, " overflow");

	d = TREE_REAL_CST (node);
	if (REAL_VALUE_ISINF (d))
	  pp_string (pp, REAL_VALUE_NEGATIVE (d) ? " -Inf" : " Inf");
	else if (REAL_VALUE_ISNAN (d))
	  pp_string (pp, " Nan");
	else
	  {
	    char string[100];
	    real_to_decimal (string, &d, sizeof (string), 0, 1);
	    pp_string (pp, string);
	  }
	break;
      }

    case FIXED_CST:
      {
	char string[100];
	fixed_to_decimal (string, TREE_FIXED_CST_PTR (node), sizeof (string));
	pp_string (pp, string);
	break;
      }

    case COMPLEX_CST:
      pp_string (pp, "__complex__ (");
      dump_generic_node (pp, TREE_REALPART (node), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_IMAGPART (node), spc, flags, false);
      pp_right_paren (pp);
      break;

    case STRING_CST:
      pp_string (pp, "\"");
      pretty_print_string (pp, TREE_STRING_POINTER (node));
      pp_string (pp, "\"");
      break;

    case VECTOR_CST:
      {
	unsigned i;
	pp_string (pp, "{ ");
	for (i = 0; i < VECTOR_CST_NELTS (node); ++i)
	  {
	    if (i != 0)
	      pp_string (pp, ", ");
	    dump_generic_node (pp, VECTOR_CST_ELT (node, i),
			       spc, flags, false);
	  }
	pp_string (pp, " }");
      }
      break;

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      dump_generic_node (pp, TREE_TYPE (node), spc, flags, false);
      pp_space (pp);
      if (TREE_CODE (node) == METHOD_TYPE)
	{
	  if (TYPE_METHOD_BASETYPE (node))
	    dump_decl_name (pp, TYPE_NAME (TYPE_METHOD_BASETYPE (node)),
			    flags);
	  else
	    pp_string (pp, "<null method basetype>");
	  pp_colon_colon (pp);
	}
      if (TYPE_NAME (node) && DECL_NAME (TYPE_NAME (node)))
	dump_decl_name (pp, TYPE_NAME (node), flags);
      else if (flags & TDF_NOUID)
	pp_printf (pp, "<Txxxx>");
      else
	pp_printf (pp, "<T%x>", TYPE_UID (node));
      dump_function_declaration (pp, node, spc, flags);
      break;

    case FUNCTION_DECL:
    case CONST_DECL:
      dump_decl_name (pp, node, flags);
      break;

    case LABEL_DECL:
      if (DECL_NAME (node))
	dump_decl_name (pp, node, flags);
      else if (LABEL_DECL_UID (node) != -1)
	pp_printf (pp, "<L%d>", (int) LABEL_DECL_UID (node));
      else
	{
	  if (flags & TDF_NOUID)
	    pp_string (pp, "<D.xxxx>");
	  else
	    pp_printf (pp, "<D.%u>", DECL_UID (node));
	}
      break;

    case TYPE_DECL:
      if (DECL_IS_BUILTIN (node))
	{
	  /* Don't print the declaration of built-in types.  */
	  break;
	}
      if (DECL_NAME (node))
	dump_decl_name (pp, node, flags);
      else if (TYPE_NAME (TREE_TYPE (node)) != node)
	{
	  if ((TREE_CODE (TREE_TYPE (node)) == RECORD_TYPE
	       || TREE_CODE (TREE_TYPE (node)) == UNION_TYPE)
	      && TYPE_METHODS (TREE_TYPE (node)))
	    {
	      /* The type is a c++ class: all structures have at least
		 4 methods.  */
	      pp_string (pp, "class ");
	      dump_generic_node (pp, TREE_TYPE (node), spc, flags, false);
	    }
	  else
	    {
	      pp_string (pp,
			 (TREE_CODE (TREE_TYPE (node)) == UNION_TYPE
			  ? "union" : "struct "));
	      dump_generic_node (pp, TREE_TYPE (node), spc, flags, false);
	    }
	}
      else
	pp_string (pp, "<anon>");
      break;

    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case DEBUG_EXPR_DECL:
    case NAMESPACE_DECL:
    case NAMELIST_DECL:
      dump_decl_name (pp, node, flags);
      break;

    case RESULT_DECL:
      pp_string (pp, "<retval>");
      break;

    case COMPONENT_REF:
      op0 = TREE_OPERAND (node, 0);
      str = ".";
      if (op0
	  && (TREE_CODE (op0) == INDIRECT_REF
	      || (TREE_CODE (op0) == MEM_REF
		  && TREE_CODE (TREE_OPERAND (op0, 0)) != ADDR_EXPR
		  && integer_zerop (TREE_OPERAND (op0, 1))
		  /* Dump the types of INTEGER_CSTs explicitly, for we
		     can't infer them and MEM_ATTR caching will share
		     MEM_REFs with differently-typed op0s.  */
		  && TREE_CODE (TREE_OPERAND (op0, 0)) != INTEGER_CST
		  /* Released SSA_NAMES have no TREE_TYPE.  */
		  && TREE_TYPE (TREE_OPERAND (op0, 0)) != NULL_TREE
		  /* Same pointer types, but ignoring POINTER_TYPE vs.
		     REFERENCE_TYPE.  */
		  && (TREE_TYPE (TREE_TYPE (TREE_OPERAND (op0, 0)))
		      == TREE_TYPE (TREE_TYPE (TREE_OPERAND (op0, 1))))
		  && (TYPE_MODE (TREE_TYPE (TREE_OPERAND (op0, 0)))
		      == TYPE_MODE (TREE_TYPE (TREE_OPERAND (op0, 1))))
		  && (TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (TREE_OPERAND (op0, 0)))
		      == TYPE_REF_CAN_ALIAS_ALL (TREE_TYPE (TREE_OPERAND (op0, 1))))
		  /* Same value types ignoring qualifiers.  */
		  && (TYPE_MAIN_VARIANT (TREE_TYPE (op0))
		      == TYPE_MAIN_VARIANT
		          (TREE_TYPE (TREE_TYPE (TREE_OPERAND (op0, 1)))))
		  && MR_DEPENDENCE_CLIQUE (op0) == 0)))
	{
	  op0 = TREE_OPERAND (op0, 0);
	  str = "->";
	}
      if (op_prio (op0) < op_prio (node))
	pp_left_paren (pp);
      dump_generic_node (pp, op0, spc, flags, false);
      if (op_prio (op0) < op_prio (node))
	pp_right_paren (pp);
      pp_string (pp, str);
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      op0 = component_ref_field_offset (node);
      if (op0 && TREE_CODE (op0) != INTEGER_CST)
	{
	  pp_string (pp, "{off: ");
	      dump_generic_node (pp, op0, spc, flags, false);
	      pp_right_brace (pp);
	}
      break;

    case BIT_FIELD_REF:
      pp_string (pp, "BIT_FIELD_REF <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 2), spc, flags, false);
      pp_greater (pp);
      break;

    case BIT_INSERT_EXPR:
      pp_string (pp, "BIT_INSERT_EXPR <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 2), spc, flags, false);
      pp_string (pp, " (");
      if (INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (node, 1))))
	pp_decimal_int (pp,
			TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (node, 1))));
      else
	dump_generic_node (pp, TYPE_SIZE (TREE_TYPE (TREE_OPERAND (node, 1))),
			   spc, flags, false);
      pp_string (pp, " bits)>");
      break;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      op0 = TREE_OPERAND (node, 0);
      if (op_prio (op0) < op_prio (node))
	pp_left_paren (pp);
      dump_generic_node (pp, op0, spc, flags, false);
      if (op_prio (op0) < op_prio (node))
	pp_right_paren (pp);
      pp_left_bracket (pp);
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      if (TREE_CODE (node) == ARRAY_RANGE_REF)
	pp_string (pp, " ...");
      pp_right_bracket (pp);

      op0 = array_ref_low_bound (node);
      op1 = array_ref_element_size (node);

      if (!integer_zerop (op0)
	  || TREE_OPERAND (node, 2)
	  || TREE_OPERAND (node, 3))
	{
	  pp_string (pp, "{lb: ");
	  dump_generic_node (pp, op0, spc, flags, false);
	  pp_string (pp, " sz: ");
	  dump_generic_node (pp, op1, spc, flags, false);
	  pp_right_brace (pp);
	}
      break;

    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT ix;
	tree field, val;
	bool is_struct_init = false;
	bool is_array_init = false;
	widest_int curidx;
	pp_left_brace (pp);
	if (TREE_CLOBBER_P (node))
	  pp_string (pp, "CLOBBER");
	else if (TREE_CODE (TREE_TYPE (node)) == RECORD_TYPE
		 || TREE_CODE (TREE_TYPE (node)) == UNION_TYPE)
	  is_struct_init = true;
        else if (TREE_CODE (TREE_TYPE (node)) == ARRAY_TYPE
		 && TYPE_DOMAIN (TREE_TYPE (node))
		 && TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (node)))
		 && TREE_CODE (TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (node))))
		    == INTEGER_CST)
	  {
	    tree minv = TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (node)));
	    is_array_init = true;
	    curidx = wi::to_widest (minv);
	  }
	FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (node), ix, field, val)
	  {
	    if (field)
	      {
		if (is_struct_init)
		  {
		    pp_dot (pp);
		    dump_generic_node (pp, field, spc, flags, false);
		    pp_equal (pp);
		  }
		else if (is_array_init
			 && (TREE_CODE (field) != INTEGER_CST
			     || curidx != wi::to_widest (field)))
		  {
		    pp_left_bracket (pp);
		    if (TREE_CODE (field) == RANGE_EXPR)
		      {
			dump_generic_node (pp, TREE_OPERAND (field, 0), spc,
					   flags, false);
			pp_string (pp, " ... ");
			dump_generic_node (pp, TREE_OPERAND (field, 1), spc,
					   flags, false);
			if (TREE_CODE (TREE_OPERAND (field, 1)) == INTEGER_CST)
			  curidx = wi::to_widest (TREE_OPERAND (field, 1));
		      }
		    else
		      dump_generic_node (pp, field, spc, flags, false);
		    if (TREE_CODE (field) == INTEGER_CST)
		      curidx = wi::to_widest (field);
		    pp_string (pp, "]=");
		  }
	      }
            if (is_array_init)
	      curidx += 1;
	    if (val && TREE_CODE (val) == ADDR_EXPR)
	      if (TREE_CODE (TREE_OPERAND (val, 0)) == FUNCTION_DECL)
		val = TREE_OPERAND (val, 0);
	    if (val && TREE_CODE (val) == FUNCTION_DECL)
		dump_decl_name (pp, val, flags);
	    else
		dump_generic_node (pp, val, spc, flags, false);
	    if (ix != vec_safe_length (CONSTRUCTOR_ELTS (node)) - 1)
	      {
		pp_comma (pp);
		pp_space (pp);
	      }
	  }
	pp_right_brace (pp);
      }
      break;

    case COMPOUND_EXPR:
      {
	tree *tp;
	if (flags & TDF_SLIM)
	  {
	    pp_string (pp, "<COMPOUND_EXPR>");
	    break;
	  }

	dump_generic_node (pp, TREE_OPERAND (node, 0),
			   spc, flags, !(flags & TDF_SLIM));
	if (flags & TDF_SLIM)
	  newline_and_indent (pp, spc);
	else
	  {
	    pp_comma (pp);
	    pp_space (pp);
	  }

	for (tp = &TREE_OPERAND (node, 1);
	     TREE_CODE (*tp) == COMPOUND_EXPR;
	     tp = &TREE_OPERAND (*tp, 1))
	  {
	    dump_generic_node (pp, TREE_OPERAND (*tp, 0),
			       spc, flags, !(flags & TDF_SLIM));
	    if (flags & TDF_SLIM)
	      newline_and_indent (pp, spc);
	    else
	      {
	        pp_comma (pp);
	        pp_space (pp);
	      }
	  }

	dump_generic_node (pp, *tp, spc, flags, !(flags & TDF_SLIM));
      }
      break;

    case STATEMENT_LIST:
      {
	tree_stmt_iterator si;
	bool first = true;

	if (flags & TDF_SLIM)
	  {
	    pp_string (pp, "<STATEMENT_LIST>");
	    break;
	  }

	for (si = tsi_start (node); !tsi_end_p (si); tsi_next (&si))
	  {
	    if (!first)
	      newline_and_indent (pp, spc);
	    else
	      first = false;
	    dump_generic_node (pp, tsi_stmt (si), spc, flags, true);
	  }
      }
      break;

    case MODIFY_EXPR:
    case INIT_EXPR:
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags,
	  		 false);
      pp_space (pp);
      pp_equal (pp);
      pp_space (pp);
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags,
	  		 false);
      break;

    case TARGET_EXPR:
      pp_string (pp, "TARGET_EXPR <");
      dump_generic_node (pp, TARGET_EXPR_SLOT (node), spc, flags, false);
      pp_comma (pp);
      pp_space (pp);
      dump_generic_node (pp, TARGET_EXPR_INITIAL (node), spc, flags, false);
      pp_greater (pp);
      break;

    case DECL_EXPR:
      print_declaration (pp, DECL_EXPR_DECL (node), spc, flags);
      is_stmt = false;
      break;

    case COND_EXPR:
      if (TREE_TYPE (node) == NULL || TREE_TYPE (node) == void_type_node)
	{
	  pp_string (pp, "if (");
	  dump_generic_node (pp, COND_EXPR_COND (node), spc, flags, false);
	  pp_right_paren (pp);
	  /* The lowered cond_exprs should always be printed in full.  */
	  if (COND_EXPR_THEN (node)
	      && (IS_EMPTY_STMT (COND_EXPR_THEN (node))
		  || TREE_CODE (COND_EXPR_THEN (node)) == GOTO_EXPR)
	      && COND_EXPR_ELSE (node)
	      && (IS_EMPTY_STMT (COND_EXPR_ELSE (node))
		  || TREE_CODE (COND_EXPR_ELSE (node)) == GOTO_EXPR))
	    {
	      pp_space (pp);
	      dump_generic_node (pp, COND_EXPR_THEN (node),
				 0, flags, true);
	      if (!IS_EMPTY_STMT (COND_EXPR_ELSE (node)))
		{
		  pp_string (pp, " else ");
		  dump_generic_node (pp, COND_EXPR_ELSE (node),
				     0, flags, true);
		}
	    }
	  else if (!(flags & TDF_SLIM))
	    {
	      /* Output COND_EXPR_THEN.  */
	      if (COND_EXPR_THEN (node))
		{
		  newline_and_indent (pp, spc+2);
		  pp_left_brace (pp);
		  newline_and_indent (pp, spc+4);
		  dump_generic_node (pp, COND_EXPR_THEN (node), spc+4,
				     flags, true);
		  newline_and_indent (pp, spc+2);
		  pp_right_brace (pp);
		}

	      /* Output COND_EXPR_ELSE.  */
	      if (COND_EXPR_ELSE (node)
		  && !IS_EMPTY_STMT (COND_EXPR_ELSE (node)))
		{
		  newline_and_indent (pp, spc);
		  pp_string (pp, "else");
		  newline_and_indent (pp, spc+2);
		  pp_left_brace (pp);
		  newline_and_indent (pp, spc+4);
		  dump_generic_node (pp, COND_EXPR_ELSE (node), spc+4,
			             flags, true);
		  newline_and_indent (pp, spc+2);
		  pp_right_brace (pp);
		}
	    }
	  is_expr = false;
	}
      else
	{
	  dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
	  pp_space (pp);
	  pp_question (pp);
	  pp_space (pp);
	  dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
	  pp_space (pp);
	  pp_colon (pp);
	  pp_space (pp);
	  dump_generic_node (pp, TREE_OPERAND (node, 2), spc, flags, false);
	}
      break;

    case BIND_EXPR:
      pp_left_brace (pp);
      if (!(flags & TDF_SLIM))
	{
	  if (BIND_EXPR_VARS (node))
	    {
	      pp_newline (pp);

	      for (op0 = BIND_EXPR_VARS (node); op0; op0 = DECL_CHAIN (op0))
		{
		  print_declaration (pp, op0, spc+2, flags);
		  pp_newline (pp);
		}
	    }

	  newline_and_indent (pp, spc+2);
	  dump_generic_node (pp, BIND_EXPR_BODY (node), spc+2, flags, true);
	  newline_and_indent (pp, spc);
	  pp_right_brace (pp);
	}
      is_expr = false;
      break;

    case CALL_EXPR:
      if (CALL_EXPR_FN (node) != NULL_TREE)
	print_call_name (pp, CALL_EXPR_FN (node), flags);
      else
	pp_string (pp, internal_fn_name (CALL_EXPR_IFN (node)));

      /* Print parameters.  */
      pp_space (pp);
      pp_left_paren (pp);
      {
	tree arg;
	call_expr_arg_iterator iter;
	FOR_EACH_CALL_EXPR_ARG (arg, iter, node)
	  {
	    dump_generic_node (pp, arg, spc, flags, false);
	    if (more_call_expr_args_p (&iter))
	      {
		pp_comma (pp);
		pp_space (pp);
	      }
	  }
      }
      if (CALL_EXPR_VA_ARG_PACK (node))
	{
	  if (call_expr_nargs (node) > 0)
	    {
	      pp_comma (pp);
	      pp_space (pp);
	    }
	  pp_string (pp, "__builtin_va_arg_pack ()");
	}
      pp_right_paren (pp);

      op1 = CALL_EXPR_STATIC_CHAIN (node);
      if (op1)
	{
	  pp_string (pp, " [static-chain: ");
	  dump_generic_node (pp, op1, spc, flags, false);
	  pp_right_bracket (pp);
	}

      if (CALL_EXPR_RETURN_SLOT_OPT (node))
	pp_string (pp, " [return slot optimization]");
      if (CALL_EXPR_TAILCALL (node))
	pp_string (pp, " [tail call]");
      break;

    case WITH_CLEANUP_EXPR:
      NIY;
      break;

    case CLEANUP_POINT_EXPR:
      pp_string (pp, "<<cleanup_point ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ">>");
      break;

    case PLACEHOLDER_EXPR:
      pp_string (pp, "<PLACEHOLDER_EXPR ");
      dump_generic_node (pp, TREE_TYPE (node), spc, flags, false);
      pp_greater (pp);
      break;

      /* Binary arithmetic and logic expressions.  */
    case WIDEN_SUM_EXPR:
    case WIDEN_MULT_EXPR:
    case MULT_EXPR:
    case MULT_HIGHPART_EXPR:
    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case WIDEN_LSHIFT_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case ORDERED_EXPR:
    case UNORDERED_EXPR:
      {
	const char *op = op_symbol (node);
	op0 = TREE_OPERAND (node, 0);
	op1 = TREE_OPERAND (node, 1);

	/* When the operands are expressions with less priority,
	   keep semantics of the tree representation.  */
	if (op_prio (op0) <= op_prio (node))
	  {
	    pp_left_paren (pp);
	    dump_generic_node (pp, op0, spc, flags, false);
	    pp_right_paren (pp);
	  }
	else
	  dump_generic_node (pp, op0, spc, flags, false);

	pp_space (pp);
	pp_string (pp, op);
	pp_space (pp);

	/* When the operands are expressions with less priority,
	   keep semantics of the tree representation.  */
	if (op_prio (op1) <= op_prio (node))
	  {
	    pp_left_paren (pp);
	    dump_generic_node (pp, op1, spc, flags, false);
	    pp_right_paren (pp);
	  }
	else
	  dump_generic_node (pp, op1, spc, flags, false);
      }
      break;

      /* Unary arithmetic and logic expressions.  */
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case ADDR_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case INDIRECT_REF:
      if (TREE_CODE (node) == ADDR_EXPR
	  && (TREE_CODE (TREE_OPERAND (node, 0)) == STRING_CST
	      || TREE_CODE (TREE_OPERAND (node, 0)) == FUNCTION_DECL))
	;	/* Do not output '&' for strings and function pointers.  */
      else
	pp_string (pp, op_symbol (node));

      if (op_prio (TREE_OPERAND (node, 0)) < op_prio (node))
	{
	  pp_left_paren (pp);
	  dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
	  pp_right_paren (pp);
	}
      else
	dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      break;

    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      if (op_prio (TREE_OPERAND (node, 0)) < op_prio (node))
	{
	  pp_left_paren (pp);
	  dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
	  pp_right_paren (pp);
	}
      else
	dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, op_symbol (node));
      break;

    case MIN_EXPR:
      pp_string (pp, "MIN_EXPR <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_greater (pp);
      break;

    case MAX_EXPR:
      pp_string (pp, "MAX_EXPR <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_greater (pp);
      break;

    case ABS_EXPR:
      pp_string (pp, "ABS_EXPR <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_greater (pp);
      break;

    case RANGE_EXPR:
      NIY;
      break;

    case ADDR_SPACE_CONVERT_EXPR:
    case FIXED_CONVERT_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    CASE_CONVERT:
      type = TREE_TYPE (node);
      op0 = TREE_OPERAND (node, 0);
      if (type != TREE_TYPE (op0))
	{
	  pp_left_paren (pp);
	  dump_generic_node (pp, type, spc, flags, false);
	  pp_string (pp, ") ");
	}
      if (op_prio (op0) < op_prio (node))
	pp_left_paren (pp);
      dump_generic_node (pp, op0, spc, flags, false);
      if (op_prio (op0) < op_prio (node))
	pp_right_paren (pp);
      break;

    case VIEW_CONVERT_EXPR:
      pp_string (pp, "VIEW_CONVERT_EXPR<");
      dump_generic_node (pp, TREE_TYPE (node), spc, flags, false);
      pp_string (pp, ">(");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_right_paren (pp);
      break;

    case PAREN_EXPR:
      pp_string (pp, "((");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, "))");
      break;

    case NON_LVALUE_EXPR:
      pp_string (pp, "NON_LVALUE_EXPR <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_greater (pp);
      break;

    case SAVE_EXPR:
      pp_string (pp, "SAVE_EXPR <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_greater (pp);
      break;

    case COMPLEX_EXPR:
      pp_string (pp, "COMPLEX_EXPR <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_greater (pp);
      break;

    case CONJ_EXPR:
      pp_string (pp, "CONJ_EXPR <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_greater (pp);
      break;

    case REALPART_EXPR:
      pp_string (pp, "REALPART_EXPR <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_greater (pp);
      break;

    case IMAGPART_EXPR:
      pp_string (pp, "IMAGPART_EXPR <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_greater (pp);
      break;

    case VA_ARG_EXPR:
      pp_string (pp, "VA_ARG_EXPR <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_greater (pp);
      break;

    case TRY_FINALLY_EXPR:
    case TRY_CATCH_EXPR:
      pp_string (pp, "try");
      newline_and_indent (pp, spc+2);
      pp_left_brace (pp);
      newline_and_indent (pp, spc+4);
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc+4, flags, true);
      newline_and_indent (pp, spc+2);
      pp_right_brace (pp);
      newline_and_indent (pp, spc);
      pp_string (pp,
			 (TREE_CODE (node) == TRY_CATCH_EXPR) ? "catch" : "finally");
      newline_and_indent (pp, spc+2);
      pp_left_brace (pp);
      newline_and_indent (pp, spc+4);
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc+4, flags, true);
      newline_and_indent (pp, spc+2);
      pp_right_brace (pp);
      is_expr = false;
      break;

    case CATCH_EXPR:
      pp_string (pp, "catch (");
      dump_generic_node (pp, CATCH_TYPES (node), spc+2, flags, false);
      pp_right_paren (pp);
      newline_and_indent (pp, spc+2);
      pp_left_brace (pp);
      newline_and_indent (pp, spc+4);
      dump_generic_node (pp, CATCH_BODY (node), spc+4, flags, true);
      newline_and_indent (pp, spc+2);
      pp_right_brace (pp);
      is_expr = false;
      break;

    case EH_FILTER_EXPR:
      pp_string (pp, "<<<eh_filter (");
      dump_generic_node (pp, EH_FILTER_TYPES (node), spc+2, flags, false);
      pp_string (pp, ")>>>");
      newline_and_indent (pp, spc+2);
      pp_left_brace (pp);
      newline_and_indent (pp, spc+4);
      dump_generic_node (pp, EH_FILTER_FAILURE (node), spc+4, flags, true);
      newline_and_indent (pp, spc+2);
      pp_right_brace (pp);
      is_expr = false;
      break;

    case LABEL_EXPR:
      op0 = TREE_OPERAND (node, 0);
      /* If this is for break or continue, don't bother printing it.  */
      if (DECL_NAME (op0))
	{
	  const char *name = IDENTIFIER_POINTER (DECL_NAME (op0));
	  if (strcmp (name, "break") == 0
	      || strcmp (name, "continue") == 0)
	    break;
	}
      dump_generic_node (pp, op0, spc, flags, false);
      pp_colon (pp);
      if (DECL_NONLOCAL (op0))
	pp_string (pp, " [non-local]");
      break;

    case LOOP_EXPR:
      pp_string (pp, "while (1)");
      if (!(flags & TDF_SLIM))
	{
	  newline_and_indent (pp, spc+2);
	  pp_left_brace (pp);
	  newline_and_indent (pp, spc+4);
	  dump_generic_node (pp, LOOP_EXPR_BODY (node), spc+4, flags, true);
	  newline_and_indent (pp, spc+2);
	  pp_right_brace (pp);
	}
      is_expr = false;
      break;

    case PREDICT_EXPR:
      pp_string (pp, "// predicted ");
      if (PREDICT_EXPR_OUTCOME (node))
        pp_string (pp, "likely by ");
      else
        pp_string (pp, "unlikely by ");
      pp_string (pp, predictor_name (PREDICT_EXPR_PREDICTOR (node)));
      pp_string (pp, " predictor.");
      break;

    case ANNOTATE_EXPR:
      pp_string (pp, "ANNOTATE_EXPR <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      switch ((enum annot_expr_kind) TREE_INT_CST_LOW (TREE_OPERAND (node, 1)))
	{
	case annot_expr_ivdep_kind:
	  pp_string (pp, ", ivdep");
	  break;
	case annot_expr_no_vector_kind:
	  pp_string (pp, ", no-vector");
	  break;
	case annot_expr_vector_kind:
	  pp_string (pp, ", vector");
	  break;
	default:
	  gcc_unreachable ();
	}
      pp_greater (pp);
      break;

    case RETURN_EXPR:
      pp_string (pp, "return");
      op0 = TREE_OPERAND (node, 0);
      if (op0)
	{
	  pp_space (pp);
	  if (TREE_CODE (op0) == MODIFY_EXPR)
	    dump_generic_node (pp, TREE_OPERAND (op0, 1),
			       spc, flags, false);
	  else
	    dump_generic_node (pp, op0, spc, flags, false);
	}
      break;

    case EXIT_EXPR:
      pp_string (pp, "if (");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ") break");
      break;

    case SWITCH_EXPR:
      pp_string (pp, "switch (");
      dump_generic_node (pp, SWITCH_COND (node), spc, flags, false);
      pp_right_paren (pp);
      if (!(flags & TDF_SLIM))
	{
	  newline_and_indent (pp, spc+2);
	  pp_left_brace (pp);
	  if (SWITCH_BODY (node))
	    {
	      newline_and_indent (pp, spc+4);
	      dump_generic_node (pp, SWITCH_BODY (node), spc+4, flags,
		                 true);
	    }
	  else
	    {
	      tree vec = SWITCH_LABELS (node);
	      size_t i, n = TREE_VEC_LENGTH (vec);
	      for (i = 0; i < n; ++i)
		{
		  tree elt = TREE_VEC_ELT (vec, i);
		  newline_and_indent (pp, spc+4);
		  if (elt)
		    {
		      dump_generic_node (pp, elt, spc+4, flags, false);
		      pp_string (pp, " goto ");
		      dump_generic_node (pp, CASE_LABEL (elt), spc+4,
					 flags, true);
		      pp_semicolon (pp);
		    }
		  else
		    pp_string (pp, "case ???: goto ???;");
		}
	    }
	  newline_and_indent (pp, spc+2);
	  pp_right_brace (pp);
	}
      is_expr = false;
      break;

    case GOTO_EXPR:
      op0 = GOTO_DESTINATION (node);
      if (TREE_CODE (op0) != SSA_NAME && DECL_P (op0) && DECL_NAME (op0))
	{
	  const char *name = IDENTIFIER_POINTER (DECL_NAME (op0));
	  if (strcmp (name, "break") == 0
	      || strcmp (name, "continue") == 0)
	    {
	      pp_string (pp, name);
	      break;
	    }
	}
      pp_string (pp, "goto ");
      dump_generic_node (pp, op0, spc, flags, false);
      break;

    case ASM_EXPR:
      pp_string (pp, "__asm__");
      if (ASM_VOLATILE_P (node))
	pp_string (pp, " __volatile__");
      pp_left_paren (pp);
      dump_generic_node (pp, ASM_STRING (node), spc, flags, false);
      pp_colon (pp);
      dump_generic_node (pp, ASM_OUTPUTS (node), spc, flags, false);
      pp_colon (pp);
      dump_generic_node (pp, ASM_INPUTS (node), spc, flags, false);
      if (ASM_CLOBBERS (node))
	{
	  pp_colon (pp);
	  dump_generic_node (pp, ASM_CLOBBERS (node), spc, flags, false);
	}
      pp_right_paren (pp);
      break;

    case CASE_LABEL_EXPR:
      if (CASE_LOW (node) && CASE_HIGH (node))
	{
	  pp_string (pp, "case ");
	  dump_generic_node (pp, CASE_LOW (node), spc, flags, false);
	  pp_string (pp, " ... ");
	  dump_generic_node (pp, CASE_HIGH (node), spc, flags, false);
	}
      else if (CASE_LOW (node))
	{
	  pp_string (pp, "case ");
	  dump_generic_node (pp, CASE_LOW (node), spc, flags, false);
	}
      else
	pp_string (pp, "default");
      pp_colon (pp);
      break;

    case OBJ_TYPE_REF:
      pp_string (pp, "OBJ_TYPE_REF(");
      dump_generic_node (pp, OBJ_TYPE_REF_EXPR (node), spc, flags, false);
      pp_semicolon (pp);
      if (!(flags & TDF_SLIM) && virtual_method_call_p (node))
	{
	  pp_string (pp, "(");
	  dump_generic_node (pp, obj_type_ref_class (node), spc, flags, false);
	  pp_string (pp, ")");
	}
      dump_generic_node (pp, OBJ_TYPE_REF_OBJECT (node), spc, flags, false);
      pp_arrow (pp);
      dump_generic_node (pp, OBJ_TYPE_REF_TOKEN (node), spc, flags, false);
      pp_right_paren (pp);
      break;

    case SSA_NAME:
      if (SSA_NAME_IDENTIFIER (node))
	{
	  if ((flags & TDF_NOUID)
	      && SSA_NAME_VAR (node)
	      && DECL_NAMELESS (SSA_NAME_VAR (node)))
	    dump_fancy_name (pp, SSA_NAME_IDENTIFIER (node));
	  else
	    dump_generic_node (pp, SSA_NAME_IDENTIFIER (node),
			       spc, flags, false);
	}
      pp_underscore (pp);
      pp_decimal_int (pp, SSA_NAME_VERSION (node));
      if (SSA_NAME_IS_DEFAULT_DEF (node))
	pp_string (pp, "(D)");
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (node))
	pp_string (pp, "(ab)");
      break;

    case WITH_SIZE_EXPR:
      pp_string (pp, "WITH_SIZE_EXPR <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_greater (pp);
      break;

    case ASSERT_EXPR:
      pp_string (pp, "ASSERT_EXPR <");
      dump_generic_node (pp, ASSERT_EXPR_VAR (node), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, ASSERT_EXPR_COND (node), spc, flags, false);
      pp_greater (pp);
      break;

    case SCEV_KNOWN:
      pp_string (pp, "scev_known");
      break;

    case SCEV_NOT_KNOWN:
      pp_string (pp, "scev_not_known");
      break;

    case POLYNOMIAL_CHREC:
      pp_left_brace (pp);
      dump_generic_node (pp, CHREC_LEFT (node), spc, flags, false);
      pp_string (pp, ", +, ");
      dump_generic_node (pp, CHREC_RIGHT (node), spc, flags, false);
      pp_string (pp, "}_");
      dump_generic_node (pp, CHREC_VAR (node), spc, flags, false);
      is_stmt = false;
      break;

    case REALIGN_LOAD_EXPR:
      pp_string (pp, "REALIGN_LOAD <");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 2), spc, flags, false);
      pp_greater (pp);
      break;

    case VEC_COND_EXPR:
      pp_string (pp, " VEC_COND_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, " , ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (pp, " , ");
      dump_generic_node (pp, TREE_OPERAND (node, 2), spc, flags, false);
      pp_string (pp, " > ");
      break;
    
    case VEC_PERM_EXPR:
      pp_string (pp, " VEC_PERM_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, " , ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (pp, " , ");
      dump_generic_node (pp, TREE_OPERAND (node, 2), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case DOT_PROD_EXPR:
      pp_string (pp, " DOT_PROD_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 2), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case WIDEN_MULT_PLUS_EXPR:
      pp_string (pp, " WIDEN_MULT_PLUS_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 2), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case WIDEN_MULT_MINUS_EXPR:
      pp_string (pp, " WIDEN_MULT_MINUS_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 2), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case FMA_EXPR:
      pp_string (pp, " FMA_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 2), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case OACC_PARALLEL:
      pp_string (pp, "#pragma acc parallel");
      goto dump_omp_clauses_body;

    case OACC_KERNELS:
      pp_string (pp, "#pragma acc kernels");
      goto dump_omp_clauses_body;

    case OACC_DATA:
      pp_string (pp, "#pragma acc data");
      dump_omp_clauses (pp, OACC_DATA_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    case OACC_HOST_DATA:
      pp_string (pp, "#pragma acc host_data");
      dump_omp_clauses (pp, OACC_HOST_DATA_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    case OACC_DECLARE:
      pp_string (pp, "#pragma acc declare");
      dump_omp_clauses (pp, OACC_DECLARE_CLAUSES (node), spc, flags);
      break;

    case OACC_UPDATE:
      pp_string (pp, "#pragma acc update");
      dump_omp_clauses (pp, OACC_UPDATE_CLAUSES (node), spc, flags);
      break;

    case OACC_ENTER_DATA:
      pp_string (pp, "#pragma acc enter data");
      dump_omp_clauses (pp, OACC_ENTER_DATA_CLAUSES (node), spc, flags);
      break;

    case OACC_EXIT_DATA:
      pp_string (pp, "#pragma acc exit data");
      dump_omp_clauses (pp, OACC_EXIT_DATA_CLAUSES (node), spc, flags);
      break;

    case OACC_CACHE:
      pp_string (pp, "#pragma acc cache");
      dump_omp_clauses (pp, OACC_CACHE_CLAUSES (node), spc, flags);
      break;

    case OMP_PARALLEL:
      pp_string (pp, "#pragma omp parallel");
      dump_omp_clauses (pp, OMP_PARALLEL_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    dump_omp_clauses_body:
      dump_omp_clauses (pp, OMP_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    dump_omp_body:
      if (!(flags & TDF_SLIM) && OMP_BODY (node))
	{
	  newline_and_indent (pp, spc + 2);
	  pp_left_brace (pp);
	  newline_and_indent (pp, spc + 4);
	  dump_generic_node (pp, OMP_BODY (node), spc + 4, flags, false);
	  newline_and_indent (pp, spc + 2);
	  pp_right_brace (pp);
	}
      is_expr = false;
      break;

    case OMP_TASK:
      pp_string (pp, "#pragma omp task");
      dump_omp_clauses (pp, OMP_TASK_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    case OMP_FOR:
      pp_string (pp, "#pragma omp for");
      goto dump_omp_loop;

    case OMP_SIMD:
      pp_string (pp, "#pragma omp simd");
      goto dump_omp_loop;

    case CILK_SIMD:
      pp_string (pp, "#pragma simd");
      goto dump_omp_loop;

    case CILK_FOR:
      /* This label points one line after dumping the clauses.
	 For _Cilk_for the clauses are dumped after the _Cilk_for (...)
	 parameters are printed out.  */
      goto dump_omp_loop_cilk_for;

    case OMP_DISTRIBUTE:
      pp_string (pp, "#pragma omp distribute");
      goto dump_omp_loop;

    case OMP_TASKLOOP:
      pp_string (pp, "#pragma omp taskloop");
      goto dump_omp_loop;

    case OACC_LOOP:
      pp_string (pp, "#pragma acc loop");
      goto dump_omp_loop;

    case OMP_TEAMS:
      pp_string (pp, "#pragma omp teams");
      dump_omp_clauses (pp, OMP_TEAMS_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    case OMP_TARGET_DATA:
      pp_string (pp, "#pragma omp target data");
      dump_omp_clauses (pp, OMP_TARGET_DATA_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    case OMP_TARGET_ENTER_DATA:
      pp_string (pp, "#pragma omp target enter data");
      dump_omp_clauses (pp, OMP_TARGET_ENTER_DATA_CLAUSES (node), spc, flags);
      is_expr = false;
      break;

    case OMP_TARGET_EXIT_DATA:
      pp_string (pp, "#pragma omp target exit data");
      dump_omp_clauses (pp, OMP_TARGET_EXIT_DATA_CLAUSES (node), spc, flags);
      is_expr = false;
      break;

    case OMP_TARGET:
      pp_string (pp, "#pragma omp target");
      dump_omp_clauses (pp, OMP_TARGET_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    case OMP_TARGET_UPDATE:
      pp_string (pp, "#pragma omp target update");
      dump_omp_clauses (pp, OMP_TARGET_UPDATE_CLAUSES (node), spc, flags);
      is_expr = false;
      break;

    dump_omp_loop:
      dump_omp_clauses (pp, OMP_FOR_CLAUSES (node), spc, flags);

    dump_omp_loop_cilk_for:
      if (!(flags & TDF_SLIM))
	{
	  int i;

	  if (OMP_FOR_PRE_BODY (node))
	    {
	      if (TREE_CODE (node) == CILK_FOR)
		pp_string (pp, "  ");
	      else
		newline_and_indent (pp, spc + 2);
	      pp_left_brace (pp);
	      spc += 4;
	      newline_and_indent (pp, spc);
	      dump_generic_node (pp, OMP_FOR_PRE_BODY (node),
				 spc, flags, false);
	    }
	  if (OMP_FOR_INIT (node))
	    {
	      spc -= 2;
	      for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (node)); i++)
		{
		  spc += 2;
		  if (TREE_CODE (node) != CILK_FOR || OMP_FOR_PRE_BODY (node))
		    newline_and_indent (pp, spc);
		  if (TREE_CODE (node) == CILK_FOR)
		    pp_string (pp, "_Cilk_for (");
		  else
		    pp_string (pp, "for (");
		  dump_generic_node (pp,
				     TREE_VEC_ELT (OMP_FOR_INIT (node), i),
				     spc, flags, false);
		  pp_string (pp, "; ");
		  dump_generic_node (pp,
				     TREE_VEC_ELT (OMP_FOR_COND (node), i),
				     spc, flags, false);
		  pp_string (pp, "; ");
		  dump_generic_node (pp,
				     TREE_VEC_ELT (OMP_FOR_INCR (node), i),
				     spc, flags, false);
		  pp_right_paren (pp);
		}
	      if (TREE_CODE (node) == CILK_FOR)
		dump_omp_clauses (pp, OMP_FOR_CLAUSES (node), spc, flags);
	    }
	  if (OMP_FOR_BODY (node))
	    {
	      newline_and_indent (pp, spc + 2);
	      pp_left_brace (pp);
	      newline_and_indent (pp, spc + 4);
	      dump_generic_node (pp, OMP_FOR_BODY (node), spc + 4, flags,
		  false);
	      newline_and_indent (pp, spc + 2);
	      pp_right_brace (pp);
	    }
	  if (OMP_FOR_INIT (node))
	    spc -= 2 * TREE_VEC_LENGTH (OMP_FOR_INIT (node)) - 2;
	  if (OMP_FOR_PRE_BODY (node))
	    {
	      spc -= 4;
	      newline_and_indent (pp, spc + 2);
	      pp_right_brace (pp);
	    }
	}
      is_expr = false;
      break;

    case OMP_SECTIONS:
      pp_string (pp, "#pragma omp sections");
      dump_omp_clauses (pp, OMP_SECTIONS_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    case OMP_SECTION:
      pp_string (pp, "#pragma omp section");
      goto dump_omp_body;

    case OMP_MASTER:
      pp_string (pp, "#pragma omp master");
      goto dump_omp_body;

    case OMP_TASKGROUP:
      pp_string (pp, "#pragma omp taskgroup");
      goto dump_omp_body;

    case OMP_ORDERED:
      pp_string (pp, "#pragma omp ordered");
      dump_omp_clauses (pp, OMP_ORDERED_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    case OMP_CRITICAL:
      pp_string (pp, "#pragma omp critical");
      if (OMP_CRITICAL_NAME (node))
	{
	  pp_space (pp);
	  pp_left_paren (pp);
          dump_generic_node (pp, OMP_CRITICAL_NAME (node), spc,
			     flags, false);
	  pp_right_paren (pp);
	}
      dump_omp_clauses (pp, OMP_CRITICAL_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    case OMP_ATOMIC:
      pp_string (pp, "#pragma omp atomic");
      if (OMP_ATOMIC_SEQ_CST (node))
	pp_string (pp, " seq_cst");
      newline_and_indent (pp, spc + 2);
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_space (pp);
      pp_equal (pp);
      pp_space (pp);
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      break;

    case OMP_ATOMIC_READ:
      pp_string (pp, "#pragma omp atomic read");
      if (OMP_ATOMIC_SEQ_CST (node))
	pp_string (pp, " seq_cst");
      newline_and_indent (pp, spc + 2);
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_space (pp);
      break;

    case OMP_ATOMIC_CAPTURE_OLD:
    case OMP_ATOMIC_CAPTURE_NEW:
      pp_string (pp, "#pragma omp atomic capture");
      if (OMP_ATOMIC_SEQ_CST (node))
	pp_string (pp, " seq_cst");
      newline_and_indent (pp, spc + 2);
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_space (pp);
      pp_equal (pp);
      pp_space (pp);
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      break;

    case OMP_SINGLE:
      pp_string (pp, "#pragma omp single");
      dump_omp_clauses (pp, OMP_SINGLE_CLAUSES (node), spc, flags);
      goto dump_omp_body;

    case OMP_CLAUSE:
      dump_omp_clause (pp, node, spc, flags);
      is_expr = false;
      break;

    case TRANSACTION_EXPR:
      if (TRANSACTION_EXPR_OUTER (node))
	pp_string (pp, "__transaction_atomic [[outer]]");
      else if (TRANSACTION_EXPR_RELAXED (node))
	pp_string (pp, "__transaction_relaxed");
      else
	pp_string (pp, "__transaction_atomic");
      if (!(flags & TDF_SLIM) && TRANSACTION_EXPR_BODY (node))
	{
	  newline_and_indent (pp, spc);
	  pp_left_brace (pp);
	  newline_and_indent (pp, spc + 2);
	  dump_generic_node (pp, TRANSACTION_EXPR_BODY (node),
			     spc + 2, flags, false);
	  newline_and_indent (pp, spc);
	  pp_right_brace (pp);
	}
      is_expr = false;
      break;

    case REDUC_MAX_EXPR:
      pp_string (pp, " REDUC_MAX_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case REDUC_MIN_EXPR:
      pp_string (pp, " REDUC_MIN_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case REDUC_PLUS_EXPR:
      pp_string (pp, " REDUC_PLUS_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_LO_EXPR:
    case VEC_WIDEN_MULT_EVEN_EXPR:
    case VEC_WIDEN_MULT_ODD_EXPR:
    case VEC_WIDEN_LSHIFT_HI_EXPR:
    case VEC_WIDEN_LSHIFT_LO_EXPR:
      pp_space (pp);
      for (str = get_tree_code_name (code); *str; str++)
	pp_character (pp, TOUPPER (*str));
      pp_string (pp, " < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case VEC_UNPACK_HI_EXPR:
      pp_string (pp, " VEC_UNPACK_HI_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case VEC_UNPACK_LO_EXPR:
      pp_string (pp, " VEC_UNPACK_LO_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case VEC_UNPACK_FLOAT_HI_EXPR:
      pp_string (pp, " VEC_UNPACK_FLOAT_HI_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case VEC_UNPACK_FLOAT_LO_EXPR:
      pp_string (pp, " VEC_UNPACK_FLOAT_LO_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case VEC_PACK_TRUNC_EXPR:
      pp_string (pp, " VEC_PACK_TRUNC_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case VEC_PACK_SAT_EXPR:
      pp_string (pp, " VEC_PACK_SAT_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case VEC_PACK_FIX_TRUNC_EXPR:
      pp_string (pp, " VEC_PACK_FIX_TRUNC_EXPR < ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      pp_string (pp, ", ");
      dump_generic_node (pp, TREE_OPERAND (node, 1), spc, flags, false);
      pp_string (pp, " > ");
      break;

    case BLOCK:
      dump_block_node (pp, node, spc, flags);
      break;

    case CILK_SPAWN_STMT:
      pp_string (pp, "_Cilk_spawn ");
      dump_generic_node (pp, TREE_OPERAND (node, 0), spc, flags, false);
      break;

    case CILK_SYNC_STMT:
      pp_string (pp, "_Cilk_sync");
      break;

    default:
      NIY;
    }

  if (is_stmt && is_expr)
    pp_semicolon (pp);

  return spc;
}

/* Print the declaration of a variable.  */

void
print_declaration (pretty_printer *pp, tree t, int spc, int flags)
{
  INDENT (spc);

  if (TREE_CODE(t) == NAMELIST_DECL)
    {
      pp_string(pp, "namelist ");
      dump_decl_name (pp, t, flags);
      pp_semicolon (pp);
      return;
    }

  if (TREE_CODE (t) == TYPE_DECL)
    pp_string (pp, "typedef ");

  if (CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_DECL_WRTL) && DECL_REGISTER (t))
    pp_string (pp, "register ");

  if (TREE_PUBLIC (t) && DECL_EXTERNAL (t))
    pp_string (pp, "extern ");
  else if (TREE_STATIC (t))
    pp_string (pp, "static ");

  /* Print the type and name.  */
  if (TREE_TYPE (t) && TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
    {
      tree tmp;

      /* Print array's type.  */
      tmp = TREE_TYPE (t);
      while (TREE_CODE (TREE_TYPE (tmp)) == ARRAY_TYPE)
	tmp = TREE_TYPE (tmp);
      dump_generic_node (pp, TREE_TYPE (tmp), spc, flags, false);

      /* Print variable's name.  */
      pp_space (pp);
      dump_generic_node (pp, t, spc, flags, false);

      /* Print the dimensions.  */
      tmp = TREE_TYPE (t);
      while (TREE_CODE (tmp) == ARRAY_TYPE)
	{
	  dump_array_domain (pp, TYPE_DOMAIN (tmp), spc, flags);
	  tmp = TREE_TYPE (tmp);
	}
    }
  else if (TREE_CODE (t) == FUNCTION_DECL)
    {
      dump_generic_node (pp, TREE_TYPE (TREE_TYPE (t)), spc, flags, false);
      pp_space (pp);
      dump_decl_name (pp, t, flags);
      dump_function_declaration (pp, TREE_TYPE (t), spc, flags);
    }
  else
    {
      /* Print type declaration.  */
      dump_generic_node (pp, TREE_TYPE (t), spc, flags, false);

      /* Print variable's name.  */
      pp_space (pp);
      dump_generic_node (pp, t, spc, flags, false);
    }

  if (TREE_CODE (t) == VAR_DECL && DECL_HARD_REGISTER (t))
    {
      pp_string (pp, " __asm__ ");
      pp_left_paren (pp);
      dump_generic_node (pp, DECL_ASSEMBLER_NAME (t), spc, flags, false);
      pp_right_paren (pp);
    }

  /* The initial value of a function serves to determine whether the function
     is declared or defined.  So the following does not apply to function
     nodes.  */
  if (TREE_CODE (t) != FUNCTION_DECL)
    {
      /* Print the initial value.  */
      if (DECL_INITIAL (t))
	{
	  pp_space (pp);
	  pp_equal (pp);
	  pp_space (pp);
	  dump_generic_node (pp, DECL_INITIAL (t), spc, flags, false);
	}
    }

  if (TREE_CODE (t) == VAR_DECL && DECL_HAS_VALUE_EXPR_P (t))
    {
      pp_string (pp, " [value-expr: ");
      dump_generic_node (pp, DECL_VALUE_EXPR (t), spc, flags, false);
      pp_right_bracket (pp);
    }

  pp_semicolon (pp);
}


/* Prints a structure: name, fields, and methods.
   FIXME: Still incomplete.  */

static void
print_struct_decl (pretty_printer *pp, const_tree node, int spc, int flags)
{
  /* Print the name of the structure.  */
  if (TYPE_NAME (node))
    {
      INDENT (spc);
      if (TREE_CODE (node) == RECORD_TYPE)
	pp_string (pp, "struct ");
      else if ((TREE_CODE (node) == UNION_TYPE
		|| TREE_CODE (node) == QUAL_UNION_TYPE))
	pp_string (pp, "union ");

      dump_generic_node (pp, TYPE_NAME (node), spc, 0, false);
    }

  /* Print the contents of the structure.  */
  pp_newline (pp);
  INDENT (spc);
  pp_left_brace (pp);
  pp_newline (pp);

  /* Print the fields of the structure.  */
  {
    tree tmp;
    tmp = TYPE_FIELDS (node);
    while (tmp)
      {
	/* Avoid to print recursively the structure.  */
	/* FIXME : Not implemented correctly...,
	   what about the case when we have a cycle in the contain graph? ...
	   Maybe this could be solved by looking at the scope in which the
	   structure was declared.  */
	if (TREE_TYPE (tmp) != node
	    && (TREE_CODE (TREE_TYPE (tmp)) != POINTER_TYPE
		|| TREE_TYPE (TREE_TYPE (tmp)) != node))
	  {
	    print_declaration (pp, tmp, spc+2, flags);
	    pp_newline (pp);
	  }
	tmp = DECL_CHAIN (tmp);
      }
  }
  INDENT (spc);
  pp_right_brace (pp);
}

/* Return the priority of the operator CODE.

   From lowest to highest precedence with either left-to-right (L-R)
   or right-to-left (R-L) associativity]:

     1	[L-R] ,
     2	[R-L] = += -= *= /= %= &= ^= |= <<= >>=
     3	[R-L] ?:
     4	[L-R] ||
     5	[L-R] &&
     6	[L-R] |
     7	[L-R] ^
     8	[L-R] &
     9	[L-R] == !=
    10	[L-R] < <= > >=
    11	[L-R] << >>
    12	[L-R] + -
    13	[L-R] * / %
    14	[R-L] ! ~ ++ -- + - * & (type) sizeof
    15	[L-R] fn() [] -> .

   unary +, - and * have higher precedence than the corresponding binary
   operators.  */

int
op_code_prio (enum tree_code code)
{
  switch (code)
    {
    case TREE_LIST:
    case COMPOUND_EXPR:
    case BIND_EXPR:
      return 1;

    case MODIFY_EXPR:
    case INIT_EXPR:
      return 2;

    case COND_EXPR:
      return 3;

    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
      return 4;

    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
      return 5;

    case BIT_IOR_EXPR:
      return 6;

    case BIT_XOR_EXPR:
    case TRUTH_XOR_EXPR:
      return 7;

    case BIT_AND_EXPR:
      return 8;

    case EQ_EXPR:
    case NE_EXPR:
      return 9;

    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case ORDERED_EXPR:
    case UNORDERED_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
      return 10;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case VEC_WIDEN_LSHIFT_HI_EXPR:
    case VEC_WIDEN_LSHIFT_LO_EXPR:
    case WIDEN_LSHIFT_EXPR:
      return 11;

    case WIDEN_SUM_EXPR:
    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      return 12;

    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_LO_EXPR:
    case WIDEN_MULT_EXPR:
    case DOT_PROD_EXPR:
    case WIDEN_MULT_PLUS_EXPR:
    case WIDEN_MULT_MINUS_EXPR:
    case MULT_EXPR:
    case MULT_HIGHPART_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case FMA_EXPR:
      return 13;

    case TRUTH_NOT_EXPR:
    case BIT_NOT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case NEGATE_EXPR:
    case INDIRECT_REF:
    case ADDR_EXPR:
    case FLOAT_EXPR:
    CASE_CONVERT:
    case FIX_TRUNC_EXPR:
    case TARGET_EXPR:
      return 14;

    case CALL_EXPR:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case COMPONENT_REF:
      return 15;

      /* Special expressions.  */
    case MIN_EXPR:
    case MAX_EXPR:
    case ABS_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case REDUC_MAX_EXPR:
    case REDUC_MIN_EXPR:
    case REDUC_PLUS_EXPR:
    case VEC_UNPACK_HI_EXPR:
    case VEC_UNPACK_LO_EXPR:
    case VEC_UNPACK_FLOAT_HI_EXPR:
    case VEC_UNPACK_FLOAT_LO_EXPR:
    case VEC_PACK_TRUNC_EXPR:
    case VEC_PACK_SAT_EXPR:
      return 16;

    default:
      /* Return an arbitrarily high precedence to avoid surrounding single
	 VAR_DECLs in ()s.  */
      return 9999;
    }
}

/* Return the priority of the operator OP.  */

int
op_prio (const_tree op)
{
  enum tree_code code;

  if (op == NULL)
    return 9999;

  code = TREE_CODE (op);
  if (code == SAVE_EXPR || code == NON_LVALUE_EXPR)
    return op_prio (TREE_OPERAND (op, 0));

  return op_code_prio (code);
}

/* Return the symbol associated with operator CODE.  */

const char *
op_symbol_code (enum tree_code code)
{
  switch (code)
    {
    case MODIFY_EXPR:
      return "=";

    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
      return "||";

    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
      return "&&";

    case BIT_IOR_EXPR:
      return "|";

    case TRUTH_XOR_EXPR:
    case BIT_XOR_EXPR:
      return "^";

    case ADDR_EXPR:
    case BIT_AND_EXPR:
      return "&";

    case ORDERED_EXPR:
      return "ord";
    case UNORDERED_EXPR:
      return "unord";

    case EQ_EXPR:
      return "==";
    case UNEQ_EXPR:
      return "u==";

    case NE_EXPR:
      return "!=";

    case LT_EXPR:
      return "<";
    case UNLT_EXPR:
      return "u<";

    case LE_EXPR:
      return "<=";
    case UNLE_EXPR:
      return "u<=";

    case GT_EXPR:
      return ">";
    case UNGT_EXPR:
      return "u>";

    case GE_EXPR:
      return ">=";
    case UNGE_EXPR:
      return "u>=";

    case LTGT_EXPR:
      return "<>";

    case LSHIFT_EXPR:
      return "<<";

    case RSHIFT_EXPR:
      return ">>";

    case LROTATE_EXPR:
      return "r<<";

    case RROTATE_EXPR:
      return "r>>";

    case WIDEN_LSHIFT_EXPR:
      return "w<<";

    case POINTER_PLUS_EXPR:
      return "+";

    case PLUS_EXPR:
      return "+";

    case REDUC_PLUS_EXPR:
      return "r+";

    case WIDEN_SUM_EXPR:
      return "w+";

    case WIDEN_MULT_EXPR:
      return "w*";

    case MULT_HIGHPART_EXPR:
      return "h*";

    case NEGATE_EXPR:
    case MINUS_EXPR:
      return "-";

    case BIT_NOT_EXPR:
      return "~";

    case TRUTH_NOT_EXPR:
      return "!";

    case MULT_EXPR:
    case INDIRECT_REF:
      return "*";

    case TRUNC_DIV_EXPR:
    case RDIV_EXPR:
      return "/";

    case CEIL_DIV_EXPR:
      return "/[cl]";

    case FLOOR_DIV_EXPR:
      return "/[fl]";

    case ROUND_DIV_EXPR:
      return "/[rd]";

    case EXACT_DIV_EXPR:
      return "/[ex]";

    case TRUNC_MOD_EXPR:
      return "%";

    case CEIL_MOD_EXPR:
      return "%[cl]";

    case FLOOR_MOD_EXPR:
      return "%[fl]";

    case ROUND_MOD_EXPR:
      return "%[rd]";

    case PREDECREMENT_EXPR:
      return " --";

    case PREINCREMENT_EXPR:
      return " ++";

    case POSTDECREMENT_EXPR:
      return "-- ";

    case POSTINCREMENT_EXPR:
      return "++ ";

    case MAX_EXPR:
      return "max";

    case MIN_EXPR:
      return "min";

    default:
      return "<<< ??? >>>";
    }
}

/* Return the symbol associated with operator OP.  */

static const char *
op_symbol (const_tree op)
{
  return op_symbol_code (TREE_CODE (op));
}

/* Prints the name of a call.  NODE is the CALL_EXPR_FN of a CALL_EXPR or
   the gimple_call_fn of a GIMPLE_CALL.  */

void
print_call_name (pretty_printer *pp, tree node, int flags)
{
  tree op0 = node;

  if (TREE_CODE (op0) == NON_LVALUE_EXPR)
    op0 = TREE_OPERAND (op0, 0);

 again:
  switch (TREE_CODE (op0))
    {
    case VAR_DECL:
    case PARM_DECL:
    case FUNCTION_DECL:
      dump_function_name (pp, op0, flags);
      break;

    case ADDR_EXPR:
    case INDIRECT_REF:
    CASE_CONVERT:
      op0 = TREE_OPERAND (op0, 0);
      goto again;

    case COND_EXPR:
      pp_left_paren (pp);
      dump_generic_node (pp, TREE_OPERAND (op0, 0), 0, flags, false);
      pp_string (pp, ") ? ");
      dump_generic_node (pp, TREE_OPERAND (op0, 1), 0, flags, false);
      pp_string (pp, " : ");
      dump_generic_node (pp, TREE_OPERAND (op0, 2), 0, flags, false);
      break;

    case ARRAY_REF:
      if (TREE_CODE (TREE_OPERAND (op0, 0)) == VAR_DECL)
	dump_function_name (pp, TREE_OPERAND (op0, 0), flags);
      else
	dump_generic_node (pp, op0, 0, flags, false);
      break;

    case MEM_REF:
      if (integer_zerop (TREE_OPERAND (op0, 1)))
	{
	  op0 = TREE_OPERAND (op0, 0);
	  goto again;
	}
      /* Fallthru.  */
    case COMPONENT_REF:
    case SSA_NAME:
    case OBJ_TYPE_REF:
      dump_generic_node (pp, op0, 0, flags, false);
      break;

    default:
      NIY;
    }
}

/* Parses the string STR and replaces new-lines by '\n', tabs by '\t', ...  */

static void
pretty_print_string (pretty_printer *pp, const char *str)
{
  if (str == NULL)
    return;

  while (*str)
    {
      switch (str[0])
	{
	case '\b':
	  pp_string (pp, "\\b");
	  break;

	case '\f':
	  pp_string (pp, "\\f");
	  break;

	case '\n':
	  pp_string (pp, "\\n");
	  break;

	case '\r':
	  pp_string (pp, "\\r");
	  break;

	case '\t':
	  pp_string (pp, "\\t");
	  break;

	case '\v':
	  pp_string (pp, "\\v");
	  break;

	case '\\':
	  pp_string (pp, "\\\\");
	  break;

	case '\"':
	  pp_string (pp, "\\\"");
	  break;

	case '\'':
	  pp_string (pp, "\\'");
	  break;

	  /* No need to handle \0; the loop terminates on \0.  */

	case '\1':
	  pp_string (pp, "\\1");
	  break;

	case '\2':
	  pp_string (pp, "\\2");
	  break;

	case '\3':
	  pp_string (pp, "\\3");
	  break;

	case '\4':
	  pp_string (pp, "\\4");
	  break;

	case '\5':
	  pp_string (pp, "\\5");
	  break;

	case '\6':
	  pp_string (pp, "\\6");
	  break;

	case '\7':
	  pp_string (pp, "\\7");
	  break;

	default:
	  pp_character (pp, str[0]);
	  break;
	}
      str++;
    }
}

static void
maybe_init_pretty_print (FILE *file)
{
  if (!tree_pp)
    {
      tree_pp = new pretty_printer ();
      pp_needs_newline (tree_pp) = true;
      pp_translate_identifiers (tree_pp) = false;
    }

  tree_pp->buffer->stream = file;
}

static void
newline_and_indent (pretty_printer *pp, int spc)
{
  pp_newline (pp);
  INDENT (spc);
}

/* Handle a %K format for TEXT.  Separate from default_tree_printer so
   it can also be used in front ends.
   %K: a statement, from which EXPR_LOCATION and TREE_BLOCK will be recorded.
*/

void
percent_K_format (text_info *text)
{
  tree t = va_arg (*text->args_ptr, tree), block;
  text->set_location (0, EXPR_LOCATION (t), true);
  gcc_assert (pp_ti_abstract_origin (text) != NULL);
  block = TREE_BLOCK (t);
  *pp_ti_abstract_origin (text) = NULL;

  if (in_lto_p)
    {
      /* ???  LTO drops all BLOCK_ABSTRACT_ORIGINs apart from those
         representing the outermost block of an inlined function.
	 So walk the BLOCK tree until we hit such a scope.  */
      while (block
	     && TREE_CODE (block) == BLOCK)
	{
	  if (inlined_function_outer_scope_p (block))
	    {
	      *pp_ti_abstract_origin (text) = block;
	      break;
	    }
	  block = BLOCK_SUPERCONTEXT (block);
	}
      return;
    }

  while (block
	 && TREE_CODE (block) == BLOCK
	 && BLOCK_ABSTRACT_ORIGIN (block))
    {
      tree ao = BLOCK_ABSTRACT_ORIGIN (block);

      while (TREE_CODE (ao) == BLOCK
	     && BLOCK_ABSTRACT_ORIGIN (ao)
	     && BLOCK_ABSTRACT_ORIGIN (ao) != ao)
	ao = BLOCK_ABSTRACT_ORIGIN (ao);

      if (TREE_CODE (ao) == FUNCTION_DECL)
	{
	  *pp_ti_abstract_origin (text) = block;
	  break;
	}
      block = BLOCK_SUPERCONTEXT (block);
    }
}

/* Print the identifier ID to PRETTY-PRINTER.  */

void
pp_tree_identifier (pretty_printer *pp, tree id)
{
  if (pp_translate_identifiers (pp))
    {
      const char *text = identifier_to_locale (IDENTIFIER_POINTER (id));
      pp_append_text (pp, text, text + strlen (text));
    }
  else
    pp_append_text (pp, IDENTIFIER_POINTER (id),
		    IDENTIFIER_POINTER (id) + IDENTIFIER_LENGTH (id));
}

/* A helper function that is used to dump function information before the
   function dump.  */

void
dump_function_header (FILE *dump_file, tree fdecl, int flags)
{
  const char *dname, *aname;
  struct cgraph_node *node = cgraph_node::get (fdecl);
  struct function *fun = DECL_STRUCT_FUNCTION (fdecl);

  dname = lang_hooks.decl_printable_name (fdecl, 2);

  if (DECL_ASSEMBLER_NAME_SET_P (fdecl))
    aname = (IDENTIFIER_POINTER
             (DECL_ASSEMBLER_NAME (fdecl)));
  else
    aname = "<unset-asm-name>";

  fprintf (dump_file, "\n;; Function %s (%s, funcdef_no=%d",
	   dname, aname, fun->funcdef_no);
  if (!(flags & TDF_NOUID))
    fprintf (dump_file, ", decl_uid=%d", DECL_UID (fdecl));
  if (node)
    {
      fprintf (dump_file, ", cgraph_uid=%d", node->uid);
      fprintf (dump_file, ", symbol_order=%d)%s\n\n", node->order,
               node->frequency == NODE_FREQUENCY_HOT
               ? " (hot)"
               : node->frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED
               ? " (unlikely executed)"
               : node->frequency == NODE_FREQUENCY_EXECUTED_ONCE
               ? " (executed once)"
               : "");
    }
  else
    fprintf (dump_file, ")\n\n");
}

/* Dump double_int D to pretty_printer PP.  UNS is true
   if D is unsigned and false otherwise.  */
void
pp_double_int (pretty_printer *pp, double_int d, bool uns)
{
  if (d.fits_shwi ())
    pp_wide_integer (pp, d.low);
  else if (d.fits_uhwi ())
    pp_unsigned_wide_integer (pp, d.low);
  else
    {
      unsigned HOST_WIDE_INT low = d.low;
      HOST_WIDE_INT high = d.high;
      if (!uns && d.is_negative ())
	{
	  pp_minus (pp);
	  high = ~high + !low;
	  low = -low;
	}
      /* Would "%x%0*x" or "%x%*0x" get zero-padding on all
	 systems?  */
      sprintf (pp_buffer (pp)->digit_buffer,
	       HOST_WIDE_INT_PRINT_DOUBLE_HEX,
	       (unsigned HOST_WIDE_INT) high, low);
      pp_string (pp, pp_buffer (pp)->digit_buffer);
    }
}
