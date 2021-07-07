/* rtegraph.c graph and nodes used by m2rte.

Copyright (C) 2019-2021 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "gcc-consolidation.h"

#include "../gm2-lang.h"
#include "../m2-tree.h"

#include "langhooks-def.h" /* FIXME: for lhd_set_decl_assembler_name.  */
#include "tree-pass.h"     /* FIXME: only for PROP_gimple_any.  */
#include "toplev.h"
#include "debug.h"

#include "opts.h"
#include "mpfr.h"

#undef DEBUGGING

struct GTY (()) rtenode
{
  bool constructor_reachable;   /* is this guarenteed to be reachable by a constructor?  */
  bool export_reachable;  /* is this reachable via exported functions?  */
  bool exception_routine;   /* is this an exception routine?  */
  bool constructor_final;   /* have we walked this rtenode during constructor testing?  */
  bool export_final;   /* walked this rtenode during exported testing?  */
  bool is_call;    /* is this a function call?  */
  gimple *grtenode;
  tree func;
  rtenode *reachable_src;  /* if this is reachable which src function will call us?  */

  vec<rtenode *, va_gc> *function_call;
  vec<rtenode *, va_gc> *rts_call;
  void dump (void);
  void dump_vec (const char *title, vec<rtenode *, va_gc> *list);

  void propagate_constructor_reachable (rtenode *);
  void propagate_export_reachable (rtenode *);
  void error_message (void);
  void warning_message (void);
  void note_message (void);
  const char *get_func_name (void);
  const char *create_message (const char *with_name, const char *without_name);
};


typedef vec<rtenode *, va_gc> rtevec;

static GTY (()) rtevec *allnodes;
static GTY (()) rtevec *candidates;
static GTY (()) rtevec *externs;
static GTY (()) rtevec *constructors;


static void determine_reachable (void);
static void issue_messages (void);
void rtegraph_dump (void);


static GTY (()) rtenode *rtegraph_current_function = NULL;


/* rtegraph_get_func returns the function associated with the rtenode.  */

tree
rtegraph_get_func (rtenode *n)
{
  return n->func;
}

/* rtegraph_set_current_function assigns rtegraph_current_function with func.  */

void
rtegraph_set_current_function (rtenode *func)
{
  rtegraph_current_function = func;
}

/* rtegraph_include_rtscall mark func as an exception routine and remember
   that it is called from rtegraph_current_function in the rts_call array.  */

void rtegraph_include_rtscall (rtenode *func)
{
  /* this is a runtime exception, mark it as such.  */
  func->exception_routine = true;
  /* and remember it.  */
  vec_safe_push (rtegraph_current_function->rts_call, func);
}


/* rtegraph_include_rtscall remember that rtegraph_current_function calls
   func.  */

void rtegraph_include_function_call (rtenode *func)
{
  vec_safe_push (rtegraph_current_function->function_call, func);
}


/* rtegraph_discover performs the main work, called by m2rte.c analyse_graph.
   It determines which function calls a reachable and then issues any warning
   message if a reachable function is a call to a runtime exception handler.  */

void rtegraph_discover (void)
{
  determine_reachable ();
#if defined (DEBUGGING)
  rtegraph_dump ();
#endif
  issue_messages ();
}

/* rtegraph_candidates_include include node n in the array of candidates.  */

void rtegraph_candidates_include (rtenode *n)
{
  unsigned int len = vec_safe_length (candidates);

  for (unsigned int i = 0; i < len; i++)
    if ((*candidates)[i] == n)
      return;
  vec_safe_push (candidates, n);
}

/* rtegraph_allnodes_include include node n in the array of allnodes.  */

void rtegraph_allnodes_include (rtenode *n)
{
  unsigned int len = vec_safe_length (allnodes);

  for (unsigned int i = 0; i < len; i++)
    if ((*allnodes)[i] == n)
      return;
  vec_safe_push (allnodes, n);
}

/* rtegraph_externs_include include node n in the array of externs.  */

void rtegraph_externs_include (rtenode *n)
{
  unsigned int len = vec_safe_length (externs);

  for (unsigned int i = 0; i < len; i++)
    if ((*externs)[i] == n)
      return;
  vec_safe_push (externs, n);
}

/* rtegraph_constructors_include include node n in the array of constructors.  */

void rtegraph_constructors_include (rtenode *n)
{
  unsigned int len = vec_safe_length (constructors);

  for (unsigned int i = 0; i < len; i++)
    if ((*constructors)[i] == n)
      return;
  vec_safe_push (constructors, n);
}

/* determine_reachable mark modules constructors as reachable and
   also mark the exported functions as also reachable.  */

void determine_reachable (void)
{
  unsigned int len = vec_safe_length (constructors);
  for (unsigned int i = 0; i < len; i++)
    (*constructors)[i]->propagate_constructor_reachable ((*constructors)[i]);
  len = vec_safe_length (externs);
  for (unsigned int i = 0; i < len; i++)
    (*externs)[i]->propagate_export_reachable ((*externs)[i]);
}

/* issue_messages for every candidate which is constructor reachable issue
   an error.  For each candidate which is reachable via an external call
   issue a warning, for any other candidate (of a local procedure) issue
   a note.  */

void issue_messages (void)
{
  unsigned int len = vec_safe_length (candidates);
  for (unsigned int i = 0; i < len; i++)
    {
      if ((*candidates)[i]->constructor_reachable)
	(*candidates)[i]->error_message ();
      else if ((*candidates)[i]->export_reachable)
	(*candidates)[i]->warning_message ();
      else
	(*candidates)[i]->note_message ();
    }
}


#if defined (DEBUGGING)
/* rtegraph_dump_vec display the contents of a vector array.  */

void
rtegraph_dump_vec (const char *title, vec<rtenode *, va_gc> *list)
{
  unsigned int len = vec_safe_length (list);
  printf ("%s (length = %d)\n", title, len);
  for (unsigned int i = 0; i < len; i++)
    {
      printf ("[%d]: rtenode %p ", i, (*list)[i]);
      (*list)[i]->dump ();
    }
  printf ("end\n");
}

/* rtegraph_dump display the contents of each vector array.  */

void rtegraph_dump (void)
{
  rtegraph_dump_vec ("allnodes", allnodes);
  rtegraph_dump_vec ("candidates", candidates);
  rtegraph_dump_vec ("externs", externs);
  rtegraph_dump_vec ("constructors", constructors);
}
#endif

/* rtegraph_init_rtenode create and return a new rtenode.  */

rtenode *
rtegraph_init_rtenode (gimple *g, tree fndecl, bool is_func_call)
{
  rtenode *n = ggc_alloc<rtenode> ();

  n->constructor_reachable = false;
  n->export_reachable = false;
  n->constructor_final = false;
  n->export_final = false;
  n->is_call = is_func_call;
  n->grtenode = g;
  n->func = fndecl;
  n->reachable_src = NULL;

  vec_alloc (n->function_call, 0);
  // n->function_call = ggc_alloc<rtevec> ();
  gcc_assert (vec_safe_length (n->function_call) == 0);
  vec_alloc (n->rts_call, 0);
  // n->rts_call = ggc_alloc<rtevec> ();
  gcc_assert (vec_safe_length (n->rts_call) == 0);
  return n;
}

/* rtegraph_lookup attempts to lookup a rtenode associated with a fndecl
   which is a function call from node g.  */

rtenode *
rtegraph_lookup (gimple *g, tree fndecl, bool is_call)
{
  unsigned int len = vec_safe_length (allnodes);
  for (unsigned int i = 0; i < len; i++)
    if ((*allnodes)[i]->grtenode == g
	&& (*allnodes)[i]->func == fndecl
	&& (*allnodes)[i]->is_call == is_call)
      return (*allnodes)[i];
  rtenode *n = rtegraph_init_rtenode (g, fndecl, is_call);
  vec_safe_push (allnodes, n);
#if defined (DEBUGGING)
  rtegraph_dump ();
#endif
  return n;
}

/* rte_error_at - wraps up an error message.  */

static void
rte_error_at (location_t location, diagnostic_t kind, const char *message, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  rich_location richloc (line_table, location);

  va_start (ap, message);
  diagnostic_set_info (&diagnostic, message, &ap, &richloc, kind);
  diagnostic_report_diagnostic (global_dc, &diagnostic);
  va_end (ap);
}

/* access_int return true if the tree t contains a constant integer, if so then
   its value is assigned to *value.  */

static bool
access_int (tree t, int *value)
{
  enum tree_code code = TREE_CODE (t);

  if (code == SSA_NAME)
    return access_int (SSA_NAME_VAR (t), value);
  if (code == INTEGER_CST)
    {
      *value = TREE_INT_CST_LOW (t);
      return true;
    }
  if ((code == VAR_DECL || code == PARM_DECL)
      && DECL_HAS_VALUE_EXPR_P (t))
    return access_int (DECL_VALUE_EXPR (t), value);
  return false;
}

/* access_string return true if the tree t contains a constant string, if so then
   its value is assigned to *value.  */

static bool
access_string (tree t, const char **value)
{
  if (TREE_CODE (t) == ADDR_EXPR)
    {
      if (TREE_CODE (TREE_OPERAND (t, 0)) == STRING_CST)
	{
	  *value = TREE_STRING_POINTER (TREE_OPERAND (t, 0));
	  return true;
	}
    }
  return false;
}

/* generate an error using the parameters of the M2RTS exception handler to
   locate the source code.  We dont use location, as the error_at function will
   give the function context which might be misleading if this is inlined.  */

static void
generate_report (gimple *stmt, const char *report, diagnostic_t kind)
{
  if (gimple_call_num_args (stmt) == 5)
    {
      tree s0 = gimple_call_arg (stmt, 0);
      tree i1 = gimple_call_arg (stmt, 1);
      tree i2 = gimple_call_arg (stmt, 2);
      tree s1 = gimple_call_arg (stmt, 3);
      tree s2 = gimple_call_arg (stmt, 4);
      const char *file;
      int line;
      int col;
      const char *scope;
      const char *message;

      if (access_string (s0, &file)
	  && access_int (i1, &line)
	  && access_int (i2, &col)
	  && access_string (s1, &scope)
	  && access_string (s2, &message))
	{
	  /* continue to use scope as this will survive any
	     optimization transforms.  */
	  location_t location = gimple_location (stmt);
	  rte_error_at (location, kind, "%s, %s (in %s)\n",
			report, message, scope);
	}
    }
}

/* get_func_name returns the name of the function associated with rtenode.  */

const char *rtenode::get_func_name (void)
{
  if (func != NULL && (DECL_NAME (func) != NULL))
    return IDENTIFIER_POINTER (DECL_NAME (func));
  return NULL;
}

/* create_message if the current rtenode has a named function associated with it then
   create a new message using with_name and the function name, otherwise
   return without_name.  */

const char *rtenode::create_message (const char *with_name, const char *without_name)
{
  const char *name = get_func_name ();
  if (name == NULL)
    return without_name;

  int len = strlen (with_name) + 1 + strlen (name);
  char *message = XNEWVEC (char, len);
  snprintf (message, len, with_name, name);
  return message;
}

/* error_message issue an DK_ERROR from grtenode.  */

void rtenode::error_message (void)
{
  if (grtenode != NULL)
    generate_report (grtenode, "runtime error will occur", DK_ERROR);
}

/* warning_message issue an DK_WARNING from grtenode.  */

void rtenode::warning_message (void)
{
  const char *message = reachable_src->create_message
    ("runtime error will occur if an exported procedure is called from %s",
     "runtime error will occur if an exported procedure is called");
  if (grtenode != NULL)
    generate_report (grtenode, message, DK_WARNING);
}

/* note_message issue an DK_NOTE from grtenode.  */

void rtenode::note_message (void)
{
  if (grtenode != NULL)
    generate_report (grtenode, "runtime will occur if this procedure is called", DK_NOTE);
}

/* dump_vec display contents of vector array list.  */
#if defined (DEBUGGING)
void
rtenode::dump_vec (const char *title, vec<rtenode *, va_gc> *list)
{
  printf ("  %s (length = %d)\n", title, vec_safe_length (list));
  for (unsigned int i = 0; i < vec_safe_length (list); i++)
    printf ("   [%d]: rtenode %p\n", i, (*list)[i]);
}
#endif

/* dump display all vector arrays associated with rtenode.  */

void
rtenode::dump (void)
{
#if defined (DEBUGGING)
  printf ("rtenode::dump:");
  if (func != NULL && (DECL_NAME (func) != NULL))
    {
      const char *n = IDENTIFIER_POINTER (DECL_NAME (func));
      printf ("%s", n);
    }
  if (constructor_reachable)
    printf (", constructor_reachable");
  if (export_reachable)
    printf (", export_reachable");
  if (constructor_final)
    printf (", constructor_final");
  if (export_final)
    printf (", export_final");
  if (is_call)
    printf (", is_call");
  else
    printf (", decl");
  printf (", grtenode %p, func = %p\n", grtenode, func);
  dump_vec ("function_call", function_call);
  dump_vec ("rts_call", rts_call);
#endif
}

/* propagate_constructor_reachable for every function which is reachable from
   rtenode call the callee rtenode and mark it as reachable from a
   constructor.  */

void rtenode::propagate_constructor_reachable (rtenode *src)
{
  if (constructor_final)
    return;
  constructor_final = true;
  constructor_reachable = true;
  reachable_src = src;
  for (unsigned int i = 0; i < vec_safe_length (function_call); i++)
    (*function_call)[i]->propagate_constructor_reachable (src);
  for (unsigned int i = 0; i < vec_safe_length (rts_call); i++)
    (*rts_call)[i]->propagate_constructor_reachable (src);
}

/* propagate_export_reachable for every function which is reachable from
   rtenode call the callee rtenode and mark it as reachable from an exported
   function.  */

void rtenode::propagate_export_reachable (rtenode *src)
{
  if (export_final)
    return;
  export_final = true;
  export_reachable = true;
  reachable_src = src;
  for (unsigned int i = 0; i < vec_safe_length (function_call); i++)
    (*function_call)[i]->propagate_export_reachable (src);
  for (unsigned int i = 0; i < vec_safe_length (rts_call); i++)
    (*rts_call)[i]->propagate_export_reachable (src);
}

/* rtegraph_init initialize the data structures (vec arrays) in this file.  */

void rtegraph_init (void)
{
  vec_alloc (allnodes, 0);
  gcc_assert (vec_safe_length (allnodes) == 0);
  vec_alloc (candidates, 0);
  gcc_assert (vec_safe_length (candidates) == 0);
  vec_alloc (externs, 0);
  gcc_assert (vec_safe_length (externs) == 0);
  vec_alloc (constructors, 0);
  gcc_assert (vec_safe_length (constructors) == 0);
#if defined (DEBUGGING)
  rtegraph_dump ();
#endif
}

/* rtegraph_finish deallocate all vec arrays in this file.  */

void rtegraph_finish (void)
{
  rtegraph_current_function = NULL;
  vec_free (allnodes);
  vec_free (candidates);
  vec_free (externs);
  vec_free (constructors);
}

#include "gt-m2-rtegraph.h"
