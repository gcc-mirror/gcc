/* OpenMP directive matching and resolving.
   Copyright (C) 2005, 2006, 2007
   Free Software Foundation, Inc.
   Contributed by Jakub Jelinek

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
#include "flags.h"
#include "gfortran.h"
#include "match.h"
#include "parse.h"
#include "pointer-set.h"
#include "target.h"
#include "toplev.h"

/* Match an end of OpenMP directive.  End of OpenMP directive is optional
   whitespace, followed by '\n' or comment '!'.  */

match
gfc_match_omp_eos (void)
{
  locus old_loc;
  int c;

  old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();

  c = gfc_next_char ();
  switch (c)
    {
    case '!':
      do
	c = gfc_next_char ();
      while (c != '\n');
      /* Fall through */

    case '\n':
      return MATCH_YES;
    }

  gfc_current_locus = old_loc;
  return MATCH_NO;
}

/* Free an omp_clauses structure.  */

void
gfc_free_omp_clauses (gfc_omp_clauses *c)
{
  int i;
  if (c == NULL)
    return;

  gfc_free_expr (c->if_expr);
  gfc_free_expr (c->num_threads);
  gfc_free_expr (c->chunk_size);
  for (i = 0; i < OMP_LIST_NUM; i++)
    gfc_free_namelist (c->lists[i]);
  gfc_free (c);
}

/* Match a variable/common block list and construct a namelist from it.  */

static match
gfc_match_omp_variable_list (const char *str, gfc_namelist **list,
			     bool allow_common)
{
  gfc_namelist *head, *tail, *p;
  locus old_loc;
  char n[GFC_MAX_SYMBOL_LEN+1];
  gfc_symbol *sym;
  match m;
  gfc_symtree *st;

  head = tail = NULL;

  old_loc = gfc_current_locus;

  m = gfc_match (str);
  if (m != MATCH_YES)
    return m;

  for (;;)
    {
      m = gfc_match_symbol (&sym, 1);
      switch (m)
	{
	case MATCH_YES:
	  gfc_set_sym_referenced (sym);
	  p = gfc_get_namelist ();
	  if (head == NULL)
	    head = tail = p;
	  else
	    {
	      tail->next = p;
	      tail = tail->next;
	    }
	  tail->sym = sym;
	  goto next_item;
	case MATCH_NO:
	  break;
	case MATCH_ERROR:
	  goto cleanup;
	}

      if (!allow_common)
	goto syntax;

      m = gfc_match (" / %n /", n);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;

      st = gfc_find_symtree (gfc_current_ns->common_root, n);
      if (st == NULL)
	{
	  gfc_error ("COMMON block /%s/ not found at %C", n);
	  goto cleanup;
	}
      for (sym = st->n.common->head; sym; sym = sym->common_next)
	{
	  gfc_set_sym_referenced (sym);
	  p = gfc_get_namelist ();
	  if (head == NULL)
	    head = tail = p;
	  else
	    {
	      tail->next = p;
	      tail = tail->next;
	    }
	  tail->sym = sym;
	}

    next_item:
      if (gfc_match_char (')') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  while (*list)
    list = &(*list)->next;

  *list = head;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in OpenMP variable list at %C");

cleanup:
  gfc_free_namelist (head);
  gfc_current_locus = old_loc;
  return MATCH_ERROR;
}

#define OMP_CLAUSE_PRIVATE	(1 << 0)
#define OMP_CLAUSE_FIRSTPRIVATE	(1 << 1)
#define OMP_CLAUSE_LASTPRIVATE	(1 << 2)
#define OMP_CLAUSE_COPYPRIVATE	(1 << 3)
#define OMP_CLAUSE_SHARED	(1 << 4)
#define OMP_CLAUSE_COPYIN	(1 << 5)
#define OMP_CLAUSE_REDUCTION	(1 << 6)
#define OMP_CLAUSE_IF		(1 << 7)
#define OMP_CLAUSE_NUM_THREADS	(1 << 8)
#define OMP_CLAUSE_SCHEDULE	(1 << 9)
#define OMP_CLAUSE_DEFAULT	(1 << 10)
#define OMP_CLAUSE_ORDERED	(1 << 11)

/* Match OpenMP directive clauses. MASK is a bitmask of
   clauses that are allowed for a particular directive.  */

static match
gfc_match_omp_clauses (gfc_omp_clauses **cp, int mask)
{
  gfc_omp_clauses *c = gfc_get_omp_clauses ();
  locus old_loc;
  bool needs_space = true, first = true;

  *cp = NULL;
  while (1)
    {
      if ((first || gfc_match_char (',') != MATCH_YES)
	  && (needs_space && gfc_match_space () != MATCH_YES))
	break;
      needs_space = false;
      first = false;
      gfc_gobble_whitespace ();
      if ((mask & OMP_CLAUSE_IF) && c->if_expr == NULL
	  && gfc_match ("if ( %e )", &c->if_expr) == MATCH_YES)
	continue;
      if ((mask & OMP_CLAUSE_NUM_THREADS) && c->num_threads == NULL
	  && gfc_match ("num_threads ( %e )", &c->num_threads) == MATCH_YES)
	continue;
      if ((mask & OMP_CLAUSE_PRIVATE)
	  && gfc_match_omp_variable_list ("private (",
					  &c->lists[OMP_LIST_PRIVATE], true)
	     == MATCH_YES)
	continue;
      if ((mask & OMP_CLAUSE_FIRSTPRIVATE)
	  && gfc_match_omp_variable_list ("firstprivate (",
					  &c->lists[OMP_LIST_FIRSTPRIVATE],
					  true)
	     == MATCH_YES)
	continue;
      if ((mask & OMP_CLAUSE_LASTPRIVATE)
	  && gfc_match_omp_variable_list ("lastprivate (",
					  &c->lists[OMP_LIST_LASTPRIVATE],
					  true)
	     == MATCH_YES)
	continue;
      if ((mask & OMP_CLAUSE_COPYPRIVATE)
	  && gfc_match_omp_variable_list ("copyprivate (",
					  &c->lists[OMP_LIST_COPYPRIVATE],
					  true)
	     == MATCH_YES)
	continue;
      if ((mask & OMP_CLAUSE_SHARED)
	  && gfc_match_omp_variable_list ("shared (",
					  &c->lists[OMP_LIST_SHARED], true)
	     == MATCH_YES)
	continue;
      if ((mask & OMP_CLAUSE_COPYIN)
	  && gfc_match_omp_variable_list ("copyin (",
					  &c->lists[OMP_LIST_COPYIN], true)
	     == MATCH_YES)
	continue;
      old_loc = gfc_current_locus;
      if ((mask & OMP_CLAUSE_REDUCTION)
	  && gfc_match ("reduction ( ") == MATCH_YES)
	{
	  int reduction = OMP_LIST_NUM;
	  char buffer[GFC_MAX_SYMBOL_LEN + 1];
	  if (gfc_match_char ('+') == MATCH_YES)
	    reduction = OMP_LIST_PLUS;
	  else if (gfc_match_char ('*') == MATCH_YES)
	    reduction = OMP_LIST_MULT;
	  else if (gfc_match_char ('-') == MATCH_YES)
	    reduction = OMP_LIST_SUB;
	  else if (gfc_match (".and.") == MATCH_YES)
	    reduction = OMP_LIST_AND;
	  else if (gfc_match (".or.") == MATCH_YES)
	    reduction = OMP_LIST_OR;
	  else if (gfc_match (".eqv.") == MATCH_YES)
	    reduction = OMP_LIST_EQV;
	  else if (gfc_match (".neqv.") == MATCH_YES)
	    reduction = OMP_LIST_NEQV;
	  else if (gfc_match_name (buffer) == MATCH_YES)
	    {
	      gfc_symbol *sym;
	      const char *n = buffer;

	      gfc_find_symbol (buffer, NULL, 1, &sym);
	      if (sym != NULL)
		{
		  if (sym->attr.intrinsic)
		    n = sym->name;
		  else if ((sym->attr.flavor != FL_UNKNOWN
			    && sym->attr.flavor != FL_PROCEDURE)
			   || sym->attr.external
			   || sym->attr.generic
			   || sym->attr.entry
			   || sym->attr.result
			   || sym->attr.dummy
			   || sym->attr.subroutine
			   || sym->attr.pointer
			   || sym->attr.target
			   || sym->attr.cray_pointer
			   || sym->attr.cray_pointee
			   || (sym->attr.proc != PROC_UNKNOWN
			       && sym->attr.proc != PROC_INTRINSIC)
			   || sym->attr.if_source != IFSRC_UNKNOWN
			   || sym == sym->ns->proc_name)
		    {
		      gfc_error_now ("%s is not INTRINSIC procedure name "
				     "at %C", buffer);
		      sym = NULL;
		    }
		  else
		    n = sym->name;
		}
	      if (strcmp (n, "max") == 0)
		reduction = OMP_LIST_MAX;
	      else if (strcmp (n, "min") == 0)
		reduction = OMP_LIST_MIN;
	      else if (strcmp (n, "iand") == 0)
		reduction = OMP_LIST_IAND;
	      else if (strcmp (n, "ior") == 0)
		reduction = OMP_LIST_IOR;
	      else if (strcmp (n, "ieor") == 0)
		reduction = OMP_LIST_IEOR;
	      if (reduction != OMP_LIST_NUM
		  && sym != NULL
		  && ! sym->attr.intrinsic
		  && ! sym->attr.use_assoc
		  && ((sym->attr.flavor == FL_UNKNOWN
		       && gfc_add_flavor (&sym->attr, FL_PROCEDURE,
					  sym->name, NULL) == FAILURE)
		      || gfc_add_intrinsic (&sym->attr, NULL) == FAILURE))
		{
		  gfc_free_omp_clauses (c);
		  return MATCH_ERROR;
		}
	    }
	  if (reduction != OMP_LIST_NUM
	      && gfc_match_omp_variable_list (" :", &c->lists[reduction],
					      false)
		 == MATCH_YES)
	    continue;
	  else
	    gfc_current_locus = old_loc;
	}
      if ((mask & OMP_CLAUSE_DEFAULT)
	  && c->default_sharing == OMP_DEFAULT_UNKNOWN)
	{
	  if (gfc_match ("default ( shared )") == MATCH_YES)
	    c->default_sharing = OMP_DEFAULT_SHARED;
	  else if (gfc_match ("default ( private )") == MATCH_YES)
	    c->default_sharing = OMP_DEFAULT_PRIVATE;
	  else if (gfc_match ("default ( none )") == MATCH_YES)
	    c->default_sharing = OMP_DEFAULT_NONE;
	  if (c->default_sharing != OMP_DEFAULT_UNKNOWN)
	    continue;
	}
      old_loc = gfc_current_locus;
      if ((mask & OMP_CLAUSE_SCHEDULE)
	  && c->sched_kind == OMP_SCHED_NONE
	  && gfc_match ("schedule ( ") == MATCH_YES)
	{
	  if (gfc_match ("static") == MATCH_YES)
	    c->sched_kind = OMP_SCHED_STATIC;
	  else if (gfc_match ("dynamic") == MATCH_YES)
	    c->sched_kind = OMP_SCHED_DYNAMIC;
	  else if (gfc_match ("guided") == MATCH_YES)
	    c->sched_kind = OMP_SCHED_GUIDED;
	  else if (gfc_match ("runtime") == MATCH_YES)
	    c->sched_kind = OMP_SCHED_RUNTIME;
	  if (c->sched_kind != OMP_SCHED_NONE)
	    {
	      match m = MATCH_NO;
	      if (c->sched_kind != OMP_SCHED_RUNTIME)
		m = gfc_match (" , %e )", &c->chunk_size);
	      if (m != MATCH_YES)
		m = gfc_match_char (')');
	      if (m != MATCH_YES)
		c->sched_kind = OMP_SCHED_NONE;
	    }
	  if (c->sched_kind != OMP_SCHED_NONE)
	    continue;
	  else
	    gfc_current_locus = old_loc;
	}
      if ((mask & OMP_CLAUSE_ORDERED) && !c->ordered
	  && gfc_match ("ordered") == MATCH_YES)
	{
	  c->ordered = needs_space = true;
	  continue;
	}

      break;
    }

  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_free_omp_clauses (c);
      return MATCH_ERROR;
    }

  *cp = c;
  return MATCH_YES;
}

#define OMP_PARALLEL_CLAUSES \
  (OMP_CLAUSE_PRIVATE | OMP_CLAUSE_FIRSTPRIVATE | OMP_CLAUSE_SHARED	\
   | OMP_CLAUSE_COPYIN | OMP_CLAUSE_REDUCTION | OMP_CLAUSE_IF		\
   | OMP_CLAUSE_NUM_THREADS | OMP_CLAUSE_DEFAULT)
#define OMP_DO_CLAUSES \
  (OMP_CLAUSE_PRIVATE | OMP_CLAUSE_FIRSTPRIVATE				\
   | OMP_CLAUSE_LASTPRIVATE | OMP_CLAUSE_REDUCTION			\
   | OMP_CLAUSE_SCHEDULE | OMP_CLAUSE_ORDERED)
#define OMP_SECTIONS_CLAUSES \
  (OMP_CLAUSE_PRIVATE | OMP_CLAUSE_FIRSTPRIVATE				\
   | OMP_CLAUSE_LASTPRIVATE | OMP_CLAUSE_REDUCTION)

match
gfc_match_omp_parallel (void)
{
  gfc_omp_clauses *c;
  if (gfc_match_omp_clauses (&c, OMP_PARALLEL_CLAUSES) != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_PARALLEL;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


match
gfc_match_omp_critical (void)
{
  char n[GFC_MAX_SYMBOL_LEN+1];

  if (gfc_match (" ( %n )", n) != MATCH_YES)
    n[0] = '\0';
  if (gfc_match_omp_eos () != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_CRITICAL;
  new_st.ext.omp_name = n[0] ? xstrdup (n) : NULL;
  return MATCH_YES;
}


match
gfc_match_omp_do (void)
{
  gfc_omp_clauses *c;
  if (gfc_match_omp_clauses (&c, OMP_DO_CLAUSES) != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_DO;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


match
gfc_match_omp_flush (void)
{
  gfc_namelist *list = NULL;
  gfc_match_omp_variable_list (" (", &list, true);
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_free_namelist (list);
      return MATCH_ERROR;
    }
  new_st.op = EXEC_OMP_FLUSH;
  new_st.ext.omp_namelist = list;
  return MATCH_YES;
}


match
gfc_match_omp_threadprivate (void)
{
  locus old_loc;
  char n[GFC_MAX_SYMBOL_LEN+1];
  gfc_symbol *sym;
  match m;
  gfc_symtree *st;

  old_loc = gfc_current_locus;

  m = gfc_match (" (");
  if (m != MATCH_YES)
    return m;

  for (;;)
    {
      m = gfc_match_symbol (&sym, 0);
      switch (m)
	{
	case MATCH_YES:
	  if (sym->attr.in_common)
	    gfc_error_now ("Threadprivate variable at %C is an element of "
			   "a COMMON block");
	  else if (gfc_add_threadprivate (&sym->attr, sym->name,
		   &sym->declared_at) == FAILURE)
	    goto cleanup;
	  goto next_item;
	case MATCH_NO:
	  break;
	case MATCH_ERROR:
	  goto cleanup;
	}

      m = gfc_match (" / %n /", n);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO || n[0] == '\0')
	goto syntax;

      st = gfc_find_symtree (gfc_current_ns->common_root, n);
      if (st == NULL)
	{
	  gfc_error ("COMMON block /%s/ not found at %C", n);
	  goto cleanup;
	}
      st->n.common->threadprivate = 1;
      for (sym = st->n.common->head; sym; sym = sym->common_next)
	if (gfc_add_threadprivate (&sym->attr, sym->name,
				   &sym->declared_at) == FAILURE)
	  goto cleanup;

    next_item:
      if (gfc_match_char (')') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in !$OMP THREADPRIVATE list at %C");

cleanup:
  gfc_current_locus = old_loc;
  return MATCH_ERROR;
}


match
gfc_match_omp_parallel_do (void)
{
  gfc_omp_clauses *c;
  if (gfc_match_omp_clauses (&c, OMP_PARALLEL_CLAUSES | OMP_DO_CLAUSES)
      != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_PARALLEL_DO;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


match
gfc_match_omp_parallel_sections (void)
{
  gfc_omp_clauses *c;
  if (gfc_match_omp_clauses (&c, OMP_PARALLEL_CLAUSES | OMP_SECTIONS_CLAUSES)
      != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_PARALLEL_SECTIONS;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


match
gfc_match_omp_parallel_workshare (void)
{
  gfc_omp_clauses *c;
  if (gfc_match_omp_clauses (&c, OMP_PARALLEL_CLAUSES) != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_PARALLEL_WORKSHARE;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


match
gfc_match_omp_sections (void)
{
  gfc_omp_clauses *c;
  if (gfc_match_omp_clauses (&c, OMP_SECTIONS_CLAUSES) != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_SECTIONS;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


match
gfc_match_omp_single (void)
{
  gfc_omp_clauses *c;
  if (gfc_match_omp_clauses (&c, OMP_CLAUSE_PRIVATE | OMP_CLAUSE_FIRSTPRIVATE)
      != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_SINGLE;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


match
gfc_match_omp_workshare (void)
{
  if (gfc_match_omp_eos () != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_WORKSHARE;
  new_st.ext.omp_clauses = gfc_get_omp_clauses ();
  return MATCH_YES;
}


match
gfc_match_omp_master (void)
{
  if (gfc_match_omp_eos () != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_MASTER;
  new_st.ext.omp_clauses = NULL;
  return MATCH_YES;
}


match
gfc_match_omp_ordered (void)
{
  if (gfc_match_omp_eos () != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_ORDERED;
  new_st.ext.omp_clauses = NULL;
  return MATCH_YES;
}


match
gfc_match_omp_atomic (void)
{
  if (gfc_match_omp_eos () != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_ATOMIC;
  new_st.ext.omp_clauses = NULL;
  return MATCH_YES;
}


match
gfc_match_omp_barrier (void)
{
  if (gfc_match_omp_eos () != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_BARRIER;
  new_st.ext.omp_clauses = NULL;
  return MATCH_YES;
}


match
gfc_match_omp_end_nowait (void)
{
  bool nowait = false;
  if (gfc_match ("% nowait") == MATCH_YES)
    nowait = true;
  if (gfc_match_omp_eos () != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_END_NOWAIT;
  new_st.ext.omp_bool = nowait;
  return MATCH_YES;
}


match
gfc_match_omp_end_single (void)
{
  gfc_omp_clauses *c;
  if (gfc_match ("% nowait") == MATCH_YES)
    {
      new_st.op = EXEC_OMP_END_NOWAIT;
      new_st.ext.omp_bool = true;
      return MATCH_YES;
    }
  if (gfc_match_omp_clauses (&c, OMP_CLAUSE_COPYPRIVATE) != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_END_SINGLE;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


/* OpenMP directive resolving routines.  */

static void
resolve_omp_clauses (gfc_code *code)
{
  gfc_omp_clauses *omp_clauses = code->ext.omp_clauses;
  gfc_namelist *n;
  int list;
  static const char *clause_names[]
    = { "PRIVATE", "FIRSTPRIVATE", "LASTPRIVATE", "COPYPRIVATE", "SHARED",
	"COPYIN", "REDUCTION" };

  if (omp_clauses == NULL)
    return;

  if (omp_clauses->if_expr)
    {
      gfc_expr *expr = omp_clauses->if_expr;
      if (gfc_resolve_expr (expr) == FAILURE
	  || expr->ts.type != BT_LOGICAL || expr->rank != 0)
	gfc_error ("IF clause at %L requires a scalar LOGICAL expression",
		   &expr->where);
    }
  if (omp_clauses->num_threads)
    {
      gfc_expr *expr = omp_clauses->num_threads;
      if (gfc_resolve_expr (expr) == FAILURE
	  || expr->ts.type != BT_INTEGER || expr->rank != 0)
	gfc_error ("NUM_THREADS clause at %L requires a scalar "
		   "INTEGER expression", &expr->where);
    }
  if (omp_clauses->chunk_size)
    {
      gfc_expr *expr = omp_clauses->chunk_size;
      if (gfc_resolve_expr (expr) == FAILURE
	  || expr->ts.type != BT_INTEGER || expr->rank != 0)
	gfc_error ("SCHEDULE clause's chunk_size at %L requires "
		   "a scalar INTEGER expression", &expr->where);
    }

  /* Check that no symbol appears on multiple clauses, except that
     a symbol can appear on both firstprivate and lastprivate.  */
  for (list = 0; list < OMP_LIST_NUM; list++)
    for (n = omp_clauses->lists[list]; n; n = n->next)
      {
	n->sym->mark = 0;
	if (n->sym->attr.flavor == FL_VARIABLE)
	  continue;
	if (n->sym->attr.flavor == FL_PROCEDURE
	    && n->sym->result == n->sym
	    && n->sym->attr.function)
	  {
	    if (gfc_current_ns->proc_name == n->sym
		|| (gfc_current_ns->parent
		    && gfc_current_ns->parent->proc_name == n->sym))
	      continue;
	    if (gfc_current_ns->proc_name->attr.entry_master)
	      {
		gfc_entry_list *el = gfc_current_ns->entries;
		for (; el; el = el->next)
		  if (el->sym == n->sym)
		    break;
		if (el)
		  continue;
	      }
	    if (gfc_current_ns->parent
		&& gfc_current_ns->parent->proc_name->attr.entry_master)
	      {
		gfc_entry_list *el = gfc_current_ns->parent->entries;
		for (; el; el = el->next)
		  if (el->sym == n->sym)
		    break;
		if (el)
		  continue;
	      }
	  }
	gfc_error ("Object '%s' is not a variable at %L", n->sym->name,
		   &code->loc);
      }

  for (list = 0; list < OMP_LIST_NUM; list++)
    if (list != OMP_LIST_FIRSTPRIVATE && list != OMP_LIST_LASTPRIVATE)
      for (n = omp_clauses->lists[list]; n; n = n->next)
	if (n->sym->mark)
	  gfc_error ("Symbol '%s' present on multiple clauses at %L",
		     n->sym->name, &code->loc);
	else
	  n->sym->mark = 1;

  gcc_assert (OMP_LIST_LASTPRIVATE == OMP_LIST_FIRSTPRIVATE + 1);
  for (list = OMP_LIST_FIRSTPRIVATE; list <= OMP_LIST_LASTPRIVATE; list++)
    for (n = omp_clauses->lists[list]; n; n = n->next)
      if (n->sym->mark)
	{
	  gfc_error ("Symbol '%s' present on multiple clauses at %L",
		     n->sym->name, &code->loc);
	  n->sym->mark = 0;
	}

  for (n = omp_clauses->lists[OMP_LIST_FIRSTPRIVATE]; n; n = n->next)
    if (n->sym->mark)
      gfc_error ("Symbol '%s' present on multiple clauses at %L",
		 n->sym->name, &code->loc);
    else
      n->sym->mark = 1;

  for (n = omp_clauses->lists[OMP_LIST_LASTPRIVATE]; n; n = n->next)
    n->sym->mark = 0;

  for (n = omp_clauses->lists[OMP_LIST_LASTPRIVATE]; n; n = n->next)
    if (n->sym->mark)
      gfc_error ("Symbol '%s' present on multiple clauses at %L",
		 n->sym->name, &code->loc);
    else
      n->sym->mark = 1;

  for (list = 0; list < OMP_LIST_NUM; list++)
    if ((n = omp_clauses->lists[list]) != NULL)
      {
	const char *name;

	if (list < OMP_LIST_REDUCTION_FIRST)
	  name = clause_names[list];
	else if (list <= OMP_LIST_REDUCTION_LAST)
	  name = clause_names[OMP_LIST_REDUCTION_FIRST];
	else
	  gcc_unreachable ();

	switch (list)
	  {
	  case OMP_LIST_COPYIN:
	    for (; n != NULL; n = n->next)
	      {
		if (!n->sym->attr.threadprivate)
		  gfc_error ("Non-THREADPRIVATE object '%s' in COPYIN clause"
			     " at %L", n->sym->name, &code->loc);
		if (n->sym->attr.allocatable)
		  gfc_error ("COPYIN clause object '%s' is ALLOCATABLE at %L",
			     n->sym->name, &code->loc);
		if (n->sym->ts.type == BT_DERIVED && n->sym->ts.derived->attr.alloc_comp)
		  gfc_error ("COPYIN clause object '%s' at %L has ALLOCATABLE components",
			     n->sym->name, &code->loc);
	      }
	    break;
	  case OMP_LIST_COPYPRIVATE:
	    for (; n != NULL; n = n->next)
	      {
		if (n->sym->as && n->sym->as->type == AS_ASSUMED_SIZE)
		  gfc_error ("Assumed size array '%s' in COPYPRIVATE clause "
			     "at %L", n->sym->name, &code->loc);
		if (n->sym->attr.allocatable)
		  gfc_error ("COPYPRIVATE clause object '%s' is ALLOCATABLE "
			     "at %L", n->sym->name, &code->loc);
		if (n->sym->ts.type == BT_DERIVED && n->sym->ts.derived->attr.alloc_comp)
		  gfc_error ("COPYPRIVATE clause object '%s' at %L has ALLOCATABLE components",
			     n->sym->name, &code->loc);
	      }
	    break;
	  case OMP_LIST_SHARED:
	    for (; n != NULL; n = n->next)
	      {
		if (n->sym->attr.threadprivate)
		  gfc_error ("THREADPRIVATE object '%s' in SHARED clause at "
			     "%L", n->sym->name, &code->loc);
		if (n->sym->attr.cray_pointee)
		  gfc_error ("Cray pointee '%s' in SHARED clause at %L",
			    n->sym->name, &code->loc);
	      }
	    break;
	  default:
	    for (; n != NULL; n = n->next)
	      {
		if (n->sym->attr.threadprivate)
		  gfc_error ("THREADPRIVATE object '%s' in %s clause at %L",
			     n->sym->name, name, &code->loc);
		if (n->sym->attr.cray_pointee)
		  gfc_error ("Cray pointee '%s' in %s clause at %L",
			    n->sym->name, name, &code->loc);
		if (list != OMP_LIST_PRIVATE)
		  {
		    if (n->sym->attr.pointer)
		      gfc_error ("POINTER object '%s' in %s clause at %L",
				 n->sym->name, name, &code->loc);
		    if (n->sym->attr.allocatable)
		      gfc_error ("%s clause object '%s' is ALLOCATABLE at %L",
				 name, n->sym->name, &code->loc);
		    /* Variables in REDUCTION-clauses must be of intrinsic type (flagged below).  */
		    if ((list < OMP_LIST_REDUCTION_FIRST || list > OMP_LIST_REDUCTION_LAST) &&
		        n->sym->ts.type == BT_DERIVED && n->sym->ts.derived->attr.alloc_comp)
		      gfc_error ("%s clause object '%s' has ALLOCATABLE components at %L",
				 name, n->sym->name, &code->loc);
		    if (n->sym->attr.cray_pointer)
		      gfc_error ("Cray pointer '%s' in %s clause at %L",
				 n->sym->name, name, &code->loc);
		  }
		if (n->sym->as && n->sym->as->type == AS_ASSUMED_SIZE)
		  gfc_error ("Assumed size array '%s' in %s clause at %L",
			     n->sym->name, name, &code->loc);
		if (n->sym->attr.in_namelist
		    && (list < OMP_LIST_REDUCTION_FIRST
			|| list > OMP_LIST_REDUCTION_LAST))
		  gfc_error ("Variable '%s' in %s clause is used in "
			     "NAMELIST statement at %L",
			     n->sym->name, name, &code->loc);
		switch (list)
		  {
		  case OMP_LIST_PLUS:
		  case OMP_LIST_MULT:
		  case OMP_LIST_SUB:
		    if (!gfc_numeric_ts (&n->sym->ts))
		      gfc_error ("%c REDUCTION variable '%s' at %L must be of numeric type, got %s",
				 list == OMP_LIST_PLUS ? '+'
				 : list == OMP_LIST_MULT ? '*' : '-',
				 n->sym->name, &code->loc,
				 gfc_typename (&n->sym->ts));
		    break;
		  case OMP_LIST_AND:
		  case OMP_LIST_OR:
		  case OMP_LIST_EQV:
		  case OMP_LIST_NEQV:
		    if (n->sym->ts.type != BT_LOGICAL)
		      gfc_error ("%s REDUCTION variable '%s' must be LOGICAL "
				 "at %L",
				 list == OMP_LIST_AND ? ".AND."
				 : list == OMP_LIST_OR ? ".OR."
				 : list == OMP_LIST_EQV ? ".EQV." : ".NEQV.",
				 n->sym->name, &code->loc);
		    break;
		  case OMP_LIST_MAX:
		  case OMP_LIST_MIN:
		    if (n->sym->ts.type != BT_INTEGER
			&& n->sym->ts.type != BT_REAL)
		      gfc_error ("%s REDUCTION variable '%s' must be "
				 "INTEGER or REAL at %L",
				 list == OMP_LIST_MAX ? "MAX" : "MIN",
				 n->sym->name, &code->loc);
		    break;
		  case OMP_LIST_IAND:
		  case OMP_LIST_IOR:
		  case OMP_LIST_IEOR:
		    if (n->sym->ts.type != BT_INTEGER)
		      gfc_error ("%s REDUCTION variable '%s' must be INTEGER "
				 "at %L",
				 list == OMP_LIST_IAND ? "IAND"
				 : list == OMP_LIST_MULT ? "IOR" : "IEOR",
				 n->sym->name, &code->loc);
		    break;
		  /* Workaround for PR middle-end/26316, nothing really needs
		     to be done here for OMP_LIST_PRIVATE.  */
		  case OMP_LIST_PRIVATE:
		    gcc_assert (code->op != EXEC_NOP);
		  default:
		    break;
		  }
	      }
	    break;
	  }
      }
}


/* Return true if SYM is ever referenced in EXPR except in the SE node.  */

static bool
expr_references_sym (gfc_expr *e, gfc_symbol *s, gfc_expr *se)
{
  gfc_actual_arglist *arg;
  if (e == NULL || e == se)
    return false;
  switch (e->expr_type)
    {
    case EXPR_CONSTANT:
    case EXPR_NULL:
    case EXPR_VARIABLE:
    case EXPR_STRUCTURE:
    case EXPR_ARRAY:
      if (e->symtree != NULL
	  && e->symtree->n.sym == s)
	return true;
      return false;
    case EXPR_SUBSTRING:
      if (e->ref != NULL
	  && (expr_references_sym (e->ref->u.ss.start, s, se)
	      || expr_references_sym (e->ref->u.ss.end, s, se)))
	return true;
      return false;
    case EXPR_OP:
      if (expr_references_sym (e->value.op.op2, s, se))
	return true;
      return expr_references_sym (e->value.op.op1, s, se);
    case EXPR_FUNCTION:
      for (arg = e->value.function.actual; arg; arg = arg->next)
	if (expr_references_sym (arg->expr, s, se))
	  return true;
      return false;
    default:
      gcc_unreachable ();
    }
}


/* If EXPR is a conversion function that widens the type
   if WIDENING is true or narrows the type if WIDENING is false,
   return the inner expression, otherwise return NULL.  */

static gfc_expr *
is_conversion (gfc_expr *expr, bool widening)
{
  gfc_typespec *ts1, *ts2;

  if (expr->expr_type != EXPR_FUNCTION
      || expr->value.function.isym == NULL
      || expr->value.function.esym != NULL
      || expr->value.function.isym->id != GFC_ISYM_CONVERSION)
    return NULL;

  if (widening)
    {
      ts1 = &expr->ts;
      ts2 = &expr->value.function.actual->expr->ts;
    }
  else
    {
      ts1 = &expr->value.function.actual->expr->ts;
      ts2 = &expr->ts;
    }

  if (ts1->type > ts2->type
      || (ts1->type == ts2->type && ts1->kind > ts2->kind))
    return expr->value.function.actual->expr;

  return NULL;
}


static void
resolve_omp_atomic (gfc_code *code)
{
  gfc_symbol *var;
  gfc_expr *expr2;

  code = code->block->next;
  gcc_assert (code->op == EXEC_ASSIGN);
  gcc_assert (code->next == NULL);

  if (code->expr->expr_type != EXPR_VARIABLE
      || code->expr->symtree == NULL
      || code->expr->rank != 0
      || (code->expr->ts.type != BT_INTEGER
	  && code->expr->ts.type != BT_REAL
	  && code->expr->ts.type != BT_COMPLEX
	  && code->expr->ts.type != BT_LOGICAL))
    {
      gfc_error ("!$OMP ATOMIC statement must set a scalar variable of "
		 "intrinsic type at %L", &code->loc);
      return;
    }

  var = code->expr->symtree->n.sym;
  expr2 = is_conversion (code->expr2, false);
  if (expr2 == NULL)
    expr2 = code->expr2;

  if (expr2->expr_type == EXPR_OP)
    {
      gfc_expr *v = NULL, *e, *c;
      gfc_intrinsic_op op = expr2->value.op.operator;
      gfc_intrinsic_op alt_op = INTRINSIC_NONE;

      switch (op)
	{
	case INTRINSIC_PLUS:
	  alt_op = INTRINSIC_MINUS;
	  break;
	case INTRINSIC_TIMES:
	  alt_op = INTRINSIC_DIVIDE;
	  break;
	case INTRINSIC_MINUS:
	  alt_op = INTRINSIC_PLUS;
	  break;
	case INTRINSIC_DIVIDE:
	  alt_op = INTRINSIC_TIMES;
	  break;
	case INTRINSIC_AND:
	case INTRINSIC_OR:
	  break;
	case INTRINSIC_EQV:
	  alt_op = INTRINSIC_NEQV;
	  break;
	case INTRINSIC_NEQV:
	  alt_op = INTRINSIC_EQV;
	  break;
	default:
	  gfc_error ("!$OMP ATOMIC assignment operator must be "
		     "+, *, -, /, .AND., .OR., .EQV. or .NEQV. at %L",
		     &expr2->where);
	  return;
	}

      /* Check for var = var op expr resp. var = expr op var where
	 expr doesn't reference var and var op expr is mathematically
	 equivalent to var op (expr) resp. expr op var equivalent to
	 (expr) op var.  We rely here on the fact that the matcher
	 for x op1 y op2 z where op1 and op2 have equal precedence
	 returns (x op1 y) op2 z.  */
      e = expr2->value.op.op2;
      if (e->expr_type == EXPR_VARIABLE
	  && e->symtree != NULL
	  && e->symtree->n.sym == var)
	v = e;
      else if ((c = is_conversion (e, true)) != NULL
	       && c->expr_type == EXPR_VARIABLE
	       && c->symtree != NULL
	       && c->symtree->n.sym == var)
	v = c;
      else
	{
	  gfc_expr **p = NULL, **q;
	  for (q = &expr2->value.op.op1; (e = *q) != NULL; )
	    if (e->expr_type == EXPR_VARIABLE
		&& e->symtree != NULL
		&& e->symtree->n.sym == var)
	      {
		v = e;
		break;
	      }
	    else if ((c = is_conversion (e, true)) != NULL)
	      q = &e->value.function.actual->expr;
	    else if (e->expr_type != EXPR_OP
		     || (e->value.op.operator != op
			 && e->value.op.operator != alt_op)
		     || e->rank != 0)
	      break;
	    else
	      {
		p = q;
		q = &e->value.op.op1;
	      }

	  if (v == NULL)
	    {
	      gfc_error ("!$OMP ATOMIC assignment must be var = var op expr "
			 "or var = expr op var at %L", &expr2->where);
	      return;
	    }

	  if (p != NULL)
	    {
	      e = *p;
	      switch (e->value.op.operator)
		{
		case INTRINSIC_MINUS:
		case INTRINSIC_DIVIDE:
		case INTRINSIC_EQV:
		case INTRINSIC_NEQV:
		  gfc_error ("!$OMP ATOMIC var = var op expr not "
			     "mathematically equivalent to var = var op "
			     "(expr) at %L", &expr2->where);
		  break;
		default:
		  break;
		}

	      /* Canonicalize into var = var op (expr).  */
	      *p = e->value.op.op2;
	      e->value.op.op2 = expr2;
	      e->ts = expr2->ts;
	      if (code->expr2 == expr2)
		code->expr2 = expr2 = e;
	      else
		code->expr2->value.function.actual->expr = expr2 = e;

	      if (!gfc_compare_types (&expr2->value.op.op1->ts, &expr2->ts))
		{
		  for (p = &expr2->value.op.op1; *p != v;
		       p = &(*p)->value.function.actual->expr)
		    ;
		  *p = NULL;
		  gfc_free_expr (expr2->value.op.op1);
		  expr2->value.op.op1 = v;
		  gfc_convert_type (v, &expr2->ts, 2);
		}
	    }
	}

      if (e->rank != 0 || expr_references_sym (code->expr2, var, v))
	{
	  gfc_error ("expr in !$OMP ATOMIC assignment var = var op expr "
		     "must be scalar and cannot reference var at %L",
		     &expr2->where);
	  return;
	}
    }
  else if (expr2->expr_type == EXPR_FUNCTION
	   && expr2->value.function.isym != NULL
	   && expr2->value.function.esym == NULL
	   && expr2->value.function.actual != NULL
	   && expr2->value.function.actual->next != NULL)
    {
      gfc_actual_arglist *arg, *var_arg;

      switch (expr2->value.function.isym->id)
	{
	case GFC_ISYM_MIN:
	case GFC_ISYM_MAX:
	  break;
	case GFC_ISYM_IAND:
	case GFC_ISYM_IOR:
	case GFC_ISYM_IEOR:
	  if (expr2->value.function.actual->next->next != NULL)
	    {
	      gfc_error ("!$OMP ATOMIC assignment intrinsic IAND, IOR "
			 "or IEOR must have two arguments at %L",
			 &expr2->where);
	      return;
	    }
	  break;
	default:
	  gfc_error ("!$OMP ATOMIC assignment intrinsic must be "
		     "MIN, MAX, IAND, IOR or IEOR at %L",
		     &expr2->where);
	  return;
	}

      var_arg = NULL;
      for (arg = expr2->value.function.actual; arg; arg = arg->next)
	{
	  if ((arg == expr2->value.function.actual
	       || (var_arg == NULL && arg->next == NULL))
	      && arg->expr->expr_type == EXPR_VARIABLE
	      && arg->expr->symtree != NULL
	      && arg->expr->symtree->n.sym == var)
	    var_arg = arg;
	  else if (expr_references_sym (arg->expr, var, NULL))
	    gfc_error ("!$OMP ATOMIC intrinsic arguments except one must not "
		       "reference '%s' at %L", var->name, &arg->expr->where);
	  if (arg->expr->rank != 0)
	    gfc_error ("!$OMP ATOMIC intrinsic arguments must be scalar "
		       "at %L", &arg->expr->where);
	}

      if (var_arg == NULL)
	{
	  gfc_error ("First or last !$OMP ATOMIC intrinsic argument must "
		     "be '%s' at %L", var->name, &expr2->where);
	  return;
	}

      if (var_arg != expr2->value.function.actual)
	{
	  /* Canonicalize, so that var comes first.  */
	  gcc_assert (var_arg->next == NULL);
	  for (arg = expr2->value.function.actual;
	       arg->next != var_arg; arg = arg->next)
	    ;
	  var_arg->next = expr2->value.function.actual;
	  expr2->value.function.actual = var_arg;
	  arg->next = NULL;
	}
    }
  else
    gfc_error ("!$OMP ATOMIC assignment must have an operator or intrinsic "
	       "on right hand side at %L", &expr2->where);
}


struct omp_context
{
  gfc_code *code;
  struct pointer_set_t *sharing_clauses;
  struct pointer_set_t *private_iterators;
  struct omp_context *previous;
} *omp_current_ctx;
gfc_code *omp_current_do_code;


void
gfc_resolve_omp_do_blocks (gfc_code *code, gfc_namespace *ns)
{
  if (code->block->next && code->block->next->op == EXEC_DO)
    omp_current_do_code = code->block->next;
  gfc_resolve_blocks (code->block, ns);
}


void
gfc_resolve_omp_parallel_blocks (gfc_code *code, gfc_namespace *ns)
{
  struct omp_context ctx;
  gfc_omp_clauses *omp_clauses = code->ext.omp_clauses;
  gfc_namelist *n;
  int list;

  ctx.code = code;
  ctx.sharing_clauses = pointer_set_create ();
  ctx.private_iterators = pointer_set_create ();
  ctx.previous = omp_current_ctx;
  omp_current_ctx = &ctx;

  for (list = 0; list < OMP_LIST_NUM; list++)
    for (n = omp_clauses->lists[list]; n; n = n->next)
      pointer_set_insert (ctx.sharing_clauses, n->sym);

  if (code->op == EXEC_OMP_PARALLEL_DO)
    gfc_resolve_omp_do_blocks (code, ns);
  else
    gfc_resolve_blocks (code->block, ns);

  omp_current_ctx = ctx.previous;
  pointer_set_destroy (ctx.sharing_clauses);
  pointer_set_destroy (ctx.private_iterators);
}


/* Note a DO iterator variable.  This is special in !$omp parallel
   construct, where they are predetermined private.  */

void
gfc_resolve_do_iterator (gfc_code *code, gfc_symbol *sym)
{
  struct omp_context *ctx;

  if (sym->attr.threadprivate)
    return;

  /* !$omp do and !$omp parallel do iteration variable is predetermined
     private just in the !$omp do resp. !$omp parallel do construct,
     with no implications for the outer parallel constructs.  */
  if (code == omp_current_do_code)
    return;

  for (ctx = omp_current_ctx; ctx; ctx = ctx->previous)
    {
      if (pointer_set_contains (ctx->sharing_clauses, sym))
	continue;

      if (! pointer_set_insert (ctx->private_iterators, sym))
	{
	  gfc_omp_clauses *omp_clauses = ctx->code->ext.omp_clauses;
	  gfc_namelist *p;

	  p = gfc_get_namelist ();
	  p->sym = sym;
	  p->next = omp_clauses->lists[OMP_LIST_PRIVATE];
	  omp_clauses->lists[OMP_LIST_PRIVATE] = p;
	}
    }
}


static void
resolve_omp_do (gfc_code *code)
{
  gfc_code *do_code;
  int list;
  gfc_namelist *n;
  gfc_symbol *dovar;

  if (code->ext.omp_clauses)
    resolve_omp_clauses (code);

  do_code = code->block->next;
  if (do_code->op == EXEC_DO_WHILE)
    gfc_error ("!$OMP DO cannot be a DO WHILE or DO without loop control "
	       "at %L", &do_code->loc);
  else
    {
      gcc_assert (do_code->op == EXEC_DO);
      if (do_code->ext.iterator->var->ts.type != BT_INTEGER)
	gfc_error ("!$OMP DO iteration variable must be of type integer at %L",
		   &do_code->loc);
      dovar = do_code->ext.iterator->var->symtree->n.sym;
      if (dovar->attr.threadprivate)
	gfc_error ("!$OMP DO iteration variable must not be THREADPRIVATE "
		   "at %L", &do_code->loc);
      if (code->ext.omp_clauses)
	for (list = 0; list < OMP_LIST_NUM; list++)
	  if (list != OMP_LIST_PRIVATE && list != OMP_LIST_LASTPRIVATE)
	    for (n = code->ext.omp_clauses->lists[list]; n; n = n->next)
	      if (dovar == n->sym)
		{
		  gfc_error ("!$OMP DO iteration variable present on clause "
			     "other than PRIVATE or LASTPRIVATE at %L",
			     &do_code->loc);
		  break;
		}
    }
}


/* Resolve OpenMP directive clauses and check various requirements
   of each directive.  */

void
gfc_resolve_omp_directive (gfc_code *code, gfc_namespace *ns ATTRIBUTE_UNUSED)
{
  switch (code->op)
    {
    case EXEC_OMP_DO:
    case EXEC_OMP_PARALLEL_DO:
      resolve_omp_do (code);
      break;
    case EXEC_OMP_WORKSHARE:
    case EXEC_OMP_PARALLEL_WORKSHARE:
    case EXEC_OMP_PARALLEL:
    case EXEC_OMP_PARALLEL_SECTIONS:
    case EXEC_OMP_SECTIONS:
    case EXEC_OMP_SINGLE:
      if (code->ext.omp_clauses)
	resolve_omp_clauses (code);
      break;
    case EXEC_OMP_ATOMIC:
      resolve_omp_atomic (code);
      break;
    default:
      break;
    }
}
