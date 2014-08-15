/* OpenMP directive matching and resolving.
   Copyright (C) 2005-2014 Free Software Foundation, Inc.
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
#include "coretypes.h"
#include "flags.h"
#include "gfortran.h"
#include "arith.h"
#include "match.h"
#include "parse.h"
#include "pointer-set.h"

/* Match an end of OpenMP directive.  End of OpenMP directive is optional
   whitespace, followed by '\n' or comment '!'.  */

match
gfc_match_omp_eos (void)
{
  locus old_loc;
  char c;

  old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();

  c = gfc_next_ascii_char ();
  switch (c)
    {
    case '!':
      do
	c = gfc_next_ascii_char ();
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
  gfc_free_expr (c->final_expr);
  gfc_free_expr (c->num_threads);
  gfc_free_expr (c->chunk_size);
  gfc_free_expr (c->safelen_expr);
  gfc_free_expr (c->simdlen_expr);
  gfc_free_expr (c->num_teams);
  gfc_free_expr (c->device);
  gfc_free_expr (c->thread_limit);
  gfc_free_expr (c->dist_chunk_size);
  for (i = 0; i < OMP_LIST_NUM; i++)
    gfc_free_omp_namelist (c->lists[i]);
  free (c);
}

/* Free an !$omp declare simd construct list.  */

void
gfc_free_omp_declare_simd (gfc_omp_declare_simd *ods)
{
  if (ods)
    {
      gfc_free_omp_clauses (ods->clauses);
      free (ods);
    }
}

void
gfc_free_omp_declare_simd_list (gfc_omp_declare_simd *list)
{
  while (list)
    {
      gfc_omp_declare_simd *current = list;
      list = list->next;
      gfc_free_omp_declare_simd (current);
    }
}

/* Free an !$omp declare reduction.  */

void
gfc_free_omp_udr (gfc_omp_udr *omp_udr)
{
  if (omp_udr)
    {
      gfc_free_omp_udr (omp_udr->next);
      gfc_free_namespace (omp_udr->combiner_ns);
      if (omp_udr->initializer_ns)
	gfc_free_namespace (omp_udr->initializer_ns);
      free (omp_udr);
    }
}


static gfc_omp_udr *
gfc_find_omp_udr (gfc_namespace *ns, const char *name, gfc_typespec *ts)
{
  gfc_symtree *st;

  if (ns == NULL)
    ns = gfc_current_ns;
  do
    {
      gfc_omp_udr *omp_udr;

      st = gfc_find_symtree (ns->omp_udr_root, name);
      if (st != NULL)
	for (omp_udr = st->n.omp_udr; omp_udr; omp_udr = omp_udr->next)
	  if (ts == NULL)
	    return omp_udr;
	  else if (gfc_compare_types (&omp_udr->ts, ts))
	    {
	      if (ts->type == BT_CHARACTER)
		{
		  if (omp_udr->ts.u.cl->length == NULL)
		    return omp_udr;
		  if (ts->u.cl->length == NULL)
		    continue;
		  if (gfc_compare_expr (omp_udr->ts.u.cl->length,
					ts->u.cl->length,
					INTRINSIC_EQ) != 0)
		    continue;
		}
	      return omp_udr;
	    }

      /* Don't escape an interface block.  */
      if (ns && !ns->has_import_set
	  && ns->proc_name && ns->proc_name->attr.if_source == IFSRC_IFBODY)
	break;

      ns = ns->parent;
    }
  while (ns != NULL);

  return NULL;
}


/* Match a variable/common block list and construct a namelist from it.  */

static match
gfc_match_omp_variable_list (const char *str, gfc_omp_namelist **list,
			     bool allow_common, bool *end_colon = NULL,
			     gfc_omp_namelist ***headp = NULL,
			     bool allow_sections = false)
{
  gfc_omp_namelist *head, *tail, *p;
  locus old_loc, cur_loc;
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
      cur_loc = gfc_current_locus;
      m = gfc_match_symbol (&sym, 1);
      switch (m)
	{
	case MATCH_YES:
	  gfc_expr *expr;
	  expr = NULL;
	  if (allow_sections && gfc_peek_ascii_char () == '(')
	    {
	      gfc_current_locus = cur_loc;
	      m = gfc_match_variable (&expr, 0);
	      switch (m)
		{
		case MATCH_ERROR:
		  goto cleanup;
		case MATCH_NO:
		  goto syntax;
		default:
		  break;
		}
	    }
	  gfc_set_sym_referenced (sym);
	  p = gfc_get_omp_namelist ();
	  if (head == NULL)
	    head = tail = p;
	  else
	    {
	      tail->next = p;
	      tail = tail->next;
	    }
	  tail->sym = sym;
	  tail->expr = expr;
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
	  p = gfc_get_omp_namelist ();
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
      if (end_colon && gfc_match_char (':') == MATCH_YES)
	{
	  *end_colon = true;
	  break;
	}
      if (gfc_match_char (')') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  while (*list)
    list = &(*list)->next;

  *list = head;
  if (headp)
    *headp = list;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in OpenMP variable list at %C");

cleanup:
  gfc_free_omp_namelist (head);
  gfc_current_locus = old_loc;
  return MATCH_ERROR;
}

#define OMP_CLAUSE_PRIVATE	(1U << 0)
#define OMP_CLAUSE_FIRSTPRIVATE	(1U << 1)
#define OMP_CLAUSE_LASTPRIVATE	(1U << 2)
#define OMP_CLAUSE_COPYPRIVATE	(1U << 3)
#define OMP_CLAUSE_SHARED	(1U << 4)
#define OMP_CLAUSE_COPYIN	(1U << 5)
#define OMP_CLAUSE_REDUCTION	(1U << 6)
#define OMP_CLAUSE_IF		(1U << 7)
#define OMP_CLAUSE_NUM_THREADS	(1U << 8)
#define OMP_CLAUSE_SCHEDULE	(1U << 9)
#define OMP_CLAUSE_DEFAULT	(1U << 10)
#define OMP_CLAUSE_ORDERED	(1U << 11)
#define OMP_CLAUSE_COLLAPSE	(1U << 12)
#define OMP_CLAUSE_UNTIED	(1U << 13)
#define OMP_CLAUSE_FINAL	(1U << 14)
#define OMP_CLAUSE_MERGEABLE	(1U << 15)
#define OMP_CLAUSE_ALIGNED	(1U << 16)
#define OMP_CLAUSE_DEPEND	(1U << 17)
#define OMP_CLAUSE_INBRANCH	(1U << 18)
#define OMP_CLAUSE_LINEAR	(1U << 19)
#define OMP_CLAUSE_NOTINBRANCH	(1U << 20)
#define OMP_CLAUSE_PROC_BIND	(1U << 21)
#define OMP_CLAUSE_SAFELEN	(1U << 22)
#define OMP_CLAUSE_SIMDLEN	(1U << 23)
#define OMP_CLAUSE_UNIFORM	(1U << 24)
#define OMP_CLAUSE_DEVICE	(1U << 25)
#define OMP_CLAUSE_MAP		(1U << 26)
#define OMP_CLAUSE_TO		(1U << 27)
#define OMP_CLAUSE_FROM		(1U << 28)
#define OMP_CLAUSE_NUM_TEAMS	(1U << 29)
#define OMP_CLAUSE_THREAD_LIMIT	(1U << 30)
#define OMP_CLAUSE_DIST_SCHEDULE	(1U << 31)

/* Match OpenMP directive clauses. MASK is a bitmask of
   clauses that are allowed for a particular directive.  */

static match
gfc_match_omp_clauses (gfc_omp_clauses **cp, unsigned int mask,
		       bool first = true, bool needs_space = true)
{
  gfc_omp_clauses *c = gfc_get_omp_clauses ();
  locus old_loc;

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
      if ((mask & OMP_CLAUSE_FINAL) && c->final_expr == NULL
	  && gfc_match ("final ( %e )", &c->final_expr) == MATCH_YES)
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
	  gfc_omp_reduction_op rop = OMP_REDUCTION_NONE;
	  char buffer[GFC_MAX_SYMBOL_LEN + 3];
	  if (gfc_match_char ('+') == MATCH_YES)
	    rop = OMP_REDUCTION_PLUS;
	  else if (gfc_match_char ('*') == MATCH_YES)
	    rop = OMP_REDUCTION_TIMES;
	  else if (gfc_match_char ('-') == MATCH_YES)
	    rop = OMP_REDUCTION_MINUS;
	  else if (gfc_match (".and.") == MATCH_YES)
	    rop = OMP_REDUCTION_AND;
	  else if (gfc_match (".or.") == MATCH_YES)
	    rop = OMP_REDUCTION_OR;
	  else if (gfc_match (".eqv.") == MATCH_YES)
	    rop = OMP_REDUCTION_EQV;
	  else if (gfc_match (".neqv.") == MATCH_YES)
	    rop = OMP_REDUCTION_NEQV;
	  if (rop != OMP_REDUCTION_NONE)
	    snprintf (buffer, sizeof buffer,
		      "operator %s", gfc_op2string ((gfc_intrinsic_op) rop));
	  else if (gfc_match_defined_op_name (buffer + 1, 1) == MATCH_YES)
	    {
	      buffer[0] = '.';
	      strcat (buffer, ".");
	    }
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
		      sym = NULL;
		      n = NULL;
		    }
		  else
		    n = sym->name;
		}
	      if (n == NULL)
		rop = OMP_REDUCTION_NONE;
	      else if (strcmp (n, "max") == 0)
		rop = OMP_REDUCTION_MAX;
	      else if (strcmp (n, "min") == 0)
		rop = OMP_REDUCTION_MIN;
	      else if (strcmp (n, "iand") == 0)
		rop = OMP_REDUCTION_IAND;
	      else if (strcmp (n, "ior") == 0)
		rop = OMP_REDUCTION_IOR;
	      else if (strcmp (n, "ieor") == 0)
		rop = OMP_REDUCTION_IEOR;
	      if (rop != OMP_REDUCTION_NONE
		  && sym != NULL
		  && ! sym->attr.intrinsic
		  && ! sym->attr.use_assoc
		  && ((sym->attr.flavor == FL_UNKNOWN
		       && !gfc_add_flavor (&sym->attr, FL_PROCEDURE,
					   sym->name, NULL))
		      || !gfc_add_intrinsic (&sym->attr, NULL)))
		rop = OMP_REDUCTION_NONE;
	    }
	  else
	    buffer[0] = '\0';
	  gfc_omp_udr *udr
	    = (buffer[0]
	       ? gfc_find_omp_udr (gfc_current_ns, buffer, NULL) : NULL);
	  gfc_omp_namelist **head = NULL;
	  if (rop == OMP_REDUCTION_NONE && udr)
	    rop = OMP_REDUCTION_USER;

	  if (gfc_match_omp_variable_list (" :",
					   &c->lists[OMP_LIST_REDUCTION],
					   false, NULL, &head) == MATCH_YES)
	    {
	      gfc_omp_namelist *n;
	      if (rop == OMP_REDUCTION_NONE)
		{
		  n = *head;
		  *head = NULL;
		  gfc_error_now ("!$OMP DECLARE REDUCTION %s not found "
				 "at %L", buffer, &old_loc);
		  gfc_free_omp_namelist (n);
		}
	      else
		for (n = *head; n; n = n->next)
		  {
		    n->u.reduction_op = rop;
		    if (udr)
		      {
			n->udr = gfc_get_omp_namelist_udr ();
			n->udr->udr = udr;
		      }
		  }
	      continue;
	    }
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
	  else if (gfc_match ("default ( firstprivate )") == MATCH_YES)
	    c->default_sharing = OMP_DEFAULT_FIRSTPRIVATE;
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
	  else if (gfc_match ("auto") == MATCH_YES)
	    c->sched_kind = OMP_SCHED_AUTO;
	  if (c->sched_kind != OMP_SCHED_NONE)
	    {
	      match m = MATCH_NO;
	      if (c->sched_kind != OMP_SCHED_RUNTIME
		  && c->sched_kind != OMP_SCHED_AUTO)
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
      if ((mask & OMP_CLAUSE_UNTIED) && !c->untied
	  && gfc_match ("untied") == MATCH_YES)
	{
	  c->untied = needs_space = true;
	  continue;
	}
      if ((mask & OMP_CLAUSE_MERGEABLE) && !c->mergeable
	  && gfc_match ("mergeable") == MATCH_YES)
	{
	  c->mergeable = needs_space = true;
	  continue;
	}
      if ((mask & OMP_CLAUSE_COLLAPSE) && !c->collapse)
	{
	  gfc_expr *cexpr = NULL;
	  match m = gfc_match ("collapse ( %e )", &cexpr);

	  if (m == MATCH_YES)
	    {
	      int collapse;
	      const char *p = gfc_extract_int (cexpr, &collapse);
	      if (p)
		{
		  gfc_error_now (p);
		  collapse = 1;
		}
	      else if (collapse <= 0)
		{
		  gfc_error_now ("COLLAPSE clause argument not"
				 " constant positive integer at %C");
		  collapse = 1;
		}
	      c->collapse = collapse;
	      gfc_free_expr (cexpr);
	      continue;
	    }
	}
      if ((mask & OMP_CLAUSE_INBRANCH) && !c->inbranch && !c->notinbranch
	  && gfc_match ("inbranch") == MATCH_YES)
	{
	  c->inbranch = needs_space = true;
	  continue;
	}
      if ((mask & OMP_CLAUSE_NOTINBRANCH) && !c->notinbranch && !c->inbranch
	  && gfc_match ("notinbranch") == MATCH_YES)
	{
	  c->notinbranch = needs_space = true;
	  continue;
	}
      if ((mask & OMP_CLAUSE_PROC_BIND)
	  && c->proc_bind == OMP_PROC_BIND_UNKNOWN)
	{
	  if (gfc_match ("proc_bind ( master )") == MATCH_YES)
	    c->proc_bind = OMP_PROC_BIND_MASTER;
	  else if (gfc_match ("proc_bind ( spread )") == MATCH_YES)
	    c->proc_bind = OMP_PROC_BIND_SPREAD;
	  else if (gfc_match ("proc_bind ( close )") == MATCH_YES)
	    c->proc_bind = OMP_PROC_BIND_CLOSE;
	  if (c->proc_bind != OMP_PROC_BIND_UNKNOWN)
	    continue;
	}
      if ((mask & OMP_CLAUSE_SAFELEN) && c->safelen_expr == NULL
	  && gfc_match ("safelen ( %e )", &c->safelen_expr) == MATCH_YES)
	continue;
      if ((mask & OMP_CLAUSE_SIMDLEN) && c->simdlen_expr == NULL
	  && gfc_match ("simdlen ( %e )", &c->simdlen_expr) == MATCH_YES)
	continue;
      if ((mask & OMP_CLAUSE_UNIFORM)
	  && gfc_match_omp_variable_list ("uniform (",
					  &c->lists[OMP_LIST_UNIFORM], false)
	     == MATCH_YES)
	continue;
      bool end_colon = false;
      gfc_omp_namelist **head = NULL;
      old_loc = gfc_current_locus;
      if ((mask & OMP_CLAUSE_ALIGNED)
	  && gfc_match_omp_variable_list ("aligned (",
					  &c->lists[OMP_LIST_ALIGNED], false,
					  &end_colon, &head)
	     == MATCH_YES)
	{
	  gfc_expr *alignment = NULL;
	  gfc_omp_namelist *n;

	  if (end_colon
	      && gfc_match (" %e )", &alignment) != MATCH_YES)
	    {
	      gfc_free_omp_namelist (*head);
	      gfc_current_locus = old_loc;
	      *head = NULL;
	      break;
	    }
	  for (n = *head; n; n = n->next)
	    if (n->next && alignment)
	      n->expr = gfc_copy_expr (alignment);
	    else
	      n->expr = alignment;
	  continue;
	}
      end_colon = false;
      head = NULL;
      old_loc = gfc_current_locus;
      if ((mask & OMP_CLAUSE_LINEAR)
	  && gfc_match_omp_variable_list ("linear (",
					  &c->lists[OMP_LIST_LINEAR], false,
					  &end_colon, &head)
	     == MATCH_YES)
	{
	  gfc_expr *step = NULL;

	  if (end_colon
	      && gfc_match (" %e )", &step) != MATCH_YES)
	    {
	      gfc_free_omp_namelist (*head);
	      gfc_current_locus = old_loc;
	      *head = NULL;
	      break;
	    }
	  else if (!end_colon)
	    {
	      step = gfc_get_constant_expr (BT_INTEGER,
					    gfc_default_integer_kind,
					    &old_loc);
	      mpz_set_si (step->value.integer, 1);
	    }
	  (*head)->expr = step;
	  continue;
	}
      if ((mask & OMP_CLAUSE_DEPEND)
	  && gfc_match ("depend ( ") == MATCH_YES)
	{
	  match m = MATCH_YES;
	  gfc_omp_depend_op depend_op = OMP_DEPEND_OUT;
	  if (gfc_match ("inout") == MATCH_YES)
	    depend_op = OMP_DEPEND_INOUT;
	  else if (gfc_match ("in") == MATCH_YES)
	    depend_op = OMP_DEPEND_IN;
	  else if (gfc_match ("out") == MATCH_YES)
	    depend_op = OMP_DEPEND_OUT;
	  else
	    m = MATCH_NO;
	  head = NULL;
	  if (m == MATCH_YES
	      && gfc_match_omp_variable_list (" : ",
					      &c->lists[OMP_LIST_DEPEND],
					      false, NULL, &head, true)
		 == MATCH_YES)
	    {
	      gfc_omp_namelist *n;
	      for (n = *head; n; n = n->next)
		n->u.depend_op = depend_op;
	      continue;
	    }
	  else
	    gfc_current_locus = old_loc;
	}
      if ((mask & OMP_CLAUSE_DIST_SCHEDULE)
	  && c->dist_sched_kind == OMP_SCHED_NONE
	  && gfc_match ("dist_schedule ( static") == MATCH_YES)
	{
	  match m = MATCH_NO;
	  c->dist_sched_kind = OMP_SCHED_STATIC;
	  m = gfc_match (" , %e )", &c->dist_chunk_size);
	  if (m != MATCH_YES)
	    m = gfc_match_char (')');
	  if (m != MATCH_YES)
	    {
	      c->dist_sched_kind = OMP_SCHED_NONE;
	      gfc_current_locus = old_loc;
	    }
	  else
	    continue;
	}
      if ((mask & OMP_CLAUSE_NUM_TEAMS) && c->num_teams == NULL
	  && gfc_match ("num_teams ( %e )", &c->num_teams) == MATCH_YES)
	continue;
      if ((mask & OMP_CLAUSE_DEVICE) && c->device == NULL
	  && gfc_match ("device ( %e )", &c->device) == MATCH_YES)
	continue;
      if ((mask & OMP_CLAUSE_THREAD_LIMIT) && c->thread_limit == NULL
	  && gfc_match ("thread_limit ( %e )", &c->thread_limit) == MATCH_YES)
	continue;
      if ((mask & OMP_CLAUSE_MAP)
	  && gfc_match ("map ( ") == MATCH_YES)
	{
	  gfc_omp_map_op map_op = OMP_MAP_TOFROM;
	  if (gfc_match ("alloc : ") == MATCH_YES)
	    map_op = OMP_MAP_ALLOC;
	  else if (gfc_match ("tofrom : ") == MATCH_YES)
	    map_op = OMP_MAP_TOFROM;
	  else if (gfc_match ("to : ") == MATCH_YES)
	    map_op = OMP_MAP_TO;
	  else if (gfc_match ("from : ") == MATCH_YES)
	    map_op = OMP_MAP_FROM;
	  head = NULL;
	  if (gfc_match_omp_variable_list ("", &c->lists[OMP_LIST_MAP],
					   false, NULL, &head, true)
	      == MATCH_YES)
	    {
	      gfc_omp_namelist *n;
	      for (n = *head; n; n = n->next)
		n->u.map_op = map_op;
	      continue;
	    }
	  else
	    gfc_current_locus = old_loc;
	}
      if ((mask & OMP_CLAUSE_TO)
	  && gfc_match_omp_variable_list ("to (",
					  &c->lists[OMP_LIST_TO], false,
					  NULL, &head, true)
	     == MATCH_YES)
	continue;
      if ((mask & OMP_CLAUSE_FROM)
	  && gfc_match_omp_variable_list ("from (",
					  &c->lists[OMP_LIST_FROM], false,
					  NULL, &head, true)
	     == MATCH_YES)
	continue;

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
   | OMP_CLAUSE_NUM_THREADS | OMP_CLAUSE_DEFAULT | OMP_CLAUSE_PROC_BIND)
#define OMP_DECLARE_SIMD_CLAUSES \
  (OMP_CLAUSE_SIMDLEN | OMP_CLAUSE_LINEAR | OMP_CLAUSE_UNIFORM		\
   | OMP_CLAUSE_ALIGNED | OMP_CLAUSE_INBRANCH | OMP_CLAUSE_NOTINBRANCH)
#define OMP_DO_CLAUSES \
  (OMP_CLAUSE_PRIVATE | OMP_CLAUSE_FIRSTPRIVATE				\
   | OMP_CLAUSE_LASTPRIVATE | OMP_CLAUSE_REDUCTION			\
   | OMP_CLAUSE_SCHEDULE | OMP_CLAUSE_ORDERED | OMP_CLAUSE_COLLAPSE)
#define OMP_SECTIONS_CLAUSES \
  (OMP_CLAUSE_PRIVATE | OMP_CLAUSE_FIRSTPRIVATE				\
   | OMP_CLAUSE_LASTPRIVATE | OMP_CLAUSE_REDUCTION)
#define OMP_SIMD_CLAUSES \
  (OMP_CLAUSE_PRIVATE | OMP_CLAUSE_LASTPRIVATE | OMP_CLAUSE_REDUCTION	\
   | OMP_CLAUSE_COLLAPSE | OMP_CLAUSE_SAFELEN | OMP_CLAUSE_LINEAR	\
   | OMP_CLAUSE_ALIGNED)
#define OMP_TASK_CLAUSES \
  (OMP_CLAUSE_PRIVATE | OMP_CLAUSE_FIRSTPRIVATE | OMP_CLAUSE_SHARED	\
   | OMP_CLAUSE_IF | OMP_CLAUSE_DEFAULT | OMP_CLAUSE_UNTIED		\
   | OMP_CLAUSE_FINAL | OMP_CLAUSE_MERGEABLE | OMP_CLAUSE_DEPEND)
#define OMP_TARGET_CLAUSES \
  (OMP_CLAUSE_DEVICE | OMP_CLAUSE_MAP | OMP_CLAUSE_IF)
#define OMP_TARGET_DATA_CLAUSES \
  (OMP_CLAUSE_DEVICE | OMP_CLAUSE_MAP | OMP_CLAUSE_IF)
#define OMP_TARGET_UPDATE_CLAUSES \
  (OMP_CLAUSE_DEVICE | OMP_CLAUSE_IF | OMP_CLAUSE_TO | OMP_CLAUSE_FROM)
#define OMP_TEAMS_CLAUSES \
  (OMP_CLAUSE_NUM_TEAMS | OMP_CLAUSE_THREAD_LIMIT | OMP_CLAUSE_DEFAULT	\
   | OMP_CLAUSE_PRIVATE | OMP_CLAUSE_FIRSTPRIVATE | OMP_CLAUSE_SHARED	\
   | OMP_CLAUSE_REDUCTION)
#define OMP_DISTRIBUTE_CLAUSES \
  (OMP_CLAUSE_PRIVATE | OMP_CLAUSE_FIRSTPRIVATE | OMP_CLAUSE_COLLAPSE	\
   | OMP_CLAUSE_DIST_SCHEDULE)


static match
match_omp (gfc_exec_op op, unsigned int mask)
{
  gfc_omp_clauses *c;
  if (gfc_match_omp_clauses (&c, mask) != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = op;
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
    {
      gfc_error ("Unexpected junk after $OMP CRITICAL statement at %C");
      return MATCH_ERROR;
    }
  new_st.op = EXEC_OMP_CRITICAL;
  new_st.ext.omp_name = n[0] ? xstrdup (n) : NULL;
  return MATCH_YES;
}


match
gfc_match_omp_distribute (void)
{
  return match_omp (EXEC_OMP_DISTRIBUTE, OMP_DISTRIBUTE_CLAUSES);
}


match
gfc_match_omp_distribute_parallel_do (void)
{
  return match_omp (EXEC_OMP_DISTRIBUTE_PARALLEL_DO,
		    OMP_DISTRIBUTE_CLAUSES | OMP_PARALLEL_CLAUSES
		    | OMP_DO_CLAUSES);
}


match
gfc_match_omp_distribute_parallel_do_simd (void)
{
  return match_omp (EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD,
		    (OMP_DISTRIBUTE_CLAUSES | OMP_PARALLEL_CLAUSES
		     | OMP_DO_CLAUSES | OMP_SIMD_CLAUSES)
		    & ~OMP_CLAUSE_ORDERED);
}


match
gfc_match_omp_distribute_simd (void)
{
  return match_omp (EXEC_OMP_DISTRIBUTE_SIMD,
		    OMP_DISTRIBUTE_CLAUSES | OMP_SIMD_CLAUSES);
}


match
gfc_match_omp_do (void)
{
  return match_omp (EXEC_OMP_DO, OMP_DO_CLAUSES);
}


match
gfc_match_omp_do_simd (void)
{
  return match_omp (EXEC_OMP_DO_SIMD, ((OMP_DO_CLAUSES | OMP_SIMD_CLAUSES)
				       & ~OMP_CLAUSE_ORDERED));
}


match
gfc_match_omp_flush (void)
{
  gfc_omp_namelist *list = NULL;
  gfc_match_omp_variable_list (" (", &list, true);
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after $OMP FLUSH statement at %C");
      gfc_free_omp_namelist (list);
      return MATCH_ERROR;
    }
  new_st.op = EXEC_OMP_FLUSH;
  new_st.ext.omp_namelist = list;
  return MATCH_YES;
}


match
gfc_match_omp_declare_simd (void)
{
  locus where = gfc_current_locus;
  gfc_symbol *proc_name;
  gfc_omp_clauses *c;
  gfc_omp_declare_simd *ods;

  if (gfc_match (" ( %s ) ", &proc_name) != MATCH_YES)
    return MATCH_ERROR;

  if (gfc_match_omp_clauses (&c, OMP_DECLARE_SIMD_CLAUSES, true,
			     false) != MATCH_YES)
    return MATCH_ERROR;

  ods = gfc_get_omp_declare_simd ();
  ods->where = where;
  ods->proc_name = proc_name;
  ods->clauses = c;
  ods->next = gfc_current_ns->omp_declare_simd;
  gfc_current_ns->omp_declare_simd = ods;
  return MATCH_YES;
}


static bool
match_udr_expr (gfc_symtree *omp_sym1, gfc_symtree *omp_sym2)
{
  match m;
  locus old_loc = gfc_current_locus;
  char sname[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  gfc_namespace *ns = gfc_current_ns;
  gfc_expr *lvalue = NULL, *rvalue = NULL;
  gfc_symtree *st;
  gfc_actual_arglist *arglist;

  m = gfc_match (" %v =", &lvalue);
  if (m != MATCH_YES)
    gfc_current_locus = old_loc;
  else
    {
      m = gfc_match (" %e )", &rvalue);
      if (m == MATCH_YES)
	{
	  ns->code = gfc_get_code (EXEC_ASSIGN);
	  ns->code->expr1 = lvalue;
	  ns->code->expr2 = rvalue;
	  ns->code->loc = old_loc;
	  return true;
	}

      gfc_current_locus = old_loc;
      gfc_free_expr (lvalue);
    }

  m = gfc_match (" %n", sname);
  if (m != MATCH_YES)
    return false;

  if (strcmp (sname, omp_sym1->name) == 0
      || strcmp (sname, omp_sym2->name) == 0)
    return false;

  gfc_current_ns = ns->parent;
  if (gfc_get_ha_sym_tree (sname, &st))
    return false;

  sym = st->n.sym;
  if (sym->attr.flavor != FL_PROCEDURE
      && sym->attr.flavor != FL_UNKNOWN)
    return false;

  if (!sym->attr.generic
      && !sym->attr.subroutine
      && !sym->attr.function)
    {
      if (!(sym->attr.external && !sym->attr.referenced))
	{
	  /* ...create a symbol in this scope...  */
	  if (sym->ns != gfc_current_ns
	      && gfc_get_sym_tree (sname, NULL, &st, false) == 1)
	    return false;

	  if (sym != st->n.sym)
	    sym = st->n.sym;
	}

      /* ...and then to try to make the symbol into a subroutine.  */
      if (!gfc_add_subroutine (&sym->attr, sym->name, NULL))
	return false;
    }

  gfc_set_sym_referenced (sym);
  gfc_gobble_whitespace ();
  if (gfc_peek_ascii_char () != '(')
    return false;

  gfc_current_ns = ns;
  m = gfc_match_actual_arglist (1, &arglist);
  if (m != MATCH_YES)
    return false;

  if (gfc_match_char (')') != MATCH_YES)
    return false;

  ns->code = gfc_get_code (EXEC_CALL);
  ns->code->symtree = st;
  ns->code->ext.actual = arglist;
  ns->code->loc = old_loc;
  return true;
}

static bool
gfc_omp_udr_predef (gfc_omp_reduction_op rop, const char *name,
		    gfc_typespec *ts, const char **n)
{
  if (!gfc_numeric_ts (ts) && ts->type != BT_LOGICAL)
    return false;

  switch (rop)
    {
    case OMP_REDUCTION_PLUS:
    case OMP_REDUCTION_MINUS:
    case OMP_REDUCTION_TIMES:
      return ts->type != BT_LOGICAL;
    case OMP_REDUCTION_AND:
    case OMP_REDUCTION_OR:
    case OMP_REDUCTION_EQV:
    case OMP_REDUCTION_NEQV:
      return ts->type == BT_LOGICAL;
    case OMP_REDUCTION_USER:
      if (name[0] != '.' && (ts->type == BT_INTEGER || ts->type == BT_REAL))
	{
	  gfc_symbol *sym;

	  gfc_find_symbol (name, NULL, 1, &sym);
	  if (sym != NULL)
	    {
	      if (sym->attr.intrinsic)
		*n = sym->name;
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
		*n = NULL;
	      else
		*n = sym->name;
	    }
	  else
	    *n = name;
	  if (*n
	      && (strcmp (*n, "max") == 0 || strcmp (*n, "min") == 0))
	    return true;
	  else if (*n
		   && ts->type == BT_INTEGER
		   && (strcmp (*n, "iand") == 0
		       || strcmp (*n, "ior") == 0
		       || strcmp (*n, "ieor") == 0))
	    return true;
	}
      break;
    default:
      break;
    }
  return false;
}

gfc_omp_udr *
gfc_omp_udr_find (gfc_symtree *st, gfc_typespec *ts)
{
  gfc_omp_udr *omp_udr;

  if (st == NULL)
    return NULL;

  for (omp_udr = st->n.omp_udr; omp_udr; omp_udr = omp_udr->next)
    if (omp_udr->ts.type == ts->type
	|| ((omp_udr->ts.type == BT_DERIVED || omp_udr->ts.type == BT_CLASS)
	    && (ts->type == BT_DERIVED && ts->type == BT_CLASS)))
      {
	if (omp_udr->ts.type == BT_DERIVED || omp_udr->ts.type == BT_CLASS)
	  {
	    if (strcmp (omp_udr->ts.u.derived->name, ts->u.derived->name) == 0)
	      return omp_udr;
	  }
	else if (omp_udr->ts.kind == ts->kind)
	  {
	    if (omp_udr->ts.type == BT_CHARACTER)
	      {
		if (omp_udr->ts.u.cl->length == NULL
		    || ts->u.cl->length == NULL)
		  return omp_udr;
		if (omp_udr->ts.u.cl->length->expr_type != EXPR_CONSTANT)
		  return omp_udr;
		if (ts->u.cl->length->expr_type != EXPR_CONSTANT)
		  return omp_udr;
		if (omp_udr->ts.u.cl->length->ts.type != BT_INTEGER)
		  return omp_udr;
		if (ts->u.cl->length->ts.type != BT_INTEGER)
		  return omp_udr;
		if (gfc_compare_expr (omp_udr->ts.u.cl->length,
				      ts->u.cl->length, INTRINSIC_EQ) != 0)
		  continue;
	      }
	    return omp_udr;
	  }
      }
  return NULL;
}

match
gfc_match_omp_declare_reduction (void)
{
  match m;
  gfc_intrinsic_op op;
  char name[GFC_MAX_SYMBOL_LEN + 3];
  auto_vec<gfc_typespec, 5> tss;
  gfc_typespec ts;
  unsigned int i;
  gfc_symtree *st;
  locus where = gfc_current_locus;
  locus end_loc = gfc_current_locus;
  bool end_loc_set = false;
  gfc_omp_reduction_op rop = OMP_REDUCTION_NONE;

  if (gfc_match_char ('(') != MATCH_YES)
    return MATCH_ERROR;

  m = gfc_match (" %o : ", &op);
  if (m == MATCH_ERROR)
    return MATCH_ERROR;
  if (m == MATCH_YES)
    {
      snprintf (name, sizeof name, "operator %s", gfc_op2string (op));
      rop = (gfc_omp_reduction_op) op;
    }
  else
    {
      m = gfc_match_defined_op_name (name + 1, 1);
      if (m == MATCH_ERROR)
	return MATCH_ERROR;
      if (m == MATCH_YES)
	{
	  name[0] = '.';
	  strcat (name, ".");
	  if (gfc_match (" : ") != MATCH_YES)
	    return MATCH_ERROR;
	}
      else
	{
	  if (gfc_match (" %n : ", name) != MATCH_YES)
	    return MATCH_ERROR;
	}
      rop = OMP_REDUCTION_USER;
    }

  m = gfc_match_type_spec (&ts);
  if (m != MATCH_YES)
    return MATCH_ERROR;
  /* Treat len=: the same as len=*.  */
  if (ts.type == BT_CHARACTER)
    ts.deferred = false;
  tss.safe_push (ts);

  while (gfc_match_char (',') == MATCH_YES)
    {
      m = gfc_match_type_spec (&ts);
      if (m != MATCH_YES)
	return MATCH_ERROR;
      tss.safe_push (ts);
    }
  if (gfc_match_char (':') != MATCH_YES)
    return MATCH_ERROR;

  st = gfc_find_symtree (gfc_current_ns->omp_udr_root, name);
  for (i = 0; i < tss.length (); i++)
    {
      gfc_symtree *omp_out, *omp_in;
      gfc_symtree *omp_priv = NULL, *omp_orig = NULL;
      gfc_namespace *combiner_ns, *initializer_ns = NULL;
      gfc_omp_udr *prev_udr, *omp_udr;
      const char *predef_name = NULL;

      omp_udr = gfc_get_omp_udr ();
      omp_udr->name = gfc_get_string (name);
      omp_udr->rop = rop;
      omp_udr->ts = tss[i];
      omp_udr->where = where;

      gfc_current_ns = combiner_ns = gfc_get_namespace (gfc_current_ns, 1);
      combiner_ns->proc_name = combiner_ns->parent->proc_name;

      gfc_get_sym_tree ("omp_out", combiner_ns, &omp_out, false);
      gfc_get_sym_tree ("omp_in", combiner_ns, &omp_in, false);
      combiner_ns->omp_udr_ns = 1;
      omp_out->n.sym->ts = tss[i];
      omp_in->n.sym->ts = tss[i];
      omp_out->n.sym->attr.omp_udr_artificial_var = 1;
      omp_in->n.sym->attr.omp_udr_artificial_var = 1;
      omp_out->n.sym->attr.flavor = FL_VARIABLE;
      omp_in->n.sym->attr.flavor = FL_VARIABLE;
      gfc_commit_symbols ();
      omp_udr->combiner_ns = combiner_ns;
      omp_udr->omp_out = omp_out->n.sym;
      omp_udr->omp_in = omp_in->n.sym;

      locus old_loc = gfc_current_locus;

      if (!match_udr_expr (omp_out, omp_in))
	{
	 syntax:
	  gfc_current_locus = old_loc;
	  gfc_current_ns = combiner_ns->parent;
	  gfc_undo_symbols ();
	  gfc_free_omp_udr (omp_udr);
	  return MATCH_ERROR;
	}

      if (gfc_match (" initializer ( ") == MATCH_YES)
	{
	  gfc_current_ns = combiner_ns->parent;
	  initializer_ns = gfc_get_namespace (gfc_current_ns, 1);
	  gfc_current_ns = initializer_ns;
	  initializer_ns->proc_name = initializer_ns->parent->proc_name;

	  gfc_get_sym_tree ("omp_priv", initializer_ns, &omp_priv, false);
	  gfc_get_sym_tree ("omp_orig", initializer_ns, &omp_orig, false);
	  initializer_ns->omp_udr_ns = 1;
	  omp_priv->n.sym->ts = tss[i];
	  omp_orig->n.sym->ts = tss[i];
	  omp_priv->n.sym->attr.omp_udr_artificial_var = 1;
	  omp_orig->n.sym->attr.omp_udr_artificial_var = 1;
	  omp_priv->n.sym->attr.flavor = FL_VARIABLE;
	  omp_orig->n.sym->attr.flavor = FL_VARIABLE;
	  gfc_commit_symbols ();
	  omp_udr->initializer_ns = initializer_ns;
	  omp_udr->omp_priv = omp_priv->n.sym;
	  omp_udr->omp_orig = omp_orig->n.sym;

	  if (!match_udr_expr (omp_priv, omp_orig))
	    goto syntax;
	}

      gfc_current_ns = combiner_ns->parent;
      if (!end_loc_set)
	{
	  end_loc_set = true;
	  end_loc = gfc_current_locus;
	}
      gfc_current_locus = old_loc;

      prev_udr = gfc_omp_udr_find (st, &tss[i]);
      if (gfc_omp_udr_predef (rop, name, &tss[i], &predef_name)
	  /* Don't error on !$omp declare reduction (min : integer : ...)
	     just yet, there could be integer :: min afterwards,
	     making it valid.  When the UDR is resolved, we'll get
	     to it again.  */
	  && (rop != OMP_REDUCTION_USER || name[0] == '.'))
	{
	  if (predef_name)
	    gfc_error_now ("Redefinition of predefined %s "
			   "!$OMP DECLARE REDUCTION at %L",
			   predef_name, &where);
	  else
	    gfc_error_now ("Redefinition of predefined "
			   "!$OMP DECLARE REDUCTION at %L", &where);
	}
      else if (prev_udr)
	{
	  gfc_error_now ("Redefinition of !$OMP DECLARE REDUCTION at %L",
			 &where);
	  gfc_error_now ("Previous !$OMP DECLARE REDUCTION at %L",
			 &prev_udr->where);
	}
      else if (st)
	{
	  omp_udr->next = st->n.omp_udr;
	  st->n.omp_udr = omp_udr;
	}
      else
	{
	  st = gfc_new_symtree (&gfc_current_ns->omp_udr_root, name);
	  st->n.omp_udr = omp_udr;
	}
    }

  if (end_loc_set)
    {
      gfc_current_locus = end_loc;
      if (gfc_match_omp_eos () != MATCH_YES)
	{
	  gfc_error ("Unexpected junk after !$OMP DECLARE REDUCTION at %C");
	  gfc_current_locus = where;
	  return MATCH_ERROR;
	}

      return MATCH_YES;
    }
  gfc_clear_error ();
  return MATCH_ERROR;
}


match
gfc_match_omp_declare_target (void)
{
  locus old_loc;
  char n[GFC_MAX_SYMBOL_LEN+1];
  gfc_symbol *sym;
  match m;
  gfc_symtree *st;

  old_loc = gfc_current_locus;

  m = gfc_match (" (");

  if (gfc_current_ns->proc_name
      && gfc_current_ns->proc_name->attr.if_source == IFSRC_IFBODY
      && m == MATCH_YES)
    {
      gfc_error ("Only the !$OMP DECLARE TARGET form without "
		 "list is allowed in interface block at %C");
      goto cleanup;
    }

  if (m == MATCH_NO
      && gfc_current_ns->proc_name
      && gfc_match_omp_eos () == MATCH_YES)
    {
      if (!gfc_add_omp_declare_target (&gfc_current_ns->proc_name->attr,
				       gfc_current_ns->proc_name->name,
				       &old_loc))
	goto cleanup;
      return MATCH_YES;
    }

  if (m != MATCH_YES)
    return m;

  for (;;)
    {
      m = gfc_match_symbol (&sym, 0);
      switch (m)
	{
	case MATCH_YES:
	  if (sym->attr.in_common)
	    gfc_error_now ("OMP DECLARE TARGET on a variable at %C is an "
			   "element of a COMMON block");
	  else if (!gfc_add_omp_declare_target (&sym->attr, sym->name,
						&sym->declared_at))
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
      st->n.common->omp_declare_target = 1;
      for (sym = st->n.common->head; sym; sym = sym->common_next)
	if (!gfc_add_omp_declare_target (&sym->attr, sym->name,
					 &sym->declared_at))
	  goto cleanup;

    next_item:
      if (gfc_match_char (')') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after !$OMP DECLARE TARGET at %C");
      goto cleanup;
    }
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in !$OMP DECLARE TARGET list at %C");

cleanup:
  gfc_current_locus = old_loc;
  return MATCH_ERROR;
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
	  else if (!gfc_add_threadprivate (&sym->attr, sym->name, &sym->declared_at))
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
	if (!gfc_add_threadprivate (&sym->attr, sym->name, &sym->declared_at))
	  goto cleanup;

    next_item:
      if (gfc_match_char (')') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after OMP THREADPRIVATE at %C");
      goto cleanup;
    }

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in !$OMP THREADPRIVATE list at %C");

cleanup:
  gfc_current_locus = old_loc;
  return MATCH_ERROR;
}


match
gfc_match_omp_parallel (void)
{
  return match_omp (EXEC_OMP_PARALLEL, OMP_PARALLEL_CLAUSES);
}


match
gfc_match_omp_parallel_do (void)
{
  return match_omp (EXEC_OMP_PARALLEL_DO,
		    OMP_PARALLEL_CLAUSES | OMP_DO_CLAUSES);
}


match
gfc_match_omp_parallel_do_simd (void)
{
  return match_omp (EXEC_OMP_PARALLEL_DO_SIMD,
		    (OMP_PARALLEL_CLAUSES | OMP_DO_CLAUSES | OMP_SIMD_CLAUSES)
		    & ~OMP_CLAUSE_ORDERED);
}


match
gfc_match_omp_parallel_sections (void)
{
  return match_omp (EXEC_OMP_PARALLEL_SECTIONS,
		    OMP_PARALLEL_CLAUSES | OMP_SECTIONS_CLAUSES);
}


match
gfc_match_omp_parallel_workshare (void)
{
  return match_omp (EXEC_OMP_PARALLEL_WORKSHARE, OMP_PARALLEL_CLAUSES);
}


match
gfc_match_omp_sections (void)
{
  return match_omp (EXEC_OMP_SECTIONS, OMP_SECTIONS_CLAUSES);
}


match
gfc_match_omp_simd (void)
{
  return match_omp (EXEC_OMP_SIMD, OMP_SIMD_CLAUSES);
}


match
gfc_match_omp_single (void)
{
  return match_omp (EXEC_OMP_SINGLE,
		    OMP_CLAUSE_PRIVATE | OMP_CLAUSE_FIRSTPRIVATE);
}


match
gfc_match_omp_task (void)
{
  return match_omp (EXEC_OMP_TASK, OMP_TASK_CLAUSES);
}


match
gfc_match_omp_taskwait (void)
{
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after TASKWAIT clause at %C");
      return MATCH_ERROR;
    }
  new_st.op = EXEC_OMP_TASKWAIT;
  new_st.ext.omp_clauses = NULL;
  return MATCH_YES;
}


match
gfc_match_omp_taskyield (void)
{
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after TASKYIELD clause at %C");
      return MATCH_ERROR;
    }
  new_st.op = EXEC_OMP_TASKYIELD;
  new_st.ext.omp_clauses = NULL;
  return MATCH_YES;
}


match
gfc_match_omp_target (void)
{
  return match_omp (EXEC_OMP_TARGET, OMP_TARGET_CLAUSES);
}


match
gfc_match_omp_target_data (void)
{
  return match_omp (EXEC_OMP_TARGET_DATA, OMP_TARGET_DATA_CLAUSES);
}


match
gfc_match_omp_target_teams (void)
{
  return match_omp (EXEC_OMP_TARGET_TEAMS,
		    OMP_TARGET_CLAUSES | OMP_TEAMS_CLAUSES);
}


match
gfc_match_omp_target_teams_distribute (void)
{
  return match_omp (EXEC_OMP_TARGET_TEAMS_DISTRIBUTE,
		    OMP_TARGET_CLAUSES | OMP_TEAMS_CLAUSES
		    | OMP_DISTRIBUTE_CLAUSES);
}


match
gfc_match_omp_target_teams_distribute_parallel_do (void)
{
  return match_omp (EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO,
		    OMP_TARGET_CLAUSES | OMP_TEAMS_CLAUSES
		    | OMP_DISTRIBUTE_CLAUSES | OMP_PARALLEL_CLAUSES
		    | OMP_DO_CLAUSES);
}


match
gfc_match_omp_target_teams_distribute_parallel_do_simd (void)
{
  return match_omp (EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD,
		    (OMP_TARGET_CLAUSES | OMP_TEAMS_CLAUSES
		     | OMP_DISTRIBUTE_CLAUSES | OMP_PARALLEL_CLAUSES
		     | OMP_DO_CLAUSES | OMP_SIMD_CLAUSES)
		    & ~OMP_CLAUSE_ORDERED);
}


match
gfc_match_omp_target_teams_distribute_simd (void)
{
  return match_omp (EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD,
		    OMP_TARGET_CLAUSES | OMP_TEAMS_CLAUSES
		    | OMP_DISTRIBUTE_CLAUSES | OMP_SIMD_CLAUSES);
}


match
gfc_match_omp_target_update (void)
{
  return match_omp (EXEC_OMP_TARGET_UPDATE, OMP_TARGET_UPDATE_CLAUSES);
}


match
gfc_match_omp_teams (void)
{
  return match_omp (EXEC_OMP_TEAMS, OMP_TEAMS_CLAUSES);
}


match
gfc_match_omp_teams_distribute (void)
{
  return match_omp (EXEC_OMP_TEAMS_DISTRIBUTE,
		    OMP_TEAMS_CLAUSES | OMP_DISTRIBUTE_CLAUSES);
}


match
gfc_match_omp_teams_distribute_parallel_do (void)
{
  return match_omp (EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO,
		    OMP_TEAMS_CLAUSES | OMP_DISTRIBUTE_CLAUSES
		    | OMP_PARALLEL_CLAUSES | OMP_DO_CLAUSES);
}


match
gfc_match_omp_teams_distribute_parallel_do_simd (void)
{
  return match_omp (EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD,
		    (OMP_TEAMS_CLAUSES | OMP_DISTRIBUTE_CLAUSES
		     | OMP_PARALLEL_CLAUSES | OMP_DO_CLAUSES
		     | OMP_SIMD_CLAUSES) & ~OMP_CLAUSE_ORDERED);
}


match
gfc_match_omp_teams_distribute_simd (void)
{
  return match_omp (EXEC_OMP_TEAMS_DISTRIBUTE_SIMD,
		    OMP_TEAMS_CLAUSES | OMP_DISTRIBUTE_CLAUSES
		    | OMP_SIMD_CLAUSES);
}


match
gfc_match_omp_workshare (void)
{
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after $OMP WORKSHARE statement at %C");
      return MATCH_ERROR;
    }
  new_st.op = EXEC_OMP_WORKSHARE;
  new_st.ext.omp_clauses = gfc_get_omp_clauses ();
  return MATCH_YES;
}


match
gfc_match_omp_master (void)
{
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after $OMP MASTER statement at %C");
      return MATCH_ERROR;
    }
  new_st.op = EXEC_OMP_MASTER;
  new_st.ext.omp_clauses = NULL;
  return MATCH_YES;
}


match
gfc_match_omp_ordered (void)
{
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after $OMP ORDERED statement at %C");
      return MATCH_ERROR;
    }
  new_st.op = EXEC_OMP_ORDERED;
  new_st.ext.omp_clauses = NULL;
  return MATCH_YES;
}


match
gfc_match_omp_atomic (void)
{
  gfc_omp_atomic_op op = GFC_OMP_ATOMIC_UPDATE;
  int seq_cst = 0;
  if (gfc_match ("% seq_cst") == MATCH_YES)
    seq_cst = 1;
  locus old_loc = gfc_current_locus;
  if (seq_cst && gfc_match_char (',') == MATCH_YES)
    seq_cst = 2;
  if (seq_cst == 2
      || gfc_match_space () == MATCH_YES)
    {
      gfc_gobble_whitespace ();
      if (gfc_match ("update") == MATCH_YES)
	op = GFC_OMP_ATOMIC_UPDATE;
      else if (gfc_match ("read") == MATCH_YES)
	op = GFC_OMP_ATOMIC_READ;
      else if (gfc_match ("write") == MATCH_YES)
	op = GFC_OMP_ATOMIC_WRITE;
      else if (gfc_match ("capture") == MATCH_YES)
	op = GFC_OMP_ATOMIC_CAPTURE;
      else
	{
	  if (seq_cst == 2)
	    gfc_current_locus = old_loc;
	  goto finish;
	}
      if (!seq_cst
	  && (gfc_match (", seq_cst") == MATCH_YES
	      || gfc_match ("% seq_cst") == MATCH_YES))
	seq_cst = 1;
    }
 finish:
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after $OMP ATOMIC statement at %C");
      return MATCH_ERROR;
    }
  new_st.op = EXEC_OMP_ATOMIC;
  if (seq_cst)
    op = (gfc_omp_atomic_op) (op | GFC_OMP_ATOMIC_SEQ_CST);
  new_st.ext.omp_atomic = op;
  return MATCH_YES;
}


match
gfc_match_omp_barrier (void)
{
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after $OMP BARRIER statement at %C");
      return MATCH_ERROR;
    }
  new_st.op = EXEC_OMP_BARRIER;
  new_st.ext.omp_clauses = NULL;
  return MATCH_YES;
}


match
gfc_match_omp_taskgroup (void)
{
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after $OMP TASKGROUP statement at %C");
      return MATCH_ERROR;
    }
  new_st.op = EXEC_OMP_TASKGROUP;
  return MATCH_YES;
}


static enum gfc_omp_cancel_kind
gfc_match_omp_cancel_kind (void)
{
  if (gfc_match_space () != MATCH_YES)
    return OMP_CANCEL_UNKNOWN;
  if (gfc_match ("parallel") == MATCH_YES)
    return OMP_CANCEL_PARALLEL;
  if (gfc_match ("sections") == MATCH_YES)
    return OMP_CANCEL_SECTIONS;
  if (gfc_match ("do") == MATCH_YES)
    return OMP_CANCEL_DO;
  if (gfc_match ("taskgroup") == MATCH_YES)
    return OMP_CANCEL_TASKGROUP;
  return OMP_CANCEL_UNKNOWN;
}


match
gfc_match_omp_cancel (void)
{
  gfc_omp_clauses *c;
  enum gfc_omp_cancel_kind kind = gfc_match_omp_cancel_kind ();
  if (kind == OMP_CANCEL_UNKNOWN)
    return MATCH_ERROR;
  if (gfc_match_omp_clauses (&c, OMP_CLAUSE_IF, false) != MATCH_YES)
    return MATCH_ERROR;
  c->cancel = kind;
  new_st.op = EXEC_OMP_CANCEL;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


match
gfc_match_omp_cancellation_point (void)
{
  gfc_omp_clauses *c;
  enum gfc_omp_cancel_kind kind = gfc_match_omp_cancel_kind ();
  if (kind == OMP_CANCEL_UNKNOWN)
    return MATCH_ERROR;
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after $OMP CANCELLATION POINT statement "
		 "at %C");
      return MATCH_ERROR;
    }
  c = gfc_get_omp_clauses ();
  c->cancel = kind;
  new_st.op = EXEC_OMP_CANCELLATION_POINT;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


match
gfc_match_omp_end_nowait (void)
{
  bool nowait = false;
  if (gfc_match ("% nowait") == MATCH_YES)
    nowait = true;
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after NOWAIT clause at %C");
      return MATCH_ERROR;
    }
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


struct resolve_omp_udr_callback_data
{
  gfc_symbol *sym1, *sym2;
};


static int
resolve_omp_udr_callback (gfc_expr **e, int *, void *data)
{
  struct resolve_omp_udr_callback_data *rcd
    = (struct resolve_omp_udr_callback_data *) data;
  if ((*e)->expr_type == EXPR_VARIABLE
      && ((*e)->symtree->n.sym == rcd->sym1
	  || (*e)->symtree->n.sym == rcd->sym2))
    {
      gfc_ref *ref = gfc_get_ref ();
      ref->type = REF_ARRAY;
      ref->u.ar.where = (*e)->where;
      ref->u.ar.as = (*e)->symtree->n.sym->as;
      ref->u.ar.type = AR_FULL;
      ref->u.ar.dimen = 0;
      ref->next = (*e)->ref;
      (*e)->ref = ref;
    }
  return 0;
}


static int
resolve_omp_udr_callback2 (gfc_expr **e, int *, void *)
{
  if ((*e)->expr_type == EXPR_FUNCTION
      && (*e)->value.function.isym == NULL)
    {
      gfc_symbol *sym = (*e)->symtree->n.sym;
      if (!sym->attr.intrinsic
	  && sym->attr.if_source == IFSRC_UNKNOWN)
	gfc_error ("Implicitly declared function %s used in "
		   "!$OMP DECLARE REDUCTION at %L ", sym->name, &(*e)->where);
    }
  return 0;
}


static gfc_code *
resolve_omp_udr_clause (gfc_omp_namelist *n, gfc_namespace *ns,
			gfc_symbol *sym1, gfc_symbol *sym2)
{
  gfc_code *copy;
  gfc_symbol sym1_copy, sym2_copy;

  if (ns->code->op == EXEC_ASSIGN)
    {
      copy = gfc_get_code (EXEC_ASSIGN);
      copy->expr1 = gfc_copy_expr (ns->code->expr1);
      copy->expr2 = gfc_copy_expr (ns->code->expr2);
    }
  else
    {
      copy = gfc_get_code (EXEC_CALL);
      copy->symtree = ns->code->symtree;
      copy->ext.actual = gfc_copy_actual_arglist (ns->code->ext.actual);
    }
  copy->loc = ns->code->loc;
  sym1_copy = *sym1;
  sym2_copy = *sym2;
  *sym1 = *n->sym;
  *sym2 = *n->sym;
  sym1->name = sym1_copy.name;
  sym2->name = sym2_copy.name;
  ns->proc_name = ns->parent->proc_name;
  if (n->sym->attr.dimension)
    {
      struct resolve_omp_udr_callback_data rcd;
      rcd.sym1 = sym1;
      rcd.sym2 = sym2;
      gfc_code_walker (&copy, gfc_dummy_code_callback,
		       resolve_omp_udr_callback, &rcd);
    }
  gfc_resolve_code (copy, gfc_current_ns);
  if (copy->op == EXEC_CALL && copy->resolved_isym == NULL)
    {
      gfc_symbol *sym = copy->resolved_sym;
      if (sym
	  && !sym->attr.intrinsic
	  && sym->attr.if_source == IFSRC_UNKNOWN)
	gfc_error ("Implicitly declared subroutine %s used in "
		   "!$OMP DECLARE REDUCTION at %L ", sym->name,
		   &copy->loc);
    }
  gfc_code_walker (&copy, gfc_dummy_code_callback,
		   resolve_omp_udr_callback2, NULL);
  *sym1 = sym1_copy;
  *sym2 = sym2_copy;
  return copy;
}


/* OpenMP directive resolving routines.  */

static void
resolve_omp_clauses (gfc_code *code, locus *where,
		     gfc_omp_clauses *omp_clauses, gfc_namespace *ns)
{
  gfc_omp_namelist *n;
  int list;
  static const char *clause_names[]
    = { "PRIVATE", "FIRSTPRIVATE", "LASTPRIVATE", "COPYPRIVATE", "SHARED",
	"COPYIN", "UNIFORM", "ALIGNED", "LINEAR", "DEPEND", "MAP",
	"TO", "FROM", "REDUCTION" };

  if (omp_clauses == NULL)
    return;

  if (omp_clauses->if_expr)
    {
      gfc_expr *expr = omp_clauses->if_expr;
      if (!gfc_resolve_expr (expr)
	  || expr->ts.type != BT_LOGICAL || expr->rank != 0)
	gfc_error ("IF clause at %L requires a scalar LOGICAL expression",
		   &expr->where);
    }
  if (omp_clauses->final_expr)
    {
      gfc_expr *expr = omp_clauses->final_expr;
      if (!gfc_resolve_expr (expr)
	  || expr->ts.type != BT_LOGICAL || expr->rank != 0)
	gfc_error ("FINAL clause at %L requires a scalar LOGICAL expression",
		   &expr->where);
    }
  if (omp_clauses->num_threads)
    {
      gfc_expr *expr = omp_clauses->num_threads;
      if (!gfc_resolve_expr (expr)
	  || expr->ts.type != BT_INTEGER || expr->rank != 0)
	gfc_error ("NUM_THREADS clause at %L requires a scalar "
		   "INTEGER expression", &expr->where);
    }
  if (omp_clauses->chunk_size)
    {
      gfc_expr *expr = omp_clauses->chunk_size;
      if (!gfc_resolve_expr (expr)
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
	if (n->sym->attr.flavor == FL_VARIABLE
	    || n->sym->attr.proc_pointer
	    || (!code && (!n->sym->attr.dummy || n->sym->ns != ns)))
	  {
	    if (!code && (!n->sym->attr.dummy || n->sym->ns != ns))
	      gfc_error ("Variable '%s' is not a dummy argument at %L",
			 n->sym->name, where);
	    continue;
	  }
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
		   where);
      }

  for (list = 0; list < OMP_LIST_NUM; list++)
    if (list != OMP_LIST_FIRSTPRIVATE
	&& list != OMP_LIST_LASTPRIVATE
	&& list != OMP_LIST_ALIGNED
	&& list != OMP_LIST_DEPEND
	&& list != OMP_LIST_MAP
	&& list != OMP_LIST_FROM
	&& list != OMP_LIST_TO)
      for (n = omp_clauses->lists[list]; n; n = n->next)
	{
	  if (n->sym->mark)
	    gfc_error ("Symbol '%s' present on multiple clauses at %L",
		       n->sym->name, where);
	  else
	    n->sym->mark = 1;
	}

  gcc_assert (OMP_LIST_LASTPRIVATE == OMP_LIST_FIRSTPRIVATE + 1);
  for (list = OMP_LIST_FIRSTPRIVATE; list <= OMP_LIST_LASTPRIVATE; list++)
    for (n = omp_clauses->lists[list]; n; n = n->next)
      if (n->sym->mark)
	{
	  gfc_error ("Symbol '%s' present on multiple clauses at %L",
		     n->sym->name, where);
	  n->sym->mark = 0;
	}

  for (n = omp_clauses->lists[OMP_LIST_FIRSTPRIVATE]; n; n = n->next)
    {
      if (n->sym->mark)
	gfc_error ("Symbol '%s' present on multiple clauses at %L",
		   n->sym->name, where);
      else
	n->sym->mark = 1;
    }
  for (n = omp_clauses->lists[OMP_LIST_LASTPRIVATE]; n; n = n->next)
    n->sym->mark = 0;

  for (n = omp_clauses->lists[OMP_LIST_LASTPRIVATE]; n; n = n->next)
    {
      if (n->sym->mark)
	gfc_error ("Symbol '%s' present on multiple clauses at %L",
		   n->sym->name, where);
      else
	n->sym->mark = 1;
    }

  for (n = omp_clauses->lists[OMP_LIST_ALIGNED]; n; n = n->next)
    n->sym->mark = 0;

  for (n = omp_clauses->lists[OMP_LIST_ALIGNED]; n; n = n->next)
    {
      if (n->sym->mark)
	gfc_error ("Symbol '%s' present on multiple clauses at %L",
		   n->sym->name, where);
      else
	n->sym->mark = 1;
    }

  for (n = omp_clauses->lists[OMP_LIST_TO]; n; n = n->next)
    n->sym->mark = 0;
  for (n = omp_clauses->lists[OMP_LIST_FROM]; n; n = n->next)
    if (n->expr == NULL)
      n->sym->mark = 1;
  for (n = omp_clauses->lists[OMP_LIST_TO]; n; n = n->next)
    {
      if (n->expr == NULL && n->sym->mark)
	gfc_error ("Symbol '%s' present on both FROM and TO clauses at %L",
		   n->sym->name, where);
      else
	n->sym->mark = 1;
    }

  for (list = 0; list < OMP_LIST_NUM; list++)
    if ((n = omp_clauses->lists[list]) != NULL)
      {
	const char *name;

	if (list < OMP_LIST_NUM)
	  name = clause_names[list];
	else
	  gcc_unreachable ();

	switch (list)
	  {
	  case OMP_LIST_COPYIN:
	    for (; n != NULL; n = n->next)
	      {
		if (!n->sym->attr.threadprivate)
		  gfc_error ("Non-THREADPRIVATE object '%s' in COPYIN clause"
			     " at %L", n->sym->name, where);
	      }
	    break;
	  case OMP_LIST_COPYPRIVATE:
	    for (; n != NULL; n = n->next)
	      {
		if (n->sym->as && n->sym->as->type == AS_ASSUMED_SIZE)
		  gfc_error ("Assumed size array '%s' in COPYPRIVATE clause "
			     "at %L", n->sym->name, where);
		if (n->sym->attr.pointer && n->sym->attr.intent == INTENT_IN)
		  gfc_error ("INTENT(IN) POINTER '%s' in COPYPRIVATE clause "
			     "at %L", n->sym->name, where);
	      }
	    break;
	  case OMP_LIST_SHARED:
	    for (; n != NULL; n = n->next)
	      {
		if (n->sym->attr.threadprivate)
		  gfc_error ("THREADPRIVATE object '%s' in SHARED clause at "
			     "%L", n->sym->name, where);
		if (n->sym->attr.cray_pointee)
		  gfc_error ("Cray pointee '%s' in SHARED clause at %L",
			    n->sym->name, where);
		if (n->sym->attr.associate_var)
		  gfc_error ("ASSOCIATE name '%s' in SHARED clause at %L",
			     n->sym->name, where);
	      }
	    break;
	  case OMP_LIST_ALIGNED:
	    for (; n != NULL; n = n->next)
	      {
		if (!n->sym->attr.pointer
		    && !n->sym->attr.allocatable
		    && !n->sym->attr.cray_pointer
		    && (n->sym->ts.type != BT_DERIVED
			|| (n->sym->ts.u.derived->from_intmod
			    != INTMOD_ISO_C_BINDING)
			|| (n->sym->ts.u.derived->intmod_sym_id
			    != ISOCBINDING_PTR)))
		  gfc_error ("'%s' in ALIGNED clause must be POINTER, "
			     "ALLOCATABLE, Cray pointer or C_PTR at %L",
			     n->sym->name, where);
		else if (n->expr)
		  {
		    gfc_expr *expr = n->expr;
		    int alignment = 0;
		    if (!gfc_resolve_expr (expr)
			|| expr->ts.type != BT_INTEGER
			|| expr->rank != 0
			|| gfc_extract_int (expr, &alignment)
			|| alignment <= 0)
		      gfc_error ("'%s' in ALIGNED clause at %L requires a scalar "
				 "positive constant integer alignment "
				 "expression", n->sym->name, where);
		  }
	      }
	    break;
	  case OMP_LIST_DEPEND:
	  case OMP_LIST_MAP:
	  case OMP_LIST_TO:
	  case OMP_LIST_FROM:
	    for (; n != NULL; n = n->next)
	      if (n->expr)
		{
		  if (!gfc_resolve_expr (n->expr)
		      || n->expr->expr_type != EXPR_VARIABLE
		      || n->expr->ref == NULL
		      || n->expr->ref->next
		      || n->expr->ref->type != REF_ARRAY)
		    gfc_error ("'%s' in %s clause at %L is not a proper "
			       "array section", n->sym->name, name, where);
		  else if (n->expr->ref->u.ar.codimen)
		    gfc_error ("Coarrays not supported in %s clause at %L",
			       name, where);
		  else
		    {
		      int i;
		      gfc_array_ref *ar = &n->expr->ref->u.ar;
		      for (i = 0; i < ar->dimen; i++)
			if (ar->stride[i])
			  {
			    gfc_error ("Stride should not be specified for "
				       "array section in %s clause at %L",
				       name, where);
			    break;
			  }
			else if (ar->dimen_type[i] != DIMEN_ELEMENT
				 && ar->dimen_type[i] != DIMEN_RANGE)
			  {
			    gfc_error ("'%s' in %s clause at %L is not a "
				       "proper array section",
				       n->sym->name, name, where);
			    break;
			  }
			else if (list == OMP_LIST_DEPEND
				 && ar->start[i]
				 && ar->start[i]->expr_type == EXPR_CONSTANT
				 && ar->end[i]
				 && ar->end[i]->expr_type == EXPR_CONSTANT
				 && mpz_cmp (ar->start[i]->value.integer,
					     ar->end[i]->value.integer) > 0)
			  {
			    gfc_error ("'%s' in DEPEND clause at %L is a zero "
				       "size array section", n->sym->name,
				       where);
			    break;
			  }
		    }
		}
	    if (list != OMP_LIST_DEPEND)
	      for (n = omp_clauses->lists[list]; n != NULL; n = n->next)
		{
		  n->sym->attr.referenced = 1;
		  if (n->sym->attr.threadprivate)
		    gfc_error ("THREADPRIVATE object '%s' in %s clause at %L",
			       n->sym->name, name, where);
		  if (n->sym->attr.cray_pointee)
		    gfc_error ("Cray pointee '%s' in %s clause at %L",
			       n->sym->name, name, where);
		}
	    break;
	  default:
	    for (; n != NULL; n = n->next)
	      {
		bool bad = false;
		if (n->sym->attr.threadprivate)
		  gfc_error ("THREADPRIVATE object '%s' in %s clause at %L",
			     n->sym->name, name, where);
		if (n->sym->attr.cray_pointee)
		  gfc_error ("Cray pointee '%s' in %s clause at %L",
			    n->sym->name, name, where);
		if (n->sym->attr.associate_var)
		  gfc_error ("ASSOCIATE name '%s' in %s clause at %L",
			     n->sym->name, name, where);
		if (list != OMP_LIST_PRIVATE)
		  {
		    if (n->sym->attr.proc_pointer && list == OMP_LIST_REDUCTION)
		      gfc_error ("Procedure pointer '%s' in %s clause at %L",
				 n->sym->name, name, where);
		    if (n->sym->attr.pointer && list == OMP_LIST_REDUCTION)
		      gfc_error ("POINTER object '%s' in %s clause at %L",
				 n->sym->name, name, where);
		    if (n->sym->attr.cray_pointer && list == OMP_LIST_REDUCTION)
		      gfc_error ("Cray pointer '%s' in %s clause at %L",
				 n->sym->name, name, where);
		  }
		if (n->sym->as && n->sym->as->type == AS_ASSUMED_SIZE)
		  gfc_error ("Assumed size array '%s' in %s clause at %L",
			     n->sym->name, name, where);
		if (n->sym->attr.in_namelist && list != OMP_LIST_REDUCTION)
		  gfc_error ("Variable '%s' in %s clause is used in "
			     "NAMELIST statement at %L",
			     n->sym->name, name, where);
		if (n->sym->attr.pointer && n->sym->attr.intent == INTENT_IN)
		  switch (list)
		    {
		    case OMP_LIST_PRIVATE:
		    case OMP_LIST_LASTPRIVATE:
		    case OMP_LIST_LINEAR:
		    /* case OMP_LIST_REDUCTION: */
		      gfc_error ("INTENT(IN) POINTER '%s' in %s clause at %L",
				 n->sym->name, name, where);
		      break;
		    default:
		      break;
		    }
		switch (list)
		  {
		  case OMP_LIST_REDUCTION:
		    switch (n->u.reduction_op)
		      {
		      case OMP_REDUCTION_PLUS:
		      case OMP_REDUCTION_TIMES:
		      case OMP_REDUCTION_MINUS:
			if (!gfc_numeric_ts (&n->sym->ts))
			  bad = true;
			break;
		      case OMP_REDUCTION_AND:
		      case OMP_REDUCTION_OR:
		      case OMP_REDUCTION_EQV:
		      case OMP_REDUCTION_NEQV:
			if (n->sym->ts.type != BT_LOGICAL)
			  bad = true;
			break;
		      case OMP_REDUCTION_MAX:
		      case OMP_REDUCTION_MIN:
			if (n->sym->ts.type != BT_INTEGER
			    && n->sym->ts.type != BT_REAL)
			  bad = true;
			break;
		      case OMP_REDUCTION_IAND:
		      case OMP_REDUCTION_IOR:
		      case OMP_REDUCTION_IEOR:
			if (n->sym->ts.type != BT_INTEGER)
			  bad = true;
			break;
		      case OMP_REDUCTION_USER:
			bad = true;
			break;
		      default:
			break;
		      }
		    if (!bad)
		      n->udr = NULL;
		    else
		      {
			const char *udr_name = NULL;
			if (n->udr)
			  {
			    udr_name = n->udr->udr->name;
			    n->udr->udr
			      = gfc_find_omp_udr (NULL, udr_name,
						  &n->sym->ts);
			    if (n->udr->udr == NULL)
			      {
				free (n->udr);
				n->udr = NULL;
			      }
			  }
			if (n->udr == NULL)
			  {
			    if (udr_name == NULL)
			      switch (n->u.reduction_op)
				{
				case OMP_REDUCTION_PLUS:
				case OMP_REDUCTION_TIMES:
				case OMP_REDUCTION_MINUS:
				case OMP_REDUCTION_AND:
				case OMP_REDUCTION_OR:
				case OMP_REDUCTION_EQV:
				case OMP_REDUCTION_NEQV:
				  udr_name = gfc_op2string ((gfc_intrinsic_op)
							    n->u.reduction_op);
				  break;
				case OMP_REDUCTION_MAX:
				  udr_name = "max";
				  break;
				case OMP_REDUCTION_MIN:
				  udr_name = "min";
				  break;
				case OMP_REDUCTION_IAND:
				  udr_name = "iand";
				  break;
				case OMP_REDUCTION_IOR:
				  udr_name = "ior";
				  break;
				case OMP_REDUCTION_IEOR:
				  udr_name = "ieor";
				  break;
				default:
				  gcc_unreachable ();
				}
			    gfc_error ("!$OMP DECLARE REDUCTION %s not found "
				       "for type %s at %L", udr_name,
				       gfc_typename (&n->sym->ts), where);
			  }
			else
			  {
			    gfc_omp_udr *udr = n->udr->udr;
			    n->u.reduction_op = OMP_REDUCTION_USER;
			    n->udr->combiner
			      = resolve_omp_udr_clause (n, udr->combiner_ns,
							udr->omp_out,
							udr->omp_in);
			    if (udr->initializer_ns)
			      n->udr->initializer
				= resolve_omp_udr_clause (n,
							  udr->initializer_ns,
							  udr->omp_priv,
							  udr->omp_orig);
			  }
		      }
		    break;
		  case OMP_LIST_LINEAR:
		    if (n->sym->ts.type != BT_INTEGER)
		      gfc_error ("LINEAR variable '%s' must be INTEGER "
				 "at %L", n->sym->name, where);
		    else if (!code && !n->sym->attr.value)
		      gfc_error ("LINEAR dummy argument '%s' must have VALUE "
				 "attribute at %L", n->sym->name, where);
		    else if (n->expr)
		      {
			gfc_expr *expr = n->expr;
			if (!gfc_resolve_expr (expr)
			    || expr->ts.type != BT_INTEGER
			    || expr->rank != 0)
			  gfc_error ("'%s' in LINEAR clause at %L requires "
				     "a scalar integer linear-step expression",
				     n->sym->name, where);
			else if (!code && expr->expr_type != EXPR_CONSTANT)
			  gfc_error ("'%s' in LINEAR clause at %L requires "
				     "a constant integer linear-step expression",
				     n->sym->name, where);
		      }
		    break;
		  /* Workaround for PR middle-end/26316, nothing really needs
		     to be done here for OMP_LIST_PRIVATE.  */
		  case OMP_LIST_PRIVATE:
		    gcc_assert (code && code->op != EXEC_NOP);
		  default:
		    break;
		  }
	      }
	    break;
	  }
      }
  if (omp_clauses->safelen_expr)
    {
      gfc_expr *expr = omp_clauses->safelen_expr;
      if (!gfc_resolve_expr (expr)
	  || expr->ts.type != BT_INTEGER || expr->rank != 0)
	gfc_error ("SAFELEN clause at %L requires a scalar "
		   "INTEGER expression", &expr->where);
    }
  if (omp_clauses->simdlen_expr)
    {
      gfc_expr *expr = omp_clauses->simdlen_expr;
      if (!gfc_resolve_expr (expr)
	  || expr->ts.type != BT_INTEGER || expr->rank != 0)
	gfc_error ("SIMDLEN clause at %L requires a scalar "
		   "INTEGER expression", &expr->where);
    }
  if (omp_clauses->num_teams)
    {
      gfc_expr *expr = omp_clauses->num_teams;
      if (!gfc_resolve_expr (expr)
	  || expr->ts.type != BT_INTEGER || expr->rank != 0)
	gfc_error ("NUM_TEAMS clause at %L requires a scalar "
		   "INTEGER expression", &expr->where);
    }
  if (omp_clauses->device)
    {
      gfc_expr *expr = omp_clauses->device;
      if (!gfc_resolve_expr (expr)
	  || expr->ts.type != BT_INTEGER || expr->rank != 0)
	gfc_error ("DEVICE clause at %L requires a scalar "
		   "INTEGER expression", &expr->where);
    }
  if (omp_clauses->dist_chunk_size)
    {
      gfc_expr *expr = omp_clauses->dist_chunk_size;
      if (!gfc_resolve_expr (expr)
	  || expr->ts.type != BT_INTEGER || expr->rank != 0)
	gfc_error ("DIST_SCHEDULE clause's chunk_size at %L requires "
		   "a scalar INTEGER expression", &expr->where);
    }
  if (omp_clauses->thread_limit)
    {
      gfc_expr *expr = omp_clauses->thread_limit;
      if (!gfc_resolve_expr (expr)
	  || expr->ts.type != BT_INTEGER || expr->rank != 0)
	gfc_error ("THREAD_LIMIT clause at %L requires a scalar "
		   "INTEGER expression", &expr->where);
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
  gfc_code *atomic_code = code;
  gfc_symbol *var;
  gfc_expr *expr2, *expr2_tmp;
  gfc_omp_atomic_op aop
    = (gfc_omp_atomic_op) (atomic_code->ext.omp_atomic & GFC_OMP_ATOMIC_MASK);

  code = code->block->next;
  gcc_assert (code->op == EXEC_ASSIGN);
  gcc_assert (((aop != GFC_OMP_ATOMIC_CAPTURE) && code->next == NULL)
	      || ((aop == GFC_OMP_ATOMIC_CAPTURE)
		  && code->next != NULL
		  && code->next->op == EXEC_ASSIGN
		  && code->next->next == NULL));

  if (code->expr1->expr_type != EXPR_VARIABLE
      || code->expr1->symtree == NULL
      || code->expr1->rank != 0
      || (code->expr1->ts.type != BT_INTEGER
	  && code->expr1->ts.type != BT_REAL
	  && code->expr1->ts.type != BT_COMPLEX
	  && code->expr1->ts.type != BT_LOGICAL))
    {
      gfc_error ("!$OMP ATOMIC statement must set a scalar variable of "
		 "intrinsic type at %L", &code->loc);
      return;
    }

  var = code->expr1->symtree->n.sym;
  expr2 = is_conversion (code->expr2, false);
  if (expr2 == NULL)
    {
      if (aop == GFC_OMP_ATOMIC_READ || aop == GFC_OMP_ATOMIC_WRITE)
	expr2 = is_conversion (code->expr2, true);
      if (expr2 == NULL)
	expr2 = code->expr2;
    }

  switch (aop)
    {
    case GFC_OMP_ATOMIC_READ:
      if (expr2->expr_type != EXPR_VARIABLE
	  || expr2->symtree == NULL
	  || expr2->rank != 0
	  || (expr2->ts.type != BT_INTEGER
	      && expr2->ts.type != BT_REAL
	      && expr2->ts.type != BT_COMPLEX
	      && expr2->ts.type != BT_LOGICAL))
	gfc_error ("!$OMP ATOMIC READ statement must read from a scalar "
		   "variable of intrinsic type at %L", &expr2->where);
      return;
    case GFC_OMP_ATOMIC_WRITE:
      if (expr2->rank != 0 || expr_references_sym (code->expr2, var, NULL))
	gfc_error ("expr in !$OMP ATOMIC WRITE assignment var = expr "
		   "must be scalar and cannot reference var at %L",
		   &expr2->where);
      return;
    case GFC_OMP_ATOMIC_CAPTURE:
      expr2_tmp = expr2;
      if (expr2 == code->expr2)
	{
	  expr2_tmp = is_conversion (code->expr2, true);
	  if (expr2_tmp == NULL)
	    expr2_tmp = expr2;
	}
      if (expr2_tmp->expr_type == EXPR_VARIABLE)
	{
	  if (expr2_tmp->symtree == NULL
	      || expr2_tmp->rank != 0
	      || (expr2_tmp->ts.type != BT_INTEGER
		  && expr2_tmp->ts.type != BT_REAL
		  && expr2_tmp->ts.type != BT_COMPLEX
		  && expr2_tmp->ts.type != BT_LOGICAL)
	      || expr2_tmp->symtree->n.sym == var)
	    {
	      gfc_error ("!$OMP ATOMIC CAPTURE capture statement must read from "
			 "a scalar variable of intrinsic type at %L",
			 &expr2_tmp->where);
	      return;
	    }
	  var = expr2_tmp->symtree->n.sym;
	  code = code->next;
	  if (code->expr1->expr_type != EXPR_VARIABLE
	      || code->expr1->symtree == NULL
	      || code->expr1->rank != 0
	      || (code->expr1->ts.type != BT_INTEGER
		  && code->expr1->ts.type != BT_REAL
		  && code->expr1->ts.type != BT_COMPLEX
		  && code->expr1->ts.type != BT_LOGICAL))
	    {
	      gfc_error ("!$OMP ATOMIC CAPTURE update statement must set "
			 "a scalar variable of intrinsic type at %L",
			 &code->expr1->where);
	      return;
	    }
	  if (code->expr1->symtree->n.sym != var)
	    {
	      gfc_error ("!$OMP ATOMIC CAPTURE capture statement reads from "
			 "different variable than update statement writes "
			 "into at %L", &code->expr1->where);
	      return;
	    }
	  expr2 = is_conversion (code->expr2, false);
	  if (expr2 == NULL)
	    expr2 = code->expr2;
	}
      break;
    default:
      break;
    }

  if (gfc_expr_attr (code->expr1).allocatable)
    {
      gfc_error ("!$OMP ATOMIC with ALLOCATABLE variable at %L",
		 &code->loc);
      return;
    }

  if (aop == GFC_OMP_ATOMIC_CAPTURE
      && code->next == NULL
      && code->expr2->rank == 0
      && !expr_references_sym (code->expr2, var, NULL))
    atomic_code->ext.omp_atomic
      = (gfc_omp_atomic_op) (atomic_code->ext.omp_atomic
			     | GFC_OMP_ATOMIC_SWAP);
  else if (expr2->expr_type == EXPR_OP)
    {
      gfc_expr *v = NULL, *e, *c;
      gfc_intrinsic_op op = expr2->value.op.op;
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
	  gfc_error ("!$OMP ATOMIC assignment operator must be binary "
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
		     || (e->value.op.op != op
			 && e->value.op.op != alt_op)
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
	      switch (e->value.op.op)
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
	    {
	      gfc_error ("!$OMP ATOMIC intrinsic arguments except one must "
			 "not reference '%s' at %L",
			 var->name, &arg->expr->where);
	      return;
	    }
	  if (arg->expr->rank != 0)
	    {
	      gfc_error ("!$OMP ATOMIC intrinsic arguments must be scalar "
			 "at %L", &arg->expr->where);
	      return;
	    }
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
    gfc_error ("!$OMP ATOMIC assignment must have an operator or "
	       "intrinsic on right hand side at %L", &expr2->where);

  if (aop == GFC_OMP_ATOMIC_CAPTURE && code->next)
    {
      code = code->next;
      if (code->expr1->expr_type != EXPR_VARIABLE
	  || code->expr1->symtree == NULL
	  || code->expr1->rank != 0
	  || (code->expr1->ts.type != BT_INTEGER
	      && code->expr1->ts.type != BT_REAL
	      && code->expr1->ts.type != BT_COMPLEX
	      && code->expr1->ts.type != BT_LOGICAL))
	{
	  gfc_error ("!$OMP ATOMIC CAPTURE capture statement must set "
		     "a scalar variable of intrinsic type at %L",
		     &code->expr1->where);
	  return;
	}

      expr2 = is_conversion (code->expr2, false);
      if (expr2 == NULL)
	{
	  expr2 = is_conversion (code->expr2, true);
	  if (expr2 == NULL)
	    expr2 = code->expr2;
	}

      if (expr2->expr_type != EXPR_VARIABLE
	  || expr2->symtree == NULL
	  || expr2->rank != 0
	  || (expr2->ts.type != BT_INTEGER
	      && expr2->ts.type != BT_REAL
	      && expr2->ts.type != BT_COMPLEX
	      && expr2->ts.type != BT_LOGICAL))
	{
	  gfc_error ("!$OMP ATOMIC CAPTURE capture statement must read "
		     "from a scalar variable of intrinsic type at %L",
		     &expr2->where);
	  return;
	}
      if (expr2->symtree->n.sym != var)
	{
	  gfc_error ("!$OMP ATOMIC CAPTURE capture statement reads from "
		     "different variable than update statement writes "
		     "into at %L", &expr2->where);
	  return;
	}
    }
}


struct omp_context
{
  gfc_code *code;
  struct pointer_set_t *sharing_clauses;
  struct pointer_set_t *private_iterators;
  struct omp_context *previous;
} *omp_current_ctx;
static gfc_code *omp_current_do_code;
static int omp_current_do_collapse;

void
gfc_resolve_omp_do_blocks (gfc_code *code, gfc_namespace *ns)
{
  if (code->block->next && code->block->next->op == EXEC_DO)
    {
      int i;
      gfc_code *c;

      omp_current_do_code = code->block->next;
      omp_current_do_collapse = code->ext.omp_clauses->collapse;
      for (i = 1, c = omp_current_do_code; i < omp_current_do_collapse; i++)
	{
	  c = c->block;
	  if (c->op != EXEC_DO || c->next == NULL)
	    break;
	  c = c->next;
	  if (c->op != EXEC_DO)
	    break;
	}
      if (i < omp_current_do_collapse || omp_current_do_collapse <= 0)
	omp_current_do_collapse = 1;
    }
  gfc_resolve_blocks (code->block, ns);
  omp_current_do_collapse = 0;
  omp_current_do_code = NULL;
}


void
gfc_resolve_omp_parallel_blocks (gfc_code *code, gfc_namespace *ns)
{
  struct omp_context ctx;
  gfc_omp_clauses *omp_clauses = code->ext.omp_clauses;
  gfc_omp_namelist *n;
  int list;

  ctx.code = code;
  ctx.sharing_clauses = pointer_set_create ();
  ctx.private_iterators = pointer_set_create ();
  ctx.previous = omp_current_ctx;
  omp_current_ctx = &ctx;

  for (list = 0; list < OMP_LIST_NUM; list++)
    switch (list)
      {
      case OMP_LIST_SHARED:
      case OMP_LIST_PRIVATE:
      case OMP_LIST_FIRSTPRIVATE:
      case OMP_LIST_LASTPRIVATE:
      case OMP_LIST_REDUCTION:
      case OMP_LIST_LINEAR:
	for (n = omp_clauses->lists[list]; n; n = n->next)
	  pointer_set_insert (ctx.sharing_clauses, n->sym);
	break;
      default:
	break;
      }

  switch (code->op)
    {
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
      gfc_resolve_omp_do_blocks (code, ns);
      break;
    default:
      gfc_resolve_blocks (code->block, ns);
    }

  omp_current_ctx = ctx.previous;
  pointer_set_destroy (ctx.sharing_clauses);
  pointer_set_destroy (ctx.private_iterators);
}


/* Save and clear openmp.c private state.  */

void
gfc_omp_save_and_clear_state (struct gfc_omp_saved_state *state)
{
  state->ptrs[0] = omp_current_ctx;
  state->ptrs[1] = omp_current_do_code;
  state->ints[0] = omp_current_do_collapse;
  omp_current_ctx = NULL;
  omp_current_do_code = NULL;
  omp_current_do_collapse = 0;
}


/* Restore openmp.c private state from the saved state.  */

void
gfc_omp_restore_state (struct gfc_omp_saved_state *state)
{
  omp_current_ctx = (struct omp_context *) state->ptrs[0];
  omp_current_do_code = (gfc_code *) state->ptrs[1];
  omp_current_do_collapse = state->ints[0];
}


/* Note a DO iterator variable.  This is special in !$omp parallel
   construct, where they are predetermined private.  */

void
gfc_resolve_do_iterator (gfc_code *code, gfc_symbol *sym)
{
  int i = omp_current_do_collapse;
  gfc_code *c = omp_current_do_code;

  if (sym->attr.threadprivate)
    return;

  /* !$omp do and !$omp parallel do iteration variable is predetermined
     private just in the !$omp do resp. !$omp parallel do construct,
     with no implications for the outer parallel constructs.  */

  while (i-- >= 1)
    {
      if (code == c)
	return;

      c = c->block->next;
    }

  if (omp_current_ctx == NULL)
    return;

  if (pointer_set_contains (omp_current_ctx->sharing_clauses, sym))
    return;

  if (! pointer_set_insert (omp_current_ctx->private_iterators, sym))
    {
      gfc_omp_clauses *omp_clauses = omp_current_ctx->code->ext.omp_clauses;
      gfc_omp_namelist *p;

      p = gfc_get_omp_namelist ();
      p->sym = sym;
      p->next = omp_clauses->lists[OMP_LIST_PRIVATE];
      omp_clauses->lists[OMP_LIST_PRIVATE] = p;
    }
}


static void
resolve_omp_do (gfc_code *code)
{
  gfc_code *do_code, *c;
  int list, i, collapse;
  gfc_omp_namelist *n;
  gfc_symbol *dovar;
  const char *name;
  bool is_simd = false;

  switch (code->op)
    {
    case EXEC_OMP_DISTRIBUTE: name = "!$OMP DISTRIBUTE"; break;
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
      name = "!$OMP DISTRIBUTE PARALLEL DO";
      break;
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
      name = "!$OMP DISTRIBUTE PARALLEL DO SIMD";
      is_simd = true;
      break;
    case EXEC_OMP_DISTRIBUTE_SIMD:
      name = "!$OMP DISTRIBUTE SIMD";
      is_simd = true;
      break;
    case EXEC_OMP_DO: name = "!$OMP DO"; break;
    case EXEC_OMP_DO_SIMD: name = "!$OMP DO SIMD"; is_simd = true; break;
    case EXEC_OMP_PARALLEL_DO: name = "!$OMP PARALLEL DO"; break;
    case EXEC_OMP_PARALLEL_DO_SIMD:
      name = "!$OMP PARALLEL DO SIMD";
      is_simd = true;
      break;
    case EXEC_OMP_SIMD: name = "!$OMP SIMD"; is_simd = true; break;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
      name = "!$OMP TARGET TEAMS_DISTRIBUTE";
      break;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
      name = "!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO";
      break;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      name = "!$OMP TARGET TEAMS DISTRIBUTE PARALLEL DO SIMD";
      is_simd = true;
      break;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
      name = "!$OMP TARGET TEAMS DISTRIBUTE SIMD";
      is_simd = true;
      break;
    case EXEC_OMP_TEAMS_DISTRIBUTE: name = "!$OMP TEAMS_DISTRIBUTE"; break;
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
      name = "!$OMP TEAMS DISTRIBUTE PARALLEL DO";
      break;
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      name = "!$OMP TEAMS DISTRIBUTE PARALLEL DO SIMD";
      is_simd = true;
      break;
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
      name = "!$OMP TEAMS DISTRIBUTE SIMD";
      is_simd = true;
      break;
    default: gcc_unreachable ();
    }

  if (code->ext.omp_clauses)
    resolve_omp_clauses (code, &code->loc, code->ext.omp_clauses, NULL);

  do_code = code->block->next;
  collapse = code->ext.omp_clauses->collapse;
  if (collapse <= 0)
    collapse = 1;
  for (i = 1; i <= collapse; i++)
    {
      if (do_code->op == EXEC_DO_WHILE)
	{
	  gfc_error ("%s cannot be a DO WHILE or DO without loop control "
		     "at %L", name, &do_code->loc);
	  break;
	}
      if (do_code->op == EXEC_DO_CONCURRENT)
	{
	  gfc_error ("%s cannot be a DO CONCURRENT loop at %L", name,
		     &do_code->loc);
	  break;
	}
      gcc_assert (do_code->op == EXEC_DO);
      if (do_code->ext.iterator->var->ts.type != BT_INTEGER)
	gfc_error ("%s iteration variable must be of type integer at %L",
		   name, &do_code->loc);
      dovar = do_code->ext.iterator->var->symtree->n.sym;
      if (dovar->attr.threadprivate)
	gfc_error ("%s iteration variable must not be THREADPRIVATE "
		   "at %L", name, &do_code->loc);
      if (code->ext.omp_clauses)
	for (list = 0; list < OMP_LIST_NUM; list++)
	  if (!is_simd
	      ? (list != OMP_LIST_PRIVATE && list != OMP_LIST_LASTPRIVATE)
	      : code->ext.omp_clauses->collapse > 1
	      ? (list != OMP_LIST_LASTPRIVATE)
	      : (list != OMP_LIST_LINEAR))
	    for (n = code->ext.omp_clauses->lists[list]; n; n = n->next)
	      if (dovar == n->sym)
		{
		  if (!is_simd)
		    gfc_error ("%s iteration variable present on clause "
			       "other than PRIVATE or LASTPRIVATE at %L",
			       name, &do_code->loc);
		  else if (code->ext.omp_clauses->collapse > 1)
		    gfc_error ("%s iteration variable present on clause "
			       "other than LASTPRIVATE at %L",
			       name, &do_code->loc);
		  else
		    gfc_error ("%s iteration variable present on clause "
			       "other than LINEAR at %L",
			       name, &do_code->loc);
		  break;
		}
      if (i > 1)
	{
	  gfc_code *do_code2 = code->block->next;
	  int j;

	  for (j = 1; j < i; j++)
	    {
	      gfc_symbol *ivar = do_code2->ext.iterator->var->symtree->n.sym;
	      if (dovar == ivar
		  || gfc_find_sym_in_expr (ivar, do_code->ext.iterator->start)
		  || gfc_find_sym_in_expr (ivar, do_code->ext.iterator->end)
		  || gfc_find_sym_in_expr (ivar, do_code->ext.iterator->step))
		{
		  gfc_error ("%s collapsed loops don't form rectangular "
			     "iteration space at %L", name, &do_code->loc);
		  break;
		}
	      if (j < i)
		break;
	      do_code2 = do_code2->block->next;
	    }
	}
      if (i == collapse)
	break;
      for (c = do_code->next; c; c = c->next)
	if (c->op != EXEC_NOP && c->op != EXEC_CONTINUE)
	  {
	    gfc_error ("collapsed %s loops not perfectly nested at %L",
		       name, &c->loc);
	    break;
	  }
      if (c)
	break;
      do_code = do_code->block;
      if (do_code->op != EXEC_DO && do_code->op != EXEC_DO_WHILE)
	{
	  gfc_error ("not enough DO loops for collapsed %s at %L",
		     name, &code->loc);
	  break;
	}
      do_code = do_code->next;
      if (do_code == NULL
	  || (do_code->op != EXEC_DO && do_code->op != EXEC_DO_WHILE))
	{
	  gfc_error ("not enough DO loops for collapsed %s at %L",
		     name, &code->loc);
	  break;
	}
    }
}


/* Resolve OpenMP directive clauses and check various requirements
   of each directive.  */

void
gfc_resolve_omp_directive (gfc_code *code, gfc_namespace *ns ATTRIBUTE_UNUSED)
{
  if (code->op != EXEC_OMP_ATOMIC)
    gfc_maybe_initialize_eh ();

  switch (code->op)
    {
    case EXEC_OMP_DISTRIBUTE:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_DISTRIBUTE_SIMD:
    case EXEC_OMP_DO:
    case EXEC_OMP_DO_SIMD:
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_DO_SIMD:
    case EXEC_OMP_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
      resolve_omp_do (code);
      break;
    case EXEC_OMP_CANCEL:
    case EXEC_OMP_PARALLEL_WORKSHARE:
    case EXEC_OMP_PARALLEL:
    case EXEC_OMP_PARALLEL_SECTIONS:
    case EXEC_OMP_SECTIONS:
    case EXEC_OMP_SINGLE:
    case EXEC_OMP_TARGET:
    case EXEC_OMP_TARGET_DATA:
    case EXEC_OMP_TARGET_TEAMS:
    case EXEC_OMP_TASK:
    case EXEC_OMP_TEAMS:
    case EXEC_OMP_WORKSHARE:
      if (code->ext.omp_clauses)
	resolve_omp_clauses (code, &code->loc, code->ext.omp_clauses, NULL);
      break;
    case EXEC_OMP_TARGET_UPDATE:
      if (code->ext.omp_clauses)
	resolve_omp_clauses (code, &code->loc, code->ext.omp_clauses, NULL);
      if (code->ext.omp_clauses == NULL
	  || (code->ext.omp_clauses->lists[OMP_LIST_TO] == NULL
	      && code->ext.omp_clauses->lists[OMP_LIST_FROM] == NULL))
	gfc_error ("OMP TARGET UPDATE at %L requires at least one TO or "
		   "FROM clause", &code->loc);
      break;
    case EXEC_OMP_ATOMIC:
      resolve_omp_atomic (code);
      break;
    default:
      break;
    }
}

/* Resolve !$omp declare simd constructs in NS.  */

void
gfc_resolve_omp_declare_simd (gfc_namespace *ns)
{
  gfc_omp_declare_simd *ods;

  for (ods = ns->omp_declare_simd; ods; ods = ods->next)
    {
      if (ods->proc_name != ns->proc_name)
	gfc_error ("!$OMP DECLARE SIMD should refer to containing procedure "
		   "'%s' at %L", ns->proc_name->name, &ods->where);
      if (ods->clauses)
	resolve_omp_clauses (NULL, &ods->where, ods->clauses, ns);
    }
}

struct omp_udr_callback_data
{
  gfc_omp_udr *omp_udr;
  bool is_initializer;
};

static int
omp_udr_callback (gfc_expr **e, int *walk_subtrees ATTRIBUTE_UNUSED,
		  void *data)
{
  struct omp_udr_callback_data *cd = (struct omp_udr_callback_data *) data;
  if ((*e)->expr_type == EXPR_VARIABLE)
    {
      if (cd->is_initializer)
	{
	  if ((*e)->symtree->n.sym != cd->omp_udr->omp_priv
	      && (*e)->symtree->n.sym != cd->omp_udr->omp_orig)
	    gfc_error ("Variable other than OMP_PRIV or OMP_ORIG used in "
		       "INITIALIZER clause of !$OMP DECLARE REDUCTION at %L",
		       &(*e)->where);
	}
      else
	{
	  if ((*e)->symtree->n.sym != cd->omp_udr->omp_out
	      && (*e)->symtree->n.sym != cd->omp_udr->omp_in)
	    gfc_error ("Variable other than OMP_OUT or OMP_IN used in "
		       "combiner of !$OMP DECLARE REDUCTION at %L",
		       &(*e)->where);
	}
    }
  return 0;
}

/* Resolve !$omp declare reduction constructs.  */

static void
gfc_resolve_omp_udr (gfc_omp_udr *omp_udr)
{
  gfc_actual_arglist *a;
  const char *predef_name = NULL;

  switch (omp_udr->rop)
    {
    case OMP_REDUCTION_PLUS:
    case OMP_REDUCTION_TIMES:
    case OMP_REDUCTION_MINUS:
    case OMP_REDUCTION_AND:
    case OMP_REDUCTION_OR:
    case OMP_REDUCTION_EQV:
    case OMP_REDUCTION_NEQV:
    case OMP_REDUCTION_MAX:
    case OMP_REDUCTION_USER:
      break;
    default:
      gfc_error ("Invalid operator for !$OMP DECLARE REDUCTION %s at %L",
		 omp_udr->name, &omp_udr->where);
      return;
    }

  if (gfc_omp_udr_predef (omp_udr->rop, omp_udr->name,
			  &omp_udr->ts, &predef_name))
    {
      if (predef_name)
	gfc_error_now ("Redefinition of predefined %s "
		       "!$OMP DECLARE REDUCTION at %L",
		       predef_name, &omp_udr->where);
      else
	gfc_error_now ("Redefinition of predefined "
		       "!$OMP DECLARE REDUCTION at %L", &omp_udr->where);
      return;
    }

  if (omp_udr->ts.type == BT_CHARACTER
      && omp_udr->ts.u.cl->length
      && omp_udr->ts.u.cl->length->expr_type != EXPR_CONSTANT)
    {
      gfc_error ("CHARACTER length in !$OMP DECLARE REDUCTION %s not "
		 "constant at %L", omp_udr->name, &omp_udr->where);
      return;
    }

  struct omp_udr_callback_data cd;
  cd.omp_udr = omp_udr;
  cd.is_initializer = false;
  gfc_code_walker (&omp_udr->combiner_ns->code, gfc_dummy_code_callback,
		   omp_udr_callback, &cd);
  if (omp_udr->combiner_ns->code->op == EXEC_CALL)
    {
      for (a = omp_udr->combiner_ns->code->ext.actual; a; a = a->next)
	if (a->expr == NULL)
	  break;
      if (a)
	gfc_error ("Subroutine call with alternate returns in combiner "
		   "of !$OMP DECLARE REDUCTION at %L",
		   &omp_udr->combiner_ns->code->loc);
    }
  if (omp_udr->initializer_ns)
    {
      cd.is_initializer = true;
      gfc_code_walker (&omp_udr->initializer_ns->code, gfc_dummy_code_callback,
		       omp_udr_callback, &cd);
      if (omp_udr->initializer_ns->code->op == EXEC_CALL)
	{
	  for (a = omp_udr->initializer_ns->code->ext.actual; a; a = a->next)
	    if (a->expr == NULL)
	      break;
	  if (a)
	    gfc_error ("Subroutine call with alternate returns in "
		       "INITIALIZER clause of !$OMP DECLARE REDUCTION "
		       "at %L", &omp_udr->initializer_ns->code->loc);
	  for (a = omp_udr->initializer_ns->code->ext.actual; a; a = a->next)
	    if (a->expr
		&& a->expr->expr_type == EXPR_VARIABLE
		&& a->expr->symtree->n.sym == omp_udr->omp_priv
		&& a->expr->ref == NULL)
	      break;
	  if (a == NULL)
	    gfc_error ("One of actual subroutine arguments in INITIALIZER "
		       "clause of !$OMP DECLARE REDUCTION must be OMP_PRIV "
		       "at %L", &omp_udr->initializer_ns->code->loc);
	}
    }
  else if (omp_udr->ts.type == BT_DERIVED
	   && !gfc_has_default_initializer (omp_udr->ts.u.derived))
    {
      gfc_error ("Missing INITIALIZER clause for !$OMP DECLARE REDUCTION "
		 "of derived type without default initializer at %L",
		 &omp_udr->where);
      return;
    }
}

void
gfc_resolve_omp_udrs (gfc_symtree *st)
{
  gfc_omp_udr *omp_udr;

  if (st == NULL)
    return;
  gfc_resolve_omp_udrs (st->left);
  gfc_resolve_omp_udrs (st->right);
  for (omp_udr = st->n.omp_udr; omp_udr; omp_udr = omp_udr->next)
    gfc_resolve_omp_udr (omp_udr);
}
