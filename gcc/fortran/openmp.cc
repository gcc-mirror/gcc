/* OpenMP directive matching and resolving.
   Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

#define INCLUDE_VECTOR
#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"
#include "gfortran.h"
#include "arith.h"
#include "match.h"
#include "parse.h"
#include "constructor.h"
#include "diagnostic.h"
#include "gomp-constants.h"
#include "target-memory.h"  /* For gfc_encode_character.  */
#include "bitmap.h"
#include "omp-api.h"  /* For omp_runtime_api_procname.  */

location_t gfc_get_location (locus *);

static gfc_statement omp_code_to_statement (gfc_code *);

enum gfc_omp_directive_kind {
  GFC_OMP_DIR_DECLARATIVE,
  GFC_OMP_DIR_EXECUTABLE,
  GFC_OMP_DIR_INFORMATIONAL,
  GFC_OMP_DIR_META,
  GFC_OMP_DIR_SUBSIDIARY,
  GFC_OMP_DIR_UTILITY
};

struct gfc_omp_directive {
  const char *name;
  enum gfc_omp_directive_kind kind;
  gfc_statement st;
};

/* Alphabetically sorted OpenMP clauses, except that longer strings are before
   substrings; excludes combined/composite directives. See note for "ordered"
   and "nothing".  */

static const struct gfc_omp_directive gfc_omp_directives[] = {
  {"allocate", GFC_OMP_DIR_DECLARATIVE, ST_OMP_ALLOCATE},
  {"allocators", GFC_OMP_DIR_EXECUTABLE, ST_OMP_ALLOCATORS},
  {"assumes", GFC_OMP_DIR_INFORMATIONAL, ST_OMP_ASSUMES},
  {"assume", GFC_OMP_DIR_INFORMATIONAL, ST_OMP_ASSUME},
  {"atomic", GFC_OMP_DIR_EXECUTABLE, ST_OMP_ATOMIC},
  {"barrier", GFC_OMP_DIR_EXECUTABLE, ST_OMP_BARRIER},
  {"cancellation point", GFC_OMP_DIR_EXECUTABLE, ST_OMP_CANCELLATION_POINT},
  {"cancel", GFC_OMP_DIR_EXECUTABLE, ST_OMP_CANCEL},
  {"critical", GFC_OMP_DIR_EXECUTABLE, ST_OMP_CRITICAL},
  /* {"declare mapper", GFC_OMP_DIR_DECLARATIVE, ST_OMP_DECLARE_MAPPER}, */
  {"declare reduction", GFC_OMP_DIR_DECLARATIVE, ST_OMP_DECLARE_REDUCTION},
  {"declare simd", GFC_OMP_DIR_DECLARATIVE, ST_OMP_DECLARE_SIMD},
  {"declare target", GFC_OMP_DIR_DECLARATIVE, ST_OMP_DECLARE_TARGET},
  {"declare variant", GFC_OMP_DIR_DECLARATIVE, ST_OMP_DECLARE_VARIANT},
  {"depobj", GFC_OMP_DIR_EXECUTABLE, ST_OMP_DEPOBJ},
  {"dispatch", GFC_OMP_DIR_EXECUTABLE, ST_OMP_DISPATCH},
  {"distribute", GFC_OMP_DIR_EXECUTABLE, ST_OMP_DISTRIBUTE},
  {"do", GFC_OMP_DIR_EXECUTABLE, ST_OMP_DO},
  /* "error" becomes GFC_OMP_DIR_EXECUTABLE with at(execution) */
  {"error", GFC_OMP_DIR_UTILITY, ST_OMP_ERROR},
  {"flush", GFC_OMP_DIR_EXECUTABLE, ST_OMP_FLUSH},
  {"interop", GFC_OMP_DIR_EXECUTABLE, ST_OMP_INTEROP},
  {"loop", GFC_OMP_DIR_EXECUTABLE, ST_OMP_LOOP},
  {"masked", GFC_OMP_DIR_EXECUTABLE, ST_OMP_MASKED},
  {"metadirective", GFC_OMP_DIR_META, ST_OMP_METADIRECTIVE},
  /* Note: gfc_match_omp_nothing returns ST_NONE.  */
  {"nothing", GFC_OMP_DIR_UTILITY, ST_OMP_NOTHING},
  /* Special case; for now map to the first one.
     ordered-blockassoc = ST_OMP_ORDERED
     ordered-standalone = ST_OMP_ORDERED_DEPEND + depend/doacross.  */
  {"ordered", GFC_OMP_DIR_EXECUTABLE, ST_OMP_ORDERED},
  {"parallel", GFC_OMP_DIR_EXECUTABLE, ST_OMP_PARALLEL},
  {"requires", GFC_OMP_DIR_INFORMATIONAL, ST_OMP_REQUIRES},
  {"scan", GFC_OMP_DIR_SUBSIDIARY, ST_OMP_SCAN},
  {"scope", GFC_OMP_DIR_EXECUTABLE, ST_OMP_SCOPE},
  {"sections", GFC_OMP_DIR_EXECUTABLE, ST_OMP_SECTIONS},
  {"section", GFC_OMP_DIR_SUBSIDIARY, ST_OMP_SECTION},
  {"simd", GFC_OMP_DIR_EXECUTABLE, ST_OMP_SIMD},
  {"single", GFC_OMP_DIR_EXECUTABLE, ST_OMP_SINGLE},
  {"target data", GFC_OMP_DIR_EXECUTABLE, ST_OMP_TARGET_DATA},
  {"target enter data", GFC_OMP_DIR_EXECUTABLE, ST_OMP_TARGET_ENTER_DATA},
  {"target exit data", GFC_OMP_DIR_EXECUTABLE, ST_OMP_TARGET_EXIT_DATA},
  {"target update", GFC_OMP_DIR_EXECUTABLE, ST_OMP_TARGET_UPDATE},
  {"target", GFC_OMP_DIR_EXECUTABLE, ST_OMP_TARGET},
  {"taskloop", GFC_OMP_DIR_EXECUTABLE, ST_OMP_TASKLOOP},
  {"taskwait", GFC_OMP_DIR_EXECUTABLE, ST_OMP_TASKWAIT},
  {"taskyield", GFC_OMP_DIR_EXECUTABLE, ST_OMP_TASKYIELD},
  {"task", GFC_OMP_DIR_EXECUTABLE, ST_OMP_TASK},
  {"teams", GFC_OMP_DIR_EXECUTABLE, ST_OMP_TEAMS},
  {"threadprivate", GFC_OMP_DIR_DECLARATIVE, ST_OMP_THREADPRIVATE},
  {"tile", GFC_OMP_DIR_EXECUTABLE, ST_OMP_TILE},
  {"unroll", GFC_OMP_DIR_EXECUTABLE, ST_OMP_UNROLL},
  {"workshare", GFC_OMP_DIR_EXECUTABLE, ST_OMP_WORKSHARE},
};


/* Match an end of OpenMP directive.  End of OpenMP directive is optional
   whitespace, followed by '\n' or comment '!'.  In the special case where a
   context selector is being matched, match against ')' instead.  */

static match
gfc_match_omp_eos (void)
{
  locus old_loc;
  char c;

  old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();

  if (gfc_matching_omp_context_selector)
    {
      if (gfc_peek_ascii_char () == ')')
	return MATCH_YES;
    }
  else
    {
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
    }

  gfc_current_locus = old_loc;
  return MATCH_NO;
}

match
gfc_match_omp_eos_error (void)
{
  if (gfc_match_omp_eos() == MATCH_YES)
    return MATCH_YES;

  gfc_error ("Unexpected junk at %C");
  return MATCH_ERROR;
}


/* Free an omp_clauses structure.  */

void
gfc_free_omp_clauses (gfc_omp_clauses *c)
{
  int i;
  if (c == NULL)
    return;

  gfc_free_expr (c->if_expr);
  for (i = 0; i < OMP_IF_LAST; i++)
    gfc_free_expr (c->if_exprs[i]);
  gfc_free_expr (c->self_expr);
  gfc_free_expr (c->final_expr);
  gfc_free_expr (c->num_threads);
  gfc_free_expr (c->chunk_size);
  gfc_free_expr (c->safelen_expr);
  gfc_free_expr (c->simdlen_expr);
  gfc_free_expr (c->num_teams_lower);
  gfc_free_expr (c->num_teams_upper);
  gfc_free_expr (c->device);
  gfc_free_expr (c->thread_limit);
  gfc_free_expr (c->dist_chunk_size);
  gfc_free_expr (c->grainsize);
  gfc_free_expr (c->hint);
  gfc_free_expr (c->num_tasks);
  gfc_free_expr (c->priority);
  gfc_free_expr (c->detach);
  gfc_free_expr (c->novariants);
  gfc_free_expr (c->nocontext);
  gfc_free_expr (c->async_expr);
  gfc_free_expr (c->gang_num_expr);
  gfc_free_expr (c->gang_static_expr);
  gfc_free_expr (c->worker_expr);
  gfc_free_expr (c->vector_expr);
  gfc_free_expr (c->num_gangs_expr);
  gfc_free_expr (c->num_workers_expr);
  gfc_free_expr (c->vector_length_expr);
  for (i = 0; i < OMP_LIST_NUM; i++)
    gfc_free_omp_namelist (c->lists[i],
			   i == OMP_LIST_AFFINITY || i == OMP_LIST_DEPEND,
			   i == OMP_LIST_ALLOCATE,
			   i == OMP_LIST_USES_ALLOCATORS,
			   i == OMP_LIST_INIT);
  gfc_free_expr_list (c->wait_list);
  gfc_free_expr_list (c->tile_list);
  gfc_free_expr_list (c->sizes_list);
  free (CONST_CAST (char *, c->critical_name));
  if (c->assume)
    {
      free (c->assume->absent);
      free (c->assume->contains);
      gfc_free_expr_list (c->assume->holds);
      free (c->assume);
    }
  free (c);
}

/* Free oacc_declare structures.  */

void
gfc_free_oacc_declare_clauses (struct gfc_oacc_declare *oc)
{
  struct gfc_oacc_declare *decl = oc;

  do
    {
      struct gfc_oacc_declare *next;

      next = decl->next;
      gfc_free_omp_clauses (decl->clauses);
      free (decl);
      decl = next;
    }
  while (decl);
}

/* Free expression list. */
void
gfc_free_expr_list (gfc_expr_list *list)
{
  gfc_expr_list *n;

  for (; list; list = n)
    {
      n = list->next;
      free (list);
    }
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

static void
gfc_free_omp_trait_property_list (gfc_omp_trait_property *list)
{
  while (list)
    {
      gfc_omp_trait_property *current = list;
      list = list->next;
      switch (current->property_kind)
	{
	case OMP_TRAIT_PROPERTY_ID:
	  free (current->name);
	  break;
	case OMP_TRAIT_PROPERTY_NAME_LIST:
	  if (current->is_name)
	    free (current->name);
	  break;
	case OMP_TRAIT_PROPERTY_CLAUSE_LIST:
	  gfc_free_omp_clauses (current->clauses);
	  break;
	default:
	  break;
	}
      free (current);
    }
}

static void
gfc_free_omp_selector_list (gfc_omp_selector *list)
{
  while (list)
    {
      gfc_omp_selector *current = list;
      list = list->next;
      gfc_free_omp_trait_property_list (current->properties);
      free (current);
    }
}

static void
gfc_free_omp_set_selector_list (gfc_omp_set_selector *list)
{
  while (list)
    {
      gfc_omp_set_selector *current = list;
      list = list->next;
      gfc_free_omp_selector_list (current->trait_selectors);
      free (current);
    }
}

/* Free an !$omp declare variant construct list.  */

void
gfc_free_omp_declare_variant_list (gfc_omp_declare_variant *list)
{
  while (list)
    {
      gfc_omp_declare_variant *current = list;
      list = list->next;
      gfc_free_omp_set_selector_list (current->set_selectors);
      gfc_free_omp_namelist (current->adjust_args_list, false, false, false,
			     false);
      free (current);
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

/* Free variants of an !$omp metadirective construct.  */

void
gfc_free_omp_variants (gfc_omp_variant *variant)
{
  while (variant)
    {
      gfc_omp_variant *next_variant = variant->next;
      gfc_free_omp_set_selector_list (variant->selectors);
      free (variant);
      variant = next_variant;
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
	{
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


/* Match a variable/common block list and construct a namelist from it;
   if has_all_memory != NULL, *has_all_memory is set and omp_all_memory
   yields a list->sym NULL entry. */

static match
gfc_match_omp_variable_list (const char *str, gfc_omp_namelist **list,
			     bool allow_common, bool *end_colon = NULL,
			     gfc_omp_namelist ***headp = NULL,
			     bool allow_sections = false,
			     bool allow_derived = false,
			     bool *has_all_memory = NULL,
			     bool reject_common_vars = false,
			     bool reverse_order = false)
{
  gfc_omp_namelist *head, *tail, *p;
  locus old_loc, cur_loc;
  char n[GFC_MAX_SYMBOL_LEN+1];
  gfc_symbol *sym;
  match m;
  gfc_symtree *st;

  head = tail = NULL;

  old_loc = gfc_current_locus;
  if (has_all_memory)
    *has_all_memory = false;
  m = gfc_match (str);
  if (m != MATCH_YES)
    return m;

  for (;;)
    {
      gfc_gobble_whitespace ();
      cur_loc = gfc_current_locus;

      m = gfc_match_name (n);
      if (m == MATCH_YES && strcmp (n, "omp_all_memory") == 0)
	{
	  locus loc = gfc_get_location_range (NULL, 0, &cur_loc, 1,
					      &gfc_current_locus);
	  if (!has_all_memory)
	    {
	      gfc_error ("%<omp_all_memory%> at %L not permitted in this "
			 "clause", &loc);
	      goto cleanup;
	    }
	  *has_all_memory = true;
	  p = gfc_get_omp_namelist ();
	  if (head == NULL)
	    head = tail = p;
	  else
	    {
	      tail->next = p;
	      tail = tail->next;
	    }
	  tail->where = loc;
	  goto next_item;
	}
      if (m == MATCH_YES)
	{
	  gfc_symtree *st;
	  if ((m = gfc_get_ha_sym_tree (n, &st) ? MATCH_ERROR : MATCH_YES)
	      == MATCH_YES)
	    sym = st->n.sym;
	}
      switch (m)
	{
	case MATCH_YES:
	  gfc_expr *expr;
	  expr = NULL;
	  gfc_gobble_whitespace ();
	  if ((allow_sections && gfc_peek_ascii_char () == '(')
	      || (allow_derived && gfc_peek_ascii_char () == '%'))
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
	      if (gfc_is_coindexed (expr))
		{
		  gfc_error ("List item shall not be coindexed at %L",
			     &expr->where);
		  goto cleanup;
		}
	    }
	  gfc_set_sym_referenced (sym);
	  p = gfc_get_omp_namelist ();
	  if (head == NULL)
	    head = tail = p;
	  else if (reverse_order)
	    {
	      p->next = head;
	      head = p;
	    }
	  else
	    {
	      tail->next = p;
	      tail = tail->next;
	    }
	  p->sym = sym;
	  p->expr = expr;
	  p->where = gfc_get_location_range (NULL, 0, &cur_loc, 1,
					     &gfc_current_locus);
	  if (reject_common_vars && sym->attr.in_common)
	    {
	      gcc_assert (allow_common);
	      gfc_error ("%qs at %L is part of the common block %</%s/%> and "
			 "may only be specificed implicitly via the named "
			 "common block", sym->name, &cur_loc,
			 sym->common_head->name);
	      goto cleanup;
	    }
	  goto next_item;
	case MATCH_NO:
	  break;
	case MATCH_ERROR:
	  goto cleanup;
	}

      if (!allow_common)
	goto syntax;

      m = gfc_match ("/ %n /", n);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	goto syntax;

      cur_loc = gfc_get_location_range (NULL, 0, &cur_loc, 1,
					&gfc_current_locus);
      st = gfc_find_symtree (gfc_current_ns->common_root, n);
      if (st == NULL)
	{
	  gfc_error ("COMMON block %</%s/%> not found at %L", n, &cur_loc);
	  goto cleanup;
	}
      for (sym = st->n.common->head; sym; sym = sym->common_next)
	{
	  gfc_set_sym_referenced (sym);
	  p = gfc_get_omp_namelist ();
	  if (head == NULL)
	    head = tail = p;
	  else if (reverse_order)
	    {
	      p->next = head;
	      head = p;
	    }
	  else
	    {
	      tail->next = p;
	      tail = tail->next;
	    }
	  p->sym = sym;
	  p->where = cur_loc;
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
  gfc_free_omp_namelist (head, false, false, false, false);
  gfc_current_locus = old_loc;
  return MATCH_ERROR;
}

/* Match a variable/procedure/common block list and construct a namelist
   from it.  */

static match
gfc_match_omp_to_link (const char *str, gfc_omp_namelist **list)
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
	  p = gfc_get_omp_namelist ();
	  if (head == NULL)
	    head = tail = p;
	  else
	    {
	      tail->next = p;
	      tail = tail->next;
	    }
	  tail->sym = sym;
	  tail->where = cur_loc;
	  goto next_item;
	case MATCH_NO:
	  break;
	case MATCH_ERROR:
	  goto cleanup;
	}

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
      p = gfc_get_omp_namelist ();
      if (head == NULL)
	head = tail = p;
      else
	{
	  tail->next = p;
	  tail = tail->next;
	}
      tail->u.common = st->n.common;
      tail->where = cur_loc;

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
  gfc_free_omp_namelist (head, false, false, false, false);
  gfc_current_locus = old_loc;
  return MATCH_ERROR;
}

/* Match detach(event-handle).  */

static match
gfc_match_omp_detach (gfc_expr **expr)
{
  locus old_loc = gfc_current_locus;

  if (gfc_match ("detach ( ") != MATCH_YES)
    goto syntax_error;

  if (gfc_match_variable (expr, 0) != MATCH_YES)
    goto syntax_error;

  if (gfc_match_char (')') != MATCH_YES)
    goto syntax_error;

  return MATCH_YES;

syntax_error:
   gfc_error ("Syntax error in OpenMP detach clause at %C");
   gfc_current_locus = old_loc;
   return MATCH_ERROR;

}

/* Match doacross(sink : ...) construct a namelist from it;
   if depend is true, match legacy 'depend(sink : ...)'.  */

static match
gfc_match_omp_doacross_sink (gfc_omp_namelist **list, bool depend)
{
  char n[GFC_MAX_SYMBOL_LEN+1];
  gfc_omp_namelist *head, *tail, *p;
  locus old_loc, cur_loc;
  gfc_symbol *sym;

  head = tail = NULL;

  old_loc = gfc_current_locus;

  for (;;)
    {
      gfc_gobble_whitespace ();
      cur_loc = gfc_current_locus;

      if (gfc_match_name (n) != MATCH_YES)
	goto syntax;
      locus loc = gfc_get_location_range (NULL, 0, &cur_loc, 1,
					  &gfc_current_locus);
      if (UNLIKELY (strcmp (n, "omp_all_memory") == 0))
	{
	  gfc_error ("%<omp_all_memory%> used with dependence-type "
		     "other than OUT or INOUT at %L", &loc);
	  goto cleanup;
	}
      sym = NULL;
      if (!(strcmp (n, "omp_cur_iteration") == 0))
	{
	  gfc_symtree *st;
	  if (gfc_get_ha_sym_tree (n, &st))
	    goto syntax;
	  sym = st->n.sym;
	  gfc_set_sym_referenced (sym);
	}
      p = gfc_get_omp_namelist ();
      if (head == NULL)
	{
	  head = tail = p;
	  head->u.depend_doacross_op = (depend ? OMP_DEPEND_SINK_FIRST
					       : OMP_DOACROSS_SINK_FIRST);
	}
      else
	{
	  tail->next = p;
	  tail = tail->next;
	  tail->u.depend_doacross_op = OMP_DOACROSS_SINK;
	}
      tail->sym = sym;
      tail->expr = NULL;
      tail->where = loc;
      if (gfc_match_char ('+') == MATCH_YES)
	{
	  if (gfc_match_literal_constant (&tail->expr, 0) != MATCH_YES)
	    goto syntax;
	}
      else if (gfc_match_char ('-') == MATCH_YES)
	{
	  if (gfc_match_literal_constant (&tail->expr, 0) != MATCH_YES)
	    goto syntax;
	  tail->expr = gfc_uminus (tail->expr);
	}
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
  gfc_error ("Syntax error in OpenMP SINK dependence-type list at %C");

cleanup:
  gfc_free_omp_namelist (head, false, false, false, false);
  gfc_current_locus = old_loc;
  return MATCH_ERROR;
}

static match
match_omp_oacc_expr_list (const char *str, gfc_expr_list **list,
			  bool allow_asterisk, bool is_omp)
{
  gfc_expr_list *head, *tail, *p;
  locus old_loc;
  gfc_expr *expr;
  match m;

  head = tail = NULL;

  old_loc = gfc_current_locus;

  m = gfc_match (str);
  if (m != MATCH_YES)
    return m;

  for (;;)
    {
      m = gfc_match_expr (&expr);
      if (m == MATCH_YES || allow_asterisk)
	{
	  p = gfc_get_expr_list ();
	  if (head == NULL)
	    head = tail = p;
	  else
	    {
	      tail->next = p;
	      tail = tail->next;
	    }
	  if (m == MATCH_YES)
	    tail->expr = expr;
	  else if (gfc_match (" *") != MATCH_YES)
	    goto syntax;
	  goto next_item;
	}
      if (m == MATCH_ERROR)
	goto cleanup;
      goto syntax;

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
  if (is_omp)
    gfc_error ("Syntax error in OpenMP expression list at %C");
  else
    gfc_error ("Syntax error in OpenACC expression list at %C");

cleanup:
  gfc_free_expr_list (head);
  gfc_current_locus = old_loc;
  return MATCH_ERROR;
}

static match
match_oacc_clause_gwv (gfc_omp_clauses *cp, unsigned gwv)
{
  match ret = MATCH_YES;

  if (gfc_match (" ( ") != MATCH_YES)
    return MATCH_NO;

  if (gwv == GOMP_DIM_GANG)
    {
        /* The gang clause accepts two optional arguments, num and static.
	 The num argument may either be explicit (num: <val>) or
	 implicit without (<val> without num:).  */

      while (ret == MATCH_YES)
	{
	  if (gfc_match (" static :") == MATCH_YES)
	    {
	      if (cp->gang_static)
		return MATCH_ERROR;
	      else
		cp->gang_static = true;
	      if (gfc_match_char ('*') == MATCH_YES)
		cp->gang_static_expr = NULL;
	      else if (gfc_match (" %e ", &cp->gang_static_expr) != MATCH_YES)
		return MATCH_ERROR;
	    }
	  else
	    {
	      if (cp->gang_num_expr)
		return MATCH_ERROR;

	      /* The 'num' argument is optional.  */
	      gfc_match (" num :");

	      if (gfc_match (" %e ", &cp->gang_num_expr) != MATCH_YES)
		return MATCH_ERROR;
	    }

	  ret = gfc_match (" , ");
	}
    }
  else if (gwv == GOMP_DIM_WORKER)
    {
      /* The 'num' argument is optional.  */
      gfc_match (" num :");

      if (gfc_match (" %e ", &cp->worker_expr) != MATCH_YES)
	return MATCH_ERROR;
    }
  else if (gwv == GOMP_DIM_VECTOR)
    {
      /* The 'length' argument is optional.  */
      gfc_match (" length :");

      if (gfc_match (" %e ", &cp->vector_expr) != MATCH_YES)
	return MATCH_ERROR;
    }
  else
    gfc_fatal_error ("Unexpected OpenACC parallelism.");

  return gfc_match (" )");
}

static match
gfc_match_oacc_clause_link (const char *str, gfc_omp_namelist **list)
{
  gfc_omp_namelist *head = NULL;
  gfc_omp_namelist *tail, *p;
  locus old_loc;
  char n[GFC_MAX_SYMBOL_LEN+1];
  gfc_symbol *sym;
  match m;
  gfc_symtree *st;

  old_loc = gfc_current_locus;

  m = gfc_match (str);
  if (m != MATCH_YES)
    return m;

  m = gfc_match (" (");

  for (;;)
    {
      m = gfc_match_symbol (&sym, 0);
      switch (m)
	{
	case MATCH_YES:
	  if (sym->attr.in_common)
	    {
	      gfc_error_now ("Variable at %C is an element of a COMMON block");
	      goto cleanup;
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
	  tail->expr = NULL;
	  tail->where = gfc_current_locus;
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
	  tail->where = gfc_current_locus;
	}

    next_item:
      if (gfc_match_char (')') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after !$ACC DECLARE at %C");
      goto cleanup;
    }

  while (*list)
    list = &(*list)->next;
  *list = head;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in !$ACC DECLARE list at %C");

cleanup:
  gfc_current_locus = old_loc;
  return MATCH_ERROR;
}

/* OpenMP clauses.  */
enum omp_mask1
{
  OMP_CLAUSE_PRIVATE,
  OMP_CLAUSE_FIRSTPRIVATE,
  OMP_CLAUSE_LASTPRIVATE,
  OMP_CLAUSE_COPYPRIVATE,
  OMP_CLAUSE_SHARED,
  OMP_CLAUSE_COPYIN,
  OMP_CLAUSE_REDUCTION,
  OMP_CLAUSE_IN_REDUCTION,
  OMP_CLAUSE_TASK_REDUCTION,
  OMP_CLAUSE_IF,
  OMP_CLAUSE_NUM_THREADS,
  OMP_CLAUSE_SCHEDULE,
  OMP_CLAUSE_DEFAULT,
  OMP_CLAUSE_ORDER,
  OMP_CLAUSE_ORDERED,
  OMP_CLAUSE_COLLAPSE,
  OMP_CLAUSE_UNTIED,
  OMP_CLAUSE_FINAL,
  OMP_CLAUSE_MERGEABLE,
  OMP_CLAUSE_ALIGNED,
  OMP_CLAUSE_DEPEND,
  OMP_CLAUSE_INBRANCH,
  OMP_CLAUSE_LINEAR,
  OMP_CLAUSE_NOTINBRANCH,
  OMP_CLAUSE_PROC_BIND,
  OMP_CLAUSE_SAFELEN,
  OMP_CLAUSE_SIMDLEN,
  OMP_CLAUSE_UNIFORM,
  OMP_CLAUSE_DEVICE,
  OMP_CLAUSE_MAP,
  OMP_CLAUSE_TO,
  OMP_CLAUSE_FROM,
  OMP_CLAUSE_NUM_TEAMS,
  OMP_CLAUSE_THREAD_LIMIT,
  OMP_CLAUSE_DIST_SCHEDULE,
  OMP_CLAUSE_DEFAULTMAP,
  OMP_CLAUSE_GRAINSIZE,
  OMP_CLAUSE_HINT,
  OMP_CLAUSE_IS_DEVICE_PTR,
  OMP_CLAUSE_LINK,
  OMP_CLAUSE_NOGROUP,
  OMP_CLAUSE_NOTEMPORAL,
  OMP_CLAUSE_NUM_TASKS,
  OMP_CLAUSE_PRIORITY,
  OMP_CLAUSE_SIMD,
  OMP_CLAUSE_THREADS,
  OMP_CLAUSE_USE_DEVICE_PTR,
  OMP_CLAUSE_USE_DEVICE_ADDR,  /* OpenMP 5.0.  */
  OMP_CLAUSE_DEVICE_TYPE,  /* OpenMP 5.0.  */
  OMP_CLAUSE_ATOMIC,  /* OpenMP 5.0.  */
  OMP_CLAUSE_CAPTURE,  /* OpenMP 5.0.  */
  OMP_CLAUSE_MEMORDER,  /* OpenMP 5.0.  */
  OMP_CLAUSE_DETACH,  /* OpenMP 5.0.  */
  OMP_CLAUSE_AFFINITY,  /* OpenMP 5.0.  */
  OMP_CLAUSE_ALLOCATE,  /* OpenMP 5.0.  */
  OMP_CLAUSE_BIND,  /* OpenMP 5.0.  */
  OMP_CLAUSE_FILTER,  /* OpenMP 5.1.  */
  OMP_CLAUSE_AT,  /* OpenMP 5.1.  */
  OMP_CLAUSE_MESSAGE,  /* OpenMP 5.1.  */
  OMP_CLAUSE_SEVERITY,  /* OpenMP 5.1.  */
  OMP_CLAUSE_COMPARE,  /* OpenMP 5.1.  */
  OMP_CLAUSE_FAIL,  /* OpenMP 5.1.  */
  OMP_CLAUSE_WEAK,  /* OpenMP 5.1.  */
  OMP_CLAUSE_NOWAIT,
  /* This must come last.  */
  OMP_MASK1_LAST
};

/* More OpenMP clauses and OpenACC 2.0+ specific clauses. */
enum omp_mask2
{
  OMP_CLAUSE_ASYNC,
  OMP_CLAUSE_NUM_GANGS,
  OMP_CLAUSE_NUM_WORKERS,
  OMP_CLAUSE_VECTOR_LENGTH,
  OMP_CLAUSE_COPY,
  OMP_CLAUSE_COPYOUT,
  OMP_CLAUSE_CREATE,
  OMP_CLAUSE_NO_CREATE,
  OMP_CLAUSE_PRESENT,
  OMP_CLAUSE_DEVICEPTR,
  OMP_CLAUSE_GANG,
  OMP_CLAUSE_WORKER,
  OMP_CLAUSE_VECTOR,
  OMP_CLAUSE_SEQ,
  OMP_CLAUSE_INDEPENDENT,
  OMP_CLAUSE_USE_DEVICE,
  OMP_CLAUSE_DEVICE_RESIDENT,
  OMP_CLAUSE_SELF,
  OMP_CLAUSE_HOST,
  OMP_CLAUSE_WAIT,
  OMP_CLAUSE_DELETE,
  OMP_CLAUSE_AUTO,
  OMP_CLAUSE_TILE,
  OMP_CLAUSE_IF_PRESENT,
  OMP_CLAUSE_FINALIZE,
  OMP_CLAUSE_ATTACH,
  OMP_CLAUSE_NOHOST,
  OMP_CLAUSE_HAS_DEVICE_ADDR,  /* OpenMP 5.1  */
  OMP_CLAUSE_ENTER, /* OpenMP 5.2 */
  OMP_CLAUSE_DOACROSS, /* OpenMP 5.2 */
  OMP_CLAUSE_ASSUMPTIONS, /* OpenMP 5.1. */
  OMP_CLAUSE_USES_ALLOCATORS, /* OpenMP 5.0  */
  OMP_CLAUSE_INDIRECT, /* OpenMP 5.1  */
  OMP_CLAUSE_FULL,  /* OpenMP 5.1.  */
  OMP_CLAUSE_PARTIAL,  /* OpenMP 5.1.  */
  OMP_CLAUSE_SIZES,  /* OpenMP 5.1.  */
  OMP_CLAUSE_INIT,  /* OpenMP 5.1.  */
  OMP_CLAUSE_DESTROY,  /* OpenMP 5.1.  */
  OMP_CLAUSE_USE,  /* OpenMP 5.1.  */
  OMP_CLAUSE_NOVARIANTS, /* OpenMP 5.1  */
  OMP_CLAUSE_NOCONTEXT, /* OpenMP 5.1  */
  OMP_CLAUSE_INTEROP, /* OpenMP 5.1  */
  /* This must come last.  */
  OMP_MASK2_LAST
};

struct omp_inv_mask;

/* Customized bitset for up to 128-bits.
   The two enums above provide bit numbers to use, and which of the
   two enums it is determines which of the two mask fields is used.
   Supported operations are defining a mask, like:
   #define XXX_CLAUSES \
     (omp_mask (OMP_CLAUSE_XXX) | OMP_CLAUSE_YYY | OMP_CLAUSE_ZZZ)
   oring such bitsets together or removing selected bits:
   (XXX_CLAUSES | YYY_CLAUSES) & ~(omp_mask (OMP_CLAUSE_VVV))
   and testing individual bits:
   if (mask & OMP_CLAUSE_UUU)  */

struct omp_mask {
  const uint64_t mask1;
  const uint64_t mask2;
  inline omp_mask ();
  inline omp_mask (omp_mask1);
  inline omp_mask (omp_mask2);
  inline omp_mask (uint64_t, uint64_t);
  inline omp_mask operator| (omp_mask1) const;
  inline omp_mask operator| (omp_mask2) const;
  inline omp_mask operator| (omp_mask) const;
  inline omp_mask operator& (const omp_inv_mask &) const;
  inline bool operator& (omp_mask1) const;
  inline bool operator& (omp_mask2) const;
  inline omp_inv_mask operator~ () const;
};

struct omp_inv_mask : public omp_mask {
  inline omp_inv_mask (const omp_mask &);
};

omp_mask::omp_mask () : mask1 (0), mask2 (0)
{
}

omp_mask::omp_mask (omp_mask1 m) : mask1 (((uint64_t) 1) << m), mask2 (0)
{
}

omp_mask::omp_mask (omp_mask2 m) : mask1 (0), mask2 (((uint64_t) 1) << m)
{
}

omp_mask::omp_mask (uint64_t m1, uint64_t m2) : mask1 (m1), mask2 (m2)
{
}

omp_mask
omp_mask::operator| (omp_mask1 m) const
{
  return omp_mask (mask1 | (((uint64_t) 1) << m), mask2);
}

omp_mask
omp_mask::operator| (omp_mask2 m) const
{
  return omp_mask (mask1, mask2 | (((uint64_t) 1) << m));
}

omp_mask
omp_mask::operator| (omp_mask m) const
{
  return omp_mask (mask1 | m.mask1, mask2 | m.mask2);
}

omp_mask
omp_mask::operator& (const omp_inv_mask &m) const
{
  return omp_mask (mask1 & ~m.mask1, mask2 & ~m.mask2);
}

bool
omp_mask::operator& (omp_mask1 m) const
{
  return (mask1 & (((uint64_t) 1) << m)) != 0;
}

bool
omp_mask::operator& (omp_mask2 m) const
{
  return (mask2 & (((uint64_t) 1) << m)) != 0;
}

omp_inv_mask
omp_mask::operator~ () const
{
  return omp_inv_mask (*this);
}

omp_inv_mask::omp_inv_mask (const omp_mask &m) : omp_mask (m)
{
}

/* Helper function for OpenACC and OpenMP clauses involving memory
   mapping.  */

static bool
gfc_match_omp_map_clause (gfc_omp_namelist **list, gfc_omp_map_op map_op,
			  bool allow_common, bool allow_derived)
{
  gfc_omp_namelist **head = NULL;
  if (gfc_match_omp_variable_list ("", list, allow_common, NULL, &head, true,
				   allow_derived)
      == MATCH_YES)
    {
      gfc_omp_namelist *n;
      for (n = *head; n; n = n->next)
	n->u.map.op = map_op;
      return true;
    }

  return false;
}

static match
gfc_match_iterator (gfc_namespace **ns, bool permit_var)
{
  locus old_loc = gfc_current_locus;

  if (gfc_match ("iterator ( ") != MATCH_YES)
    return MATCH_NO;

  gfc_typespec ts;
  gfc_symbol *last = NULL;
  gfc_expr *begin, *end, *step;
  *ns = gfc_build_block_ns (gfc_current_ns);
  char name[GFC_MAX_SYMBOL_LEN + 1];
  while (true)
    {
      locus prev_loc = gfc_current_locus;
      if (gfc_match_type_spec (&ts) == MATCH_YES
	  && gfc_match (" :: ") == MATCH_YES)
	{
	  if (ts.type != BT_INTEGER)
	    {
	      gfc_error ("Expected INTEGER type at %L", &prev_loc);
	      return MATCH_ERROR;
	    }
	  permit_var = false;
	}
      else
	{
	  ts.type = BT_INTEGER;
	  ts.kind = gfc_default_integer_kind;
	  gfc_current_locus = prev_loc;
	}
      prev_loc = gfc_current_locus;
      if (gfc_match_name (name) != MATCH_YES)
	{
	  gfc_error ("Expected identifier at %C");
	  goto failed;
	}
      if (gfc_find_symtree ((*ns)->sym_root, name))
	{
	  gfc_error ("Same identifier %qs specified again at %C", name);
	  goto failed;
	}

      gfc_symbol *sym = gfc_new_symbol (name, *ns);
      if (last)
	last->tlink = sym;
      else
	(*ns)->omp_affinity_iterators = sym;
      last = sym;
      sym->declared_at = prev_loc;
      sym->ts = ts;
      sym->attr.flavor = FL_VARIABLE;
      sym->attr.artificial = 1;
      sym->attr.referenced = 1;
      sym->refs++;
      gfc_symtree *st = gfc_new_symtree (&(*ns)->sym_root, name);
      st->n.sym = sym;

      prev_loc = gfc_current_locus;
      if (gfc_match (" = ") != MATCH_YES)
	goto failed;
      permit_var = false;
      begin = end = step = NULL;
      if (gfc_match ("%e : ", &begin) != MATCH_YES
	  || gfc_match ("%e ", &end) != MATCH_YES)
	{
	  gfc_error ("Expected range-specification at %C");
	  gfc_free_expr (begin);
	  gfc_free_expr (end);
	  return MATCH_ERROR;
	}
      if (':' == gfc_peek_ascii_char ())
	{
	  if (gfc_match (": %e ", &step) != MATCH_YES)
	    {
	      gfc_free_expr (begin);
	      gfc_free_expr (end);
	      gfc_free_expr (step);
	      goto failed;
	    }
	}

      gfc_expr *e = gfc_get_expr ();
      e->where = prev_loc;
      e->expr_type = EXPR_ARRAY;
      e->ts = ts;
      e->rank = 1;
      e->shape = gfc_get_shape (1);
      mpz_init_set_ui (e->shape[0], step ? 3 : 2);
      gfc_constructor_append_expr (&e->value.constructor, begin, &begin->where);
      gfc_constructor_append_expr (&e->value.constructor, end, &end->where);
      if (step)
	gfc_constructor_append_expr (&e->value.constructor, step, &step->where);
      sym->value = e;

      if (gfc_match (") ") == MATCH_YES)
	break;
      if (gfc_match (", ") != MATCH_YES)
	goto failed;
    }
  return MATCH_YES;

failed:
  gfc_namespace *prev_ns = NULL;
  for (gfc_namespace *it = gfc_current_ns->contained; it; it = it->sibling)
    {
      if (it == *ns)
	{
	  if (prev_ns)
	    prev_ns->sibling = it->sibling;
	  else
	    gfc_current_ns->contained = it->sibling;
	  gfc_free_namespace (it);
	  break;
	}
      prev_ns = it;
    }
  *ns = NULL;
  if (!permit_var)
    return MATCH_ERROR;
  gfc_current_locus = old_loc;
  return MATCH_NO;
}

/* Match target update's to/from( [present:] var-list).  */

static match
gfc_match_motion_var_list (const char *str, gfc_omp_namelist **list,
			   gfc_omp_namelist ***headp)
{
  match m = gfc_match (str);
  if (m != MATCH_YES)
    return m;

  match m_present = gfc_match (" present : ");

  m = gfc_match_omp_variable_list ("", list, false, NULL, headp, true, true);
  if (m != MATCH_YES)
    return m;
  if (m_present == MATCH_YES)
    {
      gfc_omp_namelist *n;
      for (n = **headp; n; n = n->next)
	n->u.present_modifier = true;
    }
  return MATCH_YES;
}

/* reduction ( reduction-modifier, reduction-operator : variable-list )
   in_reduction ( reduction-operator : variable-list )
   task_reduction ( reduction-operator : variable-list )  */

static match
gfc_match_omp_clause_reduction (char pc, gfc_omp_clauses *c, bool openacc,
				bool allow_derived, bool openmp_target = false)
{
  if (pc == 'r' && gfc_match ("reduction ( ") != MATCH_YES)
    return MATCH_NO;
  else if (pc == 'i' && gfc_match ("in_reduction ( ") != MATCH_YES)
    return MATCH_NO;
  else if (pc == 't' && gfc_match ("task_reduction ( ") != MATCH_YES)
    return MATCH_NO;

  locus old_loc = gfc_current_locus;
  int list_idx = 0;

  if (pc == 'r' && !openacc)
    {
      if (gfc_match ("inscan") == MATCH_YES)
	list_idx = OMP_LIST_REDUCTION_INSCAN;
      else if (gfc_match ("task") == MATCH_YES)
	list_idx = OMP_LIST_REDUCTION_TASK;
      else if (gfc_match ("default") == MATCH_YES)
	list_idx = OMP_LIST_REDUCTION;
      if (list_idx != 0 && gfc_match (", ") != MATCH_YES)
	{
	  gfc_error ("Comma expected at %C");
	  gfc_current_locus = old_loc;
	  return MATCH_NO;
	}
      if (list_idx == 0)
	list_idx = OMP_LIST_REDUCTION;
    }
  else if (pc == 'i')
    list_idx = OMP_LIST_IN_REDUCTION;
  else if (pc == 't')
    list_idx = OMP_LIST_TASK_REDUCTION;
  else
    list_idx = OMP_LIST_REDUCTION;

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
    snprintf (buffer, sizeof buffer, "operator %s",
	      gfc_op2string ((gfc_intrinsic_op) rop));
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
  gfc_omp_udr *udr = (buffer[0] ? gfc_find_omp_udr (gfc_current_ns, buffer, NULL)
				: NULL);
  gfc_omp_namelist **head = NULL;
  if (rop == OMP_REDUCTION_NONE && udr)
    rop = OMP_REDUCTION_USER;

  if (gfc_match_omp_variable_list (" :", &c->lists[list_idx], false, NULL,
				   &head, openacc, allow_derived) != MATCH_YES)
    {
      gfc_current_locus = old_loc;
      return MATCH_NO;
    }
  gfc_omp_namelist *n;
  if (rop == OMP_REDUCTION_NONE)
    {
      n = *head;
      *head = NULL;
      gfc_error_now ("!$OMP DECLARE REDUCTION %s not found at %L",
		     buffer, &old_loc);
      gfc_free_omp_namelist (n, false, false, false, false);
    }
  else
    for (n = *head; n; n = n->next)
      {
	n->u.reduction_op = rop;
	if (udr)
	  {
	    n->u2.udr = gfc_get_omp_namelist_udr ();
	    n->u2.udr->udr = udr;
	  }
	if (openmp_target && list_idx == OMP_LIST_IN_REDUCTION)
	  {
	    gfc_omp_namelist *p = gfc_get_omp_namelist (), **tl;
	    p->sym = n->sym;
	    p->where = p->where;
	    p->u.map.op = OMP_MAP_ALWAYS_TOFROM;

	    tl = &c->lists[OMP_LIST_MAP];
	    while (*tl)
	      tl = &((*tl)->next);
	    *tl = p;
	    p->next = NULL;
	  }
     }
  return MATCH_YES;
}

static match
gfc_omp_absent_contains_clause (gfc_omp_assumptions **assume, bool is_absent)
{
  if (*assume == NULL)
    *assume = gfc_get_omp_assumptions ();
  do
    {
      gfc_statement st = ST_NONE;
      gfc_gobble_whitespace ();
      locus old_loc = gfc_current_locus;
      char c = gfc_peek_ascii_char ();
      enum gfc_omp_directive_kind kind
	= GFC_OMP_DIR_DECLARATIVE; /* Silence warning. */
      for (size_t i = 0; i < ARRAY_SIZE (gfc_omp_directives); i++)
	{
	  if (gfc_omp_directives[i].name[0] > c)
	    break;
	  if (gfc_omp_directives[i].name[0] != c)
	    continue;
	  if (gfc_match (gfc_omp_directives[i].name) == MATCH_YES)
	    {
	      st = gfc_omp_directives[i].st;
	      kind = gfc_omp_directives[i].kind;
	    }
	}
      gfc_gobble_whitespace ();
      c = gfc_peek_ascii_char ();
      if (st == ST_NONE || (c != ',' && c != ')'))
	{
	  if (st == ST_NONE)
	    gfc_error ("Unknown directive at %L", &old_loc);
	  else
	    gfc_error ("Invalid combined or composite directive at %L",
		       &old_loc);
	  return MATCH_ERROR;
	}
      if (kind == GFC_OMP_DIR_DECLARATIVE
	  || kind == GFC_OMP_DIR_INFORMATIONAL
	  || kind == GFC_OMP_DIR_META)
	{
	  gfc_error ("Invalid %qs directive at %L in %s clause: declarative, "
		     "informational, and meta directives not permitted",
		     gfc_ascii_statement (st, true), &old_loc,
		     is_absent ? "ABSENT" : "CONTAINS");
	  return MATCH_ERROR;
	}
      if (is_absent)
	{
	  /* Use exponential allocation; equivalent to pow2p(x). */
	  int i = (*assume)->n_absent;
	  int size = ((i == 0) ? 4
		      : pow2p_hwi (i) == 1 ? i*2 : 0);
	  if (size != 0)
	    (*assume)->absent = XRESIZEVEC (gfc_statement,
					    (*assume)->absent, size);
	  (*assume)->absent[(*assume)->n_absent++] = st;
	}
      else
	{
	  int i = (*assume)->n_contains;
	  int size = ((i == 0) ? 4
		      : pow2p_hwi (i) == 1 ? i*2 : 0);
	  if (size != 0)
	    (*assume)->contains = XRESIZEVEC (gfc_statement,
					      (*assume)->contains, size);
	  (*assume)->contains[(*assume)->n_contains++] = st;
	}
      gfc_gobble_whitespace ();
      if (gfc_match(",") == MATCH_YES)
	continue;
      if (gfc_match(")") == MATCH_YES)
	break;
      gfc_error ("Expected %<,%> or %<)%> at %C");
      return MATCH_ERROR;
    }
  while (true);

  return MATCH_YES;
}

/* Check 'check' argument for duplicated statements in absent and/or contains
   clauses. If 'merge', merge them from check to 'merge'.  */

static match
omp_verify_merge_absent_contains (gfc_statement st, gfc_omp_assumptions *check,
				  gfc_omp_assumptions *merge, locus *loc)
{
  if (check == NULL)
    return MATCH_YES;
  bitmap_head absent_head, contains_head;
  bitmap_obstack_initialize (NULL);
  bitmap_initialize (&absent_head, &bitmap_default_obstack);
  bitmap_initialize (&contains_head, &bitmap_default_obstack);

  match m = MATCH_YES;
  for (int i = 0; i < check->n_absent; i++)
    if (!bitmap_set_bit (&absent_head, check->absent[i]))
      {
	gfc_error ("%qs directive mentioned multiple times in %s clause in %s "
		   "directive at %L",
		   gfc_ascii_statement (check->absent[i], true),
		   "ABSENT", gfc_ascii_statement (st), loc);
	m = MATCH_ERROR;
      }
  for (int i = 0; i < check->n_contains; i++)
    {
      if (!bitmap_set_bit (&contains_head, check->contains[i]))
	{
	  gfc_error ("%qs directive mentioned multiple times in %s clause in %s "
		     "directive at %L",
		     gfc_ascii_statement (check->contains[i], true),
		     "CONTAINS", gfc_ascii_statement (st), loc);
	  m = MATCH_ERROR;
	}
      if (bitmap_bit_p (&absent_head, check->contains[i]))
	{
	  gfc_error ("%qs directive mentioned both times in ABSENT and CONTAINS "
		     "clauses in %s directive at %L",
		     gfc_ascii_statement (check->absent[i], true),
		     gfc_ascii_statement (st), loc);
	  m = MATCH_ERROR;
	}
    }

  if (m == MATCH_ERROR)
    return MATCH_ERROR;
  if (merge == NULL)
    return MATCH_YES;
  if (merge->absent == NULL && check->absent)
    {
      merge->n_absent = check->n_absent;
      merge->absent = check->absent;
      check->absent = NULL;
    }
  else if (merge->absent && check->absent)
    {
      check->absent = XRESIZEVEC (gfc_statement, check->absent,
				  merge->n_absent + check->n_absent);
      for (int i = 0; i < merge->n_absent; i++)
	if (!bitmap_bit_p (&absent_head, merge->absent[i]))
	  check->absent[check->n_absent++] = merge->absent[i];
      free (merge->absent);
      merge->absent = check->absent;
      merge->n_absent = check->n_absent;
      check->absent = NULL;
    }
  if (merge->contains == NULL && check->contains)
    {
      merge->n_contains = check->n_contains;
      merge->contains = check->contains;
      check->contains = NULL;
    }
  else if (merge->contains && check->contains)
    {
      check->contains = XRESIZEVEC (gfc_statement, check->contains,
				    merge->n_contains + check->n_contains);
      for (int i = 0; i < merge->n_contains; i++)
	if (!bitmap_bit_p (&contains_head, merge->contains[i]))
	  check->contains[check->n_contains++] = merge->contains[i];
      free (merge->contains);
      merge->contains = check->contains;
      merge->n_contains = check->n_contains;
      check->contains = NULL;
    }
  return MATCH_YES;
}

/* OpenMP 5.0
   uses_allocators ( allocator-list )

   allocator:
     predefined-allocator
     variable ( traits-array )

   OpenMP 5.2:
   uses_allocators ( [modifier-list :] allocator-list )

   allocator:
     variable or predefined-allocator
   modifier:
     traits ( traits-array )
     memspace ( mem-space-handle )  */

static match
gfc_match_omp_clause_uses_allocators (gfc_omp_clauses *c)
{
  gfc_symbol *memspace_sym = NULL;
  gfc_symbol *traits_sym = NULL;
  gfc_omp_namelist *head = NULL;
  gfc_omp_namelist *p, *tail, **list;
  int ntraits, nmemspace;
  bool has_modifiers;
  locus old_loc, cur_loc;

  gfc_gobble_whitespace ();
  old_loc = gfc_current_locus;
  ntraits = nmemspace = 0;
  do
    {
      cur_loc = gfc_current_locus;
      if (gfc_match ("traits ( %S ) ", &traits_sym) == MATCH_YES)
	ntraits++;
      else if (gfc_match ("memspace ( %S ) ", &memspace_sym) == MATCH_YES)
	nmemspace++;
      if (ntraits > 1 || nmemspace > 1)
	{
	  gfc_error ("Duplicate %s modifier at %L in USES_ALLOCATORS clause",
		     ntraits > 1 ? "TRAITS" : "MEMSPACE", &cur_loc);
	  return MATCH_ERROR;
	}
      if (gfc_match (", ") == MATCH_YES)
	continue;
      if (gfc_match (": ") != MATCH_YES)
	{
	  /* Assume no modifier. */
	  memspace_sym = traits_sym = NULL;
	  gfc_current_locus = old_loc;
	  break;
	}
      break;
    } while (true);

  has_modifiers = traits_sym != NULL || memspace_sym != NULL;
  do
    {
      p = gfc_get_omp_namelist ();
      p->where = gfc_current_locus;
      if (head == NULL)
	head = tail = p;
      else
	{
	  tail->next = p;
	  tail = tail->next;
	}
      if (gfc_match ("%S ", &p->sym) != MATCH_YES)
	goto error;
      if (!has_modifiers)
	gfc_match ("( %S ) ", &p->u2.traits_sym);
      else if (gfc_peek_ascii_char () == '(')
	{
	  gfc_error ("Unexpected %<(%> at %C");
	  goto error;
	}
      else
	{
	  p->u.memspace_sym = memspace_sym;
	  p->u2.traits_sym = traits_sym;
	}
      if (gfc_match (", ") == MATCH_YES)
	continue;
      if (gfc_match (") ") == MATCH_YES)
	break;
      goto error;
    } while (true);

  list = &c->lists[OMP_LIST_USES_ALLOCATORS];
  while (*list)
    list = &(*list)->next;
  *list = head;

  return MATCH_YES;

error:
  gfc_free_omp_namelist (head, false, false, true, false);
  return MATCH_ERROR;
}


/* Match the 'prefer_type' modifier of the interop 'init' clause:
   with either OpenMP 5.1's
     prefer_type ( <const-int-expr|string literal> [, ...]
   or
     prefer_type ( '{' <fr(...) | attr (...)>, ...] '}' [, '{' ... '}' ] )
   where 'fr' takes a constant expression or a string literal
   and 'attr takes a list of string literals, starting with 'ompx_')

   For the foreign runtime identifiers, string values are converted to
   their integer value; unknown string or integer values are set to
   GOMP_INTEROP_IFR_KNOWN.

   Data format:
    For the foreign runtime identifiers, string values are converted to
    their integer value; unknown string or integer values are set to 0.

    Each item (a) GOMP_INTEROP_IFR_SEPARATOR
	      (b) for any 'fr', its integer value.
		  Note: Spec only permits 1 'fr' entry (6.0; changed after TR13)
	      (c) GOMP_INTEROP_IFR_SEPARATOR
	      (d) list of \0-terminated non-empty strings for 'attr'
	      (e) '\0'
    Tailing '\0'.  */

static match
gfc_match_omp_prefer_type (char **type_str, int *type_str_len)
{
  gfc_expr *e;
  std::string type_string, attr_string;
  /* New syntax.  */
  if (gfc_peek_ascii_char () == '{')
    do
      {
	attr_string.clear ();
	type_string += (char) GOMP_INTEROP_IFR_SEPARATOR;
	if (gfc_match ("{ ") != MATCH_YES)
	  {
	    gfc_error ("Expected %<{%> at %C");
	    return MATCH_ERROR;
	  }
	bool fr_found = false;
	do
	  {
	    if (gfc_match ("fr ( ") == MATCH_YES)
	      {
		if (fr_found)
		  {
		    gfc_error ("Duplicated %<fr%> preference-selector-name "
			       "at %C");
		    return MATCH_ERROR;
		  }
		fr_found = true;
		do
		  {
		    bool found_literal = false;
		    match m = MATCH_YES;
		    if (gfc_match_literal_constant (&e, false) == MATCH_YES)
		      found_literal = true;
		    else
		      m = gfc_match_expr (&e);
		    if (m != MATCH_YES
			|| !gfc_resolve_expr (e)
			|| e->rank != 0
			|| e->expr_type != EXPR_CONSTANT
			|| (e->ts.type != BT_INTEGER
			    && (!found_literal || e->ts.type != BT_CHARACTER))
			|| (e->ts.type == BT_INTEGER
			    && !mpz_fits_sint_p (e->value.integer))
			|| (e->ts.type == BT_CHARACTER
			    && (e->ts.kind != gfc_default_character_kind
			|| e->value.character.length == 0)))
		      {
			gfc_error ("Expected constant scalar integer expression"
				   " or non-empty default-kind character "
				   "literal at %L", &e->where);
			gfc_free_expr (e);
			return MATCH_ERROR;
		      }
		    gfc_gobble_whitespace ();
		    int val;
		    if (e->ts.type == BT_INTEGER)
		      {
			val = mpz_get_si (e->value.integer);
			if (val < 1 || val > GOMP_INTEROP_IFR_LAST)
			  {
			    gfc_warning_now (OPT_Wopenmp,
					     "Unknown foreign runtime "
					     "identifier %qd at %L",
					     val, &e->where);
			    val = GOMP_INTEROP_IFR_UNKNOWN;
			  }
		      }
		    else
		      {
			char *str = XALLOCAVEC (char,
						e->value.character.length+1);
			for (int i = 0; i < e->value.character.length + 1; i++)
			  str[i] = e->value.character.string[i];
			if (memchr (str, '\0', e->value.character.length) != 0)
			  {
			    gfc_error ("Unexpected null character in character "
				       "literal at %L", &e->where);
			    return MATCH_ERROR;
			  }
			val = omp_get_fr_id_from_name (str);
			if (val == GOMP_INTEROP_IFR_UNKNOWN)
			  gfc_warning_now (OPT_Wopenmp,
					   "Unknown foreign runtime identifier "
					   "%qs at %L", str, &e->where);
		      }

		    type_string += (char) val;
		    if (gfc_match (") ") == MATCH_YES)
		      break;
		    gfc_error ("Expected %<)%> at %C");
		    return MATCH_ERROR;
		  }
		while (true);
	      }
	    else if (gfc_match ("attr ( ") == MATCH_YES)
	      {
		do
		  {
		    if (gfc_match_literal_constant (&e, false) != MATCH_YES
			|| !gfc_resolve_expr (e)
			|| e->expr_type != EXPR_CONSTANT
			|| e->rank != 0
			|| e->ts.type != BT_CHARACTER
			|| e->ts.kind != gfc_default_character_kind)
		      {
			gfc_error ("Expected default-kind character literal "
				   "at %L", &e->where);
			gfc_free_expr (e);
			return MATCH_ERROR;
		      }
		    gfc_gobble_whitespace ();
		    char *str = XALLOCAVEC (char, e->value.character.length+1);
		    for (int i = 0; i < e->value.character.length + 1; i++)
		      str[i] = e->value.character.string[i];
		    if (!startswith (str, "ompx_"))
		      {
			gfc_error ("Character literal at %L must start with "
				   "%<ompx_%>", &e->where);
			gfc_free_expr (e);
			return MATCH_ERROR;
		      }
		    if (memchr (str, '\0', e->value.character.length) != 0
			|| memchr (str, ',', e->value.character.length) != 0)
		      {
			gfc_error ("Unexpected null or %<,%> character in "
				   "character literal at %L", &e->where);
			return MATCH_ERROR;
		      }
		    attr_string += str;
		    attr_string += '\0';
		    if (gfc_match (", ") == MATCH_YES)
		      continue;
		    if (gfc_match (") ") == MATCH_YES)
		      break;
		    gfc_error ("Expected %<,%> or %<)%> at %C");
		    return MATCH_ERROR;
		  }
		while (true);
	      }
	    else
	      {
		gfc_error ("Expected %<fr(%> or %<attr(%> at %C");
		return MATCH_ERROR;
	      }
	    if (gfc_match (", ") == MATCH_YES)
	      continue;
	    if (gfc_match ("} ") == MATCH_YES)
	      break;
	    gfc_error ("Expected %<,%> or %<}%> at %C");
	    return MATCH_ERROR;
	  }
	while (true);
	type_string += (char) GOMP_INTEROP_IFR_SEPARATOR;
	type_string += attr_string;
	type_string += '\0';
	if (gfc_match (", ") == MATCH_YES)
	  continue;
	if (gfc_match (") ") == MATCH_YES)
	  break;
	gfc_error ("Expected %<,%> or %<)%> at %C");
	return MATCH_ERROR;
      }
    while (true);
  else
    do
      {
	type_string += (char) GOMP_INTEROP_IFR_SEPARATOR;
	bool found_literal = false;
	match m = MATCH_YES;
	if (gfc_match_literal_constant (&e, false) == MATCH_YES)
	  found_literal = true;
	else
	  m = gfc_match_expr (&e);
	if (m != MATCH_YES
	    || !gfc_resolve_expr (e)
	    || e->rank != 0
	    || e->expr_type != EXPR_CONSTANT
	    || (e->ts.type != BT_INTEGER
		&& (!found_literal || e->ts.type != BT_CHARACTER))
	    || (e->ts.type == BT_INTEGER
		&& !mpz_fits_sint_p (e->value.integer))
	    || (e->ts.type == BT_CHARACTER
		&& (e->ts.kind != gfc_default_character_kind
		    || e->value.character.length == 0)))
	  {
	    gfc_error ("Expected constant scalar integer expression or "
		       "non-empty default-kind character literal at %L", &e->where);
	    gfc_free_expr (e);
	    return MATCH_ERROR;
	  }
	gfc_gobble_whitespace ();
	int val;
	if (e->ts.type == BT_INTEGER)
	  {
	    val = mpz_get_si (e->value.integer);
	    if (val < 1 || val > GOMP_INTEROP_IFR_LAST)
	      {
		gfc_warning_now (OPT_Wopenmp,
				 "Unknown foreign runtime identifier %qd at %L",
				 val, &e->where);
		val = 0;
	      }
	  }
	else
	  {
	    char *str = XALLOCAVEC (char, e->value.character.length+1);
	    for (int i = 0; i < e->value.character.length + 1; i++)
	      str[i] = e->value.character.string[i];
	    if (memchr (str, '\0', e->value.character.length) != 0)
	      {
		gfc_error ("Unexpected null character in character "
			   "literal at %L", &e->where);
		return MATCH_ERROR;
	      }
	    val = omp_get_fr_id_from_name (str);
	    if (val == GOMP_INTEROP_IFR_UNKNOWN)
	      gfc_warning_now (OPT_Wopenmp,
			       "Unknown foreign runtime identifier %qs at %L",
			       str, &e->where);
	  }
	type_string += (char) val;
	type_string += (char) GOMP_INTEROP_IFR_SEPARATOR;
	type_string += '\0';
	gfc_free_expr (e);
	if (gfc_match (", ") == MATCH_YES)
	  continue;
	if (gfc_match (") ") == MATCH_YES)
	  break;
	gfc_error ("Expected %<,%> or %<)%> at %C");
	return MATCH_ERROR;
      }
    while (true);
  type_string += '\0';
  *type_str_len = type_string.length();
  *type_str = XNEWVEC (char, type_string.length ());
  memcpy (*type_str, type_string.data (), type_string.length ());
  return MATCH_YES;
}


/* Match OpenMP 5.1's 'init'-clause modifiers, used by the 'init' clause of
   the 'interop' directive and the 'append_args' directive of 'declare variant'.
     [prefer_type(...)][,][<target|targetsync>, ...])

   If is_init_clause, there might be no modifiers but variables like 'target';
   additionally, the modifier parsing ends with a ':'.
   If not is_init_clause (i.e. append_args), there must be modifiers and the
   parsing ends with ')'.  */

static match
gfc_parser_omp_clause_init_modifiers (bool &target, bool &targetsync,
				      char **type_str, int &type_str_len,
				      bool is_init_clause)
{
  target = false;
  targetsync = false;
  *type_str = NULL;
  type_str_len = 0;
  match m;
  locus old_loc = gfc_current_locus;
  do {
       if (gfc_match ("prefer_type ( ") == MATCH_YES)
	{
	  if (*type_str)
	    {
	      gfc_error ("Duplicate %<prefer_type%> modifier at %C");
	      return MATCH_ERROR;
	    }
	  m = gfc_match_omp_prefer_type (type_str, &type_str_len);
	  if (m != MATCH_YES)
	    return m;
	  if (gfc_match (", ") == MATCH_YES)
	    continue;
	  if (is_init_clause)
	    {
	      if (gfc_match (": ") == MATCH_YES)
		break;
	      gfc_error ("Expected %<,%> or %<:%> at %C");
	    }
	  else
	    {
	      if (gfc_match (") ") == MATCH_YES)
		break;
	      gfc_error ("Expected %<,%> or %<)%> at %C");
	    }
	  return MATCH_ERROR;
	}
       if (gfc_match ("targetsync ") == MATCH_YES)
	{
	  if (targetsync)
	    {
	      /* Avoid the word 'modifier' as it could be also be no clauses and
		 twice a variable named 'targetsync', which is also invalid.  */
	      gfc_error ("Duplicate %<targetsync%> at %C");
	      return MATCH_ERROR;
	    }
	  targetsync = true;
	  if (gfc_match (", ") == MATCH_YES)
	    continue;
	  if (!is_init_clause)
	    {
	      if (gfc_match (") ") == MATCH_YES)
		break;
	      gfc_error ("Expected %<,%> or %<)%> at %C");
	      return MATCH_ERROR;
	    }
	  if (gfc_match (": ") == MATCH_YES)
	    break;
	  gfc_char_t c = gfc_peek_char ();
	  if (!*type_str && (c == ')' || (gfc_current_form != FORM_FREE
					  && (c == '_' || ISALPHA (c)))))
	    {
	      gfc_current_locus = old_loc;
	      break;
	    }
	  gfc_error ("Expected %<,%> or %<:%> at %C");
	  return MATCH_ERROR;
	}
      if (gfc_match ("target ") == MATCH_YES)
	{
	  if (target)
	    {
	      gfc_error ("Duplicate %<target%> at %C");
	      return MATCH_ERROR;
	    }
	  target = true;
	  if (gfc_match (", ") == MATCH_YES)
	    continue;
	  if (!is_init_clause)
	    {
	      if (gfc_match (") ") == MATCH_YES)
		break;
	      gfc_error ("Expected %<,%> or %<)%> at %C");
	      return MATCH_ERROR;
	    }
	  if (gfc_match (": ") == MATCH_YES)
	    break;
	  gfc_char_t c = gfc_peek_char ();
	  if (!*type_str && (c == ')' || (gfc_current_form != FORM_FREE
					  && (c == '_' || ISALPHA (c)))))
	    {
	      gfc_current_locus = old_loc;
	      break;
	    }
	  gfc_error ("Expected %<,%> or %<:%> at %C");
	  return MATCH_ERROR;
	}
      if (*type_str)
	{
	  gfc_error ("Expected %<target%> or %<targetsync%> at %C");
	  return MATCH_ERROR;
	}
      gfc_current_locus = old_loc;
      break;
    }
  while (true);
  return MATCH_YES;
}

/* Match OpenMP 5.1's 'init' clause for 'interop' objects:
   init([prefer_type(...)][,][<target|targetsync>, ...] :] interop-obj-list)  */

static match
gfc_match_omp_init (gfc_omp_namelist **list)
{
  bool target, targetsync;
  char *type_str = NULL;
  int type_str_len;
  if (gfc_parser_omp_clause_init_modifiers (target, targetsync, &type_str,
					    type_str_len, true) == MATCH_ERROR)
    return MATCH_ERROR;

 gfc_omp_namelist **head = NULL;
 if (gfc_match_omp_variable_list ("", list, false, NULL, &head) != MATCH_YES)
   return MATCH_ERROR;
 for (gfc_omp_namelist *n = *head; n; n = n->next)
   {
     n->u.init.target = target;
     n->u.init.targetsync = targetsync;
     n->u.init.len = type_str_len;
     n->u2.init_interop = type_str;
   }
 return MATCH_YES;
}


/* Match with duplicate check. Matches 'name'. If expr != NULL, it
   then matches '(expr)', otherwise, if open_parens is true,
   it matches a ' ( ' after 'name'.
   dupl_message requires '%qs %L' - and is used by
   gfc_match_dupl_memorder and gfc_match_dupl_atomic.  */

static match
gfc_match_dupl_check (bool not_dupl, const char *name, bool open_parens = false,
		      gfc_expr **expr = NULL, const char *dupl_msg = NULL)
{
  match m;
  locus old_loc = gfc_current_locus;
  if ((m = gfc_match (name)) != MATCH_YES)
    return m;
  if (!not_dupl)
    {
      if (dupl_msg)
	gfc_error (dupl_msg, name, &old_loc);
      else
	gfc_error ("Duplicated %qs clause at %L", name, &old_loc);
      return MATCH_ERROR;
    }
  if (open_parens || expr)
    {
      if (gfc_match (" ( ") != MATCH_YES)
	{
	  gfc_error ("Expected %<(%> after %qs at %C", name);
	  return MATCH_ERROR;
	}
      if (expr)
	{
	  if (gfc_match ("%e )", expr) != MATCH_YES)
	    {
	      gfc_error ("Invalid expression after %<%s(%> at %C", name);
	      return MATCH_ERROR;
	    }
	}
    }
  return MATCH_YES;
}

static match
gfc_match_dupl_memorder (bool not_dupl, const char *name)
{
  return gfc_match_dupl_check (not_dupl, name, false, NULL,
			       "Duplicated memory-order clause: unexpected %s "
			       "clause at %L");
}

static match
gfc_match_dupl_atomic (bool not_dupl, const char *name)
{
  return gfc_match_dupl_check (not_dupl, name, false, NULL,
			       "Duplicated atomic clause: unexpected %s "
			       "clause at %L");
}

/* Match OpenMP and OpenACC directive clauses. MASK is a bitmask of
   clauses that are allowed for a particular directive.  */

static match
gfc_match_omp_clauses (gfc_omp_clauses **cp, const omp_mask mask,
		       bool first = true, bool needs_space = true,
		       bool openacc = false, bool openmp_target = false)
{
  bool error = false;
  gfc_omp_clauses *c = gfc_get_omp_clauses ();
  locus old_loc;
  /* Determine whether we're dealing with an OpenACC directive that permits
     derived type member accesses.  This in particular disallows
     "!$acc declare" from using such accesses, because it's not clear if/how
     that should work.  */
  bool allow_derived = (openacc
			&& ((mask & OMP_CLAUSE_ATTACH)
			    || (mask & OMP_CLAUSE_DETACH)));

  gcc_checking_assert (OMP_MASK1_LAST <= 64 && OMP_MASK2_LAST <= 64);
  *cp = NULL;
  while (1)
    {
      match m = MATCH_NO;
      if ((first || (m = gfc_match_char (',')) != MATCH_YES)
	  && (needs_space && gfc_match_space () != MATCH_YES))
	break;
      needs_space = false;
      first = false;
      gfc_gobble_whitespace ();
      bool end_colon;
      gfc_omp_namelist **head;
      old_loc = gfc_current_locus;
      char pc = gfc_peek_ascii_char ();
      if (pc == '\n' && m == MATCH_YES)
	{
	  gfc_error ("Clause expected at %C after trailing comma");
	  goto error;
	}
      switch (pc)
	{
	case 'a':
	  end_colon = false;
	  head = NULL;
	  if ((mask & OMP_CLAUSE_ASSUMPTIONS)
	      && gfc_match ("absent ( ") == MATCH_YES)
	    {
	      if (gfc_omp_absent_contains_clause (&c->assume, true)
		  != MATCH_YES)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_ALIGNED)
	      && gfc_match_omp_variable_list ("aligned (",
					      &c->lists[OMP_LIST_ALIGNED],
					      false, &end_colon,
					      &head) == MATCH_YES)
	    {
	      gfc_expr *alignment = NULL;
	      gfc_omp_namelist *n;

	      if (end_colon && gfc_match (" %e )", &alignment) != MATCH_YES)
		{
		  gfc_free_omp_namelist (*head, false, false, false, false);
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
	  if ((mask & OMP_CLAUSE_MEMORDER)
	      && (m = gfc_match_dupl_memorder ((c->memorder
						== OMP_MEMORDER_UNSET),
					       "acq_rel")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->memorder = OMP_MEMORDER_ACQ_REL;
	      needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_MEMORDER)
	      && (m = gfc_match_dupl_memorder ((c->memorder
						== OMP_MEMORDER_UNSET),
					       "acquire")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->memorder = OMP_MEMORDER_ACQUIRE;
	      needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_AFFINITY)
	      && gfc_match ("affinity ( ") == MATCH_YES)
	    {
	      gfc_namespace *ns_iter = NULL, *ns_curr = gfc_current_ns;
	      m = gfc_match_iterator (&ns_iter, true);
	      if (m == MATCH_ERROR)
		break;
	      if (m == MATCH_YES && gfc_match (" : ") != MATCH_YES)
		{
		  gfc_error ("Expected %<:%> at %C");
		  break;
		}
	      if (ns_iter)
		gfc_current_ns = ns_iter;
	      head = NULL;
	      m = gfc_match_omp_variable_list ("", &c->lists[OMP_LIST_AFFINITY],
					       false, NULL, &head, true);
	      gfc_current_ns = ns_curr;
	      if (m == MATCH_ERROR)
		break;
	      if (ns_iter)
		{
		  for (gfc_omp_namelist *n = *head; n; n = n->next)
		    {
		      n->u2.ns = ns_iter;
		      ns_iter->refs++;
		    }
		}
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_ALLOCATE)
	      && gfc_match ("allocate ( ") == MATCH_YES)
	    {
	      gfc_expr *allocator = NULL;
	      gfc_expr *align = NULL;
	      old_loc = gfc_current_locus;
	      if ((m = gfc_match ("allocator ( %e )", &allocator)) == MATCH_YES)
		gfc_match (" , align ( %e )", &align);
	      else if ((m = gfc_match ("align ( %e )", &align)) == MATCH_YES)
		gfc_match (" , allocator ( %e )", &allocator);

	      if (m == MATCH_YES)
		{
		  if (gfc_match (" : ") != MATCH_YES)
		    {
		      gfc_error ("Expected %<:%> at %C");
		      goto error;
		    }
		}
	      else
		{
		  m = gfc_match_expr (&allocator);
		  if (m == MATCH_YES && gfc_match (" : ") != MATCH_YES)
		    {
		       /* If no ":" then there is no allocator, we backtrack
			  and read the variable list.  */
		      gfc_free_expr (allocator);
		      allocator = NULL;
		      gfc_current_locus = old_loc;
		    }
		}
	      gfc_omp_namelist **head = NULL;
	      m = gfc_match_omp_variable_list ("", &c->lists[OMP_LIST_ALLOCATE],
					       true, NULL, &head);

	      if (m != MATCH_YES)
		{
		  gfc_free_expr (allocator);
		  gfc_free_expr (align);
		  gfc_error ("Expected variable list at %C");
		  goto error;
		}

	      for (gfc_omp_namelist *n = *head; n; n = n->next)
		{
		  n->u2.allocator = allocator;
		  n->u.align = (align) ? gfc_copy_expr (align) : NULL;
		}
	      gfc_free_expr (align);
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_AT)
	      && (m = gfc_match_dupl_check (c->at == OMP_AT_UNSET, "at", true))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (gfc_match ("compilation )") == MATCH_YES)
		c->at = OMP_AT_COMPILATION;
	      else if (gfc_match ("execution )") == MATCH_YES)
		c->at = OMP_AT_EXECUTION;
	      else
		{
		  gfc_error ("Expected COMPILATION or EXECUTION in AT clause "
			     "at %C");
		  goto error;
		}
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_ASYNC)
	      && (m = gfc_match_dupl_check (!c->async, "async")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->async = true;
	      m = gfc_match (" ( %e )", &c->async_expr);
	      if (m == MATCH_ERROR)
		{
		  gfc_current_locus = old_loc;
		  break;
		}
	      else if (m == MATCH_NO)
		{
		  c->async_expr
		    = gfc_get_constant_expr (BT_INTEGER,
					     gfc_default_integer_kind,
					     &gfc_current_locus);
		  mpz_set_si (c->async_expr->value.integer, GOMP_ASYNC_NOVAL);
		  needs_space = true;
		}
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_AUTO)
	      && (m = gfc_match_dupl_check (!c->par_auto, "auto"))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->par_auto = true;
	      needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_ATTACH)
	      && gfc_match ("attach ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_ATTACH, false,
					   allow_derived))
	    continue;
	  break;
	case 'b':
	  if ((mask & OMP_CLAUSE_BIND)
	      && (m = gfc_match_dupl_check (c->bind == OMP_BIND_UNSET, "bind",
					    true)) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (gfc_match ("teams )") == MATCH_YES)
		c->bind = OMP_BIND_TEAMS;
	      else if (gfc_match ("parallel )") == MATCH_YES)
		c->bind = OMP_BIND_PARALLEL;
	      else if (gfc_match ("thread )") == MATCH_YES)
		c->bind = OMP_BIND_THREAD;
	      else
		{
		  gfc_error ("Expected TEAMS, PARALLEL or THREAD as binding in "
			     "BIND at %C");
		  break;
		}
	      continue;
	    }
	  break;
	case 'c':
	  if ((mask & OMP_CLAUSE_CAPTURE)
	      && (m = gfc_match_dupl_check (!c->capture, "capture"))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->capture = true;
	      needs_space = true;
	      continue;
	    }
	  if (mask & OMP_CLAUSE_COLLAPSE)
	    {
	      gfc_expr *cexpr = NULL;
	      if ((m = gfc_match_dupl_check (!c->collapse, "collapse", true,
					     &cexpr)) != MATCH_NO)
	      {
		int collapse;
		if (m == MATCH_ERROR)
		  goto error;
		if (gfc_extract_int (cexpr, &collapse, -1))
		  collapse = 1;
		else if (collapse <= 0)
		  {
		    gfc_error_now ("COLLAPSE clause argument not constant "
				   "positive integer at %C");
		    collapse = 1;
		  }
		gfc_free_expr (cexpr);
		c->collapse = collapse;
		continue;
	      }
	    }
	  if ((mask & OMP_CLAUSE_COMPARE)
	      && (m = gfc_match_dupl_check (!c->compare, "compare"))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->compare = true;
	      needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_ASSUMPTIONS)
	      && gfc_match ("contains ( ") == MATCH_YES)
	    {
	      if (gfc_omp_absent_contains_clause (&c->assume, false)
		  != MATCH_YES)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_COPY)
	      && gfc_match ("copy ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_TOFROM, true,
					   allow_derived))
	    continue;
	  if (mask & OMP_CLAUSE_COPYIN)
	    {
	      if (openacc)
		{
		  if (gfc_match ("copyin ( ") == MATCH_YES)
		    {
		      bool readonly = gfc_match ("readonly : ") == MATCH_YES;
		      head = NULL;
		      if (gfc_match_omp_variable_list ("",
						       &c->lists[OMP_LIST_MAP],
						       true, NULL, &head, true,
						       allow_derived)
			  == MATCH_YES)
			{
			  gfc_omp_namelist *n;
			  for (n = *head; n; n = n->next)
			    {
			      n->u.map.op = OMP_MAP_TO;
			      n->u.map.readonly = readonly;
			    }
			  continue;
			}
		    }
		}
	      else if (gfc_match_omp_variable_list ("copyin (",
						    &c->lists[OMP_LIST_COPYIN],
						    true) == MATCH_YES)
		continue;
	    }
	  if ((mask & OMP_CLAUSE_COPYOUT)
	      && gfc_match ("copyout ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_FROM, true, allow_derived))
	    continue;
	  if ((mask & OMP_CLAUSE_COPYPRIVATE)
	      && gfc_match_omp_variable_list ("copyprivate (",
					      &c->lists[OMP_LIST_COPYPRIVATE],
					      true) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_CREATE)
	      && gfc_match ("create ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_ALLOC, true, allow_derived))
	    continue;
	  break;
	case 'd':
	  if ((mask & OMP_CLAUSE_DEFAULTMAP)
	      && gfc_match ("defaultmap ( ") == MATCH_YES)
	    {
	      enum gfc_omp_defaultmap behavior;
	      gfc_omp_defaultmap_category category
		= OMP_DEFAULTMAP_CAT_UNCATEGORIZED;
	      if (gfc_match ("alloc ") == MATCH_YES)
		behavior = OMP_DEFAULTMAP_ALLOC;
	      else if (gfc_match ("tofrom ") == MATCH_YES)
		behavior = OMP_DEFAULTMAP_TOFROM;
	      else if (gfc_match ("to ") == MATCH_YES)
		behavior = OMP_DEFAULTMAP_TO;
	      else if (gfc_match ("from ") == MATCH_YES)
		behavior = OMP_DEFAULTMAP_FROM;
	      else if (gfc_match ("firstprivate ") == MATCH_YES)
		behavior = OMP_DEFAULTMAP_FIRSTPRIVATE;
	      else if (gfc_match ("present ") == MATCH_YES)
		behavior = OMP_DEFAULTMAP_PRESENT;
	      else if (gfc_match ("none ") == MATCH_YES)
		behavior = OMP_DEFAULTMAP_NONE;
	      else if (gfc_match ("default ") == MATCH_YES)
		behavior = OMP_DEFAULTMAP_DEFAULT;
	      else
		{
		  gfc_error ("Expected ALLOC, TO, FROM, TOFROM, FIRSTPRIVATE, "
			     "PRESENT, NONE or DEFAULT at %C");
		  break;
		}
	      if (')' == gfc_peek_ascii_char ())
		;
	      else if (gfc_match (": ") != MATCH_YES)
		break;
	      else
		{
		  if (gfc_match ("scalar ") == MATCH_YES)
		    category = OMP_DEFAULTMAP_CAT_SCALAR;
		  else if (gfc_match ("aggregate ") == MATCH_YES)
		    category = OMP_DEFAULTMAP_CAT_AGGREGATE;
		  else if (gfc_match ("allocatable ") == MATCH_YES)
		    category = OMP_DEFAULTMAP_CAT_ALLOCATABLE;
		  else if (gfc_match ("pointer ") == MATCH_YES)
		    category = OMP_DEFAULTMAP_CAT_POINTER;
		  else if (gfc_match ("all ") == MATCH_YES)
		    category = OMP_DEFAULTMAP_CAT_ALL;
		  else
		    {
		      gfc_error ("Expected SCALAR, AGGREGATE, ALLOCATABLE, "
				 "POINTER or ALL at %C");
		      break;
		    }
		}
	      for (int i = 0; i < OMP_DEFAULTMAP_CAT_NUM; ++i)
		{
		  if (i != category
		      && category != OMP_DEFAULTMAP_CAT_UNCATEGORIZED
		      && category != OMP_DEFAULTMAP_CAT_ALL
		      && i != OMP_DEFAULTMAP_CAT_UNCATEGORIZED
		      && i != OMP_DEFAULTMAP_CAT_ALL)
		    continue;
		  if (c->defaultmap[i] != OMP_DEFAULTMAP_UNSET)
		    {
		      const char *pcategory = NULL;
		      switch (i)
			{
			case OMP_DEFAULTMAP_CAT_UNCATEGORIZED: break;
			case OMP_DEFAULTMAP_CAT_ALL: pcategory = "ALL"; break;
			case OMP_DEFAULTMAP_CAT_SCALAR: pcategory = "SCALAR"; break;
			case OMP_DEFAULTMAP_CAT_AGGREGATE:
			  pcategory = "AGGREGATE";
			  break;
			case OMP_DEFAULTMAP_CAT_ALLOCATABLE:
			  pcategory = "ALLOCATABLE";
			  break;
			case OMP_DEFAULTMAP_CAT_POINTER:
			  pcategory = "POINTER";
			  break;
			default: gcc_unreachable ();
			}
		     if (i == OMP_DEFAULTMAP_CAT_UNCATEGORIZED)
		      gfc_error ("DEFAULTMAP at %C but prior DEFAULTMAP with "
				 "unspecified category");
		     else
		      gfc_error ("DEFAULTMAP at %C but prior DEFAULTMAP for "
				 "category %s", pcategory);
		     goto error;
		    }
		}
	      c->defaultmap[category] = behavior;
	      if (gfc_match (")") != MATCH_YES)
		break;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_DEFAULT)
	      && (m = gfc_match_dupl_check (c->default_sharing
					    == OMP_DEFAULT_UNKNOWN, "default",
					    true)) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (gfc_match ("none") == MATCH_YES)
		c->default_sharing = OMP_DEFAULT_NONE;
	      else if (openacc)
		{
		  if (gfc_match ("present") == MATCH_YES)
		    c->default_sharing = OMP_DEFAULT_PRESENT;
		}
	      else
		{
		  if (gfc_match ("firstprivate") == MATCH_YES)
		    c->default_sharing = OMP_DEFAULT_FIRSTPRIVATE;
		  else if (gfc_match ("private") == MATCH_YES)
		    c->default_sharing = OMP_DEFAULT_PRIVATE;
		  else if (gfc_match ("shared") == MATCH_YES)
		    c->default_sharing = OMP_DEFAULT_SHARED;
		}
	      if (c->default_sharing == OMP_DEFAULT_UNKNOWN)
		{
		  if (openacc)
		    gfc_error ("Expected NONE or PRESENT in DEFAULT clause "
			       "at %C");
		  else
		    gfc_error ("Expected NONE, FIRSTPRIVATE, PRIVATE or SHARED "
			       "in DEFAULT clause at %C");
		  goto error;
		}
	      if (gfc_match (" )") != MATCH_YES)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_DELETE)
	      && gfc_match ("delete ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_RELEASE, true,
					   allow_derived))
	    continue;
	  /* DOACROSS: match 'doacross' and 'depend' with sink/source.
	     DEPEND: match 'depend' but not sink/source.  */
	  m = MATCH_NO;
	  if (((mask & OMP_CLAUSE_DOACROSS)
	       && gfc_match ("doacross ( ") == MATCH_YES)
	      || (((mask & OMP_CLAUSE_DEPEND) || (mask & OMP_CLAUSE_DOACROSS))
		  && (m = gfc_match ("depend ( ")) == MATCH_YES))
	    {
	      bool has_omp_all_memory;
	      bool is_depend = m == MATCH_YES;
	      gfc_namespace *ns_iter = NULL, *ns_curr = gfc_current_ns;
	      match m_it = MATCH_NO;
	      if (is_depend)
		m_it = gfc_match_iterator (&ns_iter, false);
	      if (m_it == MATCH_ERROR)
		break;
	      if (m_it == MATCH_YES && gfc_match (" , ") != MATCH_YES)
		break;
	      m = MATCH_YES;
	      gfc_omp_depend_doacross_op depend_op = OMP_DEPEND_OUT;
	      if (gfc_match ("inoutset") == MATCH_YES)
		depend_op = OMP_DEPEND_INOUTSET;
	      else if (gfc_match ("inout") == MATCH_YES)
		depend_op = OMP_DEPEND_INOUT;
	      else if (gfc_match ("in") == MATCH_YES)
		depend_op = OMP_DEPEND_IN;
	      else if (gfc_match ("out") == MATCH_YES)
		depend_op = OMP_DEPEND_OUT;
	      else if (gfc_match ("mutexinoutset") == MATCH_YES)
		depend_op = OMP_DEPEND_MUTEXINOUTSET;
	      else if (gfc_match ("depobj") == MATCH_YES)
		depend_op = OMP_DEPEND_DEPOBJ;
	      else if (gfc_match ("source") == MATCH_YES)
		{
		  if (m_it == MATCH_YES)
		    {
		      gfc_error ("ITERATOR may not be combined with SOURCE "
				 "at %C");
		      goto error;
		    }
		  if (!(mask & OMP_CLAUSE_DOACROSS))
		    {
		      gfc_error ("SOURCE at %C not permitted as dependence-type"
				 " for this directive");
		      goto error;
		    }
		  if (c->doacross_source)
		    {
		      gfc_error ("Duplicated clause with SOURCE dependence-type"
				 " at %C");
		      goto error;
		    }
		  gfc_gobble_whitespace ();
		  m = gfc_match (": ");
		  if (m != MATCH_YES && !is_depend)
		    {
		      gfc_error ("Expected %<:%> at %C");
		      goto error;
		    }
		  if (gfc_match (")") != MATCH_YES
		      && !(m == MATCH_YES
			   && gfc_match ("omp_cur_iteration )") == MATCH_YES))
		    {
		      gfc_error ("Expected %<)%> or %<omp_cur_iteration)%> "
				 "at %C");
		      goto error;
		    }
		  c->doacross_source = true;
		  c->depend_source = is_depend;
		  continue;
		}
	      else if (gfc_match ("sink ") == MATCH_YES)
		{
		  if (!(mask & OMP_CLAUSE_DOACROSS))
		    {
		      gfc_error ("SINK at %C not permitted as dependence-type "
				 "for this directive");
		      goto error;
		    }
		  if (gfc_match (": ") != MATCH_YES)
		    {
		      gfc_error ("Expected %<:%> at %C");
		      goto error;
		    }
		  if (m_it == MATCH_YES)
		    {
		      gfc_error ("ITERATOR may not be combined with SINK "
				 "at %C");
		      goto error;
		    }
		  m = gfc_match_omp_doacross_sink (&c->lists[OMP_LIST_DEPEND],
						   is_depend);
		  if (m == MATCH_YES)
		    continue;
		  goto error;
		}
	      else
		m = MATCH_NO;
	      if (!(mask & OMP_CLAUSE_DEPEND))
		{
		  gfc_error ("Expected dependence-type SINK or SOURCE at %C");
		  goto error;
		}
	      head = NULL;
	      if (ns_iter)
		gfc_current_ns = ns_iter;
	      if (m == MATCH_YES)
		m = gfc_match_omp_variable_list (" : ",
						 &c->lists[OMP_LIST_DEPEND],
						 false, NULL, &head, true,
						 false, &has_omp_all_memory);
	      if (m != MATCH_YES)
		goto error;
	      gfc_current_ns = ns_curr;
	      if (has_omp_all_memory && depend_op != OMP_DEPEND_INOUT
		  && depend_op != OMP_DEPEND_OUT)
		{
		  gfc_error ("%<omp_all_memory%> used with DEPEND kind "
			     "other than OUT or INOUT at %C");
		  goto error;
		}
	      gfc_omp_namelist *n;
	      for (n = *head; n; n = n->next)
		{
		  n->u.depend_doacross_op = depend_op;
		  n->u2.ns = ns_iter;
		  if (ns_iter)
		    ns_iter->refs++;
		}
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_DESTROY)
	      && gfc_match_omp_variable_list ("destroy (",
					      &c->lists[OMP_LIST_DESTROY],
					      true) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_DETACH)
	      && !openacc
	      && !c->detach
	      && gfc_match_omp_detach (&c->detach) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_DETACH)
	      && openacc
	      && gfc_match ("detach ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_DETACH, false,
					   allow_derived))
	    continue;
	  if ((mask & OMP_CLAUSE_DEVICE)
	      && !openacc
	      && ((m = gfc_match_dupl_check (!c->device, "device", true))
		  != MATCH_NO))
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->ancestor = false;
	      if (gfc_match ("device_num : ") == MATCH_YES)
		{
		  if (gfc_match ("%e )", &c->device) != MATCH_YES)
		    {
		      gfc_error ("Expected integer expression at %C");
		      break;
		    }
		}
	      else if (gfc_match ("ancestor : ") == MATCH_YES)
		{
		  bool has_requires = false;
		  c->ancestor = true;
		  for (gfc_namespace *ns = gfc_current_ns; ns; ns = ns->parent)
		    if (ns->omp_requires & OMP_REQ_REVERSE_OFFLOAD)
		      {
			has_requires = true;
			break;
		      }
		  if (!has_requires)
		    {
		      gfc_error ("%<ancestor%> device modifier not "
				 "preceded by %<requires%> directive "
				 "with %<reverse_offload%> clause at %C");
		      break;
		    }
		  locus old_loc2 = gfc_current_locus;
		  if (gfc_match ("%e )", &c->device) == MATCH_YES)
		    {
		      int device = 0;
		      if (!gfc_extract_int (c->device, &device) && device != 1)
		      {
			gfc_current_locus = old_loc2;
			gfc_error ("the %<device%> clause expression must "
				   "evaluate to %<1%> at %C");
			break;
		      }
		    }
		  else
		    {
		      gfc_error ("Expected integer expression at %C");
		      break;
		    }
		}
	      else if (gfc_match ("%e )", &c->device) != MATCH_YES)
		{
		  gfc_error ("Expected integer expression or a single device-"
			      "modifier %<device_num%> or %<ancestor%> at %C");
		  break;
		}
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_DEVICE)
	      && openacc
	      && gfc_match ("device ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_FORCE_TO, true,
					   /* allow_derived = */ true))
	    continue;
	  if ((mask & OMP_CLAUSE_DEVICEPTR)
	      && gfc_match ("deviceptr ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_FORCE_DEVICEPTR, false,
					   allow_derived))
	    continue;
	  if ((mask & OMP_CLAUSE_DEVICE_TYPE)
	      && gfc_match ("device_type ( ") == MATCH_YES)
	    {
	      if (gfc_match ("host") == MATCH_YES)
		c->device_type = OMP_DEVICE_TYPE_HOST;
	      else if (gfc_match ("nohost") == MATCH_YES)
		c->device_type = OMP_DEVICE_TYPE_NOHOST;
	      else if (gfc_match ("any") == MATCH_YES)
		c->device_type = OMP_DEVICE_TYPE_ANY;
	      else
		{
		  gfc_error ("Expected HOST, NOHOST or ANY at %C");
		  break;
		}
	      if (gfc_match (" )") != MATCH_YES)
		break;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_DEVICE_RESIDENT)
	      && gfc_match_omp_variable_list
		   ("device_resident (",
		    &c->lists[OMP_LIST_DEVICE_RESIDENT], true) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_DIST_SCHEDULE)
	      && c->dist_sched_kind == OMP_SCHED_NONE
	      && gfc_match ("dist_schedule ( static") == MATCH_YES)
	    {
	      m = MATCH_NO;
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
	  break;
	case 'e':
	  if ((mask & OMP_CLAUSE_ENTER))
	    {
	      m = gfc_match_omp_to_link ("enter (", &c->lists[OMP_LIST_ENTER]);
	      if (m == MATCH_ERROR)
		goto error;
	      if (m == MATCH_YES)
		continue;
	    }
	  break;
	case 'f':
	  if ((mask & OMP_CLAUSE_FAIL)
	      && (m = gfc_match_dupl_check (c->fail == OMP_MEMORDER_UNSET,
					    "fail", true)) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (gfc_match ("seq_cst") == MATCH_YES)
		c->fail = OMP_MEMORDER_SEQ_CST;
	      else if (gfc_match ("acquire") == MATCH_YES)
		c->fail = OMP_MEMORDER_ACQUIRE;
	      else if (gfc_match ("relaxed") == MATCH_YES)
		c->fail = OMP_MEMORDER_RELAXED;
	      else
		{
		  gfc_error ("Expected SEQ_CST, ACQUIRE or RELAXED at %C");
		  break;
		}
	      if (gfc_match (" )") != MATCH_YES)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_FILTER)
	      && (m = gfc_match_dupl_check (!c->filter, "filter", true,
					    &c->filter)) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_FINAL)
	      && (m = gfc_match_dupl_check (!c->final_expr, "final", true,
					    &c->final_expr)) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_FINALIZE)
	      && (m = gfc_match_dupl_check (!c->finalize, "finalize"))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->finalize = true;
	      needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_FIRSTPRIVATE)
	      && gfc_match_omp_variable_list ("firstprivate (",
					      &c->lists[OMP_LIST_FIRSTPRIVATE],
					      true) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_FROM)
	      && gfc_match_motion_var_list ("from (", &c->lists[OMP_LIST_FROM],
					     &head) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_FULL)
	      && (m = gfc_match_dupl_check (!c->full, "full")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->full = needs_space = true;
	      continue;
	    }
	  break;
	case 'g':
	  if ((mask & OMP_CLAUSE_GANG)
	      && (m = gfc_match_dupl_check (!c->gang, "gang")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->gang = true;
	      m = match_oacc_clause_gwv (c, GOMP_DIM_GANG);
	      if (m == MATCH_ERROR)
		{
		  gfc_current_locus = old_loc;
		  break;
		}
	      else if (m == MATCH_NO)
		needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_GRAINSIZE)
	      && (m = gfc_match_dupl_check (!c->grainsize, "grainsize", true))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (gfc_match ("strict : ") == MATCH_YES)
		c->grainsize_strict = true;
	      if (gfc_match (" %e )", &c->grainsize) != MATCH_YES)
		goto error;
	      continue;
	    }
	  break;
	case 'h':
	  if ((mask & OMP_CLAUSE_HAS_DEVICE_ADDR)
	      && gfc_match_omp_variable_list
		   ("has_device_addr (", &c->lists[OMP_LIST_HAS_DEVICE_ADDR],
		    false, NULL, NULL, true) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_HINT)
	      && (m = gfc_match_dupl_check (!c->hint, "hint", true, &c->hint))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_ASSUMPTIONS)
	      && gfc_match ("holds ( ") == MATCH_YES)
	    {
	      gfc_expr *e;
	      if (gfc_match ("%e )", &e) != MATCH_YES)
		goto error;
	      if (c->assume == NULL)
		c->assume = gfc_get_omp_assumptions ();
	      gfc_expr_list *el = XCNEW (gfc_expr_list);
	      el->expr = e;
	      el->next = c->assume->holds;
	      c->assume->holds = el;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_HOST)
	      && gfc_match ("host ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_FORCE_FROM, true,
					   /* allow_derived = */ true))
	    continue;
	  break;
	case 'i':
	  if ((mask & OMP_CLAUSE_IF_PRESENT)
	      && (m = gfc_match_dupl_check (!c->if_present, "if_present"))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->if_present = true;
	      needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_IF)
	      && (m = gfc_match_dupl_check (!c->if_expr, "if", true))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (!openacc)
		{
		  /* This should match the enum gfc_omp_if_kind order.  */
		  static const char *ifs[OMP_IF_LAST] = {
		    "cancel : %e )",
		    "parallel : %e )",
		    "simd : %e )",
		    "task : %e )",
		    "taskloop : %e )",
		    "target : %e )",
		    "target data : %e )",
		    "target update : %e )",
		    "target enter data : %e )",
		    "target exit data : %e )" };
		  int i;
		  for (i = 0; i < OMP_IF_LAST; i++)
		    if (c->if_exprs[i] == NULL
			&& gfc_match (ifs[i], &c->if_exprs[i]) == MATCH_YES)
		      break;
		  if (i < OMP_IF_LAST)
		    continue;
		}
	      if (gfc_match (" %e )", &c->if_expr) == MATCH_YES)
		continue;
	      goto error;
	    }
	  if ((mask & OMP_CLAUSE_IN_REDUCTION)
	      && gfc_match_omp_clause_reduction (pc, c, openacc, allow_derived,
						 openmp_target) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_INBRANCH)
	      && (m = gfc_match_dupl_check (!c->inbranch && !c->notinbranch,
					    "inbranch")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->inbranch = needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_INDEPENDENT)
	      && (m = gfc_match_dupl_check (!c->independent, "independent"))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->independent = true;
	      needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_INDIRECT)
	      && (m = gfc_match_dupl_check (!c->indirect, "indirect"))
		  != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      gfc_expr *indirect_expr = NULL;
	      m = gfc_match (" ( %e )", &indirect_expr);
	      if (m == MATCH_YES)
		{
		  if (!gfc_resolve_expr (indirect_expr)
		      || indirect_expr->ts.type != BT_LOGICAL
		      || indirect_expr->expr_type != EXPR_CONSTANT)
		    {
		      gfc_error ("INDIRECT clause at %C requires a constant "
				 "logical expression");
		      gfc_free_expr (indirect_expr);
		      goto error;
		    }
		  c->indirect = indirect_expr->value.logical;
		  gfc_free_expr (indirect_expr);
		}
	      else
		c->indirect = 1;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_INIT)
	      && gfc_match ("init ( ") == MATCH_YES)
	    {
	      m = gfc_match_omp_init (&c->lists[OMP_LIST_INIT]);
	      if (m == MATCH_YES)
		continue;
	      goto error;
	    }
	  if ((mask & OMP_CLAUSE_INTEROP)
	      && (m = gfc_match_dupl_check (!c->lists[OMP_LIST_INTEROP],
					    "interop", true)) != MATCH_NO)
	    {
	      /* Note: the interop objects are saved in reverse order to match
		 the order in C/C++.  */
	      if (m == MATCH_YES
		  && (gfc_match_omp_variable_list ("",
						   &c->lists[OMP_LIST_INTEROP],
						   false, NULL, NULL, false,
						   false, NULL, false, true)
		      == MATCH_YES))
		continue;
	      goto error;
	    }
	  if ((mask & OMP_CLAUSE_IS_DEVICE_PTR)
	      && gfc_match_omp_variable_list
		   ("is_device_ptr (",
		    &c->lists[OMP_LIST_IS_DEVICE_PTR], false) == MATCH_YES)
	    continue;
	  break;
	case 'l':
	  if ((mask & OMP_CLAUSE_LASTPRIVATE)
	      && gfc_match ("lastprivate ( ") == MATCH_YES)
	    {
	      bool conditional = gfc_match ("conditional : ") == MATCH_YES;
	      head = NULL;
	      if (gfc_match_omp_variable_list ("",
					       &c->lists[OMP_LIST_LASTPRIVATE],
					       false, NULL, &head) == MATCH_YES)
		{
		  gfc_omp_namelist *n;
		  for (n = *head; n; n = n->next)
		    n->u.lastprivate_conditional = conditional;
		  continue;
		}
	      gfc_current_locus = old_loc;
	      break;
	    }
	  end_colon = false;
	  head = NULL;
	  if ((mask & OMP_CLAUSE_LINEAR)
	      && gfc_match ("linear (") == MATCH_YES)
	    {
	      bool old_linear_modifier = false;
	      gfc_omp_linear_op linear_op = OMP_LINEAR_DEFAULT;
	      gfc_expr *step = NULL;

	      if (gfc_match_omp_variable_list (" ref (",
					       &c->lists[OMP_LIST_LINEAR],
					       false, NULL, &head)
		  == MATCH_YES)
		{
		  linear_op = OMP_LINEAR_REF;
		  old_linear_modifier = true;
		}
	      else if (gfc_match_omp_variable_list (" val (",
						    &c->lists[OMP_LIST_LINEAR],
						    false, NULL, &head)
		       == MATCH_YES)
		{
		  linear_op = OMP_LINEAR_VAL;
		  old_linear_modifier = true;
		}
	      else if (gfc_match_omp_variable_list (" uval (",
						    &c->lists[OMP_LIST_LINEAR],
						    false, NULL, &head)
		       == MATCH_YES)
		{
		  linear_op = OMP_LINEAR_UVAL;
		  old_linear_modifier = true;
		}
	      else if (gfc_match_omp_variable_list ("",
						    &c->lists[OMP_LIST_LINEAR],
						    false, &end_colon, &head)
		       == MATCH_YES)
		linear_op = OMP_LINEAR_DEFAULT;
	      else
		{
		  gfc_current_locus = old_loc;
		  break;
		}
	      if (linear_op != OMP_LINEAR_DEFAULT)
		{
		  if (gfc_match (" :") == MATCH_YES)
		    end_colon = true;
		  else if (gfc_match (" )") != MATCH_YES)
		    {
		      gfc_free_omp_namelist (*head, false, false, false, false);
		      gfc_current_locus = old_loc;
		      *head = NULL;
		      break;
		    }
		}
	      gfc_gobble_whitespace ();
	      if (old_linear_modifier && end_colon)
		{
		  if (gfc_match (" %e )", &step) != MATCH_YES)
		    {
		      gfc_free_omp_namelist (*head, false, false, false, false);
		      gfc_current_locus = old_loc;
		      *head = NULL;
		      goto error;
		    }
		}
	      else if (end_colon)
		{
		  bool has_error = false;
		  bool has_modifiers = false;
		  bool has_step = false;
		  bool duplicate_step = false;
		  bool duplicate_mod = false;
		  while (true)
		    {
		      old_loc = gfc_current_locus;
		      bool close_paren = gfc_match ("val )") == MATCH_YES;
		      if (close_paren || gfc_match ("val , ") == MATCH_YES)
			{
			  if (linear_op != OMP_LINEAR_DEFAULT)
			    {
			      duplicate_mod = true;
			      break;
			    }
			  linear_op = OMP_LINEAR_VAL;
			  has_modifiers = true;
			  if (close_paren)
			    break;
			  continue;
			}
		      close_paren = gfc_match ("uval )") == MATCH_YES;
		      if (close_paren || gfc_match ("uval , ") == MATCH_YES)
			{
			  if (linear_op != OMP_LINEAR_DEFAULT)
			    {
			      duplicate_mod = true;
			      break;
			    }
			  linear_op = OMP_LINEAR_UVAL;
			  has_modifiers = true;
			  if (close_paren)
			    break;
			  continue;
			}
		      close_paren = gfc_match ("ref )") == MATCH_YES;
		      if (close_paren || gfc_match ("ref , ") == MATCH_YES)
			{
			  if (linear_op != OMP_LINEAR_DEFAULT)
			    {
			      duplicate_mod = true;
			      break;
			    }
			  linear_op = OMP_LINEAR_REF;
			  has_modifiers = true;
			  if (close_paren)
			    break;
			  continue;
			}
		      close_paren = (gfc_match ("step ( %e ) )", &step)
				     == MATCH_YES);
		      if (close_paren
			  || gfc_match ("step ( %e ) , ", &step) == MATCH_YES)
			{
			  if (has_step)
			    {
			      duplicate_step = true;
			      break;
			    }
			  has_modifiers = has_step = true;
			  if (close_paren)
			    break;
			  continue;
			}
		      if (!has_modifiers
			  && gfc_match ("%e )", &step) == MATCH_YES)
			{
			  if ((step->expr_type == EXPR_FUNCTION
				|| step->expr_type == EXPR_VARIABLE)
			      && strcmp (step->symtree->name, "step") == 0)
			    {
			      gfc_current_locus = old_loc;
			      gfc_match ("step (");
			      has_error = true;
			    }
			  break;
			}
		      has_error = true;
		      break;
		    }
		  if (duplicate_mod || duplicate_step)
		    {
		      gfc_error ("Multiple %qs modifiers specified at %C",
				 duplicate_mod ? "linear" : "step");
		      has_error = true;
		    }
		  if (has_error)
		    {
		      gfc_free_omp_namelist (*head, false, false, false, false);
		      *head = NULL;
		      goto error;
		    }
		}
	      if (step == NULL)
		{
		  step = gfc_get_constant_expr (BT_INTEGER,
						gfc_default_integer_kind,
						&old_loc);
		  mpz_set_si (step->value.integer, 1);
		}
	      (*head)->expr = step;
	      if (linear_op != OMP_LINEAR_DEFAULT || old_linear_modifier)
		for (gfc_omp_namelist *n = *head; n; n = n->next)
		  {
		    n->u.linear.op = linear_op;
		    n->u.linear.old_modifier = old_linear_modifier;
		  }
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_LINK)
	      && openacc
	      && (gfc_match_oacc_clause_link ("link (",
					      &c->lists[OMP_LIST_LINK])
		  == MATCH_YES))
	    continue;
	  else if ((mask & OMP_CLAUSE_LINK)
		   && !openacc
		   && (gfc_match_omp_to_link ("link (",
					      &c->lists[OMP_LIST_LINK])
		       == MATCH_YES))
	    continue;
	  break;
	case 'm':
	  if ((mask & OMP_CLAUSE_MAP)
	      && gfc_match ("map ( ") == MATCH_YES)
	    {
	      locus old_loc2 = gfc_current_locus;
	      int always_modifier = 0;
	      int close_modifier = 0;
	      int present_modifier = 0;
	      locus second_always_locus = old_loc2;
	      locus second_close_locus = old_loc2;
	      locus second_present_locus = old_loc2;

	      for (;;)
		{
		  locus current_locus = gfc_current_locus;
		  if (gfc_match ("always ") == MATCH_YES)
		    {
		      if (always_modifier++ == 1)
			second_always_locus = current_locus;
		    }
		  else if (gfc_match ("close ") == MATCH_YES)
		    {
		      if (close_modifier++ == 1)
			second_close_locus = current_locus;
		    }
		  else if (gfc_match ("present ") == MATCH_YES)
		    {
		      if (present_modifier++ == 1)
			second_present_locus = current_locus;
		    }
		  else
		    break;
		  gfc_match (", ");
		}

	      gfc_omp_map_op map_op = OMP_MAP_TOFROM;
	      int always_present_modifier
		= always_modifier && present_modifier;

	      if (gfc_match ("alloc : ") == MATCH_YES)
		map_op = (present_modifier ? OMP_MAP_PRESENT_ALLOC
			  : OMP_MAP_ALLOC);
	      else if (gfc_match ("tofrom : ") == MATCH_YES)
		map_op = (always_present_modifier ? OMP_MAP_ALWAYS_PRESENT_TOFROM
			  : present_modifier ? OMP_MAP_PRESENT_TOFROM
			  : always_modifier ? OMP_MAP_ALWAYS_TOFROM
			  : OMP_MAP_TOFROM);
	      else if (gfc_match ("to : ") == MATCH_YES)
		map_op = (always_present_modifier ? OMP_MAP_ALWAYS_PRESENT_TO
			  : present_modifier ? OMP_MAP_PRESENT_TO
			  : always_modifier ? OMP_MAP_ALWAYS_TO
			  : OMP_MAP_TO);
	      else if (gfc_match ("from : ") == MATCH_YES)
		map_op = (always_present_modifier ? OMP_MAP_ALWAYS_PRESENT_FROM
			  : present_modifier ? OMP_MAP_PRESENT_FROM
			  : always_modifier ? OMP_MAP_ALWAYS_FROM
			  : OMP_MAP_FROM);
	      else if (gfc_match ("release : ") == MATCH_YES)
		map_op = OMP_MAP_RELEASE;
	      else if (gfc_match ("delete : ") == MATCH_YES)
		map_op = OMP_MAP_DELETE;
	      else
		{
		  gfc_current_locus = old_loc2;
		  always_modifier = 0;
		  close_modifier = 0;
		}

	      if (always_modifier > 1)
		{
		  gfc_error ("too many %<always%> modifiers at %L",
			     &second_always_locus);
		  break;
		}
	      if (close_modifier > 1)
		{
		  gfc_error ("too many %<close%> modifiers at %L",
			     &second_close_locus);
		  break;
		}
	      if (present_modifier > 1)
		{
		  gfc_error ("too many %<present%> modifiers at %L",
			     &second_present_locus);
		  break;
		}

	      head = NULL;
	      if (gfc_match_omp_variable_list ("", &c->lists[OMP_LIST_MAP],
					       false, NULL, &head,
					       true, true) == MATCH_YES)
		{
		  gfc_omp_namelist *n;
		  for (n = *head; n; n = n->next)
		    n->u.map.op = map_op;
		  continue;
		}
	      gfc_current_locus = old_loc;
	      break;
	    }
	  if ((mask & OMP_CLAUSE_MERGEABLE)
	      && (m = gfc_match_dupl_check (!c->mergeable, "mergeable"))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->mergeable = needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_MESSAGE)
	      && (m = gfc_match_dupl_check (!c->message, "message", true,
		 &c->message)) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      continue;
	    }
	  break;
	case 'n':
	  if ((mask & OMP_CLAUSE_NO_CREATE)
	      && gfc_match ("no_create ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_IF_PRESENT, true,
					   allow_derived))
	    continue;
	  if ((mask & OMP_CLAUSE_ASSUMPTIONS)
	      && (m = gfc_match_dupl_check (!c->assume
					    || !c->assume->no_openmp_routines,
					    "no_openmp_routines")) == MATCH_YES)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (c->assume == NULL)
		c->assume = gfc_get_omp_assumptions ();
	      c->assume->no_openmp_routines = needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_ASSUMPTIONS)
	      && (m = gfc_match_dupl_check (!c->assume || !c->assume->no_openmp,
					    "no_openmp")) == MATCH_YES)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (c->assume == NULL)
		c->assume = gfc_get_omp_assumptions ();
	      c->assume->no_openmp = needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_ASSUMPTIONS)
	      && (m = gfc_match_dupl_check (!c->assume
					    || !c->assume->no_parallelism,
					    "no_parallelism")) == MATCH_YES)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (c->assume == NULL)
		c->assume = gfc_get_omp_assumptions ();
	      c->assume->no_parallelism = needs_space = true;
	      continue;
	    }

	  if ((mask & OMP_CLAUSE_NOVARIANTS)
	      && (m = gfc_match_dupl_check (!c->novariants, "novariants", true,
					    &c->novariants))
		   != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_NOCONTEXT)
	      && (m = gfc_match_dupl_check (!c->nocontext, "nocontext", true,
					    &c->nocontext))
		   != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_NOGROUP)
	      && (m = gfc_match_dupl_check (!c->nogroup, "nogroup"))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->nogroup = needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_NOHOST)
	      && (m = gfc_match_dupl_check (!c->nohost, "nohost")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->nohost = needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_NOTEMPORAL)
	      && gfc_match_omp_variable_list ("nontemporal (",
					      &c->lists[OMP_LIST_NONTEMPORAL],
					      true) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_NOTINBRANCH)
	      && (m = gfc_match_dupl_check (!c->notinbranch && !c->inbranch,
					    "notinbranch")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->notinbranch = needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_NOWAIT)
	      && (m = gfc_match_dupl_check (!c->nowait, "nowait")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->nowait = needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_NUM_GANGS)
	      && (m = gfc_match_dupl_check (!c->num_gangs_expr, "num_gangs",
					    true)) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (gfc_match (" %e )", &c->num_gangs_expr) != MATCH_YES)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_NUM_TASKS)
	      && (m = gfc_match_dupl_check (!c->num_tasks, "num_tasks", true))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (gfc_match ("strict : ") == MATCH_YES)
		c->num_tasks_strict = true;
	      if (gfc_match (" %e )", &c->num_tasks) != MATCH_YES)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_NUM_TEAMS)
	      && (m = gfc_match_dupl_check (!c->num_teams_upper, "num_teams",
					    true)) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (gfc_match ("%e ", &c->num_teams_upper) != MATCH_YES)
		goto error;
	      if (gfc_peek_ascii_char () == ':')
		{
		  c->num_teams_lower = c->num_teams_upper;
		  c->num_teams_upper = NULL;
		  if (gfc_match (": %e ", &c->num_teams_upper) != MATCH_YES)
		    goto error;
		}
	      if (gfc_match (") ") != MATCH_YES)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_NUM_THREADS)
	      && (m = gfc_match_dupl_check (!c->num_threads, "num_threads", true,
					    &c->num_threads)) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_NUM_WORKERS)
	      && (m = gfc_match_dupl_check (!c->num_workers_expr, "num_workers",
					    true, &c->num_workers_expr))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      continue;
	    }
	  break;
	case 'o':
	  if ((mask & OMP_CLAUSE_ORDER)
	      && (m = gfc_match_dupl_check (!c->order_concurrent, "order ("))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (gfc_match (" reproducible : concurrent )") == MATCH_YES)
		c->order_reproducible = true;
	      else if (gfc_match (" concurrent )") == MATCH_YES)
		;
	      else if (gfc_match (" unconstrained : concurrent )") == MATCH_YES)
		c->order_unconstrained = true;
	      else
		{
		  gfc_error ("Expected ORDER(CONCURRENT) at %C "
			     "with optional %<reproducible%> or "
			     "%<unconstrained%> modifier");
		  goto error;
		}
	      c->order_concurrent = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_ORDERED)
	      && (m = gfc_match_dupl_check (!c->ordered, "ordered"))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      gfc_expr *cexpr = NULL;
	      m = gfc_match (" ( %e )", &cexpr);

	      c->ordered = true;
	      if (m == MATCH_YES)
		{
		  int ordered = 0;
		  if (gfc_extract_int (cexpr, &ordered, -1))
		    ordered = 0;
		  else if (ordered <= 0)
		    {
		      gfc_error_now ("ORDERED clause argument not"
				     " constant positive integer at %C");
		      ordered = 0;
		    }
		  c->orderedc = ordered;
		  gfc_free_expr (cexpr);
		  continue;
		}

	      needs_space = true;
	      continue;
	    }
	  break;
	case 'p':
	  if (mask & OMP_CLAUSE_PARTIAL)
	    {
	      if ((m = gfc_match_dupl_check (!c->partial, "partial"))
		  != MATCH_NO)
		{
		  int expr;
		  if (m == MATCH_ERROR)
		    goto error;

		  c->partial = -1;

		  gfc_expr *cexpr = NULL;
		  m = gfc_match (" ( %e )", &cexpr);
		  if (m == MATCH_NO)
		    ;
		  else if (m == MATCH_YES
			   && !gfc_extract_int (cexpr, &expr, -1)
			   && expr > 0)
		    c->partial = expr;
		  else
		    gfc_error_now ("PARTIAL clause argument not constant "
				   "positive integer at %C");
		  gfc_free_expr (cexpr);
		  continue;
		}
	    }
	  if ((mask & OMP_CLAUSE_COPY)
	      && gfc_match ("pcopy ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_TOFROM, true, allow_derived))
	    continue;
	  if ((mask & OMP_CLAUSE_COPYIN)
	      && gfc_match ("pcopyin ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_TO, true, allow_derived))
	    continue;
	  if ((mask & OMP_CLAUSE_COPYOUT)
	      && gfc_match ("pcopyout ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_FROM, true, allow_derived))
	    continue;
	  if ((mask & OMP_CLAUSE_CREATE)
	      && gfc_match ("pcreate ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_ALLOC, true, allow_derived))
	    continue;
	  if ((mask & OMP_CLAUSE_PRESENT)
	      && gfc_match ("present ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_FORCE_PRESENT, false,
					   allow_derived))
	    continue;
	  if ((mask & OMP_CLAUSE_COPY)
	      && gfc_match ("present_or_copy ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_TOFROM, true,
					   allow_derived))
	    continue;
	  if ((mask & OMP_CLAUSE_COPYIN)
	      && gfc_match ("present_or_copyin ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_TO, true, allow_derived))
	    continue;
	  if ((mask & OMP_CLAUSE_COPYOUT)
	      && gfc_match ("present_or_copyout ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_FROM, true, allow_derived))
	    continue;
	  if ((mask & OMP_CLAUSE_CREATE)
	      && gfc_match ("present_or_create ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_ALLOC, true, allow_derived))
	    continue;
	  if ((mask & OMP_CLAUSE_PRIORITY)
	      && (m = gfc_match_dupl_check (!c->priority, "priority", true,
					    &c->priority)) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_PRIVATE)
	      && gfc_match_omp_variable_list ("private (",
					      &c->lists[OMP_LIST_PRIVATE],
					      true) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_PROC_BIND)
	      && (m = gfc_match_dupl_check ((c->proc_bind
					     == OMP_PROC_BIND_UNKNOWN),
					    "proc_bind", true)) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (gfc_match ("primary )") == MATCH_YES)
		c->proc_bind = OMP_PROC_BIND_PRIMARY;
	      else if (gfc_match ("master )") == MATCH_YES)
		c->proc_bind = OMP_PROC_BIND_MASTER;
	      else if (gfc_match ("spread )") == MATCH_YES)
		c->proc_bind = OMP_PROC_BIND_SPREAD;
	      else if (gfc_match ("close )") == MATCH_YES)
		c->proc_bind = OMP_PROC_BIND_CLOSE;
	      else
		goto error;
	      continue;
	    }
	  break;
	case 'r':
	  if ((mask & OMP_CLAUSE_ATOMIC)
	      && (m = gfc_match_dupl_atomic ((c->atomic_op
					      == GFC_OMP_ATOMIC_UNSET),
					     "read")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->atomic_op = GFC_OMP_ATOMIC_READ;
	      needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_REDUCTION)
	      && gfc_match_omp_clause_reduction (pc, c, openacc,
						 allow_derived) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_MEMORDER)
	      && (m = gfc_match_dupl_memorder ((c->memorder
						== OMP_MEMORDER_UNSET),
					       "relaxed")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->memorder = OMP_MEMORDER_RELAXED;
	      needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_MEMORDER)
	      && (m = gfc_match_dupl_memorder ((c->memorder
						== OMP_MEMORDER_UNSET),
					       "release")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->memorder = OMP_MEMORDER_RELEASE;
	      needs_space = true;
	      continue;
	    }
	  break;
	case 's':
	  if ((mask & OMP_CLAUSE_SAFELEN)
	      && (m = gfc_match_dupl_check (!c->safelen_expr, "safelen",
					    true, &c->safelen_expr))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_SCHEDULE)
	      && (m = gfc_match_dupl_check (c->sched_kind == OMP_SCHED_NONE,
					    "schedule", true)) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      int nmodifiers = 0;
	      locus old_loc2 = gfc_current_locus;
	      do
		{
		  if (gfc_match ("simd") == MATCH_YES)
		    {
		      c->sched_simd = true;
		      nmodifiers++;
		    }
		  else if (gfc_match ("monotonic") == MATCH_YES)
		    {
		      c->sched_monotonic = true;
		      nmodifiers++;
		    }
		  else if (gfc_match ("nonmonotonic") == MATCH_YES)
		    {
		      c->sched_nonmonotonic = true;
		      nmodifiers++;
		    }
		  else
		    {
		      if (nmodifiers)
			gfc_current_locus = old_loc2;
		      break;
		    }
		  if (nmodifiers == 1
		      && gfc_match (" , ") == MATCH_YES)
		    continue;
		  else if (gfc_match (" : ") == MATCH_YES)
		    break;
		  gfc_current_locus = old_loc2;
		  break;
		}
	      while (1);
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
		  m = MATCH_NO;
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
	  if ((mask & OMP_CLAUSE_SELF)
	      && !(mask & OMP_CLAUSE_HOST) /* OpenACC compute construct */
	      && (m = gfc_match_dupl_check (!c->self_expr, "self"))
	          != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      m = gfc_match (" ( %e )", &c->self_expr);
	      if (m == MATCH_ERROR)
		{
		  gfc_current_locus = old_loc;
		  break;
		}
	      else if (m == MATCH_NO)
		{
		  c->self_expr = gfc_get_logical_expr (gfc_default_logical_kind,
						       NULL, true);
		  needs_space = true;
		}
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_SELF)
	      && (mask & OMP_CLAUSE_HOST) /* OpenACC 'update' directive */
	      && gfc_match ("self ( ") == MATCH_YES
	      && gfc_match_omp_map_clause (&c->lists[OMP_LIST_MAP],
					   OMP_MAP_FORCE_FROM, true,
					   /* allow_derived = */ true))
	    continue;
	  if ((mask & OMP_CLAUSE_SEQ)
	      && (m = gfc_match_dupl_check (!c->seq, "seq")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->seq = true;
	      needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_MEMORDER)
	      && (m = gfc_match_dupl_memorder ((c->memorder
						== OMP_MEMORDER_UNSET),
					       "seq_cst")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->memorder = OMP_MEMORDER_SEQ_CST;
	      needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_SHARED)
	      && gfc_match_omp_variable_list ("shared (",
					      &c->lists[OMP_LIST_SHARED],
					      true) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_SIMDLEN)
	      && (m = gfc_match_dupl_check (!c->simdlen_expr, "simdlen", true,
					    &c->simdlen_expr)) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_SIMD)
	      && (m = gfc_match_dupl_check (!c->simd, "simd")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->simd = needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_SEVERITY)
	      && (m = gfc_match_dupl_check (!c->severity, "severity", true))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      if (gfc_match ("fatal )") == MATCH_YES)
		c->severity = OMP_SEVERITY_FATAL;
	      else if (gfc_match ("warning )") == MATCH_YES)
		c->severity = OMP_SEVERITY_WARNING;
	      else
		{
		  gfc_error ("Expected FATAL or WARNING in SEVERITY clause "
			     "at %C");
		  goto error;
		}
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_SIZES)
	      && ((m = gfc_match_dupl_check (!c->sizes_list, "sizes"))
		  != MATCH_NO))
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      m = match_omp_oacc_expr_list (" (", &c->sizes_list, false, true);
	      if (m == MATCH_ERROR)
		goto error;
	      if (m == MATCH_YES)
		continue;
	      gfc_error ("Expected %<(%> after %qs at %C", "sizes");
	      goto error;
	    }
	  break;
	case 't':
	  if ((mask & OMP_CLAUSE_TASK_REDUCTION)
	      && gfc_match_omp_clause_reduction (pc, c, openacc,
						 allow_derived) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_THREAD_LIMIT)
	      && (m = gfc_match_dupl_check (!c->thread_limit, "thread_limit",
					    true, &c->thread_limit))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_THREADS)
	      && (m = gfc_match_dupl_check (!c->threads, "threads"))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->threads = needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_TILE)
	      && !c->tile_list
	      && match_omp_oacc_expr_list ("tile (", &c->tile_list,
					   true, false) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_TO) && (mask & OMP_CLAUSE_LINK))
	    {
	      /* Declare target: 'to' is an alias for 'enter';
		 'to' is deprecated since 5.2.  */
	      m = gfc_match_omp_to_link ("to (", &c->lists[OMP_LIST_TO]);
	      if (m == MATCH_ERROR)
		goto error;
	      if (m == MATCH_YES)
		continue;
	    }
	  else if ((mask & OMP_CLAUSE_TO)
		   && gfc_match_motion_var_list ("to (", &c->lists[OMP_LIST_TO],
						 &head) == MATCH_YES)
	    continue;
	  break;
	case 'u':
	  if ((mask & OMP_CLAUSE_UNIFORM)
	      && gfc_match_omp_variable_list ("uniform (",
					      &c->lists[OMP_LIST_UNIFORM],
					      false) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_UNTIED)
	      && (m = gfc_match_dupl_check (!c->untied, "untied")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->untied = needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_ATOMIC)
	      && (m = gfc_match_dupl_atomic ((c->atomic_op
					      == GFC_OMP_ATOMIC_UNSET),
					     "update")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->atomic_op = GFC_OMP_ATOMIC_UPDATE;
	      needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_USE)
	      && gfc_match_omp_variable_list ("use (",
					      &c->lists[OMP_LIST_USE],
					      true) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_USE_DEVICE)
	      && gfc_match_omp_variable_list ("use_device (",
					      &c->lists[OMP_LIST_USE_DEVICE],
					      true) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_USE_DEVICE_PTR)
	      && gfc_match_omp_variable_list
		   ("use_device_ptr (",
		    &c->lists[OMP_LIST_USE_DEVICE_PTR], false) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_USE_DEVICE_ADDR)
	      && gfc_match_omp_variable_list
		   ("use_device_addr (", &c->lists[OMP_LIST_USE_DEVICE_ADDR],
		    false, NULL, NULL, true) == MATCH_YES)
	    continue;
	  if ((mask & OMP_CLAUSE_USES_ALLOCATORS)
	      && (gfc_match ("uses_allocators ( ") == MATCH_YES))
	    {
	      if (gfc_match_omp_clause_uses_allocators (c) != MATCH_YES)
		goto error;
	      continue;
	    }
	  break;
	case 'v':
	  /* VECTOR_LENGTH must be matched before VECTOR, because the latter
	     doesn't unconditionally match '('.  */
	  if ((mask & OMP_CLAUSE_VECTOR_LENGTH)
	      && (m = gfc_match_dupl_check (!c->vector_length_expr,
					    "vector_length", true,
					    &c->vector_length_expr))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_VECTOR)
	      && (m = gfc_match_dupl_check (!c->vector, "vector")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->vector = true;
	      m = match_oacc_clause_gwv (c, GOMP_DIM_VECTOR);
	      if (m == MATCH_ERROR)
		goto error;
	      if (m == MATCH_NO)
		needs_space = true;
	      continue;
	    }
	  break;
	case 'w':
	  if ((mask & OMP_CLAUSE_WAIT)
	      && gfc_match ("wait") == MATCH_YES)
	    {
	      m = match_omp_oacc_expr_list (" (", &c->wait_list, false, false);
	      if (m == MATCH_ERROR)
		goto error;
	      else if (m == MATCH_NO)
		{
		  gfc_expr *expr
		    = gfc_get_constant_expr (BT_INTEGER,
					     gfc_default_integer_kind,
					     &gfc_current_locus);
		  mpz_set_si (expr->value.integer, GOMP_ASYNC_NOVAL);
		  gfc_expr_list **expr_list = &c->wait_list;
		  while (*expr_list)
		    expr_list = &(*expr_list)->next;
		  *expr_list = gfc_get_expr_list ();
		  (*expr_list)->expr = expr;
		  needs_space = true;
		}
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_WEAK)
	      && (m = gfc_match_dupl_check (!c->weak, "weak"))
		 != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->weak = true;
	      needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_WORKER)
	      && (m = gfc_match_dupl_check (!c->worker, "worker")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->worker = true;
	      m = match_oacc_clause_gwv (c, GOMP_DIM_WORKER);
	      if (m == MATCH_ERROR)
		goto error;
	      else if (m == MATCH_NO)
		needs_space = true;
	      continue;
	    }
	  if ((mask & OMP_CLAUSE_ATOMIC)
	      && (m = gfc_match_dupl_atomic ((c->atomic_op
					      == GFC_OMP_ATOMIC_UNSET),
					     "write")) != MATCH_NO)
	    {
	      if (m == MATCH_ERROR)
		goto error;
	      c->atomic_op = GFC_OMP_ATOMIC_WRITE;
	      needs_space = true;
	      continue;
	    }
	  break;
	}
      break;
    }

end:
  if (error || gfc_match_omp_eos () != MATCH_YES)
    {
      if (!gfc_error_flag_test ())
	gfc_error ("Failed to match clause at %C");
      gfc_free_omp_clauses (c);
      return MATCH_ERROR;
    }

  *cp = c;
  return MATCH_YES;

error:
  error = true;
  goto end;
}


#define OACC_PARALLEL_CLAUSES \
  (omp_mask (OMP_CLAUSE_IF) | OMP_CLAUSE_ASYNC | OMP_CLAUSE_NUM_GANGS	      \
   | OMP_CLAUSE_NUM_WORKERS | OMP_CLAUSE_VECTOR_LENGTH | OMP_CLAUSE_REDUCTION \
   | OMP_CLAUSE_COPY | OMP_CLAUSE_COPYIN | OMP_CLAUSE_COPYOUT		      \
   | OMP_CLAUSE_CREATE | OMP_CLAUSE_NO_CREATE | OMP_CLAUSE_PRESENT	      \
   | OMP_CLAUSE_DEVICEPTR | OMP_CLAUSE_PRIVATE | OMP_CLAUSE_FIRSTPRIVATE      \
   | OMP_CLAUSE_DEFAULT | OMP_CLAUSE_WAIT | OMP_CLAUSE_ATTACH		      \
   | OMP_CLAUSE_SELF)
#define OACC_KERNELS_CLAUSES \
  (omp_mask (OMP_CLAUSE_IF) | OMP_CLAUSE_ASYNC | OMP_CLAUSE_NUM_GANGS	      \
   | OMP_CLAUSE_NUM_WORKERS | OMP_CLAUSE_VECTOR_LENGTH | OMP_CLAUSE_DEVICEPTR \
   | OMP_CLAUSE_COPY | OMP_CLAUSE_COPYIN | OMP_CLAUSE_COPYOUT		      \
   | OMP_CLAUSE_CREATE | OMP_CLAUSE_NO_CREATE | OMP_CLAUSE_PRESENT	      \
   | OMP_CLAUSE_DEFAULT | OMP_CLAUSE_WAIT | OMP_CLAUSE_ATTACH	              \
   | OMP_CLAUSE_SELF)
#define OACC_SERIAL_CLAUSES \
  (omp_mask (OMP_CLAUSE_IF) | OMP_CLAUSE_ASYNC | OMP_CLAUSE_REDUCTION	      \
   | OMP_CLAUSE_COPY | OMP_CLAUSE_COPYIN | OMP_CLAUSE_COPYOUT		      \
   | OMP_CLAUSE_CREATE | OMP_CLAUSE_NO_CREATE | OMP_CLAUSE_PRESENT	      \
   | OMP_CLAUSE_DEVICEPTR | OMP_CLAUSE_PRIVATE | OMP_CLAUSE_FIRSTPRIVATE      \
   | OMP_CLAUSE_DEFAULT | OMP_CLAUSE_WAIT | OMP_CLAUSE_ATTACH	              \
   | OMP_CLAUSE_SELF)
#define OACC_DATA_CLAUSES \
  (omp_mask (OMP_CLAUSE_IF) | OMP_CLAUSE_DEVICEPTR  | OMP_CLAUSE_COPY	      \
   | OMP_CLAUSE_COPYIN | OMP_CLAUSE_COPYOUT | OMP_CLAUSE_CREATE		      \
   | OMP_CLAUSE_NO_CREATE | OMP_CLAUSE_PRESENT | OMP_CLAUSE_ATTACH	      \
   | OMP_CLAUSE_DEFAULT)
#define OACC_LOOP_CLAUSES \
  (omp_mask (OMP_CLAUSE_COLLAPSE) | OMP_CLAUSE_GANG | OMP_CLAUSE_WORKER	      \
   | OMP_CLAUSE_VECTOR | OMP_CLAUSE_SEQ | OMP_CLAUSE_INDEPENDENT	      \
   | OMP_CLAUSE_PRIVATE | OMP_CLAUSE_REDUCTION | OMP_CLAUSE_AUTO	      \
   | OMP_CLAUSE_TILE)
#define OACC_PARALLEL_LOOP_CLAUSES \
  (OACC_LOOP_CLAUSES | OACC_PARALLEL_CLAUSES)
#define OACC_KERNELS_LOOP_CLAUSES \
  (OACC_LOOP_CLAUSES | OACC_KERNELS_CLAUSES)
#define OACC_SERIAL_LOOP_CLAUSES \
  (OACC_LOOP_CLAUSES | OACC_SERIAL_CLAUSES)
#define OACC_HOST_DATA_CLAUSES \
  (omp_mask (OMP_CLAUSE_USE_DEVICE)					      \
   | OMP_CLAUSE_IF							      \
   | OMP_CLAUSE_IF_PRESENT)
#define OACC_DECLARE_CLAUSES \
  (omp_mask (OMP_CLAUSE_COPY) | OMP_CLAUSE_COPYIN | OMP_CLAUSE_COPYOUT	      \
   | OMP_CLAUSE_CREATE | OMP_CLAUSE_DEVICEPTR | OMP_CLAUSE_DEVICE_RESIDENT    \
   | OMP_CLAUSE_PRESENT			      \
   | OMP_CLAUSE_LINK)
#define OACC_UPDATE_CLAUSES						\
  (omp_mask (OMP_CLAUSE_IF) | OMP_CLAUSE_ASYNC | OMP_CLAUSE_HOST	      \
   | OMP_CLAUSE_DEVICE | OMP_CLAUSE_WAIT | OMP_CLAUSE_IF_PRESENT	      \
   | OMP_CLAUSE_SELF)
#define OACC_ENTER_DATA_CLAUSES \
  (omp_mask (OMP_CLAUSE_IF) | OMP_CLAUSE_ASYNC | OMP_CLAUSE_WAIT	      \
   | OMP_CLAUSE_COPYIN | OMP_CLAUSE_CREATE | OMP_CLAUSE_ATTACH)
#define OACC_EXIT_DATA_CLAUSES \
  (omp_mask (OMP_CLAUSE_IF) | OMP_CLAUSE_ASYNC | OMP_CLAUSE_WAIT	      \
   | OMP_CLAUSE_COPYOUT | OMP_CLAUSE_DELETE | OMP_CLAUSE_FINALIZE	      \
   | OMP_CLAUSE_DETACH)
#define OACC_WAIT_CLAUSES \
  omp_mask (OMP_CLAUSE_ASYNC)
#define OACC_ROUTINE_CLAUSES \
  (omp_mask (OMP_CLAUSE_GANG) | OMP_CLAUSE_WORKER | OMP_CLAUSE_VECTOR	      \
   | OMP_CLAUSE_SEQ							      \
   | OMP_CLAUSE_NOHOST)


static match
match_acc (gfc_exec_op op, const omp_mask mask)
{
  gfc_omp_clauses *c;
  if (gfc_match_omp_clauses (&c, mask, false, false, true) != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = op;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}

match
gfc_match_oacc_parallel_loop (void)
{
  return match_acc (EXEC_OACC_PARALLEL_LOOP, OACC_PARALLEL_LOOP_CLAUSES);
}


match
gfc_match_oacc_parallel (void)
{
  return match_acc (EXEC_OACC_PARALLEL, OACC_PARALLEL_CLAUSES);
}


match
gfc_match_oacc_kernels_loop (void)
{
  return match_acc (EXEC_OACC_KERNELS_LOOP, OACC_KERNELS_LOOP_CLAUSES);
}


match
gfc_match_oacc_kernels (void)
{
  return match_acc (EXEC_OACC_KERNELS, OACC_KERNELS_CLAUSES);
}


match
gfc_match_oacc_serial_loop (void)
{
  return match_acc (EXEC_OACC_SERIAL_LOOP, OACC_SERIAL_LOOP_CLAUSES);
}


match
gfc_match_oacc_serial (void)
{
  return match_acc (EXEC_OACC_SERIAL, OACC_SERIAL_CLAUSES);
}


match
gfc_match_oacc_data (void)
{
  return match_acc (EXEC_OACC_DATA, OACC_DATA_CLAUSES);
}


match
gfc_match_oacc_host_data (void)
{
  return match_acc (EXEC_OACC_HOST_DATA, OACC_HOST_DATA_CLAUSES);
}


match
gfc_match_oacc_loop (void)
{
  return match_acc (EXEC_OACC_LOOP, OACC_LOOP_CLAUSES);
}


match
gfc_match_oacc_declare (void)
{
  gfc_omp_clauses *c;
  gfc_omp_namelist *n;
  gfc_namespace *ns = gfc_current_ns;
  gfc_oacc_declare *new_oc;
  bool module_var = false;
  locus where = gfc_current_locus;

  if (gfc_match_omp_clauses (&c, OACC_DECLARE_CLAUSES, false, false, true)
      != MATCH_YES)
    return MATCH_ERROR;

  for (n = c->lists[OMP_LIST_DEVICE_RESIDENT]; n != NULL; n = n->next)
    n->sym->attr.oacc_declare_device_resident = 1;

  for (n = c->lists[OMP_LIST_LINK]; n != NULL; n = n->next)
    n->sym->attr.oacc_declare_link = 1;

  for (n = c->lists[OMP_LIST_MAP]; n != NULL; n = n->next)
    {
      gfc_symbol *s = n->sym;

      if (gfc_current_ns->proc_name
	  && gfc_current_ns->proc_name->attr.flavor == FL_MODULE)
	{
	  if (n->u.map.op != OMP_MAP_ALLOC && n->u.map.op != OMP_MAP_TO)
	    {
	      gfc_error ("Invalid clause in module with !$ACC DECLARE at %L",
			 &where);
	      return MATCH_ERROR;
	    }

	  module_var = true;
	}

      if (s->attr.use_assoc)
	{
	  gfc_error ("Variable is USE-associated with !$ACC DECLARE at %L",
		     &where);
	  return MATCH_ERROR;
	}

      if ((s->result == s && s->ns->contained != gfc_current_ns)
	  || ((s->attr.flavor == FL_UNKNOWN || s->attr.flavor == FL_VARIABLE)
	      && s->ns != gfc_current_ns))
	{
	  gfc_error ("Variable %qs shall be declared in the same scoping unit "
		     "as !$ACC DECLARE at %L", s->name, &where);
	  return MATCH_ERROR;
	}

      if ((s->attr.dimension || s->attr.codimension)
	  && s->attr.dummy && s->as->type != AS_EXPLICIT)
	{
	  gfc_error ("Assumed-size dummy array with !$ACC DECLARE at %L",
		     &where);
	  return MATCH_ERROR;
	}

      switch (n->u.map.op)
	{
	  case OMP_MAP_FORCE_ALLOC:
	  case OMP_MAP_ALLOC:
	    s->attr.oacc_declare_create = 1;
	    break;

	  case OMP_MAP_FORCE_TO:
	  case OMP_MAP_TO:
	    s->attr.oacc_declare_copyin = 1;
	    break;

	  case OMP_MAP_FORCE_DEVICEPTR:
	    s->attr.oacc_declare_deviceptr = 1;
	    break;

	  default:
	    break;
	}
    }

  new_oc = gfc_get_oacc_declare ();
  new_oc->next = ns->oacc_declare;
  new_oc->module_var = module_var;
  new_oc->clauses = c;
  new_oc->loc = gfc_current_locus;
  ns->oacc_declare = new_oc;

  return MATCH_YES;
}


match
gfc_match_oacc_update (void)
{
  gfc_omp_clauses *c;
  locus here = gfc_current_locus;

  if (gfc_match_omp_clauses (&c, OACC_UPDATE_CLAUSES, false, false, true)
      != MATCH_YES)
    return MATCH_ERROR;

  if (!c->lists[OMP_LIST_MAP])
    {
      gfc_error ("%<acc update%> must contain at least one "
		 "%<device%> or %<host%> or %<self%> clause at %L", &here);
      return MATCH_ERROR;
    }

  new_st.op = EXEC_OACC_UPDATE;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


match
gfc_match_oacc_enter_data (void)
{
  return match_acc (EXEC_OACC_ENTER_DATA, OACC_ENTER_DATA_CLAUSES);
}


match
gfc_match_oacc_exit_data (void)
{
  return match_acc (EXEC_OACC_EXIT_DATA, OACC_EXIT_DATA_CLAUSES);
}


match
gfc_match_oacc_wait (void)
{
  gfc_omp_clauses *c = gfc_get_omp_clauses ();
  gfc_expr_list *wait_list = NULL, *el;
  bool space = true;
  match m;

  m = match_omp_oacc_expr_list (" (", &wait_list, true, false);
  if (m == MATCH_ERROR)
    return m;
  else if (m == MATCH_YES)
    space = false;

  if (gfc_match_omp_clauses (&c, OACC_WAIT_CLAUSES, space, space, true)
      == MATCH_ERROR)
    return MATCH_ERROR;

  if (wait_list)
    for (el = wait_list; el; el = el->next)
      {
	if (el->expr == NULL)
	  {
	    gfc_error ("Invalid argument to !$ACC WAIT at %C");
	    return MATCH_ERROR;
	  }

	if (!gfc_resolve_expr (el->expr)
	    || el->expr->ts.type != BT_INTEGER || el->expr->rank != 0)
	  {
	    gfc_error ("WAIT clause at %L requires a scalar INTEGER expression",
		       &el->expr->where);

	    return MATCH_ERROR;
	  }
      }
  c->wait_list = wait_list;
  new_st.op = EXEC_OACC_WAIT;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


match
gfc_match_oacc_cache (void)
{
  bool readonly = false;
  gfc_omp_clauses *c = gfc_get_omp_clauses ();
  /* The OpenACC cache directive explicitly only allows "array elements or
     subarrays", which we're currently not checking here.  Either check this
     after the call of gfc_match_omp_variable_list, or add something like a
     only_sections variant next to its allow_sections parameter.  */
  match m = gfc_match (" ( ");
  if (m != MATCH_YES)
    {
      gfc_free_omp_clauses(c);
      return m;
    }

  if (gfc_match ("readonly : ") == MATCH_YES)
    readonly = true;

  gfc_omp_namelist **head = NULL;
  m = gfc_match_omp_variable_list ("", &c->lists[OMP_LIST_CACHE], true,
				   NULL, &head, true);
  if (m != MATCH_YES)
    {
      gfc_free_omp_clauses(c);
      return m;
    }

  if (readonly)
    for (gfc_omp_namelist *n = *head; n; n = n->next)
      n->u.map.readonly = true;

  if (gfc_current_state() != COMP_DO
      && gfc_current_state() != COMP_DO_CONCURRENT)
    {
      gfc_error ("ACC CACHE directive must be inside of loop %C");
      gfc_free_omp_clauses(c);
      return MATCH_ERROR;
    }

  new_st.op = EXEC_OACC_CACHE;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}

/* Determine the OpenACC 'routine' directive's level of parallelism.  */

static oacc_routine_lop
gfc_oacc_routine_lop (gfc_omp_clauses *clauses)
{
  oacc_routine_lop ret = OACC_ROUTINE_LOP_SEQ;

  if (clauses)
    {
      unsigned n_lop_clauses = 0;

      if (clauses->gang)
	{
	  ++n_lop_clauses;
	  ret = OACC_ROUTINE_LOP_GANG;
	}
      if (clauses->worker)
	{
	  ++n_lop_clauses;
	  ret = OACC_ROUTINE_LOP_WORKER;
	}
      if (clauses->vector)
	{
	  ++n_lop_clauses;
	  ret = OACC_ROUTINE_LOP_VECTOR;
	}
      if (clauses->seq)
	{
	  ++n_lop_clauses;
	  ret = OACC_ROUTINE_LOP_SEQ;
	}

      if (n_lop_clauses > 1)
	ret = OACC_ROUTINE_LOP_ERROR;
    }

  return ret;
}

match
gfc_match_oacc_routine (void)
{
  locus old_loc;
  match m;
  gfc_intrinsic_sym *isym = NULL;
  gfc_symbol *sym = NULL;
  gfc_omp_clauses *c = NULL;
  gfc_oacc_routine_name *n = NULL;
  oacc_routine_lop lop = OACC_ROUTINE_LOP_NONE;
  bool nohost;

  old_loc = gfc_current_locus;

  m = gfc_match (" (");

  if (gfc_current_ns->proc_name
      && gfc_current_ns->proc_name->attr.if_source == IFSRC_IFBODY
      && m == MATCH_YES)
    {
      gfc_error ("Only the !$ACC ROUTINE form without "
		 "list is allowed in interface block at %C");
      goto cleanup;
    }

  if (m == MATCH_YES)
    {
      char buffer[GFC_MAX_SYMBOL_LEN + 1];

      m = gfc_match_name (buffer);
      if (m == MATCH_YES)
	{
	  gfc_symtree *st = NULL;

	  /* First look for an intrinsic symbol.  */
	  isym = gfc_find_function (buffer);
	  if (!isym)
	    isym = gfc_find_subroutine (buffer);
	  /* If no intrinsic symbol found, search the current namespace.  */
	  if (!isym)
	    st = gfc_find_symtree (gfc_current_ns->sym_root, buffer);
	  if (st)
	    {
	      sym = st->n.sym;
	      /* If the name in a 'routine' directive refers to the containing
		 subroutine or function, then make sure that we'll later handle
		 this accordingly.  */
	      if (gfc_current_ns->proc_name != NULL
		  && strcmp (sym->name, gfc_current_ns->proc_name->name) == 0)
	        sym = NULL;
	    }

	  if (isym == NULL && st == NULL)
	    {
	      gfc_error ("Invalid NAME %qs in !$ACC ROUTINE ( NAME ) at %C",
			 buffer);
	      gfc_current_locus = old_loc;
	      return MATCH_ERROR;
	    }
	}
      else
        {
	  gfc_error ("Syntax error in !$ACC ROUTINE ( NAME ) at %C");
	  gfc_current_locus = old_loc;
	  return MATCH_ERROR;
	}

      if (gfc_match_char (')') != MATCH_YES)
	{
	  gfc_error ("Syntax error in !$ACC ROUTINE ( NAME ) at %C, expecting"
		     " %<)%> after NAME");
	  gfc_current_locus = old_loc;
	  return MATCH_ERROR;
	}
    }

  if (gfc_match_omp_eos () != MATCH_YES
      && (gfc_match_omp_clauses (&c, OACC_ROUTINE_CLAUSES, false, false, true)
	  != MATCH_YES))
    return MATCH_ERROR;

  lop = gfc_oacc_routine_lop (c);
  if (lop == OACC_ROUTINE_LOP_ERROR)
    {
      gfc_error ("Multiple loop axes specified for routine at %C");
      goto cleanup;
    }
  nohost = c ? c->nohost : false;

  if (isym != NULL)
    {
      /* Diagnose any OpenACC 'routine' directive that doesn't match the
	 (implicit) one with a 'seq' clause.  */
      if (c && (c->gang || c->worker || c->vector))
	{
	  gfc_error ("Intrinsic symbol specified in !$ACC ROUTINE ( NAME )"
		     " at %C marked with incompatible GANG, WORKER, or VECTOR"
		     " clause");
	  goto cleanup;
	}
      /* ..., and no 'nohost' clause.  */
      if (nohost)
	{
	  gfc_error ("Intrinsic symbol specified in !$ACC ROUTINE ( NAME )"
		     " at %C marked with incompatible NOHOST clause");
	  goto cleanup;
	}
    }
  else if (sym != NULL)
    {
      bool add = true;

      /* For a repeated OpenACC 'routine' directive, diagnose if it doesn't
	 match the first one.  */
      for (gfc_oacc_routine_name *n_p = gfc_current_ns->oacc_routine_names;
	   n_p;
	   n_p = n_p->next)
	if (n_p->sym == sym)
	  {
	    add = false;
	    bool nohost_p = n_p->clauses ? n_p->clauses->nohost : false;
	    if (lop != gfc_oacc_routine_lop (n_p->clauses)
		|| nohost != nohost_p)
	      {
		gfc_error ("!$ACC ROUTINE already applied at %C");
		goto cleanup;
	      }
	  }

      if (add)
	{
	  sym->attr.oacc_routine_lop = lop;
	  sym->attr.oacc_routine_nohost = nohost;

	  n = gfc_get_oacc_routine_name ();
	  n->sym = sym;
	  n->clauses = c;
	  n->next = gfc_current_ns->oacc_routine_names;
	  n->loc = old_loc;
	  gfc_current_ns->oacc_routine_names = n;
	}
    }
  else if (gfc_current_ns->proc_name)
    {
      /* For a repeated OpenACC 'routine' directive, diagnose if it doesn't
	 match the first one.  */
      oacc_routine_lop lop_p = gfc_current_ns->proc_name->attr.oacc_routine_lop;
      bool nohost_p = gfc_current_ns->proc_name->attr.oacc_routine_nohost;
      if (lop_p != OACC_ROUTINE_LOP_NONE
	  && (lop != lop_p
	      || nohost != nohost_p))
	{
	  gfc_error ("!$ACC ROUTINE already applied at %C");
	  goto cleanup;
	}

      if (!gfc_add_omp_declare_target (&gfc_current_ns->proc_name->attr,
				       gfc_current_ns->proc_name->name,
				       &old_loc))
	goto cleanup;
      gfc_current_ns->proc_name->attr.oacc_routine_lop = lop;
      gfc_current_ns->proc_name->attr.oacc_routine_nohost = nohost;
    }
  else
    /* Something has gone wrong, possibly a syntax error.  */
    goto cleanup;

  if (gfc_pure (NULL) && c && (c->gang || c->worker || c->vector))
    {
      gfc_error ("!$ACC ROUTINE with GANG, WORKER, or VECTOR clause is not "
		 "permitted in PURE procedure at %C");
      goto cleanup;
    }


  if (n)
    n->clauses = c;
  else if (gfc_current_ns->oacc_routine)
    gfc_current_ns->oacc_routine_clauses = c;

  new_st.op = EXEC_OACC_ROUTINE;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;

cleanup:
  gfc_current_locus = old_loc;
  return MATCH_ERROR;
}


#define OMP_PARALLEL_CLAUSES \
  (omp_mask (OMP_CLAUSE_PRIVATE) | OMP_CLAUSE_FIRSTPRIVATE		\
   | OMP_CLAUSE_SHARED | OMP_CLAUSE_COPYIN | OMP_CLAUSE_REDUCTION	\
   | OMP_CLAUSE_IF | OMP_CLAUSE_NUM_THREADS | OMP_CLAUSE_DEFAULT	\
   | OMP_CLAUSE_PROC_BIND | OMP_CLAUSE_ALLOCATE)
#define OMP_DECLARE_SIMD_CLAUSES \
  (omp_mask (OMP_CLAUSE_SIMDLEN) | OMP_CLAUSE_LINEAR			\
   | OMP_CLAUSE_UNIFORM	| OMP_CLAUSE_ALIGNED | OMP_CLAUSE_INBRANCH	\
   | OMP_CLAUSE_NOTINBRANCH)
#define OMP_DO_CLAUSES \
  (omp_mask (OMP_CLAUSE_PRIVATE) | OMP_CLAUSE_FIRSTPRIVATE		\
   | OMP_CLAUSE_LASTPRIVATE | OMP_CLAUSE_REDUCTION			\
   | OMP_CLAUSE_SCHEDULE | OMP_CLAUSE_ORDERED | OMP_CLAUSE_COLLAPSE	\
   | OMP_CLAUSE_LINEAR | OMP_CLAUSE_ORDER | OMP_CLAUSE_ALLOCATE		\
   | OMP_CLAUSE_NOWAIT)
#define OMP_LOOP_CLAUSES \
  (omp_mask (OMP_CLAUSE_BIND) | OMP_CLAUSE_COLLAPSE | OMP_CLAUSE_ORDER	\
   | OMP_CLAUSE_PRIVATE | OMP_CLAUSE_LASTPRIVATE | OMP_CLAUSE_REDUCTION)

#define OMP_SCOPE_CLAUSES \
  (omp_mask (OMP_CLAUSE_PRIVATE) |OMP_CLAUSE_FIRSTPRIVATE		\
   | OMP_CLAUSE_REDUCTION | OMP_CLAUSE_ALLOCATE | OMP_CLAUSE_NOWAIT)
#define OMP_SECTIONS_CLAUSES \
  (omp_mask (OMP_CLAUSE_PRIVATE) | OMP_CLAUSE_FIRSTPRIVATE		\
   | OMP_CLAUSE_LASTPRIVATE | OMP_CLAUSE_REDUCTION			\
   | OMP_CLAUSE_ALLOCATE | OMP_CLAUSE_NOWAIT)
#define OMP_SIMD_CLAUSES \
  (omp_mask (OMP_CLAUSE_PRIVATE) | OMP_CLAUSE_LASTPRIVATE		\
   | OMP_CLAUSE_REDUCTION | OMP_CLAUSE_COLLAPSE | OMP_CLAUSE_SAFELEN	\
   | OMP_CLAUSE_LINEAR | OMP_CLAUSE_ALIGNED | OMP_CLAUSE_SIMDLEN	\
   | OMP_CLAUSE_IF | OMP_CLAUSE_ORDER | OMP_CLAUSE_NOTEMPORAL)
#define OMP_TASK_CLAUSES \
  (omp_mask (OMP_CLAUSE_PRIVATE) | OMP_CLAUSE_FIRSTPRIVATE		\
   | OMP_CLAUSE_SHARED | OMP_CLAUSE_IF | OMP_CLAUSE_DEFAULT		\
   | OMP_CLAUSE_UNTIED | OMP_CLAUSE_FINAL | OMP_CLAUSE_MERGEABLE	\
   | OMP_CLAUSE_DEPEND | OMP_CLAUSE_PRIORITY | OMP_CLAUSE_IN_REDUCTION	\
   | OMP_CLAUSE_DETACH | OMP_CLAUSE_AFFINITY | OMP_CLAUSE_ALLOCATE)
#define OMP_TASKLOOP_CLAUSES \
  (omp_mask (OMP_CLAUSE_PRIVATE) | OMP_CLAUSE_FIRSTPRIVATE		\
   | OMP_CLAUSE_LASTPRIVATE | OMP_CLAUSE_SHARED | OMP_CLAUSE_IF		\
   | OMP_CLAUSE_DEFAULT | OMP_CLAUSE_UNTIED | OMP_CLAUSE_FINAL		\
   | OMP_CLAUSE_MERGEABLE | OMP_CLAUSE_PRIORITY | OMP_CLAUSE_GRAINSIZE	\
   | OMP_CLAUSE_NUM_TASKS | OMP_CLAUSE_COLLAPSE | OMP_CLAUSE_NOGROUP	\
   | OMP_CLAUSE_REDUCTION | OMP_CLAUSE_IN_REDUCTION | OMP_CLAUSE_ALLOCATE)
#define OMP_TASKGROUP_CLAUSES \
  (omp_mask (OMP_CLAUSE_TASK_REDUCTION) | OMP_CLAUSE_ALLOCATE)
#define OMP_TARGET_CLAUSES \
  (omp_mask (OMP_CLAUSE_DEVICE) | OMP_CLAUSE_MAP | OMP_CLAUSE_IF	\
   | OMP_CLAUSE_DEPEND | OMP_CLAUSE_NOWAIT | OMP_CLAUSE_PRIVATE		\
   | OMP_CLAUSE_FIRSTPRIVATE | OMP_CLAUSE_DEFAULTMAP			\
   | OMP_CLAUSE_IS_DEVICE_PTR | OMP_CLAUSE_IN_REDUCTION			\
   | OMP_CLAUSE_THREAD_LIMIT | OMP_CLAUSE_ALLOCATE			\
   | OMP_CLAUSE_HAS_DEVICE_ADDR | OMP_CLAUSE_USES_ALLOCATORS)
#define OMP_TARGET_DATA_CLAUSES \
  (omp_mask (OMP_CLAUSE_DEVICE) | OMP_CLAUSE_MAP | OMP_CLAUSE_IF	\
   | OMP_CLAUSE_USE_DEVICE_PTR | OMP_CLAUSE_USE_DEVICE_ADDR)
#define OMP_TARGET_ENTER_DATA_CLAUSES \
  (omp_mask (OMP_CLAUSE_DEVICE) | OMP_CLAUSE_MAP | OMP_CLAUSE_IF	\
   | OMP_CLAUSE_DEPEND | OMP_CLAUSE_NOWAIT)
#define OMP_TARGET_EXIT_DATA_CLAUSES \
  (omp_mask (OMP_CLAUSE_DEVICE) | OMP_CLAUSE_MAP | OMP_CLAUSE_IF	\
   | OMP_CLAUSE_DEPEND | OMP_CLAUSE_NOWAIT)
#define OMP_TARGET_UPDATE_CLAUSES \
  (omp_mask (OMP_CLAUSE_DEVICE) | OMP_CLAUSE_IF | OMP_CLAUSE_TO		\
   | OMP_CLAUSE_FROM | OMP_CLAUSE_DEPEND | OMP_CLAUSE_NOWAIT)
#define OMP_TEAMS_CLAUSES \
  (omp_mask (OMP_CLAUSE_NUM_TEAMS) | OMP_CLAUSE_THREAD_LIMIT		\
   | OMP_CLAUSE_DEFAULT | OMP_CLAUSE_PRIVATE | OMP_CLAUSE_FIRSTPRIVATE	\
   | OMP_CLAUSE_SHARED | OMP_CLAUSE_REDUCTION | OMP_CLAUSE_ALLOCATE)
#define OMP_DISTRIBUTE_CLAUSES \
  (omp_mask (OMP_CLAUSE_PRIVATE) | OMP_CLAUSE_FIRSTPRIVATE		\
   | OMP_CLAUSE_LASTPRIVATE | OMP_CLAUSE_COLLAPSE | OMP_CLAUSE_DIST_SCHEDULE \
   | OMP_CLAUSE_ORDER | OMP_CLAUSE_ALLOCATE)
#define OMP_SINGLE_CLAUSES \
  (omp_mask (OMP_CLAUSE_PRIVATE) | OMP_CLAUSE_FIRSTPRIVATE		\
   | OMP_CLAUSE_ALLOCATE | OMP_CLAUSE_NOWAIT | OMP_CLAUSE_COPYPRIVATE)
#define OMP_ORDERED_CLAUSES \
  (omp_mask (OMP_CLAUSE_THREADS) | OMP_CLAUSE_SIMD)
#define OMP_DECLARE_TARGET_CLAUSES \
  (omp_mask (OMP_CLAUSE_ENTER) | OMP_CLAUSE_LINK | OMP_CLAUSE_DEVICE_TYPE \
   | OMP_CLAUSE_TO | OMP_CLAUSE_INDIRECT)
#define OMP_ATOMIC_CLAUSES \
  (omp_mask (OMP_CLAUSE_ATOMIC) | OMP_CLAUSE_CAPTURE | OMP_CLAUSE_HINT	\
   | OMP_CLAUSE_MEMORDER | OMP_CLAUSE_COMPARE | OMP_CLAUSE_FAIL 	\
   | OMP_CLAUSE_WEAK)
#define OMP_MASKED_CLAUSES \
  (omp_mask (OMP_CLAUSE_FILTER))
#define OMP_ERROR_CLAUSES \
  (omp_mask (OMP_CLAUSE_AT) | OMP_CLAUSE_MESSAGE | OMP_CLAUSE_SEVERITY)
#define OMP_WORKSHARE_CLAUSES \
  omp_mask (OMP_CLAUSE_NOWAIT)
#define OMP_UNROLL_CLAUSES \
  (omp_mask (OMP_CLAUSE_FULL) | OMP_CLAUSE_PARTIAL)
#define OMP_TILE_CLAUSES \
  (omp_mask (OMP_CLAUSE_SIZES))
#define OMP_ALLOCATORS_CLAUSES \
  omp_mask (OMP_CLAUSE_ALLOCATE)
#define OMP_INTEROP_CLAUSES \
  (omp_mask (OMP_CLAUSE_DEPEND) | OMP_CLAUSE_NOWAIT | OMP_CLAUSE_DEVICE \
   | OMP_CLAUSE_INIT | OMP_CLAUSE_DESTROY | OMP_CLAUSE_USE)
#define OMP_DISPATCH_CLAUSES                                                   \
  (omp_mask (OMP_CLAUSE_DEVICE) | OMP_CLAUSE_DEPEND | OMP_CLAUSE_NOVARIANTS    \
   | OMP_CLAUSE_NOCONTEXT | OMP_CLAUSE_IS_DEVICE_PTR | OMP_CLAUSE_NOWAIT       \
   | OMP_CLAUSE_HAS_DEVICE_ADDR | OMP_CLAUSE_INTEROP)


static match
match_omp (gfc_exec_op op, const omp_mask mask)
{
  gfc_omp_clauses *c;
  if (gfc_match_omp_clauses (&c, mask, true, true, false,
			     op == EXEC_OMP_TARGET) != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = op;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}

/* Handles both declarative and (deprecated) executable ALLOCATE directive;
   accepts optional list (for executable) and common blocks.
   If no variables have been provided, the single omp namelist has sym == NULL.

   Note that the executable ALLOCATE directive permits structure elements only
   in OpenMP 5.0 and 5.1 but not longer in 5.2.  See also the comment on the
   'omp allocators' directive below. The accidental change was reverted for
   OpenMP TR12, permitting them again. See also gfc_match_omp_allocators.

   Hence, structure elements are rejected for now, also to make resolving
   OMP_LIST_ALLOCATE simpler (check for duplicates, same symbol in
   Fortran allocate stmt).  TODO: Permit structure elements.  */

match
gfc_match_omp_allocate (void)
{
  match m;
  bool first = true;
  gfc_omp_namelist *vars = NULL;
  gfc_expr *align = NULL;
  gfc_expr *allocator = NULL;
  locus loc = gfc_current_locus;

  m = gfc_match_omp_variable_list (" (", &vars, true, NULL, NULL, true, true,
				   NULL, true);

  if (m == MATCH_ERROR)
    return m;

  while (true)
    {
      gfc_gobble_whitespace ();
      if (gfc_match_omp_eos () == MATCH_YES)
	break;
      if (!first)
	gfc_match (", ");
      first = false;
      if ((m = gfc_match_dupl_check (!align, "align", true, &align))
	  != MATCH_NO)
	{
	  if (m == MATCH_ERROR)
	    goto error;
	  continue;
	}
      if ((m = gfc_match_dupl_check (!allocator, "allocator",
				     true, &allocator)) != MATCH_NO)
	{
	  if (m == MATCH_ERROR)
	    goto error;
	  continue;
	}
      gfc_error ("Expected ALIGN or ALLOCATOR clause at %C");
      return MATCH_ERROR;
    }
  for (gfc_omp_namelist *n = vars; n; n = n->next)
    if (n->expr)
      {
	if ((n->expr->ref && n->expr->ref->type == REF_COMPONENT)
	    || (n->expr->ref->next && n->expr->ref->type == REF_COMPONENT))
	  gfc_error ("Sorry, structure-element list item at %L in ALLOCATE "
		     "directive is not yet supported", &n->expr->where);
	else
	  gfc_error ("Unexpected expression as list item at %L in ALLOCATE "
		     "directive", &n->expr->where);

	gfc_free_omp_namelist (vars, false, true, false, false);
	goto error;
      }

  new_st.op = EXEC_OMP_ALLOCATE;
  new_st.ext.omp_clauses = gfc_get_omp_clauses ();
  if (vars == NULL)
    {
      vars = gfc_get_omp_namelist ();
      vars->where = loc;
      vars->u.align = align;
      vars->u2.allocator = allocator;
      new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE] = vars;
    }
  else
    {
      new_st.ext.omp_clauses->lists[OMP_LIST_ALLOCATE] = vars;
      for (; vars; vars = vars->next)
	{
	  vars->u.align = (align) ? gfc_copy_expr (align) : NULL;
	  vars->u2.allocator = allocator;
	}
      gfc_free_expr (align);
    }
  return MATCH_YES;

error:
  gfc_free_expr (align);
  gfc_free_expr (allocator);
  return MATCH_ERROR;
}

/* In line with OpenMP 5.2 derived-type components are rejected.
   See also comment before gfc_match_omp_allocate.  */

match
gfc_match_omp_allocators (void)
{
  return match_omp (EXEC_OMP_ALLOCATORS, OMP_ALLOCATORS_CLAUSES);
}


match
gfc_match_omp_assume (void)
{
  gfc_omp_clauses *c;
  locus loc = gfc_current_locus;
  if ((gfc_match_omp_clauses (&c, omp_mask (OMP_CLAUSE_ASSUMPTIONS))
       != MATCH_YES)
      || (omp_verify_merge_absent_contains (ST_OMP_ASSUME, c->assume, NULL,
					    &loc) != MATCH_YES))
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_ASSUME;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


match
gfc_match_omp_assumes (void)
{
  gfc_omp_clauses *c;
  locus loc = gfc_current_locus;
  if (!gfc_current_ns->proc_name
      || (gfc_current_ns->proc_name->attr.flavor != FL_MODULE
	  && !gfc_current_ns->proc_name->attr.subroutine
	  && !gfc_current_ns->proc_name->attr.function))
    {
      gfc_error ("!$OMP ASSUMES at %C must be in the specification part of a "
		 "subprogram or module");
      return MATCH_ERROR;
    }
  if ((gfc_match_omp_clauses (&c, omp_mask (OMP_CLAUSE_ASSUMPTIONS))
       != MATCH_YES)
      || (omp_verify_merge_absent_contains (ST_OMP_ASSUMES, c->assume,
					    gfc_current_ns->omp_assumes, &loc)
	  != MATCH_YES))
    return MATCH_ERROR;
  if (gfc_current_ns->omp_assumes == NULL)
    {
      gfc_current_ns->omp_assumes = c->assume;
      c->assume = NULL;
    }
  else if (gfc_current_ns->omp_assumes && c->assume)
    {
      gfc_current_ns->omp_assumes->no_openmp |= c->assume->no_openmp;
      gfc_current_ns->omp_assumes->no_openmp_routines
	|= c->assume->no_openmp_routines;
      gfc_current_ns->omp_assumes->no_parallelism |= c->assume->no_parallelism;
      if (gfc_current_ns->omp_assumes->holds && c->assume->holds)
	{
	  gfc_expr_list *el = gfc_current_ns->omp_assumes->holds;
	  for ( ; el->next ; el = el->next)
	    ;
	  el->next = c->assume->holds;
	}
      else if (c->assume->holds)
	gfc_current_ns->omp_assumes->holds = c->assume->holds;
      c->assume->holds = NULL;
    }
  gfc_free_omp_clauses (c);
  return MATCH_YES;
}


match
gfc_match_omp_critical (void)
{
  char n[GFC_MAX_SYMBOL_LEN+1];
  gfc_omp_clauses *c = NULL;

  if (gfc_match (" ( %n )", n) != MATCH_YES)
    n[0] = '\0';

  if (gfc_match_omp_clauses (&c, omp_mask (OMP_CLAUSE_HINT),
			     /* first = */ n[0] == '\0') != MATCH_YES)
    return MATCH_ERROR;

  new_st.op = EXEC_OMP_CRITICAL;
  new_st.ext.omp_clauses = c;
  if (n[0])
    c->critical_name = xstrdup (n);
  return MATCH_YES;
}


match
gfc_match_omp_end_critical (void)
{
  char n[GFC_MAX_SYMBOL_LEN+1];

  if (gfc_match (" ( %n )", n) != MATCH_YES)
    n[0] = '\0';
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after $OMP CRITICAL statement at %C");
      return MATCH_ERROR;
    }

  new_st.op = EXEC_OMP_END_CRITICAL;
  new_st.ext.omp_name = n[0] ? xstrdup (n) : NULL;
  return MATCH_YES;
}

/* depobj(depobj) depend(dep-type:loc)|destroy|update(dep-type)
   dep-type = in/out/inout/mutexinoutset/depobj/source/sink
   depend: !source, !sink
   update: !source, !sink, !depobj
   locator = exactly one list item  .*/
match
gfc_match_omp_depobj (void)
{
  gfc_omp_clauses *c = NULL;
  gfc_expr *depobj;

  if (gfc_match (" ( %v ) ", &depobj) != MATCH_YES)
    {
      gfc_error ("Expected %<( depobj )%> at %C");
      return MATCH_ERROR;
    }
  if (gfc_match ("update ( ") == MATCH_YES)
    {
      c = gfc_get_omp_clauses ();
      if (gfc_match ("inoutset )") == MATCH_YES)
	c->depobj_update = OMP_DEPEND_INOUTSET;
      else if (gfc_match ("inout )") == MATCH_YES)
	c->depobj_update = OMP_DEPEND_INOUT;
      else if (gfc_match ("in )") == MATCH_YES)
	c->depobj_update = OMP_DEPEND_IN;
      else if (gfc_match ("out )") == MATCH_YES)
	c->depobj_update = OMP_DEPEND_OUT;
      else if (gfc_match ("mutexinoutset )") == MATCH_YES)
	c->depobj_update = OMP_DEPEND_MUTEXINOUTSET;
      else
	{
	  gfc_error ("Expected IN, OUT, INOUT, INOUTSET or MUTEXINOUTSET "
		     "followed by %<)%> at %C");
	  goto error;
	}
    }
  else if (gfc_match ("destroy ") == MATCH_YES)
    {
      gfc_expr *destroyobj = NULL;
      c = gfc_get_omp_clauses ();
      c->destroy = true;

      if (gfc_match (" ( %v ) ", &destroyobj) == MATCH_YES)
	{
	  if (destroyobj->symtree != depobj->symtree)
	    gfc_warning (OPT_Wopenmp, "The same depend object should be used as"
			 " DEPOBJ argument at %L and as DESTROY argument at %L",
			 &depobj->where, &destroyobj->where);
	  gfc_free_expr (destroyobj);
	}
    }
  else if (gfc_match_omp_clauses (&c, omp_mask (OMP_CLAUSE_DEPEND), true, false)
	   != MATCH_YES)
    goto error;

  if (c->depobj_update == OMP_DEPEND_UNSET && !c->destroy)
    {
      if (!c->doacross_source && !c->lists[OMP_LIST_DEPEND])
	{
	  gfc_error ("Expected DEPEND, UPDATE, or DESTROY clause at %C");
	  goto error;
	}
      if (c->lists[OMP_LIST_DEPEND]->u.depend_doacross_op == OMP_DEPEND_DEPOBJ)
	{
	  gfc_error ("DEPEND clause at %L of OMP DEPOBJ construct shall not "
		     "have dependence-type DEPOBJ",
		     c->lists[OMP_LIST_DEPEND]
		     ? &c->lists[OMP_LIST_DEPEND]->where : &gfc_current_locus);
	  goto error;
	}
      if (c->lists[OMP_LIST_DEPEND]->next)
	{
	  gfc_error ("DEPEND clause at %L of OMP DEPOBJ construct shall have "
		     "only a single locator",
		     &c->lists[OMP_LIST_DEPEND]->next->where);
	  goto error;
	}
    }

  c->depobj = depobj;
  new_st.op = EXEC_OMP_DEPOBJ;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;

error:
  gfc_free_expr (depobj);
  gfc_free_omp_clauses (c);
  return MATCH_ERROR;
}

match
gfc_match_omp_dispatch (void)
{
  return match_omp (EXEC_OMP_DISPATCH, OMP_DISPATCH_CLAUSES);
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
		    (OMP_DISTRIBUTE_CLAUSES | OMP_PARALLEL_CLAUSES
		     | OMP_DO_CLAUSES)
		    & ~(omp_mask (OMP_CLAUSE_ORDERED)
			| OMP_CLAUSE_LINEAR | OMP_CLAUSE_NOWAIT));
}


match
gfc_match_omp_distribute_parallel_do_simd (void)
{
  return match_omp (EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD,
		    (OMP_DISTRIBUTE_CLAUSES | OMP_PARALLEL_CLAUSES
		     | OMP_DO_CLAUSES | OMP_SIMD_CLAUSES)
		    & ~(omp_mask (OMP_CLAUSE_ORDERED) | OMP_CLAUSE_NOWAIT));
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
  return match_omp (EXEC_OMP_DO_SIMD, OMP_DO_CLAUSES | OMP_SIMD_CLAUSES);
}


match
gfc_match_omp_loop (void)
{
  return match_omp (EXEC_OMP_LOOP, OMP_LOOP_CLAUSES);
}


match
gfc_match_omp_teams_loop (void)
{
  return match_omp (EXEC_OMP_TEAMS_LOOP, OMP_TEAMS_CLAUSES | OMP_LOOP_CLAUSES);
}


match
gfc_match_omp_target_teams_loop (void)
{
  return match_omp (EXEC_OMP_TARGET_TEAMS_LOOP,
		    OMP_TARGET_CLAUSES | OMP_TEAMS_CLAUSES | OMP_LOOP_CLAUSES);
}


match
gfc_match_omp_parallel_loop (void)
{
  return match_omp (EXEC_OMP_PARALLEL_LOOP,
		    OMP_PARALLEL_CLAUSES | OMP_LOOP_CLAUSES);
}


match
gfc_match_omp_target_parallel_loop (void)
{
  return match_omp (EXEC_OMP_TARGET_PARALLEL_LOOP,
		    (OMP_TARGET_CLAUSES | OMP_PARALLEL_CLAUSES
		     | OMP_LOOP_CLAUSES));
}


match
gfc_match_omp_error (void)
{
  locus loc = gfc_current_locus;
  match m = match_omp (EXEC_OMP_ERROR, OMP_ERROR_CLAUSES);
  if (m != MATCH_YES)
    return m;

  gfc_omp_clauses *c = new_st.ext.omp_clauses;
  if (c->severity == OMP_SEVERITY_UNSET)
    c->severity = OMP_SEVERITY_FATAL;
  if (new_st.ext.omp_clauses->at == OMP_AT_EXECUTION)
    return MATCH_YES;
  if (c->message
      && (!gfc_resolve_expr (c->message)
	  || c->message->ts.type != BT_CHARACTER
	  || c->message->ts.kind != gfc_default_character_kind
	  || c->message->rank != 0))
    {
      gfc_error ("MESSAGE clause at %L requires a scalar default-kind "
		   "CHARACTER expression",
		 &new_st.ext.omp_clauses->message->where);
      return MATCH_ERROR;
    }
  if (c->message && !gfc_is_constant_expr (c->message))
    {
      gfc_error ("Constant character expression required in MESSAGE clause "
		 "at %L", &new_st.ext.omp_clauses->message->where);
      return MATCH_ERROR;
    }
  if (c->message)
    {
      const char *msg = G_("$OMP ERROR encountered at %L: %s");
      gcc_assert (c->message->expr_type == EXPR_CONSTANT);
      gfc_charlen_t slen = c->message->value.character.length;
      int i = gfc_validate_kind (BT_CHARACTER, gfc_default_character_kind,
				 false);
      size_t size = slen * gfc_character_kinds[i].bit_size / 8;
      unsigned char *s = XCNEWVAR (unsigned char, size + 1);
      gfc_encode_character (gfc_default_character_kind, slen,
			    c->message->value.character.string,
			    (unsigned char *) s, size);
      s[size] = '\0';
      if (c->severity == OMP_SEVERITY_WARNING)
	gfc_warning_now (0, msg, &loc, s);
      else
	gfc_error_now (msg, &loc, s);
      free (s);
    }
  else
    {
      const char *msg = G_("$OMP ERROR encountered at %L");
      if (c->severity == OMP_SEVERITY_WARNING)
	gfc_warning_now (0, msg, &loc);
      else
	gfc_error_now (msg, &loc);
    }
  return MATCH_YES;
}

match
gfc_match_omp_flush (void)
{
  gfc_omp_namelist *list = NULL;
  gfc_omp_clauses *c = NULL;
  gfc_gobble_whitespace ();
  enum gfc_omp_memorder mo = OMP_MEMORDER_UNSET;
  if (gfc_match_omp_eos () == MATCH_NO && gfc_peek_ascii_char () != '(')
    {
      if (gfc_match ("seq_cst") == MATCH_YES)
	mo = OMP_MEMORDER_SEQ_CST;
      else if (gfc_match ("acq_rel") == MATCH_YES)
	mo = OMP_MEMORDER_ACQ_REL;
      else if (gfc_match ("release") == MATCH_YES)
	mo = OMP_MEMORDER_RELEASE;
      else if (gfc_match ("acquire") == MATCH_YES)
	mo = OMP_MEMORDER_ACQUIRE;
      else
	{
	  gfc_error ("Expected SEQ_CST, AQC_REL, RELEASE, or ACQUIRE at %C");
	  return MATCH_ERROR;
	}
      c = gfc_get_omp_clauses ();
      c->memorder = mo;
    }
  gfc_match_omp_variable_list (" (", &list, true);
  if (list && mo != OMP_MEMORDER_UNSET)
    {
      gfc_error ("List specified together with memory order clause in FLUSH "
		 "directive at %C");
      gfc_free_omp_namelist (list, false, false, false, false);
      gfc_free_omp_clauses (c);
      return MATCH_ERROR;
    }
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after $OMP FLUSH statement at %C");
      gfc_free_omp_namelist (list, false, false, false, false);
      gfc_free_omp_clauses (c);
      return MATCH_ERROR;
    }
  new_st.op = EXEC_OMP_FLUSH;
  new_st.ext.omp_namelist = list;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


match
gfc_match_omp_declare_simd (void)
{
  locus where = gfc_current_locus;
  gfc_symbol *proc_name;
  gfc_omp_clauses *c;
  gfc_omp_declare_simd *ods;
  bool needs_space = false;

  switch (gfc_match (" ( "))
    {
    case MATCH_YES:
      if (gfc_match_symbol (&proc_name, /* host assoc = */ true) != MATCH_YES
	  || gfc_match (" ) ") != MATCH_YES)
	return MATCH_ERROR;
      break;
    case MATCH_NO: proc_name = NULL; needs_space = true; break;
    case MATCH_ERROR: return MATCH_ERROR;
    }

  if (gfc_match_omp_clauses (&c, OMP_DECLARE_SIMD_CLAUSES, true,
			     needs_space) != MATCH_YES)
    return MATCH_ERROR;

  if (gfc_current_ns->is_block_data)
    {
      gfc_free_omp_clauses (c);
      return MATCH_YES;
    }

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
	    && (ts->type == BT_DERIVED || ts->type == BT_CLASS)))
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
      omp_udr->name = gfc_get_string ("%s", name);
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
  match m;
  gfc_omp_clauses *c = NULL;
  int list;
  gfc_omp_namelist *n;
  gfc_symbol *s;

  old_loc = gfc_current_locus;

  if (gfc_current_ns->proc_name
      && gfc_match_omp_eos () == MATCH_YES)
    {
      if (!gfc_add_omp_declare_target (&gfc_current_ns->proc_name->attr,
				       gfc_current_ns->proc_name->name,
				       &old_loc))
	goto cleanup;
      return MATCH_YES;
    }

  if (gfc_current_ns->proc_name
      && gfc_current_ns->proc_name->attr.if_source == IFSRC_IFBODY)
    {
      gfc_error ("Only the !$OMP DECLARE TARGET form without "
		 "clauses is allowed in interface block at %C");
      goto cleanup;
    }

  m = gfc_match (" (");
  if (m == MATCH_YES)
    {
      c = gfc_get_omp_clauses ();
      gfc_current_locus = old_loc;
      m = gfc_match_omp_to_link (" (", &c->lists[OMP_LIST_ENTER]);
      if (m != MATCH_YES)
	goto syntax;
      if (gfc_match_omp_eos () != MATCH_YES)
	{
	  gfc_error ("Unexpected junk after !$OMP DECLARE TARGET at %C");
	  goto cleanup;
	}
    }
  else if (gfc_match_omp_clauses (&c, OMP_DECLARE_TARGET_CLAUSES) != MATCH_YES)
    return MATCH_ERROR;

  gfc_buffer_error (false);

  static const int to_enter_link_lists[]
    = { OMP_LIST_TO, OMP_LIST_ENTER, OMP_LIST_LINK };
  for (size_t listn = 0; listn < ARRAY_SIZE (to_enter_link_lists)
			 && (list = to_enter_link_lists[listn], true); ++listn)
    for (n = c->lists[list]; n; n = n->next)
      if (n->sym)
	n->sym->mark = 0;
      else if (n->u.common->head)
	n->u.common->head->mark = 0;

  for (size_t listn = 0; listn < ARRAY_SIZE (to_enter_link_lists)
			 && (list = to_enter_link_lists[listn], true); ++listn)
    for (n = c->lists[list]; n; n = n->next)
      if (n->sym)
	{
	  if (n->sym->attr.in_common)
	    gfc_error_now ("OMP DECLARE TARGET variable at %L is an "
			   "element of a COMMON block", &n->where);
	  else if (n->sym->mark)
	    gfc_error_now ("Variable at %L mentioned multiple times in "
			   "clauses of the same OMP DECLARE TARGET directive",
			   &n->where);
	  else if (n->sym->attr.omp_declare_target
		   && n->sym->attr.omp_declare_target_link
		   && list != OMP_LIST_LINK)
	    gfc_error_now ("OMP DECLARE TARGET variable at %L previously "
			   "mentioned in LINK clause and later in %s clause",
			   &n->where, list == OMP_LIST_TO ? "TO" : "ENTER");
	  else if (n->sym->attr.omp_declare_target
		   && !n->sym->attr.omp_declare_target_link
		   && list == OMP_LIST_LINK)
	    gfc_error_now ("OMP DECLARE TARGET variable at %L previously "
			   "mentioned in TO or ENTER clause and later in "
			   "LINK clause", &n->where);
	  else if (gfc_add_omp_declare_target (&n->sym->attr, n->sym->name,
					       &n->sym->declared_at))
	    {
	      if (list == OMP_LIST_LINK)
		gfc_add_omp_declare_target_link (&n->sym->attr, n->sym->name,
						 &n->sym->declared_at);
	    }
	  if (c->device_type != OMP_DEVICE_TYPE_UNSET)
	    {
	      if (n->sym->attr.omp_device_type != OMP_DEVICE_TYPE_UNSET
		  && n->sym->attr.omp_device_type != c->device_type)
		gfc_error_now ("List item %qs at %L set in previous OMP DECLARE "
			       "TARGET directive to a different DEVICE_TYPE",
			       n->sym->name, &n->where);
	      n->sym->attr.omp_device_type = c->device_type;
	    }
	  if (c->indirect)
	    {
	      if (n->sym->attr.omp_device_type != OMP_DEVICE_TYPE_UNSET
		  && n->sym->attr.omp_device_type != OMP_DEVICE_TYPE_ANY)
		gfc_error_now ("DEVICE_TYPE must be ANY when used with "
			       "INDIRECT at %L", &n->where);
	      n->sym->attr.omp_declare_target_indirect = c->indirect;
	    }

	  n->sym->mark = 1;
	}
      else if (n->u.common->omp_declare_target
	       && n->u.common->omp_declare_target_link
	       && list != OMP_LIST_LINK)
	gfc_error_now ("OMP DECLARE TARGET COMMON at %L previously "
		       "mentioned in LINK clause and later in %s clause",
		       &n->where, list == OMP_LIST_TO ? "TO" : "ENTER");
      else if (n->u.common->omp_declare_target
	       && !n->u.common->omp_declare_target_link
	       && list == OMP_LIST_LINK)
	gfc_error_now ("OMP DECLARE TARGET COMMON at %L previously "
		       "mentioned in TO or ENTER clause and later in "
		       "LINK clause", &n->where);
      else if (n->u.common->head && n->u.common->head->mark)
	gfc_error_now ("COMMON at %L mentioned multiple times in "
		       "clauses of the same OMP DECLARE TARGET directive",
		       &n->where);
      else
	{
	  n->u.common->omp_declare_target = 1;
	  n->u.common->omp_declare_target_link = (list == OMP_LIST_LINK);
	  if (n->u.common->omp_device_type != OMP_DEVICE_TYPE_UNSET
	      && n->u.common->omp_device_type != c->device_type)
	    gfc_error_now ("COMMON at %L set in previous OMP DECLARE "
			   "TARGET directive to a different DEVICE_TYPE",
			   &n->where);
	  n->u.common->omp_device_type = c->device_type;

	  for (s = n->u.common->head; s; s = s->common_next)
	    {
	      s->mark = 1;
	      if (gfc_add_omp_declare_target (&s->attr, s->name,
					      &s->declared_at))
		{
		  if (list == OMP_LIST_LINK)
		    gfc_add_omp_declare_target_link (&s->attr, s->name,
						     &s->declared_at);
		}
	      if (s->attr.omp_device_type != OMP_DEVICE_TYPE_UNSET
		  && s->attr.omp_device_type != c->device_type)
		gfc_error_now ("List item %qs at %L set in previous OMP DECLARE"
			       " TARGET directive to a different DEVICE_TYPE",
			       s->name, &n->where);
	      s->attr.omp_device_type = c->device_type;

	      if (c->indirect
		  && s->attr.omp_device_type != OMP_DEVICE_TYPE_UNSET
		  && s->attr.omp_device_type != OMP_DEVICE_TYPE_ANY)
		gfc_error_now ("DEVICE_TYPE must be ANY when used with "
			       "INDIRECT at %L", &n->where);
	      s->attr.omp_declare_target_indirect = c->indirect;
	    }
	}
  if ((c->device_type || c->indirect)
      && !c->lists[OMP_LIST_ENTER]
      && !c->lists[OMP_LIST_TO]
      && !c->lists[OMP_LIST_LINK])
    gfc_warning_now (OPT_Wopenmp,
		     "OMP DECLARE TARGET directive at %L with only "
		     "DEVICE_TYPE or INDIRECT clauses is ignored",
		     &old_loc);

  gfc_buffer_error (true);

  if (c)
    gfc_free_omp_clauses (c);
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in !$OMP DECLARE TARGET list at %C");

cleanup:
  gfc_current_locus = old_loc;
  if (c)
    gfc_free_omp_clauses (c);
  return MATCH_ERROR;
}

/* Skip over and ignore trait-property-extensions.

   trait-property-extension :
     trait-property-name
     identifier (trait-property-extension[, trait-property-extension[, ...]])
     constant integer expression
 */

static match gfc_ignore_trait_property_extension_list (void);

static match
gfc_ignore_trait_property_extension (void)
{
  char buf[GFC_MAX_SYMBOL_LEN + 1];
  gfc_expr *expr;

  /* Identifier form of trait-property name, possibly followed by
     a list of (recursive) trait-property-extensions.  */
  if (gfc_match_name (buf) == MATCH_YES)
    {
      if (gfc_match (" (") == MATCH_YES)
	return gfc_ignore_trait_property_extension_list ();
      return MATCH_YES;
    }

  /* Literal constant.  */
  if (gfc_match_literal_constant (&expr, 0) == MATCH_YES)
    return MATCH_YES;

  /* FIXME: constant integer expressions.  */
  gfc_error ("Expected trait-property-extension at %C");
  return MATCH_ERROR;
}

static match
gfc_ignore_trait_property_extension_list (void)
{
  while (1)
    {
      if (gfc_ignore_trait_property_extension () != MATCH_YES)
	return MATCH_ERROR;
      if (gfc_match (" ,") == MATCH_YES)
	continue;
      if (gfc_match (" )") == MATCH_YES)
	return MATCH_YES;
      gfc_error ("expected %<)%> at %C");
      return MATCH_ERROR;
    }
}


match
gfc_match_omp_interop (void)
{
  return match_omp (EXEC_OMP_INTEROP, OMP_INTEROP_CLAUSES);
}


/* OpenMP 5.0:

   trait-selector:
     trait-selector-name[([trait-score:]trait-property[,trait-property[,...]])]

   trait-score:
     score(score-expression)  */

match
gfc_match_omp_context_selector (gfc_omp_set_selector *oss,
				bool metadirective_p)
{
  do
    {
      char selector[GFC_MAX_SYMBOL_LEN + 1];

      if (gfc_match_name (selector) != MATCH_YES)
	{
	  gfc_error ("expected trait selector name at %C");
	  return MATCH_ERROR;
	}

      gfc_omp_selector *os = gfc_get_omp_selector ();
      if (oss->code == OMP_TRAIT_SET_CONSTRUCT
	  && !strcmp (selector, "do"))
	os->code = OMP_TRAIT_CONSTRUCT_FOR;
      else if (oss->code == OMP_TRAIT_SET_CONSTRUCT
	       && !strcmp (selector, "for"))
	os->code = OMP_TRAIT_INVALID;
      else
	os->code = omp_lookup_ts_code (oss->code, selector);
      os->next = oss->trait_selectors;
      oss->trait_selectors = os;

      if (os->code == OMP_TRAIT_INVALID)
	{
	  gfc_warning (OPT_Wopenmp,
		       "unknown selector %qs for context selector set %qs "
		       "at %C",
		       selector, omp_tss_map[oss->code]);
	  if (gfc_match (" (") == MATCH_YES
	      && gfc_ignore_trait_property_extension_list () != MATCH_YES)
	    return MATCH_ERROR;
	  if (gfc_match (" ,") == MATCH_YES)
	    continue;
	  break;
	}

      enum omp_tp_type property_kind = omp_ts_map[os->code].tp_type;
      bool allow_score = omp_ts_map[os->code].allow_score;

      if (gfc_match (" (") == MATCH_YES)
	{
	  if (property_kind == OMP_TRAIT_PROPERTY_NONE)
	    {
	      gfc_error ("selector %qs does not accept any properties at %C",
			 selector);
	      return MATCH_ERROR;
	    }

	  if (gfc_match (" score") == MATCH_YES)
	    {
	      if (!allow_score)
		{
		  gfc_error ("%<score%> cannot be specified in traits "
			     "in the %qs trait-selector-set at %C",
			     omp_tss_map[oss->code]);
		  return MATCH_ERROR;
		}
	      if (gfc_match (" (") != MATCH_YES)
		{
		  gfc_error ("expected %<(%> at %C");
		  return MATCH_ERROR;
		}
	      if (gfc_match_expr (&os->score) != MATCH_YES
		  || !gfc_resolve_expr (os->score)
		  || os->score->ts.type != BT_INTEGER
		  || os->score->rank != 0)
		{
		  gfc_error ("%<score%> argument must be constant integer "
			     "expression at %C");
		  return MATCH_ERROR;
		}

	      if (os->score->expr_type == EXPR_CONSTANT
		  && mpz_sgn (os->score->value.integer) < 0)
		{
		  gfc_error ("%<score%> argument must be non-negative at %C");
		  return MATCH_ERROR;
		}

	      if (gfc_match (" )") != MATCH_YES)
		{
		  gfc_error ("expected %<)%> at %C");
		  return MATCH_ERROR;
		}

	      if (gfc_match (" :") != MATCH_YES)
		{
		  gfc_error ("expected : at %C");
		  return MATCH_ERROR;
		}
	    }

	  gfc_omp_trait_property *otp = gfc_get_omp_trait_property ();
	  otp->property_kind = property_kind;
	  otp->next = os->properties;
	  os->properties = otp;

	  switch (property_kind)
	    {
	    case OMP_TRAIT_PROPERTY_ID:
	      {
		char buf[GFC_MAX_SYMBOL_LEN + 1];
		if (gfc_match_name (buf) == MATCH_YES)
		  {
		    otp->name = XNEWVEC (char, strlen (buf) + 1);
		    strcpy (otp->name, buf);
		  }
		else
		  {
		    gfc_error ("expected identifier at %C");
		    return MATCH_ERROR;
		  }
	      }
	      break;
	    case OMP_TRAIT_PROPERTY_NAME_LIST:
	      do
		{
		  char buf[GFC_MAX_SYMBOL_LEN + 1];
		  if (gfc_match_name (buf) == MATCH_YES)
		    {
		      otp->name = XNEWVEC (char, strlen (buf) + 1);
		      strcpy (otp->name, buf);
		      otp->is_name = true;
		    }
		  else if (gfc_match_literal_constant (&otp->expr, 0)
			   != MATCH_YES
			   || otp->expr->ts.type != BT_CHARACTER)
		    {
		      gfc_error ("expected identifier or string literal "
				 "at %C");
		      return MATCH_ERROR;
		    }

		  if (gfc_match (" ,") == MATCH_YES)
		    {
		      otp = gfc_get_omp_trait_property ();
		      otp->property_kind = property_kind;
		      otp->next = os->properties;
		      os->properties = otp;
		    }
		  else
		    break;
		}
	      while (1);
	      break;
	    case OMP_TRAIT_PROPERTY_DEV_NUM_EXPR:
	    case OMP_TRAIT_PROPERTY_BOOL_EXPR:
	      if (gfc_match_expr (&otp->expr) != MATCH_YES)
		{
		  gfc_error ("expected expression at %C");
		  return MATCH_ERROR;
		}
	      if (!gfc_resolve_expr (otp->expr)
		  || (property_kind == OMP_TRAIT_PROPERTY_BOOL_EXPR
		      && otp->expr->ts.type != BT_LOGICAL)
		  || (property_kind == OMP_TRAIT_PROPERTY_DEV_NUM_EXPR
		      && otp->expr->ts.type != BT_INTEGER)
		  || otp->expr->rank != 0
		  || (!metadirective_p
		      && otp->expr->expr_type != EXPR_CONSTANT))
		{
		  if (metadirective_p)
		    {
		      if (property_kind == OMP_TRAIT_PROPERTY_BOOL_EXPR)
			gfc_error ("property must be a "
				   "logical expression at %L",
				   &otp->expr->where);
		      else
			gfc_error ("property must be an "
				   "integer expression at %L",
				   &otp->expr->where);
		    }
		  else
		    {
		      if (property_kind == OMP_TRAIT_PROPERTY_BOOL_EXPR)
			gfc_error ("property must be a constant "
				   "logical expression at %L",
				   &otp->expr->where);
		      else
			gfc_error ("property must be a constant "
				   "integer expression at %L",
				   &otp->expr->where);
		    }
		  return MATCH_ERROR;
		}
	      /* Device number must be conforming, which includes
		 omp_initial_device (-1) and omp_invalid_device (-4).  */
	      if (property_kind == OMP_TRAIT_PROPERTY_DEV_NUM_EXPR
		  && otp->expr->expr_type == EXPR_CONSTANT
		  && mpz_sgn (otp->expr->value.integer) < 0
		  && mpz_cmp_si (otp->expr->value.integer, -1) != 0
		  && mpz_cmp_si (otp->expr->value.integer, -4) != 0)
		{
		  gfc_error ("property must be a conforming device number "
			     "at %C");
		  return MATCH_ERROR;
		}
	      break;
	    case OMP_TRAIT_PROPERTY_CLAUSE_LIST:
	      {
		if (os->code == OMP_TRAIT_CONSTRUCT_SIMD)
		  {
		    gfc_matching_omp_context_selector = true;
		    if (gfc_match_omp_clauses (&otp->clauses,
					       OMP_DECLARE_SIMD_CLAUSES,
					       true, false, false)
			!= MATCH_YES)
		      {
			gfc_matching_omp_context_selector = false;
			gfc_error ("expected simd clause at %C");
			return MATCH_ERROR;
		      }
		    gfc_matching_omp_context_selector = false;
		  }
		else if (os->code == OMP_TRAIT_IMPLEMENTATION_REQUIRES)
		  {
		    /* FIXME: The "requires" selector was added in OpenMP 5.1.
		       Currently only the now-deprecated syntax
		       from OpenMP 5.0 is supported.  */
		    sorry_at (gfc_get_location (&gfc_current_locus),
			      "%<requires%> selector is not supported yet");
		    return MATCH_ERROR;
		  }
		else
		  gcc_unreachable ();
		break;
	      }
	    default:
	      gcc_unreachable ();
	    }

	  if (gfc_match (" )") != MATCH_YES)
	    {
	      gfc_error ("expected %<)%> at %C");
	      return MATCH_ERROR;
	    }
	}
      else if (property_kind != OMP_TRAIT_PROPERTY_NONE
	       && property_kind != OMP_TRAIT_PROPERTY_CLAUSE_LIST
	       && property_kind != OMP_TRAIT_PROPERTY_EXTENSION)
	{
	  if (gfc_match (" (") != MATCH_YES)
	    {
	      gfc_error ("expected %<(%> at %C");
	      return MATCH_ERROR;
	    }
	}

      if (gfc_match (" ,") != MATCH_YES)
	break;
    }
  while (1);

  return MATCH_YES;
}

/* OpenMP 5.0:

   trait-set-selector[,trait-set-selector[,...]]

   trait-set-selector:
     trait-set-selector-name = { trait-selector[, trait-selector[, ...]] }

   trait-set-selector-name:
     constructor
     device
     implementation
     user  */

match
gfc_match_omp_context_selector_specification (gfc_omp_set_selector **oss_head,
					      bool metadirective_p)
{
  do
    {
      match m;
      char buf[GFC_MAX_SYMBOL_LEN + 1];
      enum omp_tss_code set = OMP_TRAIT_SET_INVALID;

      m = gfc_match_name (buf);
      if (m == MATCH_YES)
	set = omp_lookup_tss_code (buf);

      if (set == OMP_TRAIT_SET_INVALID)
	{
	  gfc_error ("expected context selector set name at %C");
	  return MATCH_ERROR;
	}

      m = gfc_match (" =");
      if (m != MATCH_YES)
	{
	  gfc_error ("expected %<=%> at %C");
	  return MATCH_ERROR;
	}

      m = gfc_match (" {");
      if (m != MATCH_YES)
	{
	  gfc_error ("expected %<{%> at %C");
	  return MATCH_ERROR;
	}

      gfc_omp_set_selector *oss = gfc_get_omp_set_selector ();
      oss->next = *oss_head;
      oss->code = set;
      *oss_head = oss;

      if (gfc_match_omp_context_selector (oss, metadirective_p) != MATCH_YES)
	return MATCH_ERROR;

      m = gfc_match (" }");
      if (m != MATCH_YES)
	{
	  gfc_error ("expected %<}%> at %C");
	  return MATCH_ERROR;
	}

      m = gfc_match (" ,");
      if (m != MATCH_YES)
	break;
    }
  while (1);

  return MATCH_YES;
}


match
gfc_match_omp_declare_variant (void)
{
  char buf[GFC_MAX_SYMBOL_LEN + 1];

  if (gfc_match (" (") != MATCH_YES)
    {
      gfc_error ("expected %<(%> at %C");
      return MATCH_ERROR;
    }

  gfc_symtree *base_proc_st, *variant_proc_st;
  if (gfc_match_name (buf) != MATCH_YES)
    {
      gfc_error ("expected name at %C");
      return MATCH_ERROR;
    }

  if (gfc_get_ha_sym_tree (buf, &base_proc_st))
    return MATCH_ERROR;

  if (gfc_match (" :") == MATCH_YES)
    {
      if (gfc_match_name (buf) != MATCH_YES)
	{
	  gfc_error ("expected variant name at %C");
	  return MATCH_ERROR;
	}

      if (gfc_get_ha_sym_tree (buf, &variant_proc_st))
	return MATCH_ERROR;
    }
  else
    {
      /* Base procedure not specified.  */
      variant_proc_st = base_proc_st;
      base_proc_st = NULL;
    }

  gfc_omp_declare_variant *odv;
  odv = gfc_get_omp_declare_variant ();
  odv->where = gfc_current_locus;
  odv->variant_proc_symtree = variant_proc_st;
  odv->adjust_args_list = NULL;
  odv->base_proc_symtree = base_proc_st;
  odv->next = NULL;
  odv->error_p = false;

  /* Add the new declare variant to the end of the list.  */
  gfc_omp_declare_variant **prev_next = &gfc_current_ns->omp_declare_variant;
  while (*prev_next)
    prev_next = &((*prev_next)->next);
  *prev_next = odv;

  if (gfc_match (" )") != MATCH_YES)
    {
      gfc_error ("expected %<)%> at %C");
      return MATCH_ERROR;
    }

  bool has_match = false, has_adjust_args = false, has_append_args = false;
  bool error_p = false;
  locus adjust_args_loc;
  locus append_args_loc;

  gfc_gobble_whitespace ();
  gfc_match_char (',');
  for (;;)
    {
      gfc_gobble_whitespace ();

      enum clause
      {
	clause_match,
	clause_adjust_args,
	clause_append_args
      } ccode;

      if (gfc_match ("match") == MATCH_YES)
	ccode = clause_match;
      else if (gfc_match ("adjust_args") == MATCH_YES)
	{
	  ccode = clause_adjust_args;
	  adjust_args_loc = gfc_current_locus;
	}
      else if (gfc_match ("append_args") == MATCH_YES)
	{
	  ccode = clause_append_args;
	  append_args_loc = gfc_current_locus;
	}
      else
	{
	  error_p = true;
	  break;
	}

      if (gfc_match (" ( ") != MATCH_YES)
	{
	  gfc_error ("expected %<(%> at %C");
	  return MATCH_ERROR;
	}

      if (ccode == clause_match)
	{
	  if (has_match)
	    {
	      gfc_error ("%qs clause at %L specified more than once",
			 "match", &gfc_current_locus);
	      return MATCH_ERROR;
	    }
	  has_match = true;
	  if (gfc_match_omp_context_selector_specification (&odv->set_selectors,
							    false)
	      != MATCH_YES)
	    return MATCH_ERROR;
	  if (gfc_match (" )") != MATCH_YES)
	    {
	      gfc_error ("expected %<)%> at %C");
	      return MATCH_ERROR;
	    }
	}
      else if (ccode == clause_adjust_args)
	{
	  has_adjust_args = true;
	  bool need_device_ptr_p = false;
	  bool need_device_addr_p = false;
	  if (gfc_match ("nothing ") == MATCH_YES)
	    ;
	  else if (gfc_match ("need_device_ptr ") == MATCH_YES)
	    need_device_ptr_p = true;
	  else if (gfc_match ("need_device_addr ") == MATCH_YES)
	    need_device_addr_p = true;
	  else
	    {
	      gfc_error ("expected %<nothing%>, %<need_device_ptr%> or "
			 "%<need_device_addr%> at %C");
	      return MATCH_ERROR;
	    }
	  if (gfc_match (": ") != MATCH_YES)
	    {
	      gfc_error ("expected %<:%> at %C");
	      return MATCH_ERROR;
	    }
	  gfc_omp_namelist *tail = NULL;
	  bool need_range = false, have_range = false;
	  while (true)
	    {
	      gfc_omp_namelist *p = gfc_get_omp_namelist ();
	      p->where = gfc_current_locus;
	      p->u.adj_args.need_ptr = need_device_ptr_p;
	      p->u.adj_args.need_addr = need_device_addr_p;
	      if (tail)
		{
		  tail->next = p;
		  tail = tail->next;
		}
	      else
		{
		  gfc_omp_namelist **q = &odv->adjust_args_list;
		  if (*q)
		    {
		      for (; (*q)->next; q = &(*q)->next)
			;
		      (*q)->next = p;
		    }
		  else
		    *q = p;
		  tail = p;
		}
	      if (gfc_match (": ") == MATCH_YES)
		{
		  if (have_range)
		    {
		      gfc_error ("unexpected %<:%> at %C");
		      return MATCH_ERROR;
		    }
		  p->u.adj_args.range_start = have_range = true;
		  need_range = false;
		  continue;
		}
	      if (have_range && gfc_match (", ") == MATCH_YES)
		{
		 have_range = false;
		 continue;
		}
	      if (have_range && gfc_match (") ") == MATCH_YES)
		break;
	      locus saved_loc = gfc_current_locus;

	      /* Without ranges, only arg names or integer literals permitted;
		 handle literals here as gfc_match_expr simplifies the expr.  */
	      if (gfc_match_literal_constant (&p->expr, true) == MATCH_YES)
		{
		  gfc_gobble_whitespace ();
		  char c = gfc_peek_ascii_char ();
		  if (c != ')' && c != ',' && c != ':')
		    {
		      gfc_free_expr (p->expr);
		      p->expr = NULL;
		      gfc_current_locus = saved_loc;
		    }
		}
	      if (!p->expr && gfc_match ("omp_num_args") == MATCH_YES)
		{
		  if (!have_range)
		    p->u.adj_args.range_start = need_range = true;
		  else
		    need_range = false;

		  locus saved_loc2 = gfc_current_locus;
		  gfc_gobble_whitespace ();
		  char c = gfc_peek_ascii_char ();
		  if (c == '+' || c == '-')
		    {
		      if (gfc_match ("+ %e", &p->expr) == MATCH_YES)
			p->u.adj_args.omp_num_args_plus = true;
		      else if (gfc_match ("- %e", &p->expr) == MATCH_YES)
			p->u.adj_args.omp_num_args_minus = true;
		      else if (!gfc_error_check ())
			{
			  gfc_error ("expected constant integer expression "
				     "at %C");
			  p->u.adj_args.error_p = true;
			  return MATCH_ERROR;
			}
		      p->where = gfc_get_location_range (&saved_loc, 1,
							 &saved_loc, 1,
							 &gfc_current_locus);
		    }
		  else
		    {
		      p->where = gfc_get_location_range (&saved_loc, 1,
							 &saved_loc, 1,
							 &saved_loc2);
		      p->u.adj_args.omp_num_args_plus = true;
		    }
		}
	      else if (!p->expr)
		{
		  match m = gfc_match_expr (&p->expr);
		  if (m != MATCH_YES)
		    {
		      gfc_error ("expected dummy parameter name, "
				 "%<omp_num_args%> or constant positive integer"
				 " at %C");
		      p->u.adj_args.error_p = true;
		      return MATCH_ERROR;
		    }
		  if (p->expr->expr_type == EXPR_CONSTANT && !have_range)
		    need_range = true;  /* Constant expr but not literal.  */
		  p->where = p->expr->where;
		}
	      else
		p->where = p->expr->where;
	      gfc_gobble_whitespace ();
	      match m = gfc_match (": ");
	      if (need_range && m != MATCH_YES)
		{
		  gfc_error ("expected %<:%> at %C");
		  return MATCH_ERROR;
		}
	      if (m == MATCH_YES)
		{
		  p->u.adj_args.range_start = have_range = true;
		  need_range = false;
		  continue;
		}
	      need_range = have_range = false;
	      if (gfc_match (", ") == MATCH_YES)
		continue;
	      if (gfc_match (") ") == MATCH_YES)
		break;
	    }
	}
      else if (ccode == clause_append_args)
	{
	  if (has_append_args)
	    {
	      gfc_error ("%qs clause at %L specified more than once",
			 "append_args", &gfc_current_locus);
	      return MATCH_ERROR;
	    }
	  has_append_args = true;
	  gfc_omp_namelist *append_args_last = NULL;
	  do
	    {
	      gfc_gobble_whitespace ();
	      if (gfc_match ("interop ") != MATCH_YES)
		{
		  gfc_error ("expected %<interop%> at %C");
		  return MATCH_ERROR;
		}
	      if (gfc_match ("( ") != MATCH_YES)
		{
		  gfc_error ("expected %<(%> at %C");
		  return MATCH_ERROR;
		}

	      bool target, targetsync;
	      char *type_str = NULL;
	      int type_str_len;
	      locus loc = gfc_current_locus;
	      if (gfc_parser_omp_clause_init_modifiers (target, targetsync,
							&type_str, type_str_len,
							false) == MATCH_ERROR)
		return MATCH_ERROR;

	      gfc_omp_namelist *n = gfc_get_omp_namelist();
	      n->where = loc;
	      n->u.init.target = target;
	      n->u.init.targetsync = targetsync;
	      n->u.init.len = type_str_len;
	      n->u2.init_interop = type_str;
	      if (odv->append_args_list)
		{
		  append_args_last->next = n;
		  append_args_last = n;
		}
	      else
		append_args_last = odv->append_args_list = n;

	      gfc_gobble_whitespace ();
	      if (gfc_match_char (',') == MATCH_YES)
		continue;
	      if (gfc_match_char (')') == MATCH_YES)
		break;
	      gfc_error ("Expected %<,%> or %<)%> at %C");
	      return MATCH_ERROR;
	    }
	  while (true);
	}
      gfc_gobble_whitespace ();
      if (gfc_match_omp_eos () == MATCH_YES)
	break;
      gfc_match_char (',');
    }

  if (error_p || (!has_match && !has_adjust_args && !has_append_args))
    {
      gfc_error ("expected %<match%>, %<adjust_args%> or %<append_args%> at %C");
      return MATCH_ERROR;
    }

  if ((has_adjust_args || has_append_args) && !has_match)
    {
      gfc_error ("the %qs clause at %L can only be specified if the "
		 "%<dispatch%> selector of the construct selector set appears "
		 "in the %<match%> clause",
		 has_adjust_args ? "adjust_args" : "append_args",
		 has_adjust_args ?  &adjust_args_loc : &append_args_loc);
      return MATCH_ERROR;
    }

  return MATCH_YES;
}


static match
match_omp_metadirective (bool begin_p)
{
  locus old_loc = gfc_current_locus;
  gfc_omp_variant *variants_head;
  gfc_omp_variant **next_variant = &variants_head;
  bool default_seen = false;

  /* Parse the context selectors.  */
  for (;;)
    {
      bool default_p = false;
      gfc_omp_set_selector *selectors = NULL;

      gfc_gobble_whitespace ();
      if (gfc_match_eos () == MATCH_YES)
	break;
      gfc_match_char (',');
      gfc_gobble_whitespace ();

      locus variant_locus = gfc_current_locus;

      if (gfc_match (" default ( ") == MATCH_YES)
	default_p = true;
      else if (gfc_match (" otherwise ( ") == MATCH_YES)
	default_p = true;
      else if (gfc_match (" when ( ") != MATCH_YES)
	{
	  gfc_error ("expected %<when%>, %<otherwise%>, or %<default%> at %C");
	  gfc_current_locus = old_loc;
	  return MATCH_ERROR;
	}

      if (default_p && default_seen)
	{
	  gfc_error ("too many %<otherwise%> or %<default%> clauses "
		     "in %<metadirective%> at %C");
	  gfc_current_locus = old_loc;
	  return MATCH_ERROR;
	}
      else if (default_seen)
	{
	  gfc_error ("%<otherwise%> or %<default%> clause "
		     "must appear last in %<metadirective%> at %C");
	  gfc_current_locus = old_loc;
	  return MATCH_ERROR;
	}

      if (!default_p)
	{
	  if (gfc_match_omp_context_selector_specification (&selectors, true)
	      != MATCH_YES)
	    return MATCH_ERROR;

	  if (gfc_match (" : ") != MATCH_YES)
	    {
	      gfc_error ("expected %<:%> at %C");
	      gfc_current_locus = old_loc;
	      return MATCH_ERROR;
	    }

	  gfc_commit_symbols ();
	}

      gfc_matching_omp_context_selector = true;
      gfc_statement directive = match_omp_directive ();
      gfc_matching_omp_context_selector = false;

      if (is_omp_declarative_stmt (directive))
	sorry_at (gfc_get_location (&gfc_current_locus),
		  "declarative directive variants are not supported");

      if (gfc_error_flag_test ())
	{
	  gfc_current_locus = old_loc;
	  return MATCH_ERROR;
	}

      if (gfc_match (" )") != MATCH_YES)
	{
	  gfc_error ("Expected %<)%> at %C");
	  gfc_current_locus = old_loc;
	  return MATCH_ERROR;
	}

      gfc_commit_symbols ();

      if (begin_p
	  && directive != ST_NONE
	  && gfc_omp_end_stmt (directive) == ST_NONE)
	{
	  gfc_error ("variant directive used in OMP BEGIN METADIRECTIVE "
		     "at %C must have a corresponding end directive");
	  gfc_current_locus = old_loc;
	  return MATCH_ERROR;
	}

      if (default_p)
	default_seen = true;

      gfc_omp_variant *omv = gfc_get_omp_variant ();
      omv->selectors = selectors;
      omv->stmt = directive;
      omv->where = variant_locus;

      if (directive == ST_NONE)
	{
	  /* The directive was a 'nothing' directive.  */
	  omv->code = gfc_get_code (EXEC_CONTINUE);
	  omv->code->ext.omp_clauses = NULL;
	}
      else
	{
	  omv->code = gfc_get_code (new_st.op);
	  omv->code->ext.omp_clauses = new_st.ext.omp_clauses;
	  /* Prevent the OpenMP clauses from being freed via NEW_ST.  */
	  new_st.ext.omp_clauses = NULL;
	}

      *next_variant = omv;
      next_variant = &omv->next;
    }

  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after OMP METADIRECTIVE at %C");
      gfc_current_locus = old_loc;
      return MATCH_ERROR;
    }

  /* Add a 'default (nothing)' clause if no default is explicitly given.  */
  if (!default_seen)
    {
      gfc_omp_variant *omv = gfc_get_omp_variant ();
      omv->stmt = ST_NONE;
      omv->code = gfc_get_code (EXEC_CONTINUE);
      omv->code->ext.omp_clauses = NULL;
      omv->where = old_loc;
      omv->selectors = NULL;

      *next_variant = omv;
      next_variant = &omv->next;
    }

  new_st.op = EXEC_OMP_METADIRECTIVE;
  new_st.ext.omp_variants = variants_head;

  return MATCH_YES;
}

match
gfc_match_omp_begin_metadirective (void)
{
  return match_omp_metadirective (true);
}

match
gfc_match_omp_metadirective (void)
{
  return match_omp_metadirective (false);
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
		    (OMP_PARALLEL_CLAUSES | OMP_DO_CLAUSES)
		    & ~(omp_mask (OMP_CLAUSE_NOWAIT)));
}


match
gfc_match_omp_parallel_do_simd (void)
{
  return match_omp (EXEC_OMP_PARALLEL_DO_SIMD,
		    (OMP_PARALLEL_CLAUSES | OMP_DO_CLAUSES | OMP_SIMD_CLAUSES)
		    & ~(omp_mask (OMP_CLAUSE_NOWAIT)));
}


match
gfc_match_omp_parallel_masked (void)
{
  return match_omp (EXEC_OMP_PARALLEL_MASKED,
		    OMP_PARALLEL_CLAUSES | OMP_MASKED_CLAUSES);
}

match
gfc_match_omp_parallel_masked_taskloop (void)
{
  return match_omp (EXEC_OMP_PARALLEL_MASKED_TASKLOOP,
		    (OMP_PARALLEL_CLAUSES | OMP_MASKED_CLAUSES
		     | OMP_TASKLOOP_CLAUSES)
		    & ~(omp_mask (OMP_CLAUSE_IN_REDUCTION)));
}

match
gfc_match_omp_parallel_masked_taskloop_simd (void)
{
  return match_omp (EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD,
		    (OMP_PARALLEL_CLAUSES | OMP_MASKED_CLAUSES
		     | OMP_TASKLOOP_CLAUSES | OMP_SIMD_CLAUSES)
		    & ~(omp_mask (OMP_CLAUSE_IN_REDUCTION)));
}

match
gfc_match_omp_parallel_master (void)
{
  return match_omp (EXEC_OMP_PARALLEL_MASTER, OMP_PARALLEL_CLAUSES);
}

match
gfc_match_omp_parallel_master_taskloop (void)
{
  return match_omp (EXEC_OMP_PARALLEL_MASTER_TASKLOOP,
		    (OMP_PARALLEL_CLAUSES | OMP_TASKLOOP_CLAUSES)
		    & ~(omp_mask (OMP_CLAUSE_IN_REDUCTION)));
}

match
gfc_match_omp_parallel_master_taskloop_simd (void)
{
  return match_omp (EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD,
		    (OMP_PARALLEL_CLAUSES | OMP_TASKLOOP_CLAUSES
		     | OMP_SIMD_CLAUSES)
		    & ~(omp_mask (OMP_CLAUSE_IN_REDUCTION)));
}

match
gfc_match_omp_parallel_sections (void)
{
  return match_omp (EXEC_OMP_PARALLEL_SECTIONS,
		    (OMP_PARALLEL_CLAUSES | OMP_SECTIONS_CLAUSES)
		    & ~(omp_mask (OMP_CLAUSE_NOWAIT)));
}


match
gfc_match_omp_parallel_workshare (void)
{
  return match_omp (EXEC_OMP_PARALLEL_WORKSHARE, OMP_PARALLEL_CLAUSES);
}

void
gfc_check_omp_requires (gfc_namespace *ns, int ref_omp_requires)
{
  const char *msg = G_("Program unit at %L has OpenMP device "
		       "constructs/routines but does not set !$OMP REQUIRES %s "
		       "but other program units do");
  if (ns->omp_target_seen
      && (ns->omp_requires & OMP_REQ_TARGET_MASK)
	 != (ref_omp_requires & OMP_REQ_TARGET_MASK))
    {
      gcc_assert (ns->proc_name);
      if ((ref_omp_requires & OMP_REQ_REVERSE_OFFLOAD)
	  && !(ns->omp_requires & OMP_REQ_REVERSE_OFFLOAD))
	gfc_error (msg, &ns->proc_name->declared_at, "REVERSE_OFFLOAD");
      if ((ref_omp_requires & OMP_REQ_UNIFIED_ADDRESS)
	  && !(ns->omp_requires & OMP_REQ_UNIFIED_ADDRESS))
	gfc_error (msg, &ns->proc_name->declared_at, "UNIFIED_ADDRESS");
      if ((ref_omp_requires & OMP_REQ_UNIFIED_SHARED_MEMORY)
	  && !(ns->omp_requires & OMP_REQ_UNIFIED_SHARED_MEMORY))
	gfc_error (msg, &ns->proc_name->declared_at, "UNIFIED_SHARED_MEMORY");
      if ((ref_omp_requires & OMP_REQ_SELF_MAPS)
	  && !(ns->omp_requires & OMP_REQ_UNIFIED_SHARED_MEMORY))
	gfc_error (msg, &ns->proc_name->declared_at, "SELF_MAPS");
    }
}

bool
gfc_omp_requires_add_clause (gfc_omp_requires_kind clause,
			     const char *clause_name, locus *loc,
			     const char *module_name)
{
  gfc_namespace *prog_unit = gfc_current_ns;
  while (prog_unit->parent)
    {
      if (gfc_state_stack->previous
	  && gfc_state_stack->previous->state == COMP_INTERFACE)
	break;
      prog_unit = prog_unit->parent;
    }

  /* Requires added after use.  */
  if (prog_unit->omp_target_seen
      && (clause & OMP_REQ_TARGET_MASK)
      && !(prog_unit->omp_requires & clause))
    {
      if (module_name)
	gfc_error ("!$OMP REQUIRES clause %qs specified via module %qs use "
		   "at %L comes after using a device construct/routine",
		   clause_name, module_name, loc);
      else
	gfc_error ("!$OMP REQUIRES clause %qs specified at %L comes after "
		   "using a device construct/routine", clause_name, loc);
      return false;
    }

  /* Overriding atomic_default_mem_order clause value.  */
  if ((clause & OMP_REQ_ATOMIC_MEM_ORDER_MASK)
      && (prog_unit->omp_requires & OMP_REQ_ATOMIC_MEM_ORDER_MASK)
      && (prog_unit->omp_requires & OMP_REQ_ATOMIC_MEM_ORDER_MASK)
	 != (int) clause)
    {
      const char *other;
      switch (prog_unit->omp_requires & OMP_REQ_ATOMIC_MEM_ORDER_MASK)
	{
	case OMP_REQ_ATOMIC_MEM_ORDER_SEQ_CST: other = "seq_cst"; break;
	case OMP_REQ_ATOMIC_MEM_ORDER_ACQ_REL: other = "acq_rel"; break;
	case OMP_REQ_ATOMIC_MEM_ORDER_ACQUIRE: other = "acquire"; break;
	case OMP_REQ_ATOMIC_MEM_ORDER_RELAXED: other = "relaxed"; break;
	case OMP_REQ_ATOMIC_MEM_ORDER_RELEASE: other = "release"; break;
	default: gcc_unreachable ();
	}

      if (module_name)
	gfc_error ("!$OMP REQUIRES clause %<atomic_default_mem_order(%s)%> "
		   "specified via module %qs use at %L overrides a previous "
		   "%<atomic_default_mem_order(%s)%> (which might be through "
		   "using a module)", clause_name, module_name, loc, other);
      else
	gfc_error ("!$OMP REQUIRES clause %<atomic_default_mem_order(%s)%> "
		   "specified at %L overrides a previous "
		   "%<atomic_default_mem_order(%s)%> (which might be through "
		   "using a module)", clause_name, loc, other);
      return false;
    }

  /* Requires via module not at program-unit level and not repeating clause.  */
  if (prog_unit != gfc_current_ns && !(prog_unit->omp_requires & clause))
    {
      if (clause & OMP_REQ_ATOMIC_MEM_ORDER_MASK)
	gfc_error ("!$OMP REQUIRES clause %<atomic_default_mem_order(%s)%> "
		   "specified via module %qs use at %L but same clause is "
		   "not specified for the program unit", clause_name,
		   module_name, loc);
      else
	gfc_error ("!$OMP REQUIRES clause %qs specified via module %qs use at "
		   "%L but same clause is not specified for the program unit",
		   clause_name, module_name, loc);
      return false;
    }

  if (!gfc_state_stack->previous
      || gfc_state_stack->previous->state != COMP_INTERFACE)
    prog_unit->omp_requires |= clause;
  return true;
}

match
gfc_match_omp_requires (void)
{
  static const char *clauses[] = {"reverse_offload",
				  "unified_address",
				  "unified_shared_memory",
				  "self_maps",
				  "dynamic_allocators",
				  "atomic_default"};
  const char *clause = NULL;
  int requires_clauses = 0;
  bool first = true;
  locus old_loc;

  if (gfc_current_ns->parent
      && (!gfc_state_stack->previous
	  || gfc_state_stack->previous->state != COMP_INTERFACE))
    {
      gfc_error ("!$OMP REQUIRES at %C must appear in the specification part "
		 "of a program unit");
      return MATCH_ERROR;
    }

  while (true)
    {
      old_loc = gfc_current_locus;
      gfc_omp_requires_kind requires_clause;
      if ((first || gfc_match_char (',') != MATCH_YES)
	  && (first && gfc_match_space () != MATCH_YES))
	goto error;
      first = false;
      gfc_gobble_whitespace ();
      old_loc = gfc_current_locus;

      if (gfc_match_omp_eos () != MATCH_NO)
	break;
      if (gfc_match (clauses[0]) == MATCH_YES)
	{
	  clause = clauses[0];
	  requires_clause = OMP_REQ_REVERSE_OFFLOAD;
	  if (requires_clauses & OMP_REQ_REVERSE_OFFLOAD)
	    goto duplicate_clause;
	}
      else if (gfc_match (clauses[1]) == MATCH_YES)
	{
	  clause = clauses[1];
	  requires_clause = OMP_REQ_UNIFIED_ADDRESS;
	  if (requires_clauses & OMP_REQ_UNIFIED_ADDRESS)
	    goto duplicate_clause;
	}
      else if (gfc_match (clauses[2]) == MATCH_YES)
	{
	  clause = clauses[2];
	  requires_clause = OMP_REQ_UNIFIED_SHARED_MEMORY;
	  if (requires_clauses & OMP_REQ_UNIFIED_SHARED_MEMORY)
	    goto duplicate_clause;
	}
      else if (gfc_match (clauses[3]) == MATCH_YES)
	{
	  clause = clauses[3];
	  requires_clause = OMP_REQ_SELF_MAPS;
	  if (requires_clauses & OMP_REQ_SELF_MAPS)
	    goto duplicate_clause;
	}
      else if (gfc_match (clauses[4]) == MATCH_YES)
	{
	  clause = clauses[4];
	  requires_clause = OMP_REQ_DYNAMIC_ALLOCATORS;
	  if (requires_clauses & OMP_REQ_DYNAMIC_ALLOCATORS)
	    goto duplicate_clause;
	}
      else if (gfc_match ("atomic_default_mem_order (") == MATCH_YES)
	{
	  clause = clauses[5];
	  if (requires_clauses & OMP_REQ_ATOMIC_MEM_ORDER_MASK)
	    goto duplicate_clause;
	  if (gfc_match (" seq_cst )") == MATCH_YES)
	    {
	      clause = "seq_cst";
	      requires_clause = OMP_REQ_ATOMIC_MEM_ORDER_SEQ_CST;
	    }
	  else if (gfc_match (" acq_rel )") == MATCH_YES)
	    {
	      clause = "acq_rel";
	      requires_clause = OMP_REQ_ATOMIC_MEM_ORDER_ACQ_REL;
	    }
	  else if (gfc_match (" acquire )") == MATCH_YES)
	    {
	      clause = "acquire";
	      requires_clause = OMP_REQ_ATOMIC_MEM_ORDER_ACQUIRE;
	    }
	  else if (gfc_match (" relaxed )") == MATCH_YES)
	    {
	      clause = "relaxed";
	      requires_clause = OMP_REQ_ATOMIC_MEM_ORDER_RELAXED;
	    }
	  else if (gfc_match (" release )") == MATCH_YES)
	    {
	      clause = "release";
	      requires_clause = OMP_REQ_ATOMIC_MEM_ORDER_RELEASE;
	    }
	  else
	    {
	      gfc_error ("Expected ACQ_REL, ACQUIRE, RELAXED, RELEASE or "
			 "SEQ_CST for ATOMIC_DEFAULT_MEM_ORDER clause at %C");
	      goto error;
	    }
	}
      else
	goto error;

      if (!gfc_omp_requires_add_clause (requires_clause, clause, &old_loc, NULL))
	goto error;
      requires_clauses |= requires_clause;
    }

  if (requires_clauses == 0)
    {
      if (!gfc_error_flag_test ())
	gfc_error ("Clause expected at %C");
      goto error;
    }
  return MATCH_YES;

duplicate_clause:
  gfc_error ("%qs clause at %L specified more than once", clause, &old_loc);
error:
  if (!gfc_error_flag_test ())
    gfc_error ("Expected UNIFIED_ADDRESS, UNIFIED_SHARED_MEMORY, SELF_MAPS, "
	       "DYNAMIC_ALLOCATORS, REVERSE_OFFLOAD, or "
	       "ATOMIC_DEFAULT_MEM_ORDER clause at %L", &old_loc);
  return MATCH_ERROR;
}


match
gfc_match_omp_scan (void)
{
  bool incl;
  gfc_omp_clauses *c = gfc_get_omp_clauses ();
  gfc_gobble_whitespace ();
  if ((incl = (gfc_match ("inclusive") == MATCH_YES))
      || gfc_match ("exclusive") == MATCH_YES)
    {
      if (gfc_match_omp_variable_list (" (", &c->lists[incl ? OMP_LIST_SCAN_IN
							    : OMP_LIST_SCAN_EX],
				       false) != MATCH_YES)
	{
	  gfc_free_omp_clauses (c);
	  return MATCH_ERROR;
	}
    }
  else
    {
      gfc_error ("Expected INCLUSIVE or EXCLUSIVE clause at %C");
      gfc_free_omp_clauses (c);
      return MATCH_ERROR;
    }
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after !$OMP SCAN at %C");
      gfc_free_omp_clauses (c);
      return MATCH_ERROR;
    }

  new_st.op = EXEC_OMP_SCAN;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


match
gfc_match_omp_scope (void)
{
  return match_omp (EXEC_OMP_SCOPE, OMP_SCOPE_CLAUSES);
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
  return match_omp (EXEC_OMP_SINGLE, OMP_SINGLE_CLAUSES);
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
gfc_match_omp_target_enter_data (void)
{
  return match_omp (EXEC_OMP_TARGET_ENTER_DATA, OMP_TARGET_ENTER_DATA_CLAUSES);
}


match
gfc_match_omp_target_exit_data (void)
{
  return match_omp (EXEC_OMP_TARGET_EXIT_DATA, OMP_TARGET_EXIT_DATA_CLAUSES);
}


match
gfc_match_omp_target_parallel (void)
{
  return match_omp (EXEC_OMP_TARGET_PARALLEL,
		    (OMP_TARGET_CLAUSES | OMP_PARALLEL_CLAUSES)
		    & ~(omp_mask (OMP_CLAUSE_COPYIN)));
}


match
gfc_match_omp_target_parallel_do (void)
{
  return match_omp (EXEC_OMP_TARGET_PARALLEL_DO,
		    (OMP_TARGET_CLAUSES | OMP_PARALLEL_CLAUSES
		     | OMP_DO_CLAUSES) & ~(omp_mask (OMP_CLAUSE_COPYIN)));
}


match
gfc_match_omp_target_parallel_do_simd (void)
{
  return match_omp (EXEC_OMP_TARGET_PARALLEL_DO_SIMD,
		    (OMP_TARGET_CLAUSES | OMP_PARALLEL_CLAUSES | OMP_DO_CLAUSES
		     | OMP_SIMD_CLAUSES) & ~(omp_mask (OMP_CLAUSE_COPYIN)));
}


match
gfc_match_omp_target_simd (void)
{
  return match_omp (EXEC_OMP_TARGET_SIMD,
		    OMP_TARGET_CLAUSES | OMP_SIMD_CLAUSES);
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
		    (OMP_TARGET_CLAUSES | OMP_TEAMS_CLAUSES
		     | OMP_DISTRIBUTE_CLAUSES | OMP_PARALLEL_CLAUSES
		     | OMP_DO_CLAUSES)
		    & ~(omp_mask (OMP_CLAUSE_ORDERED))
		    & ~(omp_mask (OMP_CLAUSE_LINEAR)));
}


match
gfc_match_omp_target_teams_distribute_parallel_do_simd (void)
{
  return match_omp (EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD,
		    (OMP_TARGET_CLAUSES | OMP_TEAMS_CLAUSES
		     | OMP_DISTRIBUTE_CLAUSES | OMP_PARALLEL_CLAUSES
		     | OMP_DO_CLAUSES | OMP_SIMD_CLAUSES)
		    & ~(omp_mask (OMP_CLAUSE_ORDERED)));
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
gfc_match_omp_task (void)
{
  return match_omp (EXEC_OMP_TASK, OMP_TASK_CLAUSES);
}


match
gfc_match_omp_taskloop (void)
{
  return match_omp (EXEC_OMP_TASKLOOP, OMP_TASKLOOP_CLAUSES);
}


match
gfc_match_omp_taskloop_simd (void)
{
  return match_omp (EXEC_OMP_TASKLOOP_SIMD,
		    OMP_TASKLOOP_CLAUSES | OMP_SIMD_CLAUSES);
}


match
gfc_match_omp_taskwait (void)
{
  if (gfc_match_omp_eos () == MATCH_YES)
    {
      new_st.op = EXEC_OMP_TASKWAIT;
      new_st.ext.omp_clauses = NULL;
      return MATCH_YES;
    }
  return match_omp (EXEC_OMP_TASKWAIT,
		    omp_mask (OMP_CLAUSE_DEPEND) | OMP_CLAUSE_NOWAIT);
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
		    (OMP_TEAMS_CLAUSES | OMP_DISTRIBUTE_CLAUSES
		     | OMP_PARALLEL_CLAUSES | OMP_DO_CLAUSES)
		    & ~(omp_mask (OMP_CLAUSE_ORDERED)
			| OMP_CLAUSE_LINEAR | OMP_CLAUSE_NOWAIT));
}


match
gfc_match_omp_teams_distribute_parallel_do_simd (void)
{
  return match_omp (EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD,
		    (OMP_TEAMS_CLAUSES | OMP_DISTRIBUTE_CLAUSES
		     | OMP_PARALLEL_CLAUSES | OMP_DO_CLAUSES
		     | OMP_SIMD_CLAUSES)
		    & ~(omp_mask (OMP_CLAUSE_ORDERED) | OMP_CLAUSE_NOWAIT));
}


match
gfc_match_omp_teams_distribute_simd (void)
{
  return match_omp (EXEC_OMP_TEAMS_DISTRIBUTE_SIMD,
		    OMP_TEAMS_CLAUSES | OMP_DISTRIBUTE_CLAUSES
		    | OMP_SIMD_CLAUSES);
}

match
gfc_match_omp_tile (void)
{
  return match_omp (EXEC_OMP_TILE, OMP_TILE_CLAUSES);
}

match
gfc_match_omp_unroll (void)
{
  return match_omp (EXEC_OMP_UNROLL, OMP_UNROLL_CLAUSES);
}

match
gfc_match_omp_workshare (void)
{
  return match_omp (EXEC_OMP_WORKSHARE, OMP_WORKSHARE_CLAUSES);
}


match
gfc_match_omp_masked (void)
{
  return match_omp (EXEC_OMP_MASKED, OMP_MASKED_CLAUSES);
}

match
gfc_match_omp_masked_taskloop (void)
{
  return match_omp (EXEC_OMP_MASKED_TASKLOOP,
		    OMP_MASKED_CLAUSES | OMP_TASKLOOP_CLAUSES);
}

match
gfc_match_omp_masked_taskloop_simd (void)
{
  return match_omp (EXEC_OMP_MASKED_TASKLOOP_SIMD,
		    (OMP_MASKED_CLAUSES | OMP_TASKLOOP_CLAUSES
		     | OMP_SIMD_CLAUSES));
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
gfc_match_omp_master_taskloop (void)
{
  return match_omp (EXEC_OMP_MASTER_TASKLOOP, OMP_TASKLOOP_CLAUSES);
}

match
gfc_match_omp_master_taskloop_simd (void)
{
  return match_omp (EXEC_OMP_MASTER_TASKLOOP_SIMD,
		    OMP_TASKLOOP_CLAUSES | OMP_SIMD_CLAUSES);
}

match
gfc_match_omp_ordered (void)
{
  return match_omp (EXEC_OMP_ORDERED, OMP_ORDERED_CLAUSES);
}

match
gfc_match_omp_nothing (void)
{
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after $OMP NOTHING statement at %C");
      return MATCH_ERROR;
    }
  /* Will use ST_NONE; therefore, no EXEC_OMP_ is needed.  */
  return MATCH_YES;
}

match
gfc_match_omp_ordered_depend (void)
{
  return match_omp (EXEC_OMP_ORDERED, omp_mask (OMP_CLAUSE_DOACROSS));
}


/* omp atomic [clause-list]
   - atomic-clause:  read | write | update
   - capture
   - memory-order-clause: seq_cst | acq_rel | release | acquire | relaxed
   - hint(hint-expr)
   - OpenMP 5.1: compare | fail (seq_cst | acquire | relaxed ) | weak
*/

match
gfc_match_omp_atomic (void)
{
  gfc_omp_clauses *c;
  locus loc = gfc_current_locus;

  if (gfc_match_omp_clauses (&c, OMP_ATOMIC_CLAUSES, true, true) != MATCH_YES)
    return MATCH_ERROR;

  if (c->atomic_op == GFC_OMP_ATOMIC_UNSET)
    c->atomic_op = GFC_OMP_ATOMIC_UPDATE;

  if (c->capture && c->atomic_op != GFC_OMP_ATOMIC_UPDATE)
    gfc_error ("!$OMP ATOMIC at %L with %s clause is incompatible with "
	       "READ or WRITE", &loc, "CAPTURE");
  if (c->compare && c->atomic_op != GFC_OMP_ATOMIC_UPDATE)
    gfc_error ("!$OMP ATOMIC at %L with %s clause is incompatible with "
	       "READ or WRITE", &loc, "COMPARE");
  if (c->fail != OMP_MEMORDER_UNSET && c->atomic_op != GFC_OMP_ATOMIC_UPDATE)
    gfc_error ("!$OMP ATOMIC at %L with %s clause is incompatible with "
	       "READ or WRITE", &loc, "FAIL");
  if (c->weak && !c->compare)
    {
      gfc_error ("!$OMP ATOMIC at %L with %s clause requires %s clause", &loc,
		 "WEAK", "COMPARE");
      c->weak = false;
    }

  if (c->memorder == OMP_MEMORDER_UNSET)
    {
      gfc_namespace *prog_unit = gfc_current_ns;
      while (prog_unit->parent)
	prog_unit = prog_unit->parent;
      switch (prog_unit->omp_requires & OMP_REQ_ATOMIC_MEM_ORDER_MASK)
	{
	case 0:
	case OMP_REQ_ATOMIC_MEM_ORDER_RELAXED:
	  c->memorder = OMP_MEMORDER_RELAXED;
	  break;
	case OMP_REQ_ATOMIC_MEM_ORDER_SEQ_CST:
	  c->memorder = OMP_MEMORDER_SEQ_CST;
	  break;
	case OMP_REQ_ATOMIC_MEM_ORDER_ACQ_REL:
	  if (c->capture)
	    c->memorder = OMP_MEMORDER_ACQ_REL;
	  else if (c->atomic_op == GFC_OMP_ATOMIC_READ)
	    c->memorder = OMP_MEMORDER_ACQUIRE;
	  else
	    c->memorder = OMP_MEMORDER_RELEASE;
	  break;
	case OMP_REQ_ATOMIC_MEM_ORDER_ACQUIRE:
	  if (c->atomic_op == GFC_OMP_ATOMIC_WRITE)
	    {
	      gfc_error ("!$OMP ATOMIC WRITE at %L incompatible with "
			 "ACQUIRES clause implicitly provided by a "
			 "REQUIRES directive", &loc);
	      c->memorder = OMP_MEMORDER_SEQ_CST;
	    }
	  else
	    c->memorder = OMP_MEMORDER_ACQUIRE;
	  break;
	case OMP_REQ_ATOMIC_MEM_ORDER_RELEASE:
	  if (c->atomic_op == GFC_OMP_ATOMIC_READ)
	    {
	      gfc_error ("!$OMP ATOMIC READ at %L incompatible with "
			 "RELEASE clause implicitly provided by a "
			 "REQUIRES directive", &loc);
	      c->memorder = OMP_MEMORDER_SEQ_CST;
	    }
	  else
	    c->memorder = OMP_MEMORDER_RELEASE;
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  else
    switch (c->atomic_op)
      {
      case GFC_OMP_ATOMIC_READ:
	if (c->memorder == OMP_MEMORDER_RELEASE)
	  {
	    gfc_error ("!$OMP ATOMIC READ at %L incompatible with "
		       "RELEASE clause", &loc);
	    c->memorder = OMP_MEMORDER_SEQ_CST;
	  }
	else if (c->memorder == OMP_MEMORDER_ACQ_REL)
	  c->memorder = OMP_MEMORDER_ACQUIRE;
	break;
      case GFC_OMP_ATOMIC_WRITE:
	if (c->memorder == OMP_MEMORDER_ACQUIRE)
	  {
	    gfc_error ("!$OMP ATOMIC WRITE at %L incompatible with "
		       "ACQUIRE clause", &loc);
	    c->memorder = OMP_MEMORDER_SEQ_CST;
	  }
	else if (c->memorder == OMP_MEMORDER_ACQ_REL)
	  c->memorder = OMP_MEMORDER_RELEASE;
	break;
      default:
	break;
      }
  gfc_error_check ();
  new_st.ext.omp_clauses = c;
  new_st.op = EXEC_OMP_ATOMIC;
  return MATCH_YES;
}


/* acc atomic [ read | write | update | capture]  */

match
gfc_match_oacc_atomic (void)
{
  gfc_omp_clauses *c = gfc_get_omp_clauses ();
  c->atomic_op = GFC_OMP_ATOMIC_UPDATE;
  c->memorder = OMP_MEMORDER_RELAXED;
  gfc_gobble_whitespace ();
  if (gfc_match ("update") == MATCH_YES)
    ;
  else if (gfc_match ("read") == MATCH_YES)
    c->atomic_op = GFC_OMP_ATOMIC_READ;
  else if (gfc_match ("write") == MATCH_YES)
    c->atomic_op = GFC_OMP_ATOMIC_WRITE;
  else if (gfc_match ("capture") == MATCH_YES)
    c->capture = true;
  gfc_gobble_whitespace ();
  if (gfc_match_omp_eos () != MATCH_YES)
    {
      gfc_error ("Unexpected junk after !$ACC ATOMIC statement at %C");
      gfc_free_omp_clauses (c);
      return MATCH_ERROR;
    }
  new_st.ext.omp_clauses = c;
  new_st.op = EXEC_OACC_ATOMIC;
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
  return match_omp (EXEC_OMP_TASKGROUP, OMP_TASKGROUP_CLAUSES);
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
  if (gfc_match_omp_clauses (&c, omp_mask (OMP_CLAUSE_IF), false) != MATCH_YES)
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
    {
      gfc_error ("Expected construct-type PARALLEL, SECTIONS, DO or TASKGROUP "
		 "in $OMP CANCELLATION POINT statement at %C");
      return MATCH_ERROR;
    }
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
      if (nowait)
	gfc_error ("Unexpected junk after NOWAIT clause at %C");
      else
	gfc_error ("Unexpected junk at %C");
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
  if (gfc_match_omp_clauses (&c, omp_mask (OMP_CLAUSE_COPYPRIVATE)
					   | OMP_CLAUSE_NOWAIT) != MATCH_YES)
    return MATCH_ERROR;
  new_st.op = EXEC_OMP_END_SINGLE;
  new_st.ext.omp_clauses = c;
  return MATCH_YES;
}


static bool
oacc_is_loop (gfc_code *code)
{
  return code->op == EXEC_OACC_PARALLEL_LOOP
	 || code->op == EXEC_OACC_KERNELS_LOOP
	 || code->op == EXEC_OACC_SERIAL_LOOP
	 || code->op == EXEC_OACC_LOOP;
}

static void
resolve_scalar_int_expr (gfc_expr *expr, const char *clause)
{
  if (!gfc_resolve_expr (expr)
      || expr->ts.type != BT_INTEGER
      || expr->rank != 0)
    gfc_error ("%s clause at %L requires a scalar INTEGER expression",
	       clause, &expr->where);
}

static void
resolve_positive_int_expr (gfc_expr *expr, const char *clause)
{
  resolve_scalar_int_expr (expr, clause);
  if (expr->expr_type == EXPR_CONSTANT
      && expr->ts.type == BT_INTEGER
      && mpz_sgn (expr->value.integer) <= 0)
    gfc_warning ((flag_openmp || flag_openmp_simd) ? OPT_Wopenmp : 0,
		 "INTEGER expression of %s clause at %L must be positive",
		 clause, &expr->where);
}

static void
resolve_nonnegative_int_expr (gfc_expr *expr, const char *clause)
{
  resolve_scalar_int_expr (expr, clause);
  if (expr->expr_type == EXPR_CONSTANT
      && expr->ts.type == BT_INTEGER
      && mpz_sgn (expr->value.integer) < 0)
    gfc_warning ((flag_openmp || flag_openmp_simd) ? OPT_Wopenmp : 0,
		 "INTEGER expression of %s clause at %L must be non-negative",
		 clause, &expr->where);
}

/* Emits error when symbol is pointer, cray pointer or cray pointee
   of derived of polymorphic type.  */

static void
check_symbol_not_pointer (gfc_symbol *sym, locus loc, const char *name)
{
  if (sym->ts.type == BT_DERIVED && sym->attr.cray_pointer)
    gfc_error ("Cray pointer object %qs of derived type in %s clause at %L",
	       sym->name, name, &loc);
  if (sym->ts.type == BT_DERIVED && sym->attr.cray_pointee)
    gfc_error ("Cray pointee object %qs of derived type in %s clause at %L",
	       sym->name, name, &loc);

  if ((sym->ts.type == BT_ASSUMED && sym->attr.pointer)
      || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
	  && CLASS_DATA (sym)->attr.pointer))
    gfc_error ("POINTER object %qs of polymorphic type in %s clause at %L",
	       sym->name, name, &loc);
  if ((sym->ts.type == BT_ASSUMED && sym->attr.cray_pointer)
      || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
	  && CLASS_DATA (sym)->attr.cray_pointer))
    gfc_error ("Cray pointer object %qs of polymorphic type in %s clause at %L",
	       sym->name, name, &loc);
  if ((sym->ts.type == BT_ASSUMED && sym->attr.cray_pointee)
      || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
	  && CLASS_DATA (sym)->attr.cray_pointee))
    gfc_error ("Cray pointee object %qs of polymorphic type in %s clause at %L",
	       sym->name, name, &loc);
}

/* Emits error when symbol represents assumed size/rank array.  */

static void
check_array_not_assumed (gfc_symbol *sym, locus loc, const char *name)
{
  if (sym->as && sym->as->type == AS_ASSUMED_SIZE)
    gfc_error ("Assumed size array %qs in %s clause at %L",
	       sym->name, name, &loc);
  if (sym->as && sym->as->type == AS_ASSUMED_RANK)
    gfc_error ("Assumed rank array %qs in %s clause at %L",
	       sym->name, name, &loc);
}

static void
resolve_oacc_data_clauses (gfc_symbol *sym, locus loc, const char *name)
{
  check_array_not_assumed (sym, loc, name);
}

static void
resolve_oacc_deviceptr_clause (gfc_symbol *sym, locus loc, const char *name)
{
  if (sym->attr.pointer
      || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
	  && CLASS_DATA (sym)->attr.class_pointer))
    gfc_error ("POINTER object %qs in %s clause at %L",
	       sym->name, name, &loc);
  if (sym->attr.cray_pointer
      || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
	  && CLASS_DATA (sym)->attr.cray_pointer))
    gfc_error ("Cray pointer object %qs in %s clause at %L",
	       sym->name, name, &loc);
  if (sym->attr.cray_pointee
      || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
	  && CLASS_DATA (sym)->attr.cray_pointee))
    gfc_error ("Cray pointee object %qs in %s clause at %L",
	       sym->name, name, &loc);
  if (sym->attr.allocatable
      || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)
	  && CLASS_DATA (sym)->attr.allocatable))
    gfc_error ("ALLOCATABLE object %qs in %s clause at %L",
	       sym->name, name, &loc);
  if (sym->attr.value)
    gfc_error ("VALUE object %qs in %s clause at %L",
	       sym->name, name, &loc);
  check_array_not_assumed (sym, loc, name);
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
		   "!$OMP DECLARE REDUCTION at %L", sym->name, &(*e)->where);
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
		   "!$OMP DECLARE REDUCTION at %L", sym->name,
		   &copy->loc);
    }
  gfc_code_walker (&copy, gfc_dummy_code_callback,
		   resolve_omp_udr_callback2, NULL);
  *sym1 = sym1_copy;
  *sym2 = sym2_copy;
  return copy;
}

/* Assume that a constant expression in the range 1 (omp_default_mem_alloc)
   to 8 (omp_thread_mem_alloc) range, or 200 (ompx_gnu_pinned_mem_alloc) is
   fine.  The original symbol name is already lost during matching via
   gfc_match_expr.  */
static bool
is_predefined_allocator (gfc_expr *expr)
{
  return (gfc_resolve_expr (expr)
	  && expr->rank == 0
	  && expr->ts.type == BT_INTEGER
	  && expr->ts.kind == gfc_c_intptr_kind
	  && expr->expr_type == EXPR_CONSTANT
	  && ((mpz_sgn (expr->value.integer) > 0
	       && mpz_cmp_si (expr->value.integer,
			      GOMP_OMP_PREDEF_ALLOC_MAX) <= 0)
	      || (mpz_cmp_si (expr->value.integer,
			      GOMP_OMPX_PREDEF_ALLOC_MIN) >= 0
		  && mpz_cmp_si (expr->value.integer,
				 GOMP_OMPX_PREDEF_ALLOC_MAX) <= 0)));
}

/* Resolve declarative ALLOCATE statement. Note: Common block vars only appear
   as /block/ not individual, which is ensured during parsing.  */

void
gfc_resolve_omp_allocate (gfc_namespace *ns, gfc_omp_namelist *list)
{
  for (gfc_omp_namelist *n = list; n; n = n->next)
    {
      if (n->sym->attr.result || n->sym->result == n->sym)
	{
	  gfc_error ("Unexpected function-result variable %qs at %L in "
		     "declarative !$OMP ALLOCATE", n->sym->name, &n->where);
	  continue;
	}
      if (ns->omp_allocate->sym->attr.proc_pointer)
	{
	  gfc_error ("Procedure pointer %qs not supported with !$OMP "
		     "ALLOCATE at %L", n->sym->name, &n->where);
	  continue;
	}
      if (n->sym->attr.flavor != FL_VARIABLE)
	{
	  gfc_error ("Argument %qs at %L to declarative !$OMP ALLOCATE "
		     "directive must be a variable", n->sym->name,
		     &n->where);
	  continue;
	}
      if (ns != n->sym->ns || n->sym->attr.use_assoc || n->sym->attr.imported)
	{
	  gfc_error ("Argument %qs at %L to declarative !$OMP ALLOCATE shall be"
		     " in the same scope as the variable declaration",
		     n->sym->name, &n->where);
	  continue;
	}
      if (n->sym->attr.dummy)
	{
	  gfc_error ("Unexpected dummy argument %qs as argument at %L to "
		     "declarative !$OMP ALLOCATE", n->sym->name, &n->where);
	  continue;
	}
      if (n->sym->attr.codimension)
	{
	  gfc_error ("Unexpected coarray argument %qs as argument at %L to "
		     "declarative !$OMP ALLOCATE", n->sym->name, &n->where);
	  continue;
	}
      if (n->sym->attr.omp_allocate)
	{
	  if (n->sym->attr.in_common)
	    {
	      gfc_error ("Duplicated common block %</%s/%> in !$OMP ALLOCATE "
			 "at %L", n->sym->common_head->name, &n->where);
	      while (n->next && n->next->sym
		     && n->sym->common_head == n->next->sym->common_head)
		n = n->next;
	    }
	  else
	    gfc_error ("Duplicated variable %qs in !$OMP ALLOCATE at %L",
		       n->sym->name, &n->where);
	  continue;
	}
      /* For 'equivalence(a,b)', a 'union_type {<type> a,b} equiv.0' is created
	 with a value expression for 'a' as 'equiv.0.a' (likewise for b); while
	 this can be handled, EQUIVALENCE is marked as obsolescent since Fortran
	 2018 and also not widely used.  However, it could be supported,
	 if needed. */
      if (n->sym->attr.in_equivalence)
	{
	  gfc_error ("Sorry, EQUIVALENCE object %qs not supported with !$OMP "
		     "ALLOCATE at %L", n->sym->name, &n->where);
	  continue;
	}
      /* Similar for Cray pointer/pointee - they could be implemented but as
	 common vendor extension but nowadays rarely used and requiring
	 -fcray-pointer, there is no need to support them.  */
      if (n->sym->attr.cray_pointer || n->sym->attr.cray_pointee)
	{
	  gfc_error ("Sorry, Cray pointers and pointees such as %qs are not "
		     "supported with !$OMP ALLOCATE at %L",
		     n->sym->name, &n->where);
	  continue;
	}
      n->sym->attr.omp_allocate = 1;
      if ((n->sym->ts.type == BT_CLASS && n->sym->attr.class_ok
	   && CLASS_DATA (n->sym)->attr.allocatable)
	  || (n->sym->ts.type != BT_CLASS && n->sym->attr.allocatable))
	gfc_error ("Unexpected allocatable variable %qs at %L in declarative "
		   "!$OMP ALLOCATE directive", n->sym->name, &n->where);
      else if ((n->sym->ts.type == BT_CLASS && n->sym->attr.class_ok
		&& CLASS_DATA (n->sym)->attr.class_pointer)
	       || (n->sym->ts.type != BT_CLASS && n->sym->attr.pointer))
	gfc_error ("Unexpected pointer variable %qs at %L in declarative "
		   "!$OMP ALLOCATE directive", n->sym->name, &n->where);
      HOST_WIDE_INT alignment = 0;
      if (n->u.align
	  && (!gfc_resolve_expr (n->u.align)
	      || n->u.align->ts.type != BT_INTEGER
	      || n->u.align->rank != 0
	      || n->u.align->expr_type != EXPR_CONSTANT
	      || gfc_extract_hwi (n->u.align, &alignment)
	      || !pow2p_hwi (alignment)))
	{
	  gfc_error ("ALIGN requires a scalar positive constant integer "
		     "alignment expression at %L that is a power of two",
		     &n->u.align->where);
	  while (n->sym->attr.in_common && n->next && n->next->sym
		 && n->sym->common_head == n->next->sym->common_head)
	    n = n->next;
	  continue;
	}
      if (n->sym->attr.in_common || n->sym->attr.save || n->sym->ns->save_all
	  || (n->sym->ns->proc_name
	      && (n->sym->ns->proc_name->attr.flavor == FL_PROGRAM
		  || n->sym->ns->proc_name->attr.flavor == FL_MODULE)))
	{
	  bool com = n->sym->attr.in_common;
	  if (!n->u2.allocator)
	    gfc_error ("An ALLOCATOR clause is required as the list item "
		       "%<%s%s%s%> at %L has the SAVE attribute", com ? "/" : "",
		       com ? n->sym->common_head->name : n->sym->name,
		       com ? "/" : "", &n->where);
	  else if (!is_predefined_allocator (n->u2.allocator))
	    gfc_error ("Predefined allocator required in ALLOCATOR clause at %L"
		       " as the list item %<%s%s%s%> at %L has the SAVE attribute",
		       &n->u2.allocator->where, com ? "/" : "",
		       com ? n->sym->common_head->name : n->sym->name,
		       com ? "/" : "", &n->where);
	  while (n->sym->attr.in_common && n->next && n->next->sym
		 && n->sym->common_head == n->next->sym->common_head)
	    n = n->next;
	}
      else if (n->u2.allocator
	  && (!gfc_resolve_expr (n->u2.allocator)
	      || n->u2.allocator->ts.type != BT_INTEGER
	      || n->u2.allocator->rank != 0
	      || n->u2.allocator->ts.kind != gfc_c_intptr_kind))
	gfc_error ("Expected integer expression of the "
		   "%<omp_allocator_handle_kind%> kind at %L",
		   &n->u2.allocator->where);
    }
}

/* Resolve ASSUME's and ASSUMES' assumption clauses.  Note that absent/contains
   is handled during parse time in omp_verify_merge_absent_contains.   */

void
gfc_resolve_omp_assumptions (gfc_omp_assumptions *assume)
{
  for (gfc_expr_list *el = assume->holds; el; el = el->next)
    if (!gfc_resolve_expr (el->expr)
	|| el->expr->ts.type != BT_LOGICAL
	|| el->expr->rank != 0)
      gfc_error ("HOLDS expression at %L must be a scalar logical expression",
		 &el->expr->where);
}


/* OpenMP directive resolving routines.  */

static void
resolve_omp_clauses (gfc_code *code, gfc_omp_clauses *omp_clauses,
		     gfc_namespace *ns, bool openacc = false)
{
  gfc_omp_namelist *n, *last;
  gfc_expr_list *el;
  int list;
  int ifc;
  bool if_without_mod = false;
  gfc_omp_linear_op linear_op = OMP_LINEAR_DEFAULT;
  static const char *clause_names[]
    = { "PRIVATE", "FIRSTPRIVATE", "LASTPRIVATE", "COPYPRIVATE", "SHARED",
	"COPYIN", "UNIFORM", "AFFINITY", "ALIGNED", "LINEAR", "DEPEND", "MAP",
	"TO", "FROM", "INCLUSIVE", "EXCLUSIVE",
	"REDUCTION", "REDUCTION" /*inscan*/, "REDUCTION" /*task*/,
	"IN_REDUCTION", "TASK_REDUCTION",
	"DEVICE_RESIDENT", "LINK", "USE_DEVICE",
	"CACHE", "IS_DEVICE_PTR", "USE_DEVICE_PTR", "USE_DEVICE_ADDR",
	"NONTEMPORAL", "ALLOCATE", "HAS_DEVICE_ADDR", "ENTER",
	"USES_ALLOCATORS", "INIT", "USE", "DESTROY", "INTEROP", "ADJUST_ARGS" };
  STATIC_ASSERT (ARRAY_SIZE (clause_names) == OMP_LIST_NUM);

  if (omp_clauses == NULL)
    return;

  if (ns == NULL)
    ns = gfc_current_ns;

  if (omp_clauses->orderedc && omp_clauses->orderedc < omp_clauses->collapse)
    gfc_error ("ORDERED clause parameter is less than COLLAPSE at %L",
	       &code->loc);
  if (omp_clauses->order_concurrent && omp_clauses->ordered)
    gfc_error ("ORDER clause must not be used together with ORDERED at %L",
	       &code->loc);
  if (omp_clauses->if_expr)
    {
      gfc_expr *expr = omp_clauses->if_expr;
      if (!gfc_resolve_expr (expr)
	  || expr->ts.type != BT_LOGICAL || expr->rank != 0)
	gfc_error ("IF clause at %L requires a scalar LOGICAL expression",
		   &expr->where);
      if_without_mod = true;
    }
  for (ifc = 0; ifc < OMP_IF_LAST; ifc++)
    if (omp_clauses->if_exprs[ifc])
      {
	gfc_expr *expr = omp_clauses->if_exprs[ifc];
	bool ok = true;
	if (!gfc_resolve_expr (expr)
	    || expr->ts.type != BT_LOGICAL || expr->rank != 0)
	  gfc_error ("IF clause at %L requires a scalar LOGICAL expression",
		     &expr->where);
	else if (if_without_mod)
	  {
	    gfc_error ("IF clause without modifier at %L used together with "
		       "IF clauses with modifiers",
		       &omp_clauses->if_expr->where);
	    if_without_mod = false;
	  }
	else
	  switch (code->op)
	    {
	    case EXEC_OMP_CANCEL:
	      ok = ifc == OMP_IF_CANCEL;
	      break;

	    case EXEC_OMP_PARALLEL:
	    case EXEC_OMP_PARALLEL_DO:
	    case EXEC_OMP_PARALLEL_LOOP:
	    case EXEC_OMP_PARALLEL_MASKED:
	    case EXEC_OMP_PARALLEL_MASTER:
	    case EXEC_OMP_PARALLEL_SECTIONS:
	    case EXEC_OMP_PARALLEL_WORKSHARE:
	    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
	    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
	      ok = ifc == OMP_IF_PARALLEL;
	      break;

	    case EXEC_OMP_PARALLEL_DO_SIMD:
	    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
	    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	      ok = ifc == OMP_IF_PARALLEL || ifc == OMP_IF_SIMD;
	      break;

	    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP:
	    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP:
	      ok = ifc == OMP_IF_PARALLEL || ifc == OMP_IF_TASKLOOP;
	      break;

	    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
	    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
	      ok = (ifc == OMP_IF_PARALLEL
		    || ifc == OMP_IF_TASKLOOP
		    || ifc == OMP_IF_SIMD);
	      break;

	    case EXEC_OMP_SIMD:
	    case EXEC_OMP_DO_SIMD:
	    case EXEC_OMP_DISTRIBUTE_SIMD:
	    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
	      ok = ifc == OMP_IF_SIMD;
	      break;

	    case EXEC_OMP_TASK:
	      ok = ifc == OMP_IF_TASK;
	      break;

	    case EXEC_OMP_TASKLOOP:
	    case EXEC_OMP_MASKED_TASKLOOP:
	    case EXEC_OMP_MASTER_TASKLOOP:
	      ok = ifc == OMP_IF_TASKLOOP;
	      break;

	    case EXEC_OMP_TASKLOOP_SIMD:
	    case EXEC_OMP_MASKED_TASKLOOP_SIMD:
	    case EXEC_OMP_MASTER_TASKLOOP_SIMD:
	      ok = ifc == OMP_IF_TASKLOOP || ifc == OMP_IF_SIMD;
	      break;

	    case EXEC_OMP_TARGET:
	    case EXEC_OMP_TARGET_TEAMS:
	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
	    case EXEC_OMP_TARGET_TEAMS_LOOP:
	      ok = ifc == OMP_IF_TARGET;
	      break;

	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
	    case EXEC_OMP_TARGET_SIMD:
	      ok = ifc == OMP_IF_TARGET || ifc == OMP_IF_SIMD;
	      break;

	    case EXEC_OMP_TARGET_DATA:
	      ok = ifc == OMP_IF_TARGET_DATA;
	      break;

	    case EXEC_OMP_TARGET_UPDATE:
	      ok = ifc == OMP_IF_TARGET_UPDATE;
	      break;

	    case EXEC_OMP_TARGET_ENTER_DATA:
	      ok = ifc == OMP_IF_TARGET_ENTER_DATA;
	      break;

	    case EXEC_OMP_TARGET_EXIT_DATA:
	      ok = ifc == OMP_IF_TARGET_EXIT_DATA;
	      break;

	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
	    case EXEC_OMP_TARGET_PARALLEL:
	    case EXEC_OMP_TARGET_PARALLEL_DO:
	    case EXEC_OMP_TARGET_PARALLEL_LOOP:
	      ok = ifc == OMP_IF_TARGET || ifc == OMP_IF_PARALLEL;
	      break;

	    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
	    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
	      ok = (ifc == OMP_IF_TARGET
		    || ifc == OMP_IF_PARALLEL
		    || ifc == OMP_IF_SIMD);
	      break;

	    default:
	      ok = false;
	      break;
	  }
	if (!ok)
	  {
	    static const char *ifs[] = {
	      "CANCEL",
	      "PARALLEL",
	      "SIMD",
	      "TASK",
	      "TASKLOOP",
	      "TARGET",
	      "TARGET DATA",
	      "TARGET UPDATE",
	      "TARGET ENTER DATA",
	      "TARGET EXIT DATA"
	    };
	    gfc_error ("IF clause modifier %s at %L not appropriate for "
		       "the current OpenMP construct", ifs[ifc], &expr->where);
	  }
      }

  if (omp_clauses->self_expr)
    {
      gfc_expr *expr = omp_clauses->self_expr;
      if (!gfc_resolve_expr (expr)
	  || expr->ts.type != BT_LOGICAL || expr->rank != 0)
	gfc_error ("SELF clause at %L requires a scalar LOGICAL expression",
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
  if (omp_clauses->novariants)
    {
      gfc_expr *expr = omp_clauses->novariants;
      if (!gfc_resolve_expr (expr) || expr->ts.type != BT_LOGICAL
	  || expr->rank != 0)
	gfc_error (
	  "NOVARIANTS clause at %L requires a scalar LOGICAL expression",
	  &expr->where);
      if_without_mod = true;
    }
  if (omp_clauses->nocontext)
    {
      gfc_expr *expr = omp_clauses->nocontext;
      if (!gfc_resolve_expr (expr) || expr->ts.type != BT_LOGICAL
	  || expr->rank != 0)
	gfc_error (
	  "NOCONTEXT clause at %L requires a scalar LOGICAL expression",
	  &expr->where);
      if_without_mod = true;
    }
  if (omp_clauses->num_threads)
    resolve_positive_int_expr (omp_clauses->num_threads, "NUM_THREADS");
  if (omp_clauses->chunk_size)
    {
      gfc_expr *expr = omp_clauses->chunk_size;
      if (!gfc_resolve_expr (expr)
	  || expr->ts.type != BT_INTEGER || expr->rank != 0)
	gfc_error ("SCHEDULE clause's chunk_size at %L requires "
		   "a scalar INTEGER expression", &expr->where);
      else if (expr->expr_type == EXPR_CONSTANT
	       && expr->ts.type == BT_INTEGER
	       && mpz_sgn (expr->value.integer) <= 0)
	gfc_warning (OPT_Wopenmp, "INTEGER expression of SCHEDULE clause's "
		     "chunk_size at %L must be positive", &expr->where);
    }
  if (omp_clauses->sched_kind != OMP_SCHED_NONE
      && omp_clauses->sched_nonmonotonic)
    {
      if (omp_clauses->sched_monotonic)
	gfc_error ("Both MONOTONIC and NONMONOTONIC schedule modifiers "
		   "specified at %L", &code->loc);
      else if (omp_clauses->ordered)
	gfc_error ("NONMONOTONIC schedule modifier specified with ORDERED "
		   "clause at %L", &code->loc);
    }

  if (omp_clauses->depobj
      && (!gfc_resolve_expr (omp_clauses->depobj)
	  || omp_clauses->depobj->ts.type != BT_INTEGER
	  || omp_clauses->depobj->ts.kind != 2 * gfc_index_integer_kind
	  || omp_clauses->depobj->rank != 0))
    gfc_error ("DEPOBJ in DEPOBJ construct at %L shall be a scalar integer "
	       "of OMP_DEPEND_KIND kind", &omp_clauses->depobj->where);

  /* Check that no symbol appears on multiple clauses, except that
     a symbol can appear on both firstprivate and lastprivate.  */
  for (list = 0; list < OMP_LIST_NUM; list++)
    for (n = omp_clauses->lists[list]; n; n = n->next)
      {
	if (!n->sym)  /* omp_all_memory.  */
	  continue;
	n->sym->mark = 0;
	n->sym->comp_mark = 0;
	n->sym->data_mark = 0;
	n->sym->dev_mark = 0;
	n->sym->gen_mark = 0;
	n->sym->reduc_mark = 0;
	if (n->sym->attr.flavor == FL_VARIABLE
	    || n->sym->attr.proc_pointer
	    || (!code && (!n->sym->attr.dummy || n->sym->ns != ns)))
	  {
	    if (!code && (!n->sym->attr.dummy || n->sym->ns != ns))
	      gfc_error ("Variable %qs is not a dummy argument at %L",
			 n->sym->name, &n->where);
	    continue;
	  }
	if (n->sym->attr.flavor == FL_PROCEDURE
	    && n->sym->result == n->sym
	    && n->sym->attr.function)
	  {
	    if (ns->proc_name == n->sym
		|| (ns->parent && ns->parent->proc_name == n->sym))
	      continue;
	    if (ns->proc_name->attr.entry_master)
	      {
		gfc_entry_list *el = ns->entries;
		for (; el; el = el->next)
		  if (el->sym == n->sym)
		    break;
		if (el)
		  continue;
	      }
	    if (ns->parent
		&& ns->parent->proc_name->attr.entry_master)
	      {
		gfc_entry_list *el = ns->parent->entries;
		for (; el; el = el->next)
		  if (el->sym == n->sym)
		    break;
		if (el)
		  continue;
	      }
	  }
	if (list == OMP_LIST_MAP
	    && n->sym->attr.flavor == FL_PARAMETER)
	  {
	    if (openacc)
	      gfc_error ("Object %qs is not a variable at %L; parameters"
			 " cannot be and need not be copied", n->sym->name,
			 &n->where);
	    else
	      gfc_error ("Object %qs is not a variable at %L; parameters"
			 " cannot be and need not be mapped", n->sym->name,
			 &n->where);
	  }
	else if (list != OMP_LIST_USES_ALLOCATORS)
	  gfc_error ("Object %qs is not a variable at %L", n->sym->name,
		     &n->where);
      }
  if (omp_clauses->lists[OMP_LIST_REDUCTION_INSCAN])
    {
      locus *loc = &omp_clauses->lists[OMP_LIST_REDUCTION_INSCAN]->where;
      if (code->op != EXEC_OMP_DO
	  && code->op != EXEC_OMP_SIMD
	  && code->op != EXEC_OMP_DO_SIMD
	  && code->op != EXEC_OMP_PARALLEL_DO
	  && code->op != EXEC_OMP_PARALLEL_DO_SIMD)
	gfc_error ("%<inscan%> REDUCTION clause on construct other than DO, "
		   "SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD at %L",
		   loc);
      if (omp_clauses->ordered)
	gfc_error ("ORDERED clause specified together with %<inscan%> "
		   "REDUCTION clause at %L", loc);
      if (omp_clauses->sched_kind != OMP_SCHED_NONE)
	gfc_error ("SCHEDULE clause specified together with %<inscan%> "
		   "REDUCTION clause at %L", loc);
    }

  for (list = 0; list < OMP_LIST_NUM; list++)
    if (list != OMP_LIST_FIRSTPRIVATE
	&& list != OMP_LIST_LASTPRIVATE
	&& list != OMP_LIST_ALIGNED
	&& list != OMP_LIST_DEPEND
	&& list != OMP_LIST_FROM
	&& list != OMP_LIST_TO
	&& list != OMP_LIST_INTEROP
	&& (list != OMP_LIST_REDUCTION || !openacc)
	&& list != OMP_LIST_ALLOCATE)
      for (n = omp_clauses->lists[list]; n; n = n->next)
	{
	  bool component_ref_p = false;

	  /* Allow multiple components of the same (e.g. derived-type)
	     variable here.  Duplicate components are detected elsewhere.  */
	  if (n->expr && n->expr->expr_type == EXPR_VARIABLE)
	    for (gfc_ref *ref = n->expr->ref; ref; ref = ref->next)
	      if (ref->type == REF_COMPONENT)
		component_ref_p = true;
	  if ((list == OMP_LIST_IS_DEVICE_PTR
	       || list == OMP_LIST_HAS_DEVICE_ADDR)
	      && !component_ref_p)
	    {
	      if (n->sym->gen_mark
		  || n->sym->dev_mark
		  || n->sym->reduc_mark
		  || n->sym->mark)
		gfc_error ("Symbol %qs present on multiple clauses at %L",
			   n->sym->name, &n->where);
	      else
		n->sym->dev_mark = 1;
	    }
	  else if ((list == OMP_LIST_USE_DEVICE_PTR
		    || list == OMP_LIST_USE_DEVICE_ADDR
		    || list == OMP_LIST_PRIVATE
		    || list == OMP_LIST_SHARED)
		   && !component_ref_p)
	    {
	      if (n->sym->gen_mark || n->sym->dev_mark || n->sym->reduc_mark)
		gfc_error ("Symbol %qs present on multiple clauses at %L",
			   n->sym->name, &n->where);
	      else
		{
		  n->sym->gen_mark = 1;
		  /* Set both generic and device bits if we have
		     use_device_*(x) or shared(x).  This allows us to diagnose
		     "map(x) private(x)" below.  */
		  if (list != OMP_LIST_PRIVATE)
		    n->sym->dev_mark = 1;
		}
	    }
	  else if ((list == OMP_LIST_REDUCTION
		    || list == OMP_LIST_REDUCTION_TASK
		    || list == OMP_LIST_REDUCTION_INSCAN
		    || list == OMP_LIST_IN_REDUCTION
		    || list == OMP_LIST_TASK_REDUCTION)
		   && !component_ref_p)
	    {
	      /* Attempts to mix reduction types are diagnosed below.  */
	      if (n->sym->gen_mark || n->sym->dev_mark)
		gfc_error ("Symbol %qs present on multiple clauses at %L",
			   n->sym->name, &n->where);
	      n->sym->reduc_mark = 1;
	    }
	  else if ((!component_ref_p && n->sym->comp_mark)
		   || (component_ref_p && n->sym->mark))
	    {
	      if (openacc)
		gfc_error ("Symbol %qs has mixed component and non-component "
			   "accesses at %L", n->sym->name, &n->where);
	    }
	  else if (n->sym->mark)
	    gfc_error ("Symbol %qs present on multiple clauses at %L",
		       n->sym->name, &n->where);
	  else
	    {
	      if (component_ref_p)
		n->sym->comp_mark = 1;
	      else
		n->sym->mark = 1;
	    }
	}

  if (code
      && code->op == EXEC_OMP_INTEROP
      && omp_clauses->lists[OMP_LIST_DEPEND])
    {
      if (!omp_clauses->lists[OMP_LIST_INIT]
	  && !omp_clauses->lists[OMP_LIST_USE]
	  && !omp_clauses->lists[OMP_LIST_DESTROY])
	{
	  gfc_error ("DEPEND clause at %L requires action clause with "
		     "%<targetsync%> interop-type",
		     &omp_clauses->lists[OMP_LIST_DEPEND]->where);
	}
      for (n = omp_clauses->lists[OMP_LIST_INIT]; n; n = n->next)
	if (!n->u.init.targetsync)
	  {
	    gfc_error ("DEPEND clause at %L requires %<targetsync%> "
		       "interop-type, lacking it for %qs at %L",
		       &omp_clauses->lists[OMP_LIST_DEPEND]->where,
		       n->sym->name, &n->where);
	    break;
	  }
    }
  if (code && (code->op == EXEC_OMP_INTEROP || code->op == EXEC_OMP_DISPATCH))
    for (list = OMP_LIST_INIT; list <= OMP_LIST_INTEROP; list++)
      for (n = omp_clauses->lists[list]; n; n = n->next)
	{
	  if (n->sym->ts.type != BT_INTEGER
	      || n->sym->ts.kind != gfc_index_integer_kind
	      || n->sym->attr.dimension
	      || n->sym->attr.flavor != FL_VARIABLE)
	    gfc_error ("%qs at %L in %qs clause must be a scalar integer "
		       "variable of %<omp_interop_kind%> kind", n->sym->name,
		       &n->where, clause_names[list]);
	  if (list != OMP_LIST_USE && list != OMP_LIST_INTEROP
	      && n->sym->attr.intent == INTENT_IN)
	    gfc_error ("%qs at %L in %qs clause must be definable",
		       n->sym->name, &n->where, clause_names[list]);
	}

  /* Detect specifically the case where we have "map(x) private(x)" and raise
     an error.  If we have "...simd" combined directives though, the "private"
     applies to the simd part, so this is permitted though.  */
  for (n = omp_clauses->lists[OMP_LIST_PRIVATE]; n; n = n->next)
    if (n->sym->mark
	&& n->sym->gen_mark
	&& !n->sym->dev_mark
	&& !n->sym->reduc_mark
	&& code->op != EXEC_OMP_TARGET_SIMD
	&& code->op != EXEC_OMP_TARGET_PARALLEL_DO_SIMD
	&& code->op != EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD
	&& code->op != EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD)
      gfc_error ("Symbol %qs present on multiple clauses at %L",
		 n->sym->name, &n->where);

  gcc_assert (OMP_LIST_LASTPRIVATE == OMP_LIST_FIRSTPRIVATE + 1);
  for (list = OMP_LIST_FIRSTPRIVATE; list <= OMP_LIST_LASTPRIVATE; list++)
    for (n = omp_clauses->lists[list]; n; n = n->next)
      if (n->sym->data_mark || n->sym->gen_mark || n->sym->dev_mark)
	{
	  gfc_error ("Symbol %qs present on multiple clauses at %L",
		     n->sym->name, &n->where);
	  n->sym->data_mark = n->sym->gen_mark = n->sym->dev_mark = 0;
	}
      else if (n->sym->mark
	       && code->op != EXEC_OMP_TARGET_TEAMS
	       && code->op != EXEC_OMP_TARGET_TEAMS_DISTRIBUTE
	       && code->op != EXEC_OMP_TARGET_TEAMS_LOOP
	       && code->op != EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD
	       && code->op != EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO
	       && code->op != EXEC_OMP_TARGET_PARALLEL
	       && code->op != EXEC_OMP_TARGET_PARALLEL_DO
	       && code->op != EXEC_OMP_TARGET_PARALLEL_LOOP
	       && code->op != EXEC_OMP_TARGET_PARALLEL_DO_SIMD
	       && code->op != EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD)
	gfc_error ("Symbol %qs present on both data and map clauses "
		   "at %L", n->sym->name, &n->where);

  for (n = omp_clauses->lists[OMP_LIST_FIRSTPRIVATE]; n; n = n->next)
    {
      if (n->sym->data_mark || n->sym->gen_mark || n->sym->dev_mark)
	gfc_error ("Symbol %qs present on multiple clauses at %L",
		   n->sym->name, &n->where);
      else
	n->sym->data_mark = 1;
    }
  for (n = omp_clauses->lists[OMP_LIST_LASTPRIVATE]; n; n = n->next)
    n->sym->data_mark = 0;

  for (n = omp_clauses->lists[OMP_LIST_LASTPRIVATE]; n; n = n->next)
    {
      if (n->sym->data_mark || n->sym->gen_mark || n->sym->dev_mark)
	gfc_error ("Symbol %qs present on multiple clauses at %L",
		   n->sym->name, &n->where);
      else
	n->sym->data_mark = 1;
    }

  for (n = omp_clauses->lists[OMP_LIST_ALIGNED]; n; n = n->next)
    n->sym->mark = 0;

  for (n = omp_clauses->lists[OMP_LIST_ALIGNED]; n; n = n->next)
    {
      if (n->sym->mark)
	gfc_error ("Symbol %qs present on multiple clauses at %L",
		   n->sym->name, &n->where);
      else
	n->sym->mark = 1;
    }

  if (omp_clauses->lists[OMP_LIST_ALLOCATE])
    {
      for (n = omp_clauses->lists[OMP_LIST_ALLOCATE]; n; n = n->next)
	{
	  if (n->u2.allocator
	      && (!gfc_resolve_expr (n->u2.allocator)
		  || n->u2.allocator->ts.type != BT_INTEGER
		  || n->u2.allocator->rank != 0
		  || n->u2.allocator->ts.kind != gfc_c_intptr_kind))
	    {
	      gfc_error ("Expected integer expression of the "
			 "%<omp_allocator_handle_kind%> kind at %L",
			 &n->u2.allocator->where);
	      break;
	    }
	  if (!n->u.align)
	    continue;
	  HOST_WIDE_INT alignment = 0;
	  if (!gfc_resolve_expr (n->u.align)
	      || n->u.align->ts.type != BT_INTEGER
	      || n->u.align->rank != 0
	      || n->u.align->expr_type != EXPR_CONSTANT
	      || gfc_extract_hwi (n->u.align, &alignment)
	      || alignment <= 0
	      || !pow2p_hwi (alignment))
	    {
	      gfc_error ("ALIGN requires a scalar positive constant integer "
			 "alignment expression at %L that is a power of two",
			 &n->u.align->where);
	      break;
	    }
	}

      /* Check for 2 things here.
	 1.  There is no duplication of variable in allocate clause.
	 2.  Variable in allocate clause are also present in some
	     privatization clase (non-composite case).  */
      for (n = omp_clauses->lists[OMP_LIST_ALLOCATE]; n; n = n->next)
	if (n->sym)
	  n->sym->mark = 0;

      gfc_omp_namelist *prev = NULL;
      for (n = omp_clauses->lists[OMP_LIST_ALLOCATE]; n; )
	{
	  if (n->sym == NULL)
	    {
	      n = n->next;
	      continue;
	    }
	  if (n->sym->mark == 1)
	    {
	      gfc_warning (OPT_Wopenmp, "%qs appears more than once in "
			   "%<allocate%> at %L" , n->sym->name, &n->where);
	      /* We have already seen this variable so it is a duplicate.
		 Remove it.  */
	      if (prev != NULL && prev->next == n)
		{
		  prev->next = n->next;
		  n->next = NULL;
		  gfc_free_omp_namelist (n, false, true, false, false);
		  n = prev->next;
		}
	      continue;
	    }
	  n->sym->mark = 1;
	  prev = n;
	  n = n->next;
	}

      /* Non-composite constructs.  */
      if (code && code->op < EXEC_OMP_DO_SIMD)
	{
	  for (list = 0; list < OMP_LIST_NUM; list++)
	    switch (list)
	    {
	      case OMP_LIST_PRIVATE:
	      case OMP_LIST_FIRSTPRIVATE:
	      case OMP_LIST_LASTPRIVATE:
	      case OMP_LIST_REDUCTION:
	      case OMP_LIST_REDUCTION_INSCAN:
	      case OMP_LIST_REDUCTION_TASK:
	      case OMP_LIST_IN_REDUCTION:
	      case OMP_LIST_TASK_REDUCTION:
	      case OMP_LIST_LINEAR:
		for (n = omp_clauses->lists[list]; n; n = n->next)
		  n->sym->mark = 0;
		break;
	      default:
		break;
	    }

	  for (n = omp_clauses->lists[OMP_LIST_ALLOCATE]; n; n = n->next)
	    if (n->sym->mark == 1)
	      gfc_error ("%qs specified in %<allocate%> clause at %L but not "
			 "in an explicit privatization clause",
			 n->sym->name, &n->where);
	}
      if (code
	  && (code->op == EXEC_OMP_ALLOCATORS || code->op == EXEC_OMP_ALLOCATE)
	  && code->block
	  && code->block->next
	  && code->block->next->op == EXEC_ALLOCATE)
	{
	  gfc_alloc *a;
	  gfc_omp_namelist *n_null = NULL;
	  bool missing_allocator = false;
	  gfc_symbol *missing_allocator_sym = NULL;
	  for (n = omp_clauses->lists[OMP_LIST_ALLOCATE]; n; n = n->next)
	    {
	      if (n->u2.allocator == NULL)
		{
		  if (!missing_allocator_sym)
		    missing_allocator_sym = n->sym;
		  missing_allocator = true;
		}
	      if (n->sym == NULL)
		{
		  n_null = n;
		  continue;
		}
	      if (n->sym->attr.codimension)
		gfc_error ("Unexpected coarray %qs in %<allocate%> at %L",
			   n->sym->name, &n->where);
	      for (a = code->block->next->ext.alloc.list; a; a = a->next)
		if (a->expr->expr_type == EXPR_VARIABLE
		    && a->expr->symtree->n.sym == n->sym)
		  {
		    gfc_ref *ref;
		    for (ref = a->expr->ref; ref; ref = ref->next)
		      if (ref->type == REF_COMPONENT)
			break;
		    if (ref == NULL)
		      break;
		  }
	      if (a == NULL)
		gfc_error ("%qs specified in %<allocate%> at %L but not "
			   "in the associated ALLOCATE statement",
			   n->sym->name, &n->where);
	    }
	  /* If there is an ALLOCATE directive without list argument, a
	     namelist with its allocator/align clauses and n->sym = NULL is
	     created during parsing; here, we add all not otherwise specified
	     items from the Fortran allocate to that list.
	     For an ALLOCATORS directive, not listed items use the normal
	     Fortran way.
	     The behavior of an ALLOCATE directive that does not list all
	     arguments but there is no directive without list argument is not
	     well specified.  Thus, we reject such code below. In OpenMP 5.2
	     the executable ALLOCATE directive is deprecated and in 6.0
	     deleted such that no spec clarification is to be expected.  */
	  for (a = code->block->next->ext.alloc.list; a; a = a->next)
	    if (a->expr->expr_type == EXPR_VARIABLE)
	      {
		for (n = omp_clauses->lists[OMP_LIST_ALLOCATE]; n; n = n->next)
		  if (a->expr->symtree->n.sym == n->sym)
		    {
		      gfc_ref *ref;
		      for (ref = a->expr->ref; ref; ref = ref->next)
			if (ref->type == REF_COMPONENT)
			  break;
		      if (ref == NULL)
			break;
		    }
		if (n == NULL && n_null == NULL)
		  {
		    /* OK for ALLOCATORS but for ALLOCATE: Unspecified whether
		       that should use the default allocator of OpenMP or the
		       Fortran allocator. Thus, just reject it.  */
		    if (code->op == EXEC_OMP_ALLOCATE)
		      gfc_error ("%qs listed in %<allocate%> statement at %L "
				 "but it is neither explicitly in listed in "
				 "the %<!$OMP ALLOCATE%> directive nor exists"
				 " a directive without argument list",
				 a->expr->symtree->n.sym->name,
				 &a->expr->where);
		    break;
		  }
		if (n == NULL)
		  {
		    if (a->expr->symtree->n.sym->attr.codimension)
		      gfc_error ("Unexpected coarray %qs in %<allocate%> at "
				 "%L, implicitly listed in %<!$OMP ALLOCATE%>"
				 " at %L", a->expr->symtree->n.sym->name,
				 &a->expr->where, &n_null->where);
		    break;
		  }
	    }
	  gfc_namespace *prog_unit = ns;
	  while (prog_unit->parent)
	    prog_unit = prog_unit->parent;
	  gfc_namespace *fn_ns = ns;
	  while (fn_ns)
	    {
	      if (ns->proc_name
		  && (ns->proc_name->attr.subroutine
		      || ns->proc_name->attr.function))
		break;
	      fn_ns = fn_ns->parent;
	    }
	  if (missing_allocator
	      && !(prog_unit->omp_requires & OMP_REQ_DYNAMIC_ALLOCATORS)
	      && ((fn_ns && fn_ns->proc_name->attr.omp_declare_target)
		  || omp_clauses->contained_in_target_construct))
	    {
	      if (code->op == EXEC_OMP_ALLOCATORS)
		gfc_error ("ALLOCATORS directive at %L inside a target region "
			   "must specify an ALLOCATOR modifier for %qs",
			   &code->loc, missing_allocator_sym->name);
	      else if (missing_allocator_sym)
		gfc_error ("ALLOCATE directive at %L inside a target region "
			   "must specify an ALLOCATOR clause for %qs",
			   &code->loc, missing_allocator_sym->name);
	      else
		gfc_error ("ALLOCATE directive at %L inside a target region "
			   "must specify an ALLOCATOR clause", &code->loc);
	    }

	}
    }

  /* OpenACC reductions.  */
  if (openacc)
    {
      for (n = omp_clauses->lists[OMP_LIST_REDUCTION]; n; n = n->next)
	n->sym->mark = 0;

      for (n = omp_clauses->lists[OMP_LIST_REDUCTION]; n; n = n->next)
	{
	  if (n->sym->mark)
	    gfc_error ("Symbol %qs present on multiple clauses at %L",
		       n->sym->name, &n->where);
	  else
	    n->sym->mark = 1;

	  /* OpenACC does not support reductions on arrays.  */
	  if (n->sym->as)
	    gfc_error ("Array %qs is not permitted in reduction at %L",
		       n->sym->name, &n->where);
	}
    }

  for (n = omp_clauses->lists[OMP_LIST_TO]; n; n = n->next)
    n->sym->mark = 0;
  for (n = omp_clauses->lists[OMP_LIST_FROM]; n; n = n->next)
    if (n->expr == NULL)
      n->sym->mark = 1;
  for (n = omp_clauses->lists[OMP_LIST_TO]; n; n = n->next)
    {
      if (n->expr == NULL && n->sym->mark)
	gfc_error ("Symbol %qs present on both FROM and TO clauses at %L",
		   n->sym->name, &n->where);
      else
	n->sym->mark = 1;
    }

  bool has_inscan = false, has_notinscan = false;
  for (list = 0; list < OMP_LIST_NUM; list++)
    if ((n = omp_clauses->lists[list]) != NULL)
      {
	const char *name = clause_names[list];

	switch (list)
	  {
	  case OMP_LIST_COPYIN:
	    for (; n != NULL; n = n->next)
	      {
		if (!n->sym->attr.threadprivate)
		  gfc_error ("Non-THREADPRIVATE object %qs in COPYIN clause"
			     " at %L", n->sym->name, &n->where);
	      }
	    break;
	  case OMP_LIST_COPYPRIVATE:
	    if (omp_clauses->nowait)
	      gfc_error ("NOWAIT clause must not be used with COPYPRIVATE "
			 "clause at %L", &n->where);
	    for (; n != NULL; n = n->next)
	      {
		if (n->sym->as && n->sym->as->type == AS_ASSUMED_SIZE)
		  gfc_error ("Assumed size array %qs in COPYPRIVATE clause "
			     "at %L", n->sym->name, &n->where);
		if (n->sym->attr.pointer && n->sym->attr.intent == INTENT_IN)
		  gfc_error ("INTENT(IN) POINTER %qs in COPYPRIVATE clause "
			     "at %L", n->sym->name, &n->where);
	      }
	    break;
	  case OMP_LIST_SHARED:
	    for (; n != NULL; n = n->next)
	      {
		if (n->sym->attr.threadprivate)
		  gfc_error ("THREADPRIVATE object %qs in SHARED clause at "
			     "%L", n->sym->name, &n->where);
		if (n->sym->attr.cray_pointee)
		  gfc_error ("Cray pointee %qs in SHARED clause at %L",
			    n->sym->name, &n->where);
		if (n->sym->attr.associate_var)
		  gfc_error ("Associate name %qs in SHARED clause at %L",
			     n->sym->attr.select_type_temporary
			     ? n->sym->assoc->target->symtree->n.sym->name
			     : n->sym->name, &n->where);
		if (omp_clauses->detach
		    && n->sym == omp_clauses->detach->symtree->n.sym)
		  gfc_error ("DETACH event handle %qs in SHARED clause at %L",
			     n->sym->name, &n->where);
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
		  gfc_error ("%qs in ALIGNED clause must be POINTER, "
			     "ALLOCATABLE, Cray pointer or C_PTR at %L",
			     n->sym->name, &n->where);
		else if (n->expr)
		  {
		    if (!gfc_resolve_expr (n->expr)
			|| n->expr->ts.type != BT_INTEGER
			|| n->expr->rank != 0
			|| n->expr->expr_type != EXPR_CONSTANT
			|| mpz_sgn (n->expr->value.integer) <= 0)
		      gfc_error ("%qs in ALIGNED clause at %L requires a scalar"
				 " positive constant integer alignment "
				 "expression", n->sym->name, &n->where);
		  }
	      }
	    break;
	  case OMP_LIST_AFFINITY:
	  case OMP_LIST_DEPEND:
	  case OMP_LIST_MAP:
	  case OMP_LIST_TO:
	  case OMP_LIST_FROM:
	  case OMP_LIST_CACHE:
	    for (; n != NULL; n = n->next)
	      {
		if ((list == OMP_LIST_DEPEND || list == OMP_LIST_AFFINITY)
		    && n->u2.ns && !n->u2.ns->resolved)
		  {
		    n->u2.ns->resolved = 1;
		    for (gfc_symbol *sym = n->u2.ns->omp_affinity_iterators;
			 sym; sym = sym->tlink)
		      {
			gfc_constructor *c;
			c = gfc_constructor_first (sym->value->value.constructor);
			if (!gfc_resolve_expr (c->expr)
			    || c->expr->ts.type != BT_INTEGER
			    || c->expr->rank != 0)
			  gfc_error ("Scalar integer expression for range begin"
				     " expected at %L", &c->expr->where);
			c = gfc_constructor_next (c);
			if (!gfc_resolve_expr (c->expr)
			    || c->expr->ts.type != BT_INTEGER
			    || c->expr->rank != 0)
			  gfc_error ("Scalar integer expression for range end "
				     "expected at %L", &c->expr->where);
			c = gfc_constructor_next (c);
			if (c && (!gfc_resolve_expr (c->expr)
				  || c->expr->ts.type != BT_INTEGER
				  || c->expr->rank != 0))
			  gfc_error ("Scalar integer expression for range step "
				     "expected at %L", &c->expr->where);
			else if (c
				 && c->expr->expr_type == EXPR_CONSTANT
				 && mpz_cmp_si (c->expr->value.integer, 0) == 0)
			  gfc_error ("Nonzero range step expected at %L",
				     &c->expr->where);
		      }
		  }

		if (list == OMP_LIST_DEPEND)
		  {
		    if (n->u.depend_doacross_op == OMP_DEPEND_SINK_FIRST
			|| n->u.depend_doacross_op == OMP_DOACROSS_SINK_FIRST
			|| n->u.depend_doacross_op == OMP_DOACROSS_SINK)
		      {
			if (omp_clauses->doacross_source)
			  {
			    gfc_error ("Dependence-type SINK used together with"
				       " SOURCE on the same construct at %L",
				       &n->where);
			    omp_clauses->doacross_source = false;
			  }
			else if (n->expr)
			  {
			    if (!gfc_resolve_expr (n->expr)
				|| n->expr->ts.type != BT_INTEGER
				|| n->expr->rank != 0)
			      gfc_error ("SINK addend not a constant integer "
					 "at %L", &n->where);
			  }
			if (n->sym == NULL
			    && (n->expr == NULL
				|| mpz_cmp_si (n->expr->value.integer, -1) != 0))
			  gfc_error ("omp_cur_iteration at %L requires %<-1%> "
				     "as logical offset", &n->where);
			continue;
		      }
		    else if (n->u.depend_doacross_op == OMP_DEPEND_DEPOBJ
			     && !n->expr
			     && (n->sym->ts.type != BT_INTEGER
				 || n->sym->ts.kind
				    != 2 * gfc_index_integer_kind
				 || n->sym->attr.dimension))
		      gfc_error ("Locator %qs at %L in DEPEND clause of depobj "
				 "type shall be a scalar integer of "
				 "OMP_DEPEND_KIND kind", n->sym->name,
				 &n->where);
		    else if (n->u.depend_doacross_op == OMP_DEPEND_DEPOBJ
			     && n->expr
			     && (!gfc_resolve_expr (n->expr)
				 || n->expr->ts.type != BT_INTEGER
				 || n->expr->ts.kind
				    != 2 * gfc_index_integer_kind
				 || n->expr->rank != 0))
		      gfc_error ("Locator at %L in DEPEND clause of depobj "
				 "type shall be a scalar integer of "
				 "OMP_DEPEND_KIND kind", &n->expr->where);
		  }
		gfc_ref *lastref = NULL, *lastslice = NULL;
		bool resolved = false;
		if (n->expr)
		  {
		    lastref = n->expr->ref;
		    resolved = gfc_resolve_expr (n->expr);

		    /* Look through component refs to find last array
		       reference.  */
		    if (resolved)
		      {
			for (gfc_ref *ref = n->expr->ref; ref; ref = ref->next)
			  if (ref->type == REF_COMPONENT
			      || ref->type == REF_SUBSTRING
			      || ref->type == REF_INQUIRY)
			    lastref = ref;
			  else if (ref->type == REF_ARRAY)
			    {
			      for (int i = 0; i < ref->u.ar.dimen; i++)
				if (ref->u.ar.dimen_type[i] == DIMEN_RANGE)
				  lastslice = ref;

			      lastref = ref;
			    }

			/* The "!$acc cache" directive allows rectangular
			   subarrays to be specified, with some restrictions
			   on the form of bounds (not implemented).
			   Only raise an error here if we're really sure the
			   array isn't contiguous.  An expression such as
			   arr(-n:n,-n:n) could be contiguous even if it looks
			   like it may not be.  */
			if (code->op != EXEC_OACC_UPDATE
			    && list != OMP_LIST_CACHE
			    && list != OMP_LIST_DEPEND
			    && !gfc_is_simply_contiguous (n->expr, false, true)
			    && gfc_is_not_contiguous (n->expr)
			    && !(lastslice
				 && (lastslice->next
				     || lastslice->type != REF_ARRAY)))
			  gfc_error ("Array is not contiguous at %L",
				     &n->where);
		      }
		  }
		if (openacc
		    && list == OMP_LIST_MAP
		    && (n->u.map.op == OMP_MAP_ATTACH
			|| n->u.map.op == OMP_MAP_DETACH))
		  {
		    symbol_attribute attr;
		    if (n->expr)
		      attr = gfc_expr_attr (n->expr);
		    else
		      attr = n->sym->attr;
		    if (!attr.pointer && !attr.allocatable)
		      gfc_error ("%qs clause argument must be ALLOCATABLE or "
				 "a POINTER at %L",
				 (n->u.map.op == OMP_MAP_ATTACH) ? "attach"
				 : "detach", &n->where);
		  }
		if (lastref
		    || (n->expr
			&& (!resolved || n->expr->expr_type != EXPR_VARIABLE)))
		  {
		    if (!lastslice
			&& lastref
			&& lastref->type == REF_SUBSTRING)
		      gfc_error ("Unexpected substring reference in %s clause "
				 "at %L", name, &n->where);
		    else if (!lastslice
			     && lastref
			     && lastref->type == REF_INQUIRY)
		      {
			gcc_assert (lastref->u.i == INQUIRY_RE
				    || lastref->u.i == INQUIRY_IM);
			gfc_error ("Unexpected complex-parts designator "
				   "reference in %s clause at %L",
				   name, &n->where);
		      }
		    else if (!resolved
			     || n->expr->expr_type != EXPR_VARIABLE
			     || (lastslice
				 && (lastslice->next
				     || lastslice->type != REF_ARRAY)))
		      gfc_error ("%qs in %s clause at %L is not a proper "
				 "array section", n->sym->name, name,
				 &n->where);
		    else if (lastslice)
		      {
			int i;
			gfc_array_ref *ar = &lastslice->u.ar;
			for (i = 0; i < ar->dimen; i++)
			  if (ar->stride[i] && code->op != EXEC_OACC_UPDATE)
			    {
			      gfc_error ("Stride should not be specified for "
					 "array section in %s clause at %L",
					 name, &n->where);
			      break;
			    }
			  else if (ar->dimen_type[i] != DIMEN_ELEMENT
				   && ar->dimen_type[i] != DIMEN_RANGE)
			    {
			      gfc_error ("%qs in %s clause at %L is not a "
					 "proper array section",
					 n->sym->name, name, &n->where);
			      break;
			    }
			  else if ((list == OMP_LIST_DEPEND
				    || list == OMP_LIST_AFFINITY)
				   && ar->start[i]
				   && ar->start[i]->expr_type == EXPR_CONSTANT
				   && ar->end[i]
				   && ar->end[i]->expr_type == EXPR_CONSTANT
				   && mpz_cmp (ar->start[i]->value.integer,
					       ar->end[i]->value.integer) > 0)
			    {
			      gfc_error ("%qs in %s clause at %L is a "
					 "zero size array section",
					 n->sym->name,
					 list == OMP_LIST_DEPEND
					 ? "DEPEND" : "AFFINITY", &n->where);
			      break;
			    }
		      }
		  }
		else if (openacc)
		  {
		    if (list == OMP_LIST_MAP
			&& n->u.map.op == OMP_MAP_FORCE_DEVICEPTR)
		      resolve_oacc_deviceptr_clause (n->sym, n->where, name);
		    else
		      resolve_oacc_data_clauses (n->sym, n->where, name);
		  }
		else if (list != OMP_LIST_DEPEND
			 && n->sym->as
			 && n->sym->as->type == AS_ASSUMED_SIZE)
		  gfc_error ("Assumed size array %qs in %s clause at %L",
			     n->sym->name, name, &n->where);
		if (!openacc
		    && list == OMP_LIST_MAP
		    && n->sym->ts.type == BT_DERIVED
		    && n->sym->ts.u.derived->attr.alloc_comp)
		  gfc_error ("List item %qs with allocatable components is not "
			     "permitted in map clause at %L", n->sym->name,
			     &n->where);
		if (!openacc
		    && (list == OMP_LIST_MAP
			|| list == OMP_LIST_FROM
			|| list == OMP_LIST_TO)
		    && ((n->expr && n->expr->ts.type == BT_CLASS)
			|| (!n->expr && n->sym->ts.type == BT_CLASS)))
		  gfc_warning (OPT_Wopenmp,
			       "Mapping polymorphic list item at %L is "
			       "unspecified behavior", &n->where);
		if (list == OMP_LIST_MAP && !openacc)
		  switch (code->op)
		    {
		    case EXEC_OMP_TARGET:
		    case EXEC_OMP_TARGET_PARALLEL:
		    case EXEC_OMP_TARGET_PARALLEL_DO:
		    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
		    case EXEC_OMP_TARGET_PARALLEL_LOOP:
		    case EXEC_OMP_TARGET_SIMD:
		    case EXEC_OMP_TARGET_TEAMS:
		    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
		    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
		    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
		    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
		    case EXEC_OMP_TARGET_TEAMS_LOOP:
		    case EXEC_OMP_TARGET_DATA:
		      switch (n->u.map.op)
			{
			case OMP_MAP_TO:
			case OMP_MAP_ALWAYS_TO:
			case OMP_MAP_PRESENT_TO:
			case OMP_MAP_ALWAYS_PRESENT_TO:
			case OMP_MAP_FROM:
			case OMP_MAP_ALWAYS_FROM:
			case OMP_MAP_PRESENT_FROM:
			case OMP_MAP_ALWAYS_PRESENT_FROM:
			case OMP_MAP_TOFROM:
			case OMP_MAP_ALWAYS_TOFROM:
			case OMP_MAP_PRESENT_TOFROM:
			case OMP_MAP_ALWAYS_PRESENT_TOFROM:
			case OMP_MAP_ALLOC:
			case OMP_MAP_PRESENT_ALLOC:
			  break;
			default:
			  gfc_error ("TARGET%s with map-type other than TO, "
				     "FROM, TOFROM, or ALLOC on MAP clause "
				     "at %L",
				     code->op == EXEC_OMP_TARGET_DATA
				     ? " DATA" : "", &n->where);
			  break;
			}
		      break;
		    case EXEC_OMP_TARGET_ENTER_DATA:
		      switch (n->u.map.op)
			{
			case OMP_MAP_TO:
			case OMP_MAP_ALWAYS_TO:
			case OMP_MAP_PRESENT_TO:
			case OMP_MAP_ALWAYS_PRESENT_TO:
			case OMP_MAP_ALLOC:
			case OMP_MAP_PRESENT_ALLOC:
			  break;
			case OMP_MAP_TOFROM:
			  n->u.map.op = OMP_MAP_TO;
			  break;
			case OMP_MAP_ALWAYS_TOFROM:
			  n->u.map.op = OMP_MAP_ALWAYS_TO;
			  break;
			case OMP_MAP_PRESENT_TOFROM:
			  n->u.map.op = OMP_MAP_PRESENT_TO;
			  break;
			case OMP_MAP_ALWAYS_PRESENT_TOFROM:
			  n->u.map.op = OMP_MAP_ALWAYS_PRESENT_TO;
			  break;
			default:
			  gfc_error ("TARGET ENTER DATA with map-type other "
				     "than TO, TOFROM or ALLOC on MAP clause "
				     "at %L", &n->where);
			  break;
			}
		      break;
		    case EXEC_OMP_TARGET_EXIT_DATA:
		      switch (n->u.map.op)
			{
			case OMP_MAP_FROM:
			case OMP_MAP_ALWAYS_FROM:
			case OMP_MAP_PRESENT_FROM:
			case OMP_MAP_ALWAYS_PRESENT_FROM:
			case OMP_MAP_RELEASE:
			case OMP_MAP_DELETE:
			  break;
			case OMP_MAP_TOFROM:
			  n->u.map.op = OMP_MAP_FROM;
			  break;
			case OMP_MAP_ALWAYS_TOFROM:
			  n->u.map.op = OMP_MAP_ALWAYS_FROM;
			  break;
			case OMP_MAP_PRESENT_TOFROM:
			  n->u.map.op = OMP_MAP_PRESENT_FROM;
			  break;
			case OMP_MAP_ALWAYS_PRESENT_TOFROM:
			  n->u.map.op = OMP_MAP_ALWAYS_PRESENT_FROM;
			  break;
			default:
			  gfc_error ("TARGET EXIT DATA with map-type other "
				     "than FROM, TOFROM, RELEASE, or DELETE on "
				     "MAP clause at %L", &n->where);
			  break;
			}
		      break;
		    default:
		      break;
		    }
	      }

	    if (list != OMP_LIST_DEPEND)
	      for (n = omp_clauses->lists[list]; n != NULL; n = n->next)
		{
		  n->sym->attr.referenced = 1;
		  if (n->sym->attr.threadprivate)
		    gfc_error ("THREADPRIVATE object %qs in %s clause at %L",
			       n->sym->name, name, &n->where);
		  if (n->sym->attr.cray_pointee)
		    gfc_error ("Cray pointee %qs in %s clause at %L",
			       n->sym->name, name, &n->where);
		}
	    break;
	  case OMP_LIST_IS_DEVICE_PTR:
	    last = NULL;
	    for (n = omp_clauses->lists[list]; n != NULL; )
	      {
		if ((n->sym->ts.type != BT_DERIVED
		     || !n->sym->ts.u.derived->ts.is_iso_c
		     || (n->sym->ts.u.derived->intmod_sym_id
			 != ISOCBINDING_PTR))
		    && code->op == EXEC_OMP_DISPATCH)
		  /* Non-TARGET (i.e. DISPATCH) requires a C_PTR.  */
		  gfc_error ("List item %qs in %s clause at %L must be of "
			     "TYPE(C_PTR)", n->sym->name, name, &n->where);
		else if (n->sym->ts.type != BT_DERIVED
			 || !n->sym->ts.u.derived->ts.is_iso_c
			 || (n->sym->ts.u.derived->intmod_sym_id
			     != ISOCBINDING_PTR))
		  {
		    /* For TARGET, non-C_PTR are deprecated and handled as
		       has_device_addr.  */
		    gfc_omp_namelist *n2 = n;
		    n = n->next;
		    if (last)
		      last->next = n;
		    else
		      omp_clauses->lists[list] = n;
		    n2->next = omp_clauses->lists[OMP_LIST_HAS_DEVICE_ADDR];
		    omp_clauses->lists[OMP_LIST_HAS_DEVICE_ADDR] = n2;
		    continue;
		  }
		last = n;
		n = n->next;
	      }
	    break;
	  case OMP_LIST_HAS_DEVICE_ADDR:
	  case OMP_LIST_USE_DEVICE_ADDR:
	    break;
	  case OMP_LIST_USE_DEVICE_PTR:
	    /* Non-C_PTR are deprecated and handled as use_device_ADDR.  */
	    last = NULL;
	    for (n = omp_clauses->lists[list]; n != NULL; )
	      {
		gfc_omp_namelist *n2 = n;
		if (n->sym->ts.type != BT_DERIVED
		    || !n->sym->ts.u.derived->ts.is_iso_c)
		  {
		    n = n->next;
		    if (last)
		      last->next = n;
		    else
		      omp_clauses->lists[list] = n;
		    n2->next = omp_clauses->lists[OMP_LIST_USE_DEVICE_ADDR];
		    omp_clauses->lists[OMP_LIST_USE_DEVICE_ADDR] = n2;
		    continue;
		  }
		last = n;
		n = n->next;
	      }
	    break;
	  case OMP_LIST_USES_ALLOCATORS:
	    {
	      if (n != NULL
		  && n->u.memspace_sym
		  && (n->u.memspace_sym->attr.flavor != FL_PARAMETER
		      || n->u.memspace_sym->ts.type != BT_INTEGER
		      || n->u.memspace_sym->ts.kind != gfc_c_intptr_kind
		      || n->u.memspace_sym->attr.dimension
		      || (!startswith (n->u.memspace_sym->name, "omp_")
			  && !startswith (n->u.memspace_sym->name, "ompx_"))
		      || !endswith (n->u.memspace_sym->name, "_mem_space")))
		gfc_error ("Memspace %qs at %L in USES_ALLOCATORS must be "
			   "a predefined memory space",
			   n->u.memspace_sym->name, &n->where);
	      for (; n != NULL; n = n->next)
		{
		  if (n->sym->ts.type != BT_INTEGER
		      || n->sym->ts.kind != gfc_c_intptr_kind
		      || n->sym->attr.dimension)
		    gfc_error ("Allocator %qs at %L in USES_ALLOCATORS must "
			       "be a scalar integer of kind "
			       "%<omp_allocator_handle_kind%>", n->sym->name,
			       &n->where);
		  else if (n->sym->attr.flavor != FL_VARIABLE
			   && ((!startswith (n->sym->name, "omp_")
				&& !startswith (n->sym->name, "ompx_"))
			       || !endswith (n->sym->name, "_mem_alloc")))
		    gfc_error ("Allocator %qs at %L in USES_ALLOCATORS must "
			       "either a variable or a predefined allocator",
			       n->sym->name, &n->where);
		  else if ((n->u.memspace_sym || n->u2.traits_sym)
			   && n->sym->attr.flavor != FL_VARIABLE)
		    gfc_error ("A memory space or traits array may not be "
			       "specified for predefined allocator %qs at %L",
			       n->sym->name, &n->where);
		  if (n->u2.traits_sym
		      && (n->u2.traits_sym->attr.flavor != FL_PARAMETER
			  || !n->u2.traits_sym->attr.dimension
			  || n->u2.traits_sym->as->rank != 1
			  || n->u2.traits_sym->ts.type != BT_DERIVED
			  || strcmp (n->u2.traits_sym->ts.u.derived->name,
				     "omp_alloctrait") != 0))
		    {
		      gfc_error ("Traits array %qs in USES_ALLOCATORS %L must "
				 "be a one-dimensional named constant array of "
				 "type %<omp_alloctrait%>",
				 n->u2.traits_sym->name, &n->where);
		      break;
		    }
		}
	      break;
	    }
	  default:
	    for (; n != NULL; n = n->next)
	      {
		if (n->sym == NULL)
		  {
		    gcc_assert (code->op == EXEC_OMP_ALLOCATORS
				|| code->op == EXEC_OMP_ALLOCATE);
		    continue;
		  }
		bool bad = false;
		bool is_reduction = (list == OMP_LIST_REDUCTION
				     || list == OMP_LIST_REDUCTION_INSCAN
				     || list == OMP_LIST_REDUCTION_TASK
				     || list == OMP_LIST_IN_REDUCTION
				     || list == OMP_LIST_TASK_REDUCTION);
		if (list == OMP_LIST_REDUCTION_INSCAN)
		  has_inscan = true;
		else if (is_reduction)
		  has_notinscan = true;
		if (has_inscan && has_notinscan && is_reduction)
		  {
		    gfc_error ("%<inscan%> and non-%<inscan%> %<reduction%> "
			       "clauses on the same construct at %L",
			       &n->where);
		    break;
		  }
		if (n->sym->attr.threadprivate)
		  gfc_error ("THREADPRIVATE object %qs in %s clause at %L",
			     n->sym->name, name, &n->where);
		if (n->sym->attr.cray_pointee)
		  gfc_error ("Cray pointee %qs in %s clause at %L",
			    n->sym->name, name, &n->where);
		if (n->sym->attr.associate_var)
		  gfc_error ("Associate name %qs in %s clause at %L",
			     n->sym->attr.select_type_temporary
			     ? n->sym->assoc->target->symtree->n.sym->name
			     : n->sym->name, name, &n->where);
		if (list != OMP_LIST_PRIVATE && is_reduction)
		  {
		    if (n->sym->attr.proc_pointer)
		      gfc_error ("Procedure pointer %qs in %s clause at %L",
				 n->sym->name, name, &n->where);
		    if (n->sym->attr.pointer)
		      gfc_error ("POINTER object %qs in %s clause at %L",
				 n->sym->name, name, &n->where);
		    if (n->sym->attr.cray_pointer)
		      gfc_error ("Cray pointer %qs in %s clause at %L",
				 n->sym->name, name, &n->where);
		  }
		if (code
		    && (oacc_is_loop (code)
			|| code->op == EXEC_OACC_PARALLEL
			|| code->op == EXEC_OACC_SERIAL))
		  check_array_not_assumed (n->sym, n->where, name);
		else if (list != OMP_LIST_UNIFORM
			 && n->sym->as && n->sym->as->type == AS_ASSUMED_SIZE)
		  gfc_error ("Assumed size array %qs in %s clause at %L",
			     n->sym->name, name, &n->where);
		if (n->sym->attr.in_namelist && !is_reduction)
		  gfc_error ("Variable %qs in %s clause is used in "
			     "NAMELIST statement at %L",
			     n->sym->name, name, &n->where);
		if (n->sym->attr.pointer && n->sym->attr.intent == INTENT_IN)
		  switch (list)
		    {
		    case OMP_LIST_PRIVATE:
		    case OMP_LIST_LASTPRIVATE:
		    case OMP_LIST_LINEAR:
		    /* case OMP_LIST_REDUCTION: */
		      gfc_error ("INTENT(IN) POINTER %qs in %s clause at %L",
				 n->sym->name, name, &n->where);
		      break;
		    default:
		      break;
		    }
		if (omp_clauses->detach
		    && (list == OMP_LIST_PRIVATE
			|| list == OMP_LIST_FIRSTPRIVATE
			|| list == OMP_LIST_LASTPRIVATE)
		    && n->sym == omp_clauses->detach->symtree->n.sym)
		  gfc_error ("DETACH event handle %qs in %s clause at %L",
			     n->sym->name, name, &n->where);

		if (!openacc
		    && list == OMP_LIST_FIRSTPRIVATE
		    && ((n->expr && n->expr->ts.type == BT_CLASS)
			|| (!n->expr && n->sym->ts.type == BT_CLASS)))
		  switch (code->op)
		    {
		    case EXEC_OMP_TARGET:
		    case EXEC_OMP_TARGET_PARALLEL:
		    case EXEC_OMP_TARGET_PARALLEL_DO:
		    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
		    case EXEC_OMP_TARGET_PARALLEL_LOOP:
		    case EXEC_OMP_TARGET_SIMD:
		    case EXEC_OMP_TARGET_TEAMS:
		    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
		    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
		    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
		    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
		    case EXEC_OMP_TARGET_TEAMS_LOOP:
		      gfc_warning (OPT_Wopenmp,
				   "FIRSTPRIVATE with polymorphic list item at "
				   "%L is unspecified behavior", &n->where);
		      break;
		    default:
		      break;
		    }

		switch (list)
		  {
		  case OMP_LIST_REDUCTION_TASK:
		    if (code
			&& (code->op == EXEC_OMP_LOOP
			    || code->op == EXEC_OMP_TASKLOOP
			    || code->op == EXEC_OMP_TASKLOOP_SIMD
			    || code->op == EXEC_OMP_MASKED_TASKLOOP
			    || code->op == EXEC_OMP_MASKED_TASKLOOP_SIMD
			    || code->op == EXEC_OMP_MASTER_TASKLOOP
			    || code->op == EXEC_OMP_MASTER_TASKLOOP_SIMD
			    || code->op == EXEC_OMP_PARALLEL_LOOP
			    || code->op == EXEC_OMP_PARALLEL_MASKED_TASKLOOP
			    || code->op == EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD
			    || code->op == EXEC_OMP_PARALLEL_MASTER_TASKLOOP
			    || code->op == EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD
			    || code->op == EXEC_OMP_TARGET_PARALLEL_LOOP
			    || code->op == EXEC_OMP_TARGET_TEAMS_LOOP
			    || code->op == EXEC_OMP_TEAMS
			    || code->op == EXEC_OMP_TEAMS_DISTRIBUTE
			    || code->op == EXEC_OMP_TEAMS_LOOP))
		      {
			gfc_error ("Only DEFAULT permitted as reduction-"
				   "modifier in REDUCTION clause at %L",
				   &n->where);
			break;
		      }
		    gcc_fallthrough ();
		  case OMP_LIST_REDUCTION:
		  case OMP_LIST_IN_REDUCTION:
		  case OMP_LIST_TASK_REDUCTION:
		  case OMP_LIST_REDUCTION_INSCAN:
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
		      n->u2.udr = NULL;
		    else
		      {
			const char *udr_name = NULL;
			if (n->u2.udr)
			  {
			    udr_name = n->u2.udr->udr->name;
			    n->u2.udr->udr
			      = gfc_find_omp_udr (NULL, udr_name,
						  &n->sym->ts);
			    if (n->u2.udr->udr == NULL)
			      {
				free (n->u2.udr);
				n->u2.udr = NULL;
			      }
			  }
			if (n->u2.udr == NULL)
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
				       gfc_typename (&n->sym->ts), &n->where);
			  }
			else
			  {
			    gfc_omp_udr *udr = n->u2.udr->udr;
			    n->u.reduction_op = OMP_REDUCTION_USER;
			    n->u2.udr->combiner
			      = resolve_omp_udr_clause (n, udr->combiner_ns,
							udr->omp_out,
							udr->omp_in);
			    if (udr->initializer_ns)
			      n->u2.udr->initializer
				= resolve_omp_udr_clause (n,
							  udr->initializer_ns,
							  udr->omp_priv,
							  udr->omp_orig);
			  }
		      }
		    break;
		  case OMP_LIST_LINEAR:
		    if (code
			&& n->u.linear.op != OMP_LINEAR_DEFAULT
			&& n->u.linear.op != linear_op)
		      {
			if (n->u.linear.old_modifier)
			  {
			    gfc_error ("LINEAR clause modifier used on DO or "
				       "SIMD construct at %L", &n->where);
			    linear_op = n->u.linear.op;
			  }
			else if (n->u.linear.op != OMP_LINEAR_VAL)
			  {
			    gfc_error ("LINEAR clause modifier other than VAL "
				       "used on DO or SIMD construct at %L",
				       &n->where);
			    linear_op = n->u.linear.op;
			  }
		      }
		    else if (n->u.linear.op != OMP_LINEAR_REF
			     && n->sym->ts.type != BT_INTEGER)
		      gfc_error ("LINEAR variable %qs must be INTEGER "
				 "at %L", n->sym->name, &n->where);
		    else if ((n->u.linear.op == OMP_LINEAR_REF
			      || n->u.linear.op == OMP_LINEAR_UVAL)
			     && n->sym->attr.value)
		      gfc_error ("LINEAR dummy argument %qs with VALUE "
				 "attribute with %s modifier at %L",
				 n->sym->name,
				 n->u.linear.op == OMP_LINEAR_REF
				 ? "REF" : "UVAL", &n->where);
		    else if (n->expr)
		      {
			gfc_expr *expr = n->expr;
			if (!gfc_resolve_expr (expr)
			    || expr->ts.type != BT_INTEGER
			    || expr->rank != 0)
			  gfc_error ("%qs in LINEAR clause at %L requires "
				     "a scalar integer linear-step expression",
				     n->sym->name, &n->where);
			else if (!code && expr->expr_type != EXPR_CONSTANT)
			  {
			    if (expr->expr_type == EXPR_VARIABLE
				&& expr->symtree->n.sym->attr.dummy
				&& expr->symtree->n.sym->ns == ns)
			      {
				gfc_omp_namelist *n2;
				for (n2 = omp_clauses->lists[OMP_LIST_UNIFORM];
				     n2; n2 = n2->next)
				  if (n2->sym == expr->symtree->n.sym)
				    break;
				if (n2)
				  break;
			      }
			    gfc_error ("%qs in LINEAR clause at %L requires "
				       "a constant integer linear-step "
				       "expression or dummy argument "
				       "specified in UNIFORM clause",
				       n->sym->name, &n->where);
			  }
		      }
		    break;
		  /* Workaround for PR middle-end/26316, nothing really needs
		     to be done here for OMP_LIST_PRIVATE.  */
		  case OMP_LIST_PRIVATE:
		    gcc_assert (code && code->op != EXEC_NOP);
		    break;
		  case OMP_LIST_USE_DEVICE:
		      if (n->sym->attr.allocatable
			  || (n->sym->ts.type == BT_CLASS && CLASS_DATA (n->sym)
			      && CLASS_DATA (n->sym)->attr.allocatable))
			gfc_error ("ALLOCATABLE object %qs in %s clause at %L",
				   n->sym->name, name, &n->where);
		      if (n->sym->ts.type == BT_CLASS
			  && CLASS_DATA (n->sym)
			  && CLASS_DATA (n->sym)->attr.class_pointer)
			gfc_error ("POINTER object %qs of polymorphic type in "
				   "%s clause at %L", n->sym->name, name,
				   &n->where);
		      if (n->sym->attr.cray_pointer)
			gfc_error ("Cray pointer object %qs in %s clause at %L",
				   n->sym->name, name, &n->where);
		      else if (n->sym->attr.cray_pointee)
			gfc_error ("Cray pointee object %qs in %s clause at %L",
				   n->sym->name, name, &n->where);
		      else if (n->sym->attr.flavor == FL_VARIABLE
			       && !n->sym->as
			       && !n->sym->attr.pointer)
			gfc_error ("%s clause variable %qs at %L is neither "
				   "a POINTER nor an array", name,
				   n->sym->name, &n->where);
		      /* FALLTHRU */
		  case OMP_LIST_DEVICE_RESIDENT:
		    check_symbol_not_pointer (n->sym, n->where, name);
		    check_array_not_assumed (n->sym, n->where, name);
		    break;
		  default:
		    break;
		  }
	      }
	    break;
	  }
      }
  /* OpenMP 5.1: use_device_ptr acts like use_device_addr, except for
     type(c_ptr).  */
  if (omp_clauses->lists[OMP_LIST_USE_DEVICE_PTR])
    {
      gfc_omp_namelist *n_prev, *n_next, *n_addr;
      n_addr = omp_clauses->lists[OMP_LIST_USE_DEVICE_ADDR];
      for (; n_addr && n_addr->next; n_addr = n_addr->next)
	;
      n_prev = NULL;
      n = omp_clauses->lists[OMP_LIST_USE_DEVICE_PTR];
      while (n)
	{
	  n_next = n->next;
	  if (n->sym->ts.type != BT_DERIVED
	      || n->sym->ts.u.derived->ts.f90_type != BT_VOID)
	    {
	      n->next = NULL;
	      if (n_addr)
		n_addr->next = n;
	      else
		omp_clauses->lists[OMP_LIST_USE_DEVICE_ADDR] = n;
	      n_addr = n;
	      if (n_prev)
		n_prev->next = n_next;
	      else
		omp_clauses->lists[OMP_LIST_USE_DEVICE_PTR] = n_next;
	    }
	  else
	    n_prev = n;
	  n = n_next;
	}
    }
  if (omp_clauses->safelen_expr)
    resolve_positive_int_expr (omp_clauses->safelen_expr, "SAFELEN");
  if (omp_clauses->simdlen_expr)
    resolve_positive_int_expr (omp_clauses->simdlen_expr, "SIMDLEN");
  if (omp_clauses->num_teams_lower)
    resolve_positive_int_expr (omp_clauses->num_teams_lower, "NUM_TEAMS");
  if (omp_clauses->num_teams_upper)
    resolve_positive_int_expr (omp_clauses->num_teams_upper, "NUM_TEAMS");
  if (omp_clauses->num_teams_lower
      && omp_clauses->num_teams_lower->expr_type == EXPR_CONSTANT
      && omp_clauses->num_teams_upper->expr_type == EXPR_CONSTANT
      && mpz_cmp (omp_clauses->num_teams_lower->value.integer,
		  omp_clauses->num_teams_upper->value.integer) > 0)
    gfc_warning (OPT_Wopenmp, "NUM_TEAMS lower bound at %L larger than upper "
		 "bound at %L", &omp_clauses->num_teams_lower->where,
		 &omp_clauses->num_teams_upper->where);
  if (omp_clauses->device)
    resolve_scalar_int_expr (omp_clauses->device, "DEVICE");
  if (omp_clauses->filter)
    resolve_nonnegative_int_expr (omp_clauses->filter, "FILTER");
  if (omp_clauses->hint)
    {
      resolve_scalar_int_expr (omp_clauses->hint, "HINT");
    if (omp_clauses->hint->ts.type != BT_INTEGER
	|| omp_clauses->hint->expr_type != EXPR_CONSTANT
	|| mpz_sgn (omp_clauses->hint->value.integer) < 0)
      gfc_error ("Value of HINT clause at %L shall be a valid "
		 "constant hint expression", &omp_clauses->hint->where);
    }
  if (omp_clauses->priority)
    resolve_nonnegative_int_expr (omp_clauses->priority, "PRIORITY");
  if (omp_clauses->dist_chunk_size)
    {
      gfc_expr *expr = omp_clauses->dist_chunk_size;
      if (!gfc_resolve_expr (expr)
	  || expr->ts.type != BT_INTEGER || expr->rank != 0)
	gfc_error ("DIST_SCHEDULE clause's chunk_size at %L requires "
		   "a scalar INTEGER expression", &expr->where);
    }
  if (omp_clauses->thread_limit)
    resolve_positive_int_expr (omp_clauses->thread_limit, "THREAD_LIMIT");
  if (omp_clauses->grainsize)
    resolve_positive_int_expr (omp_clauses->grainsize, "GRAINSIZE");
  if (omp_clauses->num_tasks)
    resolve_positive_int_expr (omp_clauses->num_tasks, "NUM_TASKS");
  if (omp_clauses->grainsize && omp_clauses->num_tasks)
    gfc_error ("%<GRAINSIZE%> clause at %L must not be used together with "
	       "%<NUM_TASKS%> clause", &omp_clauses->grainsize->where);
  if (omp_clauses->lists[OMP_LIST_REDUCTION] && omp_clauses->nogroup)
    gfc_error ("%<REDUCTION%> clause at %L must not be used together with "
	       "%<NOGROUP%> clause",
	       &omp_clauses->lists[OMP_LIST_REDUCTION]->where);
  if (omp_clauses->full && omp_clauses->partial)
    gfc_error ("%<FULL%> clause at %C must not be used together with "
	       "%<PARTIAL%> clause");
  if (omp_clauses->async)
    if (omp_clauses->async_expr)
      resolve_scalar_int_expr (omp_clauses->async_expr, "ASYNC");
  if (omp_clauses->num_gangs_expr)
    resolve_positive_int_expr (omp_clauses->num_gangs_expr, "NUM_GANGS");
  if (omp_clauses->num_workers_expr)
    resolve_positive_int_expr (omp_clauses->num_workers_expr, "NUM_WORKERS");
  if (omp_clauses->vector_length_expr)
    resolve_positive_int_expr (omp_clauses->vector_length_expr,
			       "VECTOR_LENGTH");
  if (omp_clauses->gang_num_expr)
    resolve_positive_int_expr (omp_clauses->gang_num_expr, "GANG");
  if (omp_clauses->gang_static_expr)
    resolve_positive_int_expr (omp_clauses->gang_static_expr, "GANG");
  if (omp_clauses->worker_expr)
    resolve_positive_int_expr (omp_clauses->worker_expr, "WORKER");
  if (omp_clauses->vector_expr)
    resolve_positive_int_expr (omp_clauses->vector_expr, "VECTOR");
  for (el = omp_clauses->wait_list; el; el = el->next)
    resolve_scalar_int_expr (el->expr, "WAIT");
  if (omp_clauses->collapse && omp_clauses->tile_list)
    gfc_error ("Incompatible use of TILE and COLLAPSE at %L", &code->loc);
  if (omp_clauses->message)
    {
      gfc_expr *expr = omp_clauses->message;
      if (!gfc_resolve_expr (expr)
	  || expr->ts.kind != gfc_default_character_kind
	  || expr->ts.type != BT_CHARACTER || expr->rank != 0)
	gfc_error ("MESSAGE clause at %L requires a scalar default-kind "
		   "CHARACTER expression", &expr->where);
    }
  if (!openacc
      && code
      && omp_clauses->lists[OMP_LIST_MAP] == NULL
      && omp_clauses->lists[OMP_LIST_USE_DEVICE_PTR] == NULL
      && omp_clauses->lists[OMP_LIST_USE_DEVICE_ADDR] == NULL)
    {
      const char *p = NULL;
      switch (code->op)
	{
	case EXEC_OMP_TARGET_ENTER_DATA: p = "TARGET ENTER DATA"; break;
	case EXEC_OMP_TARGET_EXIT_DATA: p = "TARGET EXIT DATA"; break;
	default: break;
	}
      if (code->op == EXEC_OMP_TARGET_DATA)
	gfc_error ("TARGET DATA must contain at least one MAP, USE_DEVICE_PTR, "
		   "or USE_DEVICE_ADDR clause at %L", &code->loc);
      else if (p)
	gfc_error ("%s must contain at least one MAP clause at %L",
		   p, &code->loc);
    }
  if (omp_clauses->sizes_list)
    {
      gfc_expr_list *el;
      for (el = omp_clauses->sizes_list; el; el = el->next)
	{
	  resolve_scalar_int_expr (el->expr, "SIZES");
	  if (el->expr->expr_type != EXPR_CONSTANT)
	    gfc_error ("SIZES requires constant expression at %L",
		       &el->expr->where);
	  else if (el->expr->expr_type == EXPR_CONSTANT
		   && el->expr->ts.type == BT_INTEGER
		   && mpz_sgn (el->expr->value.integer) <= 0)
	    gfc_error ("INTEGER expression of %s clause at %L must be "
		       "positive", "SIZES", &el->expr->where);
	}
    }

  if (!openacc && omp_clauses->detach)
    {
      if (!gfc_resolve_expr (omp_clauses->detach)
	  || omp_clauses->detach->ts.type != BT_INTEGER
	  || omp_clauses->detach->ts.kind != gfc_c_intptr_kind
	  || omp_clauses->detach->rank != 0)
	gfc_error ("%qs at %L should be a scalar of type "
		   "integer(kind=omp_event_handle_kind)",
		   omp_clauses->detach->symtree->n.sym->name,
		   &omp_clauses->detach->where);
      else if (omp_clauses->detach->symtree->n.sym->attr.dimension > 0)
	gfc_error ("The event handle at %L must not be an array element",
		   &omp_clauses->detach->where);
      else if (omp_clauses->detach->symtree->n.sym->ts.type == BT_DERIVED
	       || omp_clauses->detach->symtree->n.sym->ts.type == BT_CLASS)
	gfc_error ("The event handle at %L must not be part of "
		   "a derived type or class", &omp_clauses->detach->where);

      if (omp_clauses->mergeable)
	gfc_error ("%<DETACH%> clause at %L must not be used together with "
		   "%<MERGEABLE%> clause", &omp_clauses->detach->where);
    }

  if (openacc
      && code->op == EXEC_OACC_HOST_DATA
      && omp_clauses->lists[OMP_LIST_USE_DEVICE] == NULL)
    gfc_error ("%<host_data%> construct at %L requires %<use_device%> clause",
	       &code->loc);

  if (omp_clauses->assume)
    gfc_resolve_omp_assumptions (omp_clauses->assume);
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
   if WIDENING is true or narrows the type if NARROW is true,
   return the inner expression, otherwise return NULL.  */

static gfc_expr *
is_conversion (gfc_expr *expr, bool narrowing, bool widening)
{
  gfc_typespec *ts1, *ts2;

  if (expr->expr_type != EXPR_FUNCTION
      || expr->value.function.isym == NULL
      || expr->value.function.esym != NULL
      || expr->value.function.isym->id != GFC_ISYM_CONVERSION
      || (!narrowing && !widening))
    return NULL;

  if (narrowing && widening)
    return expr->value.function.actual->expr;

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

static bool
is_scalar_intrinsic_expr (gfc_expr *expr, bool must_be_var, bool conv_ok)
{
  if (must_be_var
      && (expr->expr_type != EXPR_VARIABLE || !expr->symtree))
    {
      if (!conv_ok)
	return false;
      gfc_expr *conv = is_conversion (expr, true, true);
      if (!conv)
	return false;
      if (conv->expr_type != EXPR_VARIABLE || !conv->symtree)
	return false;
    }
  return (expr->rank == 0
	  && !gfc_is_coindexed (expr)
	  && (expr->ts.type == BT_INTEGER
	      || expr->ts.type == BT_REAL
	      || expr->ts.type == BT_COMPLEX
	      || expr->ts.type == BT_LOGICAL));
}

static void
resolve_omp_atomic (gfc_code *code)
{
  gfc_code *atomic_code = code->block;
  gfc_symbol *var;
  gfc_expr *stmt_expr2, *capt_expr2;
  gfc_omp_atomic_op aop
    = (gfc_omp_atomic_op) (atomic_code->ext.omp_clauses->atomic_op
			   & GFC_OMP_ATOMIC_MASK);
  gfc_code *stmt = NULL, *capture_stmt = NULL, *tailing_stmt = NULL;
  gfc_expr *comp_cond = NULL;
  locus *loc = NULL;

  code = code->block->next;
  /* resolve_blocks asserts this is initially EXEC_ASSIGN or EXEC_IF
     If it changed to EXEC_NOP, assume an error has been emitted already.  */
  if (code->op == EXEC_NOP)
    return;

  if (atomic_code->ext.omp_clauses->compare
      && atomic_code->ext.omp_clauses->capture)
    {
      /* Must be either "if (x == e) then; x = d; else; v = x; end if"
	 or "v = expr" followed/preceded by
	 "if (x == e) then; x = d; end if" or "if (x == e) x = d".  */
      gfc_code *next = code;
      if (code->op == EXEC_ASSIGN)
	{
	  capture_stmt = code;
	  next = code->next;
	}
      if (next->op == EXEC_IF
	  && next->block
	  && next->block->op == EXEC_IF
	  && next->block->next
	  && next->block->next->op == EXEC_ASSIGN)
	{
	  comp_cond = next->block->expr1;
	  stmt = next->block->next;
	  if (stmt->next)
	    {
	      loc = &stmt->loc;
	      goto unexpected;
	    }
	}
      else if (capture_stmt)
	{
	  gfc_error ("Expected IF at %L in atomic compare capture",
		     &next->loc);
	  return;
	}
      if (stmt && !capture_stmt && next->block->block)
	{
	  if (next->block->block->expr1)
	    {
	      gfc_error ("Expected ELSE at %L in atomic compare capture",
			 &next->block->block->expr1->where);
	      return;
	    }
	  if (!code->block->block->next
	      || code->block->block->next->op != EXEC_ASSIGN)
	    {
	      loc = (code->block->block->next ? &code->block->block->next->loc
					      : &code->block->block->loc);
	      goto unexpected;
	    }
	  capture_stmt = code->block->block->next;
	  if (capture_stmt->next)
	    {
	      loc = &capture_stmt->next->loc;
	      goto unexpected;
	    }
	}
      if (stmt && !capture_stmt && next->next->op == EXEC_ASSIGN)
	capture_stmt = next->next;
      else if (!capture_stmt)
	{
	  loc = &code->loc;
	  goto unexpected;
	}
    }
  else if (atomic_code->ext.omp_clauses->compare)
    {
      /* Must be: "if (x == e) then; x = d; end if" or "if (x == e) x = d".  */
      if (code->op == EXEC_IF
	  && code->block
	  && code->block->op == EXEC_IF
	  && code->block->next
	  && code->block->next->op == EXEC_ASSIGN)
	{
	  comp_cond = code->block->expr1;
	  stmt = code->block->next;
	  if (stmt->next || code->block->block)
	    {
	      loc = stmt->next ? &stmt->next->loc : &code->block->block->loc;
	      goto unexpected;
	    }
	}
      else
	{
	  loc = &code->loc;
	  goto unexpected;
	}
    }
  else if (atomic_code->ext.omp_clauses->capture)
    {
      /* Must be: "v = x" followed/preceded by "x = ...". */
      if (code->op != EXEC_ASSIGN)
	goto unexpected;
      if (code->next->op != EXEC_ASSIGN)
	{
	  loc = &code->next->loc;
	  goto unexpected;
	}
      gfc_expr *expr2, *expr2_next;
      expr2 = is_conversion (code->expr2, true, true);
      if (expr2 == NULL)
	expr2 = code->expr2;
      expr2_next = is_conversion (code->next->expr2, true, true);
      if (expr2_next == NULL)
	expr2_next = code->next->expr2;
      if (code->expr1->expr_type == EXPR_VARIABLE
	  && code->next->expr1->expr_type == EXPR_VARIABLE
	  && expr2->expr_type == EXPR_VARIABLE
	  && expr2_next->expr_type == EXPR_VARIABLE)
	{
	  if (code->expr1->symtree->n.sym == expr2_next->symtree->n.sym)
	    {
	      stmt = code;
	      capture_stmt = code->next;
	    }
	  else
	    {
	      capture_stmt = code;
	      stmt = code->next;
	    }
	}
      else if (expr2->expr_type == EXPR_VARIABLE)
	{
	  capture_stmt = code;
	  stmt = code->next;
	}
      else
	{
	  stmt = code;
	  capture_stmt = code->next;
	}
      /* Shall be NULL but can happen for invalid code. */
      tailing_stmt = code->next->next;
    }
  else
    {
      /* x = ... */
      stmt = code;
      if (!atomic_code->ext.omp_clauses->compare && stmt->op != EXEC_ASSIGN)
	goto unexpected;
      /* Shall be NULL but can happen for invalid code. */
      tailing_stmt = code->next;
    }

  if (comp_cond)
    {
      if (comp_cond->expr_type != EXPR_OP
	  || (comp_cond->value.op.op != INTRINSIC_EQ
	      && comp_cond->value.op.op != INTRINSIC_EQ_OS
	      && comp_cond->value.op.op != INTRINSIC_EQV))
	{
	  gfc_error ("Expected %<==%>, %<.EQ.%> or %<.EQV.%> atomic comparison "
		     "expression at %L", &comp_cond->where);
	  return;
	}
      if (!is_scalar_intrinsic_expr (comp_cond->value.op.op1, true, true))
	{
	  gfc_error ("Expected scalar intrinsic variable at %L in atomic "
		     "comparison", &comp_cond->value.op.op1->where);
	  return;
	}
      if (!gfc_resolve_expr (comp_cond->value.op.op2))
	return;
      if (!is_scalar_intrinsic_expr (comp_cond->value.op.op2, false, false))
	{
	  gfc_error ("Expected scalar intrinsic expression at %L in atomic "
		     "comparison", &comp_cond->value.op.op1->where);
	  return;
	}
    }

  if (!is_scalar_intrinsic_expr (stmt->expr1, true, false))
    {
      gfc_error ("!$OMP ATOMIC statement must set a scalar variable of "
		 "intrinsic type at %L", &stmt->expr1->where);
      return;
    }

  if (!gfc_resolve_expr (stmt->expr2))
    return;
  if (!is_scalar_intrinsic_expr (stmt->expr2, false, false))
    {
      gfc_error ("!$OMP ATOMIC statement must assign an expression of "
		 "intrinsic type at %L", &stmt->expr2->where);
      return;
    }

  if (gfc_expr_attr (stmt->expr1).allocatable)
    {
      gfc_error ("!$OMP ATOMIC with ALLOCATABLE variable at %L",
		 &stmt->expr1->where);
      return;
    }

  /* Should be diagnosed above already. */
  gcc_assert (tailing_stmt == NULL);

  var = stmt->expr1->symtree->n.sym;
  stmt_expr2 = is_conversion (stmt->expr2, true, true);
  if (stmt_expr2 == NULL)
    stmt_expr2 = stmt->expr2;

  switch (aop)
    {
    case GFC_OMP_ATOMIC_READ:
      if (stmt_expr2->expr_type != EXPR_VARIABLE)
	gfc_error ("!$OMP ATOMIC READ statement must read from a scalar "
		   "variable of intrinsic type at %L", &stmt_expr2->where);
      return;
    case GFC_OMP_ATOMIC_WRITE:
      if (expr_references_sym (stmt_expr2, var, NULL))
	gfc_error ("expr in !$OMP ATOMIC WRITE assignment var = expr "
		   "must be scalar and cannot reference var at %L",
		   &stmt_expr2->where);
      return;
    default:
      break;
    }

  if (atomic_code->ext.omp_clauses->capture)
    {
      if (!is_scalar_intrinsic_expr (capture_stmt->expr1, true, false))
	{
	  gfc_error ("!$OMP ATOMIC capture-statement must set a scalar "
		     "variable of intrinsic type at %L",
		     &capture_stmt->expr1->where);
	  return;
	}

      if (!is_scalar_intrinsic_expr (capture_stmt->expr2, true, true))
	{
	  gfc_error ("!$OMP ATOMIC capture-statement requires a scalar variable"
		     " of intrinsic type at %L", &capture_stmt->expr2->where);
	  return;
	}
      capt_expr2 = is_conversion (capture_stmt->expr2, true, true);
      if (capt_expr2 == NULL)
	capt_expr2 = capture_stmt->expr2;

      if (capt_expr2->symtree->n.sym != var)
	{
	  gfc_error ("!$OMP ATOMIC CAPTURE capture statement reads from "
		     "different variable than update statement writes "
		     "into at %L", &capture_stmt->expr2->where);
	      return;
	}
    }

  if (atomic_code->ext.omp_clauses->compare)
    {
      gfc_expr *var_expr;
      if (comp_cond->value.op.op1->expr_type == EXPR_VARIABLE)
	var_expr = comp_cond->value.op.op1;
      else
	var_expr = comp_cond->value.op.op1->value.function.actual->expr;
      if (var_expr->symtree->n.sym != var)
	{
	  gfc_error ("For !$OMP ATOMIC COMPARE, the first operand in comparison"
		     " at %L must be the variable %qs that the update statement"
		     " writes into at %L", &var_expr->where, var->name,
		     &stmt->expr1->where);
	  return;
	}
      if (stmt_expr2->rank != 0 || expr_references_sym (stmt_expr2, var, NULL))
	{
	  gfc_error ("expr in !$OMP ATOMIC COMPARE assignment var = expr "
		     "must be scalar and cannot reference var at %L",
		     &stmt_expr2->where);
	  return;
	}
    }
  else if (atomic_code->ext.omp_clauses->capture
	   && !expr_references_sym (stmt_expr2, var, NULL))
    atomic_code->ext.omp_clauses->atomic_op
      = (gfc_omp_atomic_op) (atomic_code->ext.omp_clauses->atomic_op
			     | GFC_OMP_ATOMIC_SWAP);
  else if (stmt_expr2->expr_type == EXPR_OP)
    {
      gfc_expr *v = NULL, *e, *c;
      gfc_intrinsic_op op = stmt_expr2->value.op.op;
      gfc_intrinsic_op alt_op = INTRINSIC_NONE;

      if (atomic_code->ext.omp_clauses->fail != OMP_MEMORDER_UNSET)
	gfc_error ("!$OMP ATOMIC UPDATE at %L with FAIL clause requires either"
		   " the COMPARE clause or using the intrinsic MIN/MAX "
		   "procedure", &atomic_code->loc);
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
		     &stmt_expr2->where);
	  return;
	}

      /* Check for var = var op expr resp. var = expr op var where
	 expr doesn't reference var and var op expr is mathematically
	 equivalent to var op (expr) resp. expr op var equivalent to
	 (expr) op var.  We rely here on the fact that the matcher
	 for x op1 y op2 z where op1 and op2 have equal precedence
	 returns (x op1 y) op2 z.  */
      e = stmt_expr2->value.op.op2;
      if (e->expr_type == EXPR_VARIABLE
	  && e->symtree != NULL
	  && e->symtree->n.sym == var)
	v = e;
      else if ((c = is_conversion (e, false, true)) != NULL
	       && c->expr_type == EXPR_VARIABLE
	       && c->symtree != NULL
	       && c->symtree->n.sym == var)
	v = c;
      else
	{
	  gfc_expr **p = NULL, **q;
	  for (q = &stmt_expr2->value.op.op1; (e = *q) != NULL; )
	    if (e->expr_type == EXPR_VARIABLE
		&& e->symtree != NULL
		&& e->symtree->n.sym == var)
	      {
		v = e;
		break;
	      }
	    else if ((c = is_conversion (e, false, true)) != NULL)
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
			 "or var = expr op var at %L", &stmt_expr2->where);
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
			     "(expr) at %L", &stmt_expr2->where);
		  break;
		default:
		  break;
		}

	      /* Canonicalize into var = var op (expr).  */
	      *p = e->value.op.op2;
	      e->value.op.op2 = stmt_expr2;
	      e->ts = stmt_expr2->ts;
	      if (stmt->expr2 == stmt_expr2)
		stmt->expr2 = stmt_expr2 = e;
	      else
		stmt->expr2->value.function.actual->expr = stmt_expr2 = e;

	      if (!gfc_compare_types (&stmt_expr2->value.op.op1->ts,
				      &stmt_expr2->ts))
		{
		  for (p = &stmt_expr2->value.op.op1; *p != v;
		       p = &(*p)->value.function.actual->expr)
		    ;
		  *p = NULL;
		  gfc_free_expr (stmt_expr2->value.op.op1);
		  stmt_expr2->value.op.op1 = v;
		  gfc_convert_type (v, &stmt_expr2->ts, 2);
		}
	    }
	}

      if (e->rank != 0 || expr_references_sym (stmt->expr2, var, v))
	{
	  gfc_error ("expr in !$OMP ATOMIC assignment var = var op expr "
		     "must be scalar and cannot reference var at %L",
		     &stmt_expr2->where);
	  return;
	}
    }
  else if (stmt_expr2->expr_type == EXPR_FUNCTION
	   && stmt_expr2->value.function.isym != NULL
	   && stmt_expr2->value.function.esym == NULL
	   && stmt_expr2->value.function.actual != NULL
	   && stmt_expr2->value.function.actual->next != NULL)
    {
      gfc_actual_arglist *arg, *var_arg;

      switch (stmt_expr2->value.function.isym->id)
	{
	case GFC_ISYM_MIN:
	case GFC_ISYM_MAX:
	  break;
	case GFC_ISYM_IAND:
	case GFC_ISYM_IOR:
	case GFC_ISYM_IEOR:
	  if (stmt_expr2->value.function.actual->next->next != NULL)
	    {
	      gfc_error ("!$OMP ATOMIC assignment intrinsic IAND, IOR "
			 "or IEOR must have two arguments at %L",
			 &stmt_expr2->where);
	      return;
	    }
	  break;
	default:
	  gfc_error ("!$OMP ATOMIC assignment intrinsic must be "
		     "MIN, MAX, IAND, IOR or IEOR at %L",
		     &stmt_expr2->where);
	  return;
	}

      var_arg = NULL;
      for (arg = stmt_expr2->value.function.actual; arg; arg = arg->next)
	{
	  gfc_expr *e = NULL;
	  if (arg == stmt_expr2->value.function.actual
	      || (var_arg == NULL && arg->next == NULL))
	    {
	      e = is_conversion (arg->expr, false, true);
	      if (!e)
		e = arg->expr;
	      if (e->expr_type == EXPR_VARIABLE
		  && e->symtree != NULL
		  && e->symtree->n.sym == var)
		var_arg = arg;
	    }
	  if ((!var_arg || !e) && expr_references_sym (arg->expr, var, NULL))
	    {
	      gfc_error ("!$OMP ATOMIC intrinsic arguments except one must "
			 "not reference %qs at %L",
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
		     "be %qs at %L", var->name, &stmt_expr2->where);
	  return;
	}

      if (var_arg != stmt_expr2->value.function.actual)
	{
	  /* Canonicalize, so that var comes first.  */
	  gcc_assert (var_arg->next == NULL);
	  for (arg = stmt_expr2->value.function.actual;
	       arg->next != var_arg; arg = arg->next)
	    ;
	  var_arg->next = stmt_expr2->value.function.actual;
	  stmt_expr2->value.function.actual = var_arg;
	  arg->next = NULL;
	}
    }
  else
    gfc_error ("!$OMP ATOMIC assignment must have an operator or "
	       "intrinsic on right hand side at %L", &stmt_expr2->where);
  return;

unexpected:
  gfc_error ("unexpected !$OMP ATOMIC expression at %L",
	     loc ? loc : &code->loc);
  return;
}


static struct fortran_omp_context
{
  gfc_code *code;
  hash_set<gfc_symbol *> *sharing_clauses;
  hash_set<gfc_symbol *> *private_iterators;
  struct fortran_omp_context *previous;
  bool is_openmp;
} *omp_current_ctx;
static gfc_code *omp_current_do_code;
static int omp_current_do_collapse;

/* Forward declaration for mutually recursive functions.  */
static gfc_code *
find_nested_loop_in_block (gfc_code *block);

/* Return the first nested DO loop in CHAIN, or NULL if there
   isn't one.  Does no error checking on intervening code.  */

static gfc_code *
find_nested_loop_in_chain (gfc_code *chain)
{
  gfc_code *code;

  if (!chain)
    return NULL;

  for (code = chain; code; code = code->next)
    switch (code->op)
      {
      case EXEC_DO:
      case EXEC_OMP_TILE:
      case EXEC_OMP_UNROLL:
	return code;
      case EXEC_BLOCK:
	if (gfc_code *c = find_nested_loop_in_block (code))
	  return c;
	break;
      default:
	break;
      }
  return NULL;
}

/* Return the first nested DO loop in BLOCK, or NULL if there
   isn't one.  Does no error checking on intervening code.  */
static gfc_code *
find_nested_loop_in_block (gfc_code *block)
{
  gfc_namespace *ns;
  gcc_assert (block->op == EXEC_BLOCK);
  ns = block->ext.block.ns;
  gcc_assert (ns);
  return find_nested_loop_in_chain (ns->code);
}

void
gfc_resolve_omp_do_blocks (gfc_code *code, gfc_namespace *ns)
{
  if (code->block->next && code->block->next->op == EXEC_DO)
    {
      int i;

      omp_current_do_code = code->block->next;
      if (code->ext.omp_clauses->orderedc)
	omp_current_do_collapse = code->ext.omp_clauses->orderedc;
      else if (code->ext.omp_clauses->collapse)
	omp_current_do_collapse = code->ext.omp_clauses->collapse;
      else if (code->ext.omp_clauses->sizes_list)
	omp_current_do_collapse
	  = gfc_expr_list_len (code->ext.omp_clauses->sizes_list);
      else
	omp_current_do_collapse = 1;
      if (code->ext.omp_clauses->lists[OMP_LIST_REDUCTION_INSCAN])
	{
	  /* Checking that there is a matching EXEC_OMP_SCAN in the
	     innermost body cannot be deferred to resolve_omp_do because
	     we process directives nested in the loop before we get
	     there.  */
	  locus *loc
	    = &code->ext.omp_clauses->lists[OMP_LIST_REDUCTION_INSCAN]->where;
	  gfc_code *c;

	  for (i = 1, c = omp_current_do_code;
	       i < omp_current_do_collapse; i++)
	    {
	      c = find_nested_loop_in_chain (c->block->next);
	      if (!c || c->op != EXEC_DO || c->block == NULL)
		break;
	    }

	  /* Skip this if we don't have enough nested loops.  That
	     problem will be diagnosed elsewhere.  */
	  if (c && c->op == EXEC_DO)
	    {
	      gfc_code *block = c->block ? c->block->next : NULL;
	      if (block && block->op != EXEC_OMP_SCAN)
		while (block && block->next
		       && block->next->op != EXEC_OMP_SCAN)
		  block = block->next;
	      if (!block
		  || (block->op != EXEC_OMP_SCAN
		      && (!block->next || block->next->op != EXEC_OMP_SCAN)))
		gfc_error ("With INSCAN at %L, expected loop body with "
			   "!$OMP SCAN between two "
			   "structured block sequences", loc);
	      else
		{
		  if (block->op == EXEC_OMP_SCAN)
		    gfc_warning (OPT_Wopenmp,
				 "!$OMP SCAN at %L with zero executable "
				 "statements in preceding structured block "
				 "sequence", &block->loc);
		  if ((block->op == EXEC_OMP_SCAN && !block->next)
		      || (block->next && block->next->op == EXEC_OMP_SCAN
			  && !block->next->next))
		    gfc_warning (OPT_Wopenmp,
				 "!$OMP SCAN at %L with zero executable "
				 "statements in succeeding structured block "
				 "sequence", block->op == EXEC_OMP_SCAN
				 ? &block->loc : &block->next->loc);
		}
	      if (block && block->op != EXEC_OMP_SCAN)
		block = block->next;
	      if (block && block->op == EXEC_OMP_SCAN)
		/* Mark 'omp scan' as checked; flag will be unset later.  */
		block->ext.omp_clauses->if_present = true;
	    }
	}
    }
  gfc_resolve_blocks (code->block, ns);
  omp_current_do_collapse = 0;
  omp_current_do_code = NULL;
}


void
gfc_resolve_omp_parallel_blocks (gfc_code *code, gfc_namespace *ns)
{
  struct fortran_omp_context ctx;
  gfc_omp_clauses *omp_clauses = code->ext.omp_clauses;
  gfc_omp_namelist *n;
  int list;

  ctx.code = code;
  ctx.sharing_clauses = new hash_set<gfc_symbol *>;
  ctx.private_iterators = new hash_set<gfc_symbol *>;
  ctx.previous = omp_current_ctx;
  ctx.is_openmp = true;
  omp_current_ctx = &ctx;

  for (list = 0; list < OMP_LIST_NUM; list++)
    switch (list)
      {
      case OMP_LIST_SHARED:
      case OMP_LIST_PRIVATE:
      case OMP_LIST_FIRSTPRIVATE:
      case OMP_LIST_LASTPRIVATE:
      case OMP_LIST_REDUCTION:
      case OMP_LIST_REDUCTION_INSCAN:
      case OMP_LIST_REDUCTION_TASK:
      case OMP_LIST_IN_REDUCTION:
      case OMP_LIST_TASK_REDUCTION:
      case OMP_LIST_LINEAR:
	for (n = omp_clauses->lists[list]; n; n = n->next)
	  ctx.sharing_clauses->add (n->sym);
	break;
      default:
	break;
      }

  switch (code->op)
    {
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_MASKED_TASKLOOP:
    case EXEC_OMP_MASKED_TASKLOOP_SIMD:
    case EXEC_OMP_MASTER_TASKLOOP:
    case EXEC_OMP_MASTER_TASKLOOP_SIMD:
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_DO_SIMD:
    case EXEC_OMP_PARALLEL_LOOP:
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP:
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP:
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
    case EXEC_OMP_TARGET_PARALLEL_DO:
    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_PARALLEL_LOOP:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TARGET_TEAMS_LOOP:
    case EXEC_OMP_TASKLOOP:
    case EXEC_OMP_TASKLOOP_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TEAMS_LOOP:
      gfc_resolve_omp_do_blocks (code, ns);
      break;
    default:
      gfc_resolve_blocks (code->block, ns);
    }

  omp_current_ctx = ctx.previous;
  delete ctx.sharing_clauses;
  delete ctx.private_iterators;
}


/* Save and clear openmp.cc private state.  */

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


/* Restore openmp.cc private state from the saved state.  */

void
gfc_omp_restore_state (struct gfc_omp_saved_state *state)
{
  omp_current_ctx = (struct fortran_omp_context *) state->ptrs[0];
  omp_current_do_code = (gfc_code *) state->ptrs[1];
  omp_current_do_collapse = state->ints[0];
}


/* Note a DO iterator variable.  This is special in !$omp parallel
   construct, where they are predetermined private.  */

void
gfc_resolve_do_iterator (gfc_code *code, gfc_symbol *sym, bool add_clause)
{
  if (omp_current_ctx == NULL)
    return;

  int i = omp_current_do_collapse;
  gfc_code *c = omp_current_do_code;

  if (sym->attr.threadprivate)
    return;

  /* !$omp do and !$omp parallel do iteration variable is predetermined
     private just in the !$omp do resp. !$omp parallel do construct,
     with no implications for the outer parallel constructs.  */

  while (i-- >= 1 && c)
    {
      if (code == c)
	return;
      c = find_nested_loop_in_chain (c->block->next);
      if (c && (c->op == EXEC_OMP_TILE || c->op == EXEC_OMP_UNROLL))
	return;
    }

  /* An openacc context may represent a data clause.  Abort if so.  */
  if (!omp_current_ctx->is_openmp && !oacc_is_loop (omp_current_ctx->code))
    return;

  if (omp_current_ctx->sharing_clauses->contains (sym))
    return;

  if (! omp_current_ctx->private_iterators->add (sym) && add_clause)
    {
      gfc_omp_clauses *omp_clauses = omp_current_ctx->code->ext.omp_clauses;
      gfc_omp_namelist *p;

      p = gfc_get_omp_namelist ();
      p->sym = sym;
      p->where = omp_current_ctx->code->loc;
      p->next = omp_clauses->lists[OMP_LIST_PRIVATE];
      omp_clauses->lists[OMP_LIST_PRIVATE] = p;
    }
}

static void
handle_local_var (gfc_symbol *sym)
{
  if (sym->attr.flavor != FL_VARIABLE
      || sym->as != NULL
      || (sym->ts.type != BT_INTEGER && sym->ts.type != BT_REAL))
    return;
  gfc_resolve_do_iterator (sym->ns->code, sym, false);
}

void
gfc_resolve_omp_local_vars (gfc_namespace *ns)
{
  if (omp_current_ctx)
    gfc_traverse_ns (ns, handle_local_var);
}


/* Error checking on intervening code uses a code walker.  */

struct icode_error_state
{
  const char *name;
  bool errorp;
  gfc_code *nested;
  gfc_code *next;
};

static int
icode_code_error_callback (gfc_code **codep,
			   int *walk_subtrees ATTRIBUTE_UNUSED, void *opaque)
{
  gfc_code *code = *codep;
  icode_error_state *state = (icode_error_state *)opaque;

  /* gfc_code_walker walks down CODE's next chain as well as
     walking things that are actually nested in CODE.  We need to
     special-case traversal of outer blocks, so stop immediately if we
     are heading down such a next chain.  */
  if (code == state->next)
    return 1;

  switch (code->op)
    {
    case EXEC_DO:
    case EXEC_DO_WHILE:
    case EXEC_DO_CONCURRENT:
      gfc_error ("%s cannot contain loop in intervening code at %L",
		 state->name, &code->loc);
      state->errorp = true;
      break;
    case EXEC_CYCLE:
    case EXEC_EXIT:
      /* Errors have already been diagnosed in match_exit_cycle.  */
      state->errorp = true;
      break;
    case EXEC_OMP_CRITICAL:
    case EXEC_OMP_DO:
    case EXEC_OMP_FLUSH:
    case EXEC_OMP_MASTER:
    case EXEC_OMP_ORDERED:
    case EXEC_OMP_PARALLEL:
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_SECTIONS:
    case EXEC_OMP_PARALLEL_WORKSHARE:
    case EXEC_OMP_SECTIONS:
    case EXEC_OMP_SINGLE:
    case EXEC_OMP_WORKSHARE:
    case EXEC_OMP_ATOMIC:
    case EXEC_OMP_BARRIER:
    case EXEC_OMP_END_NOWAIT:
    case EXEC_OMP_END_SINGLE:
    case EXEC_OMP_TASK:
    case EXEC_OMP_TASKWAIT:
    case EXEC_OMP_TASKYIELD:
    case EXEC_OMP_CANCEL:
    case EXEC_OMP_CANCELLATION_POINT:
    case EXEC_OMP_TASKGROUP:
    case EXEC_OMP_SIMD:
    case EXEC_OMP_DO_SIMD:
    case EXEC_OMP_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET:
    case EXEC_OMP_TARGET_DATA:
    case EXEC_OMP_TEAMS:
    case EXEC_OMP_DISTRIBUTE:
    case EXEC_OMP_DISTRIBUTE_SIMD:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_TEAMS:
    case EXEC_OMP_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_UPDATE:
    case EXEC_OMP_END_CRITICAL:
    case EXEC_OMP_TARGET_ENTER_DATA:
    case EXEC_OMP_TARGET_EXIT_DATA:
    case EXEC_OMP_TARGET_PARALLEL:
    case EXEC_OMP_TARGET_PARALLEL_DO:
    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_SIMD:
    case EXEC_OMP_TASKLOOP:
    case EXEC_OMP_TASKLOOP_SIMD:
    case EXEC_OMP_SCAN:
    case EXEC_OMP_DEPOBJ:
    case EXEC_OMP_PARALLEL_MASTER:
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP:
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
    case EXEC_OMP_MASTER_TASKLOOP:
    case EXEC_OMP_MASTER_TASKLOOP_SIMD:
    case EXEC_OMP_LOOP:
    case EXEC_OMP_PARALLEL_LOOP:
    case EXEC_OMP_TEAMS_LOOP:
    case EXEC_OMP_TARGET_PARALLEL_LOOP:
    case EXEC_OMP_TARGET_TEAMS_LOOP:
    case EXEC_OMP_MASKED:
    case EXEC_OMP_PARALLEL_MASKED:
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP:
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
    case EXEC_OMP_MASKED_TASKLOOP:
    case EXEC_OMP_MASKED_TASKLOOP_SIMD:
    case EXEC_OMP_SCOPE:
    case EXEC_OMP_ERROR:
    case EXEC_OMP_DISPATCH:
      gfc_error ("%s cannot contain OpenMP directive in intervening code "
		 "at %L",
		 state->name, &code->loc);
      state->errorp = true;
      break;
    case EXEC_CALL:
      /* Per OpenMP 5.2, the "omp_" prefix is reserved, so we don't have to
	 consider the possibility that some locally-bound definition
	 overrides the runtime routine.  */
      if (code->resolved_sym
	  && omp_runtime_api_procname (code->resolved_sym->name))
	{
	  gfc_error ("%s cannot contain OpenMP API call in intervening code "
		     "at %L",
		 state->name, &code->loc);
	  state->errorp = true;
	}
      break;
    default:
      break;
    }
  return 0;
}

static int
icode_expr_error_callback (gfc_expr **expr,
			   int *walk_subtrees ATTRIBUTE_UNUSED, void *opaque)
{
  icode_error_state *state = (icode_error_state *)opaque;

  switch ((*expr)->expr_type)
    {
      /* As for EXPR_CALL with "omp_"-prefixed symbols.  */
    case EXPR_FUNCTION:
      {
	gfc_symbol *sym = (*expr)->value.function.esym;
	if (sym && omp_runtime_api_procname (sym->name))
	  {
	    gfc_error ("%s cannot contain OpenMP API call in intervening code "
		       "at %L",
		       state->name, &((*expr)->where));
	    state->errorp = true;
	  }
	}

      break;
    default:
      break;
    }

  /* FIXME: The description of canonical loop form in the OpenMP standard
     also says "array expressions" are not permitted in intervening code.
     That term is not defined in either the OpenMP spec or the Fortran
     standard, although the latter uses it informally to refer to any
     expression that is not scalar-valued.  It is also apparently not the
     thing GCC internally calls EXPR_ARRAY.  It seems the intent of the
     OpenMP restriction is to disallow elemental operations/intrinsics
     (including things that are not expressions, like assignment
     statements) that generate implicit loops over array operands
     (even if the result is a scalar), but even if the spec said
     that there is no list of all the cases that would be forbidden.
     This is OpenMP issue 3326.  */

  return 0;
}

static void
diagnose_intervening_code_errors_1 (gfc_code *chain,
				    struct icode_error_state *state)
{
  gfc_code *code;
  for (code = chain; code; code = code->next)
    {
      if (code == state->nested)
	/* Do not walk the nested loop or its body, we are only
	   interested in intervening code.  */
	;
      else if (code->op == EXEC_BLOCK
	       && find_nested_loop_in_block (code) == state->nested)
	/* This block contains the nested loop, recurse on its
	   statements.  */
	{
	  gfc_namespace* ns = code->ext.block.ns;
	  diagnose_intervening_code_errors_1 (ns->code, state);
	}
      else
	/* Treat the whole statement as a unit.  */
	{
	  gfc_code *temp = state->next;
	  state->next = code->next;
	  gfc_code_walker (&code, icode_code_error_callback,
			   icode_expr_error_callback, state);
	  state->next = temp;
	}
    }
}

/* Diagnose intervening code errors in BLOCK with nested loop NESTED.
   NAME is the user-friendly name of the OMP directive, used for error
   messages.  Returns true if any error was found.  */
static bool
diagnose_intervening_code_errors (gfc_code *chain, const char *name,
				  gfc_code *nested)
{
  struct icode_error_state state;
  state.name = name;
  state.errorp = false;
  state.nested = nested;
  state.next = NULL;
  diagnose_intervening_code_errors_1 (chain, &state);
  return state.errorp;
}

/* Helper function for restructure_intervening_code:  wrap CHAIN in
   a marker to indicate that it is a structured block sequence.  That
   information will be used later on (in omp-low.cc) for error checking.  */
static gfc_code *
make_structured_block (gfc_code *chain)
{
  gcc_assert (chain);
  gfc_namespace *ns = gfc_build_block_ns (gfc_current_ns);
  gfc_code *result = gfc_get_code (EXEC_BLOCK);
  result->op = EXEC_BLOCK;
  result->ext.block.ns = ns;
  result->ext.block.assoc = NULL;
  result->loc = chain->loc;
  ns->omp_structured_block = 1;
  ns->code = chain;
  return result;
}

/* Push intervening code surrounding a loop, including nested scopes,
   into the body of the loop.  CHAINP is the pointer to the head of
   the next-chain to scan, OUTER_LOOP is the EXEC_DO for the next outer
   loop level, and COLLAPSE is the number of nested loops we need to
   process.
   Note that CHAINP may point at outer_loop->block->next when we
   are scanning the body of a loop, but if there is an intervening block
   CHAINP points into the block's chain rather than its enclosing outer
   loop.  This is why OUTER_LOOP is passed separately.  */
static gfc_code *
restructure_intervening_code (gfc_code **chainp, gfc_code *outer_loop,
			      int count)
{
  gfc_code *code;
  gfc_code *head = *chainp;
  gfc_code *tail = NULL;
  gfc_code *innermost_loop = NULL;

  for (code = *chainp; code; code = code->next, chainp = &(*chainp)->next)
    {
      if (code->op == EXEC_DO)
	{
	  /* Cut CODE free from its chain, leaving the ends dangling.  */
	  *chainp = NULL;
	  tail = code->next;
	  code->next = NULL;

	  if (count == 1)
	    innermost_loop = code;
	  else
	    innermost_loop
	      = restructure_intervening_code (&code->block->next,
					      code, count - 1);
	  break;
	}
      else if (code->op == EXEC_BLOCK
	       && find_nested_loop_in_block (code))
	{
	  gfc_namespace *ns = code->ext.block.ns;

	  /* Cut CODE free from its chain, leaving the ends dangling.  */
	  *chainp = NULL;
	  tail = code->next;
	  code->next = NULL;

	  innermost_loop
	    = restructure_intervening_code (&ns->code, outer_loop,
					    count);

	  /* At this point we have already pulled out the nested loop and
	     pointed outer_loop at it, and moved the intervening code that
	     was previously in the block into the body of innermost_loop.
	     Now we want to move the BLOCK itself so it wraps the entire
	     current body of innermost_loop.  */
	  ns->code = innermost_loop->block->next;
	  innermost_loop->block->next = code;
	  break;
	}
    }

  gcc_assert (innermost_loop);

  /* Now we have split the intervening code into two parts:
     head is the start of the part before the loop/block, terminating
     at *chainp, and tail is the part after it.  Mark each part as
     a structured block sequence, and splice the two parts around the
     existing body of the innermost loop.  */
  if (head != code)
    {
      gfc_code *block = make_structured_block (head);
      if (innermost_loop->block->next)
	gfc_append_code (block, innermost_loop->block->next);
      innermost_loop->block->next = block;
    }
  if (tail)
    {
      gfc_code *block = make_structured_block (tail);
      if (innermost_loop->block->next)
	gfc_append_code (innermost_loop->block->next, block);
      else
	innermost_loop->block->next = block;
    }

  /* For loops, finally splice CODE into OUTER_LOOP.  We already handled
     relinking EXEC_BLOCK above.  */
  if (code->op == EXEC_DO && outer_loop)
    outer_loop->block->next = code;

  return innermost_loop;
}

/* CODE is an OMP loop construct.  Return true if VAR matches an iteration
   variable outer to level DEPTH.  */
static bool
is_outer_iteration_variable (gfc_code *code, int depth, gfc_symbol *var)
{
  int i;
  gfc_code *do_code = code;

  for (i = 1; i < depth; i++)
    {
      do_code = find_nested_loop_in_chain (do_code->block->next);
      gcc_assert (do_code);
      if (do_code->op == EXEC_OMP_TILE || do_code->op == EXEC_OMP_UNROLL)
	{
	  --i;
	  continue;
	}
      gfc_symbol *ivar = do_code->ext.iterator->var->symtree->n.sym;
      if (var == ivar)
	return true;
    }
  return false;
}

/* Forward declaration for recursive functions.  */
static gfc_code *
check_nested_loop_in_block (gfc_code *block, gfc_expr *expr, gfc_symbol *sym,
			    bool *bad);

/* Like find_nested_loop_in_chain, but additionally check that EXPR
   does not reference any variables bound in intervening EXEC_BLOCKs
   and that SYM is not bound in such intervening blocks.  Either EXPR or SYM
   may be null.  Sets *BAD to true if either test fails.  */
static gfc_code *
check_nested_loop_in_chain (gfc_code *chain, gfc_expr *expr, gfc_symbol *sym,
			    bool *bad)
{
  for (gfc_code *code = chain; code; code = code->next)
    {
      if (code->op == EXEC_DO)
	return code;
      else if (code->op == EXEC_OMP_TILE || code->op == EXEC_OMP_UNROLL)
	return check_nested_loop_in_chain (code->block->next, expr, sym, bad);
      else if (code->op == EXEC_BLOCK)
	{
	  gfc_code *c = check_nested_loop_in_block (code, expr, sym, bad);
	  if (c)
	    return c;
	}
    }
  return NULL;
}

/* Code walker for block symtrees.  It doesn't take any kind of state
   argument, so use a static variable.  */
static struct check_nested_loop_in_block_state_t {
  gfc_expr *expr;
  gfc_symbol *sym;
  bool *bad;
} check_nested_loop_in_block_state;

static void
check_nested_loop_in_block_symbol (gfc_symbol *sym)
{
  if (sym == check_nested_loop_in_block_state.sym
      || (check_nested_loop_in_block_state.expr
	  && gfc_find_sym_in_expr (sym,
				   check_nested_loop_in_block_state.expr)))
    *check_nested_loop_in_block_state.bad = true;
}

/* Return the first nested DO loop in BLOCK, or NULL if there
   isn't one.  Set *BAD to true if EXPR references any variables in BLOCK, or
   SYM is bound in BLOCK.  Either EXPR or SYM may be null.  */
static gfc_code *
check_nested_loop_in_block (gfc_code *block, gfc_expr *expr,
			    gfc_symbol *sym, bool *bad)
{
  gfc_namespace *ns;
  gcc_assert (block->op == EXEC_BLOCK);
  ns = block->ext.block.ns;
  gcc_assert (ns);

  /* Skip the check if this block doesn't contain the nested loop, or
     if we already know it's bad.  */
  gfc_code *result = check_nested_loop_in_chain (ns->code, expr, sym, bad);
  if (result && !*bad)
    {
      check_nested_loop_in_block_state.expr = expr;
      check_nested_loop_in_block_state.sym = sym;
      check_nested_loop_in_block_state.bad = bad;
      gfc_traverse_ns (ns, check_nested_loop_in_block_symbol);
      check_nested_loop_in_block_state.expr = NULL;
      check_nested_loop_in_block_state.sym = NULL;
      check_nested_loop_in_block_state.bad = NULL;
    }
  return result;
}

/* CODE is an OMP loop construct.  Return true if EXPR references
   any variables bound in intervening code, to level DEPTH.  */
static bool
expr_uses_intervening_var (gfc_code *code, int depth, gfc_expr *expr)
{
  int i;
  gfc_code *do_code = code;

  for (i = 0; i < depth; i++)
    {
      bool bad = false;
      do_code = check_nested_loop_in_chain (do_code->block->next,
					    expr, NULL, &bad);
      if (bad)
	return true;
    }
  return false;
}

/* CODE is an OMP loop construct.  Return true if SYM is bound in
   intervening code, to level DEPTH.  */
static bool
is_intervening_var (gfc_code *code, int depth, gfc_symbol *sym)
{
  int i;
  gfc_code *do_code = code;

  for (i = 0; i < depth; i++)
    {
      bool bad = false;
      do_code = check_nested_loop_in_chain (do_code->block->next,
					    NULL, sym, &bad);
      if (bad)
	return true;
    }
  return false;
}

/* CODE is an OMP loop construct.  Return true if EXPR does not reference
   any iteration variables outer to level DEPTH.  */
static bool
expr_is_invariant (gfc_code *code, int depth, gfc_expr *expr)
{
  int i;
  gfc_code *do_code = code;

  for (i = 1; i < depth; i++)
    {
      do_code = find_nested_loop_in_chain (do_code->block->next);
      gcc_assert (do_code);
      if (do_code->op == EXEC_OMP_TILE || do_code->op == EXEC_OMP_UNROLL)
	{
	  --i;
	  continue;
	}
      gfc_symbol *ivar = do_code->ext.iterator->var->symtree->n.sym;
      if (gfc_find_sym_in_expr (ivar, expr))
	return false;
    }
  return true;
}

/* CODE is an OMP loop construct.  Return true if EXPR matches one of the
   canonical forms for a bound expression.  It may include references to
   an iteration variable outer to level DEPTH; set OUTER_VARP if so.  */
static bool
bound_expr_is_canonical (gfc_code *code, int depth, gfc_expr *expr,
			 gfc_symbol **outer_varp)
{
  gfc_expr *expr2 = NULL;

  /* Rectangular case.  */
  if (depth == 0 || expr_is_invariant (code, depth, expr))
    return true;

  /* Any simple variable that didn't pass expr_is_invariant must be
     an outer_var.  */
  if (expr->expr_type == EXPR_VARIABLE && expr->rank == 0)
    {
      *outer_varp = expr->symtree->n.sym;
      return true;
    }

  /* All other permitted forms are binary operators.  */
  if (expr->expr_type != EXPR_OP)
    return false;

  /* Check for plus/minus a loop invariant expr.  */
  if (expr->value.op.op == INTRINSIC_PLUS
      || expr->value.op.op == INTRINSIC_MINUS)
    {
      if (expr_is_invariant (code, depth, expr->value.op.op1))
	expr2 = expr->value.op.op2;
      else if (expr_is_invariant (code, depth, expr->value.op.op2))
	expr2 = expr->value.op.op1;
      else
	return false;
    }
  else
    expr2 = expr;

  /* Check for a product with a loop-invariant expr.  */
  if (expr2->expr_type == EXPR_OP
      && expr2->value.op.op == INTRINSIC_TIMES)
    {
      if (expr_is_invariant (code, depth, expr2->value.op.op1))
	expr2 = expr2->value.op.op2;
      else if (expr_is_invariant (code, depth, expr2->value.op.op2))
	expr2 = expr2->value.op.op1;
      else
	return false;
    }

  /* What's left must be a reference to an outer loop variable.  */
  if (expr2->expr_type == EXPR_VARIABLE
      && expr2->rank == 0
      && is_outer_iteration_variable (code, depth, expr2->symtree->n.sym))
    {
      *outer_varp = expr2->symtree->n.sym;
      return true;
    }

  return false;
}

static void
resolve_omp_do (gfc_code *code)
{
  gfc_code *do_code, *next;
  int list, i, count, non_generated_count;
  gfc_omp_namelist *n;
  gfc_symbol *dovar;
  const char *name;
  bool is_simd = false;
  bool errorp = false;
  bool perfect_nesting_errorp = false;
  bool imperfect = false;

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
    case EXEC_OMP_LOOP: name = "!$OMP LOOP"; break;
    case EXEC_OMP_PARALLEL_DO: name = "!$OMP PARALLEL DO"; break;
    case EXEC_OMP_PARALLEL_DO_SIMD:
      name = "!$OMP PARALLEL DO SIMD";
      is_simd = true;
      break;
    case EXEC_OMP_PARALLEL_LOOP: name = "!$OMP PARALLEL LOOP"; break;
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP:
      name = "!$OMP PARALLEL MASKED TASKLOOP";
      break;
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
      name = "!$OMP PARALLEL MASKED TASKLOOP SIMD";
      is_simd = true;
      break;
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP:
      name = "!$OMP PARALLEL MASTER TASKLOOP";
      break;
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
      name = "!$OMP PARALLEL MASTER TASKLOOP SIMD";
      is_simd = true;
      break;
    case EXEC_OMP_MASKED_TASKLOOP: name = "!$OMP MASKED TASKLOOP"; break;
    case EXEC_OMP_MASKED_TASKLOOP_SIMD:
      name = "!$OMP MASKED TASKLOOP SIMD";
      is_simd = true;
      break;
    case EXEC_OMP_MASTER_TASKLOOP: name = "!$OMP MASTER TASKLOOP"; break;
    case EXEC_OMP_MASTER_TASKLOOP_SIMD:
      name = "!$OMP MASTER TASKLOOP SIMD";
      is_simd = true;
      break;
    case EXEC_OMP_SIMD: name = "!$OMP SIMD"; is_simd = true; break;
    case EXEC_OMP_TARGET_PARALLEL_DO: name = "!$OMP TARGET PARALLEL DO"; break;
    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
      name = "!$OMP TARGET PARALLEL DO SIMD";
      is_simd = true;
      break;
    case EXEC_OMP_TARGET_PARALLEL_LOOP:
      name = "!$OMP TARGET PARALLEL LOOP";
      break;
    case EXEC_OMP_TARGET_SIMD:
      name = "!$OMP TARGET SIMD";
      is_simd = true;
      break;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
      name = "!$OMP TARGET TEAMS DISTRIBUTE";
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
    case EXEC_OMP_TARGET_TEAMS_LOOP: name = "!$OMP TARGET TEAMS LOOP"; break;
    case EXEC_OMP_TASKLOOP: name = "!$OMP TASKLOOP"; break;
    case EXEC_OMP_TASKLOOP_SIMD:
      name = "!$OMP TASKLOOP SIMD";
      is_simd = true;
      break;
    case EXEC_OMP_TEAMS_DISTRIBUTE: name = "!$OMP TEAMS DISTRIBUTE"; break;
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
    case EXEC_OMP_TEAMS_LOOP: name = "!$OMP TEAMS LOOP"; break;
    case EXEC_OMP_TILE: name = "!$OMP TILE"; break;
    case EXEC_OMP_UNROLL: name = "!$OMP UNROLL"; break;
    default: gcc_unreachable ();
    }

  if (code->ext.omp_clauses)
    resolve_omp_clauses (code, code->ext.omp_clauses, NULL);

  if (code->op == EXEC_OMP_TILE && code->ext.omp_clauses->sizes_list == NULL)
    gfc_error ("SIZES clause is required on !$OMP TILE construct at %L",
	       &code->loc);

  do_code = code->block->next;
  if (code->ext.omp_clauses->orderedc)
    count = code->ext.omp_clauses->orderedc;
  else if (code->ext.omp_clauses->sizes_list)
    count = gfc_expr_list_len (code->ext.omp_clauses->sizes_list);
  else
    {
      count = code->ext.omp_clauses->collapse;
      if (count <= 0)
	count = 1;
    }

  non_generated_count = count;
  /* While the spec defines the loop nest depth independently of the COLLAPSE
     clause, in practice the middle end only pays attention to the COLLAPSE
     depth and treats any further inner loops as the final-loop-body.  So
     here we also check canonical loop nest form only for the number of
     outer loops specified by the COLLAPSE clause too.  */
  for (i = 1; i <= count; i++)
    {
      gfc_symbol *start_var = NULL, *end_var = NULL;
      /* Parse errors are not recoverable.  */
      if (do_code->op == EXEC_DO_WHILE)
	{
	  gfc_error ("%s cannot be a DO WHILE or DO without loop control "
		     "at %L", name, &do_code->loc);
	  goto fail;
	}
      if (do_code->op == EXEC_DO_CONCURRENT)
	{
	  gfc_error ("%s cannot be a DO CONCURRENT loop at %L", name,
		     &do_code->loc);
	  goto fail;
	}
      if (do_code->op == EXEC_OMP_TILE || do_code->op == EXEC_OMP_UNROLL)
	{
	  if (do_code->op == EXEC_OMP_UNROLL)
	    {
	      if (!do_code->ext.omp_clauses->partial)
		{
		  gfc_error ("Generated loop of UNROLL construct at %L "
			     "without PARTIAL clause does not have "
			     "canonical form", &do_code->loc);
		  goto fail;
		}
	      else if (i != count)
		{
		  gfc_error ("UNROLL construct at %L with PARTIAL clause "
			     "generates just one loop with canonical form "
			     "but %d loops are needed",
			     &do_code->loc, count - i + 1);
		  goto fail;
		}
	    }
	  else if (do_code->op == EXEC_OMP_TILE)
	    {
	      if (do_code->ext.omp_clauses->sizes_list == NULL)
		/* This should have been diagnosed earlier already.  */
		return;
	      int l = gfc_expr_list_len (do_code->ext.omp_clauses->sizes_list);
	      if (count - i + 1 > l)
		{
		  gfc_error ("TILE construct at %L generates %d loops "
			     "with canonical form but %d loops are needed",
			     &do_code->loc, l, count - i + 1);
		  goto fail;
		}
	    }
	  if (do_code->ext.omp_clauses && do_code->ext.omp_clauses->erroneous)
	    goto fail;
	  if (imperfect && !perfect_nesting_errorp)
	    {
	      sorry_at (gfc_get_location (&do_code->loc),
			"Imperfectly nested loop using generated loops");
	      errorp = true;
	    }
	  if (non_generated_count == count)
	    non_generated_count = i - 1;
	  --i;
	  do_code = do_code->block->next;
	  continue;
	}
      gcc_assert (do_code->op == EXEC_DO);
      if (do_code->ext.iterator->var->ts.type != BT_INTEGER)
	{
	  gfc_error ("%s iteration variable must be of type integer at %L",
		     name, &do_code->loc);
	  errorp = true;
	}
      dovar = do_code->ext.iterator->var->symtree->n.sym;
      if (dovar->attr.threadprivate)
	{
	  gfc_error ("%s iteration variable must not be THREADPRIVATE "
		     "at %L", name, &do_code->loc);
	  errorp = true;
	}
      if (code->ext.omp_clauses)
	for (list = 0; list < OMP_LIST_NUM; list++)
	  if (!is_simd || code->ext.omp_clauses->collapse > 1
	      ? (list != OMP_LIST_PRIVATE && list != OMP_LIST_LASTPRIVATE
		  && list != OMP_LIST_ALLOCATE)
	      : (list != OMP_LIST_PRIVATE && list != OMP_LIST_LASTPRIVATE
		 && list != OMP_LIST_ALLOCATE && list != OMP_LIST_LINEAR))
	    for (n = code->ext.omp_clauses->lists[list]; n; n = n->next)
	      if (dovar == n->sym)
		{
		  if (!is_simd || code->ext.omp_clauses->collapse > 1)
		    gfc_error ("%s iteration variable present on clause "
			       "other than PRIVATE, LASTPRIVATE or "
			       "ALLOCATE at %L", name, &do_code->loc);
		  else
		    gfc_error ("%s iteration variable present on clause "
			       "other than PRIVATE, LASTPRIVATE, ALLOCATE or "
			       "LINEAR at %L", name, &do_code->loc);
		  errorp = true;
		}
      if (is_outer_iteration_variable (code, i, dovar))
	{
	  gfc_error ("%s iteration variable used in more than one loop at %L",
		     name, &do_code->loc);
	  errorp = true;
	}
      else if (is_intervening_var (code, i, dovar))
	{
	  gfc_error ("%s iteration variable at %L is bound in "
		     "intervening code",
		     name, &do_code->loc);
	  errorp = true;
	}
      else if (!bound_expr_is_canonical (code, i,
					 do_code->ext.iterator->start,
					 &start_var))
	{
	  gfc_error ("%s loop start expression not in canonical form at %L",
		     name, &do_code->loc);
	  errorp = true;
	}
      else if (expr_uses_intervening_var (code, i,
					  do_code->ext.iterator->start))
	{
	  gfc_error ("%s loop start expression at %L uses variable bound in "
		     "intervening code",
		     name, &do_code->loc);
	  errorp = true;
	}
      else if (!bound_expr_is_canonical (code, i,
					 do_code->ext.iterator->end,
					 &end_var))
	{
	  gfc_error ("%s loop end expression not in canonical form at %L",
		     name, &do_code->loc);
	  errorp = true;
	}
      else if (expr_uses_intervening_var (code, i,
					  do_code->ext.iterator->end))
	{
	  gfc_error ("%s loop end expression at %L uses variable bound in "
		     "intervening code",
		     name, &do_code->loc);
	  errorp = true;
	}
      else if (start_var && end_var && start_var != end_var)
	{
	  gfc_error ("%s loop bounds reference different "
		     "iteration variables at %L", name, &do_code->loc);
	  errorp = true;
	}
      else if (!expr_is_invariant (code, i, do_code->ext.iterator->step))
	{
	  gfc_error ("%s loop increment not in canonical form at %L",
		     name, &do_code->loc);
	  errorp = true;
	}
      else if (expr_uses_intervening_var (code, i,
					  do_code->ext.iterator->step))
	{
	  gfc_error ("%s loop increment expression at %L uses variable "
		     "bound in intervening code",
		     name, &do_code->loc);
	  errorp = true;
	}
      if (start_var || end_var)
	{
	  code->ext.omp_clauses->non_rectangular = 1;
	  if (i > non_generated_count)
	    {
	      sorry_at (gfc_get_location (&do_code->loc),
			"Non-rectangular loops from generated loops "
			"unsupported");
	      errorp = true;
	    }
	}

      /* Only parse loop body into nested loop and intervening code if
	 there are supposed to be more loops in the nest to collapse.  */
      if (i == count)
	break;

      next = find_nested_loop_in_chain (do_code->block->next);

      if (!next)
	{
	  /* Parse error, can't recover from this.  */
	  gfc_error ("not enough DO loops for collapsed %s (level %d) at %L",
		     name, i, &code->loc);
	  goto fail;
	}
      else if (next != do_code->block->next || next->next)
	/* Imperfectly nested loop found.  */
	{
	  /* Only diagnose violation of imperfect nesting constraints once.  */
	  if (!perfect_nesting_errorp)
	    {
	      if (code->ext.omp_clauses->orderedc)
		{
		  gfc_error ("%s inner loops must be perfectly nested with "
			     "ORDERED clause at %L",
			     name, &code->loc);
		  perfect_nesting_errorp = true;
		}
	      else if (code->ext.omp_clauses->lists[OMP_LIST_REDUCTION_INSCAN])
		{
		  gfc_error ("%s inner loops must be perfectly nested with "
			     "REDUCTION INSCAN clause at %L",
			     name, &code->loc);
		  perfect_nesting_errorp = true;
		}
	      else if (code->op == EXEC_OMP_TILE)
		{
		  gfc_error ("%s inner loops must be perfectly nested at %L",
			     name, &code->loc);
		  perfect_nesting_errorp = true;
		}
	      if (perfect_nesting_errorp)
		errorp = true;
	    }
	  if (diagnose_intervening_code_errors (do_code->block->next,
						name, next))
	    errorp = true;
	  imperfect = true;
	}
      do_code = next;
    }

  /* Give up now if we found any constraint violations.  */
  if (errorp)
    {
    fail:
      if (code->ext.omp_clauses)
	code->ext.omp_clauses->erroneous = 1;
      return;
    }

  if (non_generated_count)
    restructure_intervening_code (&code->block->next, code,
				  non_generated_count);
}

static void
resolve_omp_metadirective (gfc_code *code, gfc_namespace *ns)
{
  gfc_omp_variant *variant = code->ext.omp_variants;

  while (variant)
    {
      gfc_code *variant_code = variant->code;
      gfc_resolve_code (variant_code, ns);
      variant = variant->next;
    }
}


static gfc_statement
omp_code_to_statement (gfc_code *code)
{
  switch (code->op)
    {
    case EXEC_OMP_PARALLEL:
      return ST_OMP_PARALLEL;
    case EXEC_OMP_PARALLEL_MASKED:
      return ST_OMP_PARALLEL_MASKED;
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP:
      return ST_OMP_PARALLEL_MASKED_TASKLOOP;
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
      return ST_OMP_PARALLEL_MASKED_TASKLOOP_SIMD;
    case EXEC_OMP_PARALLEL_MASTER:
      return ST_OMP_PARALLEL_MASTER;
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP:
      return ST_OMP_PARALLEL_MASTER_TASKLOOP;
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
      return ST_OMP_PARALLEL_MASTER_TASKLOOP_SIMD;
    case EXEC_OMP_PARALLEL_SECTIONS:
      return ST_OMP_PARALLEL_SECTIONS;
    case EXEC_OMP_SECTIONS:
      return ST_OMP_SECTIONS;
    case EXEC_OMP_ORDERED:
      return ST_OMP_ORDERED;
    case EXEC_OMP_CRITICAL:
      return ST_OMP_CRITICAL;
    case EXEC_OMP_MASKED:
      return ST_OMP_MASKED;
    case EXEC_OMP_MASKED_TASKLOOP:
      return ST_OMP_MASKED_TASKLOOP;
    case EXEC_OMP_MASKED_TASKLOOP_SIMD:
      return ST_OMP_MASKED_TASKLOOP_SIMD;
    case EXEC_OMP_MASTER:
      return ST_OMP_MASTER;
    case EXEC_OMP_MASTER_TASKLOOP:
      return ST_OMP_MASTER_TASKLOOP;
    case EXEC_OMP_MASTER_TASKLOOP_SIMD:
      return ST_OMP_MASTER_TASKLOOP_SIMD;
    case EXEC_OMP_SINGLE:
      return ST_OMP_SINGLE;
    case EXEC_OMP_TASK:
      return ST_OMP_TASK;
    case EXEC_OMP_WORKSHARE:
      return ST_OMP_WORKSHARE;
    case EXEC_OMP_PARALLEL_WORKSHARE:
      return ST_OMP_PARALLEL_WORKSHARE;
    case EXEC_OMP_DO:
      return ST_OMP_DO;
    case EXEC_OMP_LOOP:
      return ST_OMP_LOOP;
    case EXEC_OMP_ALLOCATE:
      return ST_OMP_ALLOCATE_EXEC;
    case EXEC_OMP_ALLOCATORS:
      return ST_OMP_ALLOCATORS;
    case EXEC_OMP_ASSUME:
      return ST_OMP_ASSUME;
    case EXEC_OMP_ATOMIC:
      return ST_OMP_ATOMIC;
    case EXEC_OMP_BARRIER:
      return ST_OMP_BARRIER;
    case EXEC_OMP_CANCEL:
      return ST_OMP_CANCEL;
    case EXEC_OMP_CANCELLATION_POINT:
      return ST_OMP_CANCELLATION_POINT;
    case EXEC_OMP_ERROR:
      return ST_OMP_ERROR;
    case EXEC_OMP_FLUSH:
      return ST_OMP_FLUSH;
    case EXEC_OMP_INTEROP:
      return ST_OMP_INTEROP;
    case EXEC_OMP_DISTRIBUTE:
      return ST_OMP_DISTRIBUTE;
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
      return ST_OMP_DISTRIBUTE_PARALLEL_DO;
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
      return ST_OMP_DISTRIBUTE_PARALLEL_DO_SIMD;
    case EXEC_OMP_DISTRIBUTE_SIMD:
      return ST_OMP_DISTRIBUTE_SIMD;
    case EXEC_OMP_DO_SIMD:
      return ST_OMP_DO_SIMD;
    case EXEC_OMP_SCAN:
      return ST_OMP_SCAN;
    case EXEC_OMP_SCOPE:
      return ST_OMP_SCOPE;
    case EXEC_OMP_SIMD:
      return ST_OMP_SIMD;
    case EXEC_OMP_TARGET:
      return ST_OMP_TARGET;
    case EXEC_OMP_TARGET_DATA:
      return ST_OMP_TARGET_DATA;
    case EXEC_OMP_TARGET_ENTER_DATA:
      return ST_OMP_TARGET_ENTER_DATA;
    case EXEC_OMP_TARGET_EXIT_DATA:
      return ST_OMP_TARGET_EXIT_DATA;
    case EXEC_OMP_TARGET_PARALLEL:
      return ST_OMP_TARGET_PARALLEL;
    case EXEC_OMP_TARGET_PARALLEL_DO:
      return ST_OMP_TARGET_PARALLEL_DO;
    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
      return ST_OMP_TARGET_PARALLEL_DO_SIMD;
    case EXEC_OMP_TARGET_PARALLEL_LOOP:
      return ST_OMP_TARGET_PARALLEL_LOOP;
    case EXEC_OMP_TARGET_SIMD:
      return ST_OMP_TARGET_SIMD;
    case EXEC_OMP_TARGET_TEAMS:
      return ST_OMP_TARGET_TEAMS;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
      return ST_OMP_TARGET_TEAMS_DISTRIBUTE;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
      return ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      return ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
      return ST_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD;
    case EXEC_OMP_TARGET_TEAMS_LOOP:
      return ST_OMP_TARGET_TEAMS_LOOP;
    case EXEC_OMP_TARGET_UPDATE:
      return ST_OMP_TARGET_UPDATE;
    case EXEC_OMP_TASKGROUP:
      return ST_OMP_TASKGROUP;
    case EXEC_OMP_TASKLOOP:
      return ST_OMP_TASKLOOP;
    case EXEC_OMP_TASKLOOP_SIMD:
      return ST_OMP_TASKLOOP_SIMD;
    case EXEC_OMP_TASKWAIT:
      return ST_OMP_TASKWAIT;
    case EXEC_OMP_TASKYIELD:
      return ST_OMP_TASKYIELD;
    case EXEC_OMP_TEAMS:
      return ST_OMP_TEAMS;
    case EXEC_OMP_TEAMS_DISTRIBUTE:
      return ST_OMP_TEAMS_DISTRIBUTE;
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
      return ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO;
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      return ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD;
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
      return ST_OMP_TEAMS_DISTRIBUTE_SIMD;
    case EXEC_OMP_TEAMS_LOOP:
      return ST_OMP_TEAMS_LOOP;
    case EXEC_OMP_PARALLEL_DO:
      return ST_OMP_PARALLEL_DO;
    case EXEC_OMP_PARALLEL_DO_SIMD:
      return ST_OMP_PARALLEL_DO_SIMD;
    case EXEC_OMP_PARALLEL_LOOP:
      return ST_OMP_PARALLEL_LOOP;
    case EXEC_OMP_DEPOBJ:
      return ST_OMP_DEPOBJ;
    case EXEC_OMP_TILE:
      return ST_OMP_TILE;
    case EXEC_OMP_UNROLL:
      return ST_OMP_UNROLL;
    case EXEC_OMP_DISPATCH:
      return ST_OMP_DISPATCH;
    default:
      gcc_unreachable ();
    }
}

static gfc_statement
oacc_code_to_statement (gfc_code *code)
{
  switch (code->op)
    {
    case EXEC_OACC_PARALLEL:
      return ST_OACC_PARALLEL;
    case EXEC_OACC_KERNELS:
      return ST_OACC_KERNELS;
    case EXEC_OACC_SERIAL:
      return ST_OACC_SERIAL;
    case EXEC_OACC_DATA:
      return ST_OACC_DATA;
    case EXEC_OACC_HOST_DATA:
      return ST_OACC_HOST_DATA;
    case EXEC_OACC_PARALLEL_LOOP:
      return ST_OACC_PARALLEL_LOOP;
    case EXEC_OACC_KERNELS_LOOP:
      return ST_OACC_KERNELS_LOOP;
    case EXEC_OACC_SERIAL_LOOP:
      return ST_OACC_SERIAL_LOOP;
    case EXEC_OACC_LOOP:
      return ST_OACC_LOOP;
    case EXEC_OACC_ATOMIC:
      return ST_OACC_ATOMIC;
    case EXEC_OACC_ROUTINE:
      return ST_OACC_ROUTINE;
    case EXEC_OACC_UPDATE:
      return ST_OACC_UPDATE;
    case EXEC_OACC_WAIT:
      return ST_OACC_WAIT;
    case EXEC_OACC_CACHE:
      return ST_OACC_CACHE;
    case EXEC_OACC_ENTER_DATA:
      return ST_OACC_ENTER_DATA;
    case EXEC_OACC_EXIT_DATA:
      return ST_OACC_EXIT_DATA;
    case EXEC_OACC_DECLARE:
      return ST_OACC_DECLARE;
    default:
      gcc_unreachable ();
    }
}

static void
resolve_oacc_directive_inside_omp_region (gfc_code *code)
{
  if (omp_current_ctx != NULL && omp_current_ctx->is_openmp)
    {
      gfc_statement st = omp_code_to_statement (omp_current_ctx->code);
      gfc_statement oacc_st = oacc_code_to_statement (code);
      gfc_error ("The %s directive cannot be specified within "
		 "a %s region at %L", gfc_ascii_statement (oacc_st),
		 gfc_ascii_statement (st), &code->loc);
    }
}

static void
resolve_omp_directive_inside_oacc_region (gfc_code *code)
{
  if (omp_current_ctx != NULL && !omp_current_ctx->is_openmp)
    {
      gfc_statement st = oacc_code_to_statement (omp_current_ctx->code);
      gfc_statement omp_st = omp_code_to_statement (code);
      gfc_error ("The %s directive cannot be specified within "
		 "a %s region at %L", gfc_ascii_statement (omp_st),
		 gfc_ascii_statement (st), &code->loc);
    }
}


static void
resolve_oacc_nested_loops (gfc_code *code, gfc_code* do_code, int collapse,
			  const char *clause)
{
  gfc_symbol *dovar;
  gfc_code *c;
  int i;

  for (i = 1; i <= collapse; i++)
    {
      if (do_code->op == EXEC_DO_WHILE)
	{
	  gfc_error ("!$ACC LOOP cannot be a DO WHILE or DO without loop control "
		     "at %L", &do_code->loc);
	  break;
	}
      if (do_code->op == EXEC_DO_CONCURRENT)
	{
	  gfc_error ("!$ACC LOOP cannot be a DO CONCURRENT loop at %L",
		     &do_code->loc);
	  break;
	}
      gcc_assert (do_code->op == EXEC_DO);
      if (do_code->ext.iterator->var->ts.type != BT_INTEGER)
	gfc_error ("!$ACC LOOP iteration variable must be of type integer at %L",
		   &do_code->loc);
      dovar = do_code->ext.iterator->var->symtree->n.sym;
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
		  gfc_error ("!$ACC LOOP %s loops don't form rectangular "
			     "iteration space at %L", clause, &do_code->loc);
		  break;
		}
	      do_code2 = do_code2->block->next;
	    }
	}
      if (i == collapse)
	break;
      for (c = do_code->next; c; c = c->next)
	if (c->op != EXEC_NOP && c->op != EXEC_CONTINUE)
	  {
	    gfc_error ("%s !$ACC LOOP loops not perfectly nested at %L",
		       clause, &c->loc);
	    break;
	  }
      if (c)
	break;
      do_code = do_code->block;
      if (do_code->op != EXEC_DO && do_code->op != EXEC_DO_WHILE
	  && do_code->op != EXEC_DO_CONCURRENT)
	{
	  gfc_error ("not enough DO loops for %s !$ACC LOOP at %L",
		     clause, &code->loc);
	  break;
	}
      do_code = do_code->next;
      if (do_code == NULL
	  || (do_code->op != EXEC_DO && do_code->op != EXEC_DO_WHILE
	      && do_code->op != EXEC_DO_CONCURRENT))
	{
	  gfc_error ("not enough DO loops for %s !$ACC LOOP at %L",
		     clause, &code->loc);
	  break;
	}
    }
}


static void
resolve_oacc_loop_blocks (gfc_code *code)
{
  if (!oacc_is_loop (code))
    return;

  if (code->ext.omp_clauses->tile_list && code->ext.omp_clauses->gang
      && code->ext.omp_clauses->worker && code->ext.omp_clauses->vector)
    gfc_error ("Tiled loop cannot be parallelized across gangs, workers and "
	       "vectors at the same time at %L", &code->loc);

  if (code->ext.omp_clauses->tile_list)
    {
      gfc_expr_list *el;
      for (el = code->ext.omp_clauses->tile_list; el; el = el->next)
	{
	  if (el->expr == NULL)
	    {
	      /* NULL expressions are used to represent '*' arguments.
		 Convert those to a 0 expressions.  */
	      el->expr = gfc_get_constant_expr (BT_INTEGER,
						gfc_default_integer_kind,
						&code->loc);
	      mpz_set_si (el->expr->value.integer, 0);
	    }
	  else
	    {
	      resolve_positive_int_expr (el->expr, "TILE");
	      if (el->expr->expr_type != EXPR_CONSTANT)
		gfc_error ("TILE requires constant expression at %L",
			   &code->loc);
	    }
	}
    }
}


void
gfc_resolve_oacc_blocks (gfc_code *code, gfc_namespace *ns)
{
  fortran_omp_context ctx;
  gfc_omp_clauses *omp_clauses = code->ext.omp_clauses;
  gfc_omp_namelist *n;
  int list;

  resolve_oacc_loop_blocks (code);

  ctx.code = code;
  ctx.sharing_clauses = new hash_set<gfc_symbol *>;
  ctx.private_iterators = new hash_set<gfc_symbol *>;
  ctx.previous = omp_current_ctx;
  ctx.is_openmp = false;
  omp_current_ctx = &ctx;

  for (list = 0; list < OMP_LIST_NUM; list++)
    switch (list)
      {
      case OMP_LIST_PRIVATE:
	for (n = omp_clauses->lists[list]; n; n = n->next)
	  ctx.sharing_clauses->add (n->sym);
	break;
      default:
	break;
      }

  gfc_resolve_blocks (code->block, ns);

  omp_current_ctx = ctx.previous;
  delete ctx.sharing_clauses;
  delete ctx.private_iterators;
}


static void
resolve_oacc_loop (gfc_code *code)
{
  gfc_code *do_code;
  int collapse;

  if (code->ext.omp_clauses)
    resolve_omp_clauses (code, code->ext.omp_clauses, NULL, true);

  do_code = code->block->next;
  collapse = code->ext.omp_clauses->collapse;

  /* Both collapsed and tiled loops are lowered the same way, but are not
     compatible.  In gfc_trans_omp_do, the tile is prioritized.  */
  if (code->ext.omp_clauses->tile_list)
    {
      int num = 0;
      gfc_expr_list *el;
      for (el = code->ext.omp_clauses->tile_list; el; el = el->next)
	++num;
      resolve_oacc_nested_loops (code, code->block->next, num, "tiled");
      return;
    }

  if (collapse <= 0)
    collapse = 1;
  resolve_oacc_nested_loops (code, do_code, collapse, "collapsed");
}

void
gfc_resolve_oacc_declare (gfc_namespace *ns)
{
  int list;
  gfc_omp_namelist *n;
  gfc_oacc_declare *oc;

  if (ns->oacc_declare == NULL)
    return;

  for (oc = ns->oacc_declare; oc; oc = oc->next)
    {
      for (list = 0; list < OMP_LIST_NUM; list++)
	for (n = oc->clauses->lists[list]; n; n = n->next)
	  {
	    n->sym->mark = 0;
	    if (n->sym->attr.flavor != FL_VARIABLE
		&& (n->sym->attr.flavor != FL_PROCEDURE
		    || n->sym->result != n->sym))
	      {
		gfc_error ("Object %qs is not a variable at %L",
			   n->sym->name, &oc->loc);
		continue;
	      }

	    if (n->expr && n->expr->ref->type == REF_ARRAY)
	      {
		gfc_error ("Array sections: %qs not allowed in"
			   " !$ACC DECLARE at %L", n->sym->name, &oc->loc);
		continue;
	      }
	  }

      for (n = oc->clauses->lists[OMP_LIST_DEVICE_RESIDENT]; n; n = n->next)
	check_array_not_assumed (n->sym, oc->loc, "DEVICE_RESIDENT");
    }

  for (oc = ns->oacc_declare; oc; oc = oc->next)
    {
      for (list = 0; list < OMP_LIST_NUM; list++)
	for (n = oc->clauses->lists[list]; n; n = n->next)
	  {
	    if (n->sym->mark)
	      {
		gfc_error ("Symbol %qs present on multiple clauses at %L",
			   n->sym->name, &oc->loc);
		continue;
	      }
	    else
	      n->sym->mark = 1;
	  }
    }

  for (oc = ns->oacc_declare; oc; oc = oc->next)
    {
      for (list = 0; list < OMP_LIST_NUM; list++)
	for (n = oc->clauses->lists[list]; n; n = n->next)
	  n->sym->mark = 0;
    }
}


void
gfc_resolve_oacc_routines (gfc_namespace *ns)
{
  for (gfc_oacc_routine_name *orn = ns->oacc_routine_names;
       orn;
       orn = orn->next)
    {
      gfc_symbol *sym = orn->sym;
      if (!sym->attr.external
	  && !sym->attr.function
	  && !sym->attr.subroutine)
	{
	  gfc_error ("NAME %qs does not refer to a subroutine or function"
		     " in !$ACC ROUTINE ( NAME ) at %L", sym->name, &orn->loc);
	  continue;
	}
      if (!gfc_add_omp_declare_target (&sym->attr, sym->name, &orn->loc))
	{
	  gfc_error ("NAME %qs invalid"
		     " in !$ACC ROUTINE ( NAME ) at %L", sym->name, &orn->loc);
	  continue;
	}
    }
}


void
gfc_resolve_oacc_directive (gfc_code *code, gfc_namespace *ns ATTRIBUTE_UNUSED)
{
  resolve_oacc_directive_inside_omp_region (code);

  switch (code->op)
    {
    case EXEC_OACC_PARALLEL:
    case EXEC_OACC_KERNELS:
    case EXEC_OACC_SERIAL:
    case EXEC_OACC_DATA:
    case EXEC_OACC_HOST_DATA:
    case EXEC_OACC_UPDATE:
    case EXEC_OACC_ENTER_DATA:
    case EXEC_OACC_EXIT_DATA:
    case EXEC_OACC_WAIT:
    case EXEC_OACC_CACHE:
      resolve_omp_clauses (code, code->ext.omp_clauses, NULL, true);
      break;
    case EXEC_OACC_PARALLEL_LOOP:
    case EXEC_OACC_KERNELS_LOOP:
    case EXEC_OACC_SERIAL_LOOP:
    case EXEC_OACC_LOOP:
      resolve_oacc_loop (code);
      break;
    case EXEC_OACC_ATOMIC:
      resolve_omp_atomic (code);
      break;
    default:
      break;
    }
}


static void
resolve_omp_target (gfc_code *code)
{
#define GFC_IS_TEAMS_CONSTRUCT(op)			\
  (op == EXEC_OMP_TEAMS					\
   || op == EXEC_OMP_TEAMS_DISTRIBUTE			\
   || op == EXEC_OMP_TEAMS_DISTRIBUTE_SIMD		\
   || op == EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO	\
   || op == EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD	\
   || op == EXEC_OMP_TEAMS_LOOP)

  if (!code->ext.omp_clauses->contains_teams_construct)
    return;
  gfc_code *c = code->block->next;
  if (c->op == EXEC_BLOCK)
    c = c->ext.block.ns->code;
  if (code->ext.omp_clauses->target_first_st_is_teams_or_meta)
    {
      if (c->op == EXEC_OMP_METADIRECTIVE)
	{
	  struct gfc_omp_variant *mc
	    = c->ext.omp_variants;
	  /* All mc->(next...->)code should be identical with regards
	     to the diagnostic below.  */
	  do
	    {
	      if (mc->stmt != ST_NONE
		  && GFC_IS_TEAMS_CONSTRUCT (mc->code->op))
		{
		  if (c->next == NULL && mc->code->next == NULL)
		    return;
		  c = mc->code;
		  break;
		}
	      mc = mc->next;
	    }
	  while (mc);
	}
      else if (GFC_IS_TEAMS_CONSTRUCT (c->op) && c->next == NULL)
	return;
    }

  while (c && !GFC_IS_TEAMS_CONSTRUCT (c->op))
    c = c->next;
  if (c)
    gfc_error ("!$OMP TARGET region at %L with a nested TEAMS at %L may not "
	       "contain any other statement, declaration or directive outside "
	       "of the single TEAMS construct", &c->loc, &code->loc);
  else
    gfc_error ("!$OMP TARGET region at %L with a nested TEAMS may not "
	       "contain any other statement, declaration or directive outside "
	       "of the single TEAMS construct", &code->loc);
#undef GFC_IS_TEAMS_CONSTRUCT
}

static void
resolve_omp_dispatch (gfc_code *code)
{
  gfc_code *next = code->block->next;
  if (next == NULL)
    return;

  gfc_exec_op op = next->op;
  gcc_assert (op == EXEC_CALL || op == EXEC_ASSIGN);
  if (op != EXEC_CALL
      && (op != EXEC_ASSIGN || next->expr2->expr_type != EXPR_FUNCTION))
    gfc_error (
      "%<OMP DISPATCH%> directive at %L must be followed by a procedure "
      "call with optional assignment",
      &code->loc);

  if ((op == EXEC_CALL && next->resolved_sym != NULL
       && next->resolved_sym->attr.proc_pointer)
      || (op == EXEC_ASSIGN && gfc_expr_attr (next->expr2).proc_pointer))
    gfc_error ("%<OMP DISPATCH%> directive at %L cannot be followed by a "
	       "procedure pointer",
	       &code->loc);
}

/* Resolve OpenMP directive clauses and check various requirements
   of each directive.  */

void
gfc_resolve_omp_directive (gfc_code *code, gfc_namespace *ns)
{
  resolve_omp_directive_inside_oacc_region (code);

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
    case EXEC_OMP_LOOP:
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_DO_SIMD:
    case EXEC_OMP_PARALLEL_LOOP:
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP:
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP:
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
    case EXEC_OMP_MASKED_TASKLOOP:
    case EXEC_OMP_MASKED_TASKLOOP_SIMD:
    case EXEC_OMP_MASTER_TASKLOOP:
    case EXEC_OMP_MASTER_TASKLOOP_SIMD:
    case EXEC_OMP_SIMD:
    case EXEC_OMP_TARGET_PARALLEL_DO:
    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_PARALLEL_LOOP:
    case EXEC_OMP_TARGET_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TARGET_TEAMS_LOOP:
    case EXEC_OMP_TASKLOOP:
    case EXEC_OMP_TASKLOOP_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TEAMS_LOOP:
    case EXEC_OMP_TILE:
    case EXEC_OMP_UNROLL:
      resolve_omp_do (code);
      break;
    case EXEC_OMP_TARGET:
      resolve_omp_target (code);
      gcc_fallthrough ();
    case EXEC_OMP_ALLOCATE:
    case EXEC_OMP_ALLOCATORS:
    case EXEC_OMP_ASSUME:
    case EXEC_OMP_CANCEL:
    case EXEC_OMP_ERROR:
    case EXEC_OMP_INTEROP:
    case EXEC_OMP_MASKED:
    case EXEC_OMP_ORDERED:
    case EXEC_OMP_PARALLEL_WORKSHARE:
    case EXEC_OMP_PARALLEL:
    case EXEC_OMP_PARALLEL_MASKED:
    case EXEC_OMP_PARALLEL_MASTER:
    case EXEC_OMP_PARALLEL_SECTIONS:
    case EXEC_OMP_SCOPE:
    case EXEC_OMP_SECTIONS:
    case EXEC_OMP_SINGLE:
    case EXEC_OMP_TARGET_DATA:
    case EXEC_OMP_TARGET_ENTER_DATA:
    case EXEC_OMP_TARGET_EXIT_DATA:
    case EXEC_OMP_TARGET_PARALLEL:
    case EXEC_OMP_TARGET_TEAMS:
    case EXEC_OMP_TASK:
    case EXEC_OMP_TASKWAIT:
    case EXEC_OMP_TEAMS:
    case EXEC_OMP_WORKSHARE:
    case EXEC_OMP_DEPOBJ:
      if (code->ext.omp_clauses)
	resolve_omp_clauses (code, code->ext.omp_clauses, NULL);
      break;
    case EXEC_OMP_TARGET_UPDATE:
      if (code->ext.omp_clauses)
	resolve_omp_clauses (code, code->ext.omp_clauses, NULL);
      if (code->ext.omp_clauses == NULL
	  || (code->ext.omp_clauses->lists[OMP_LIST_TO] == NULL
	      && code->ext.omp_clauses->lists[OMP_LIST_FROM] == NULL))
	gfc_error ("OMP TARGET UPDATE at %L requires at least one TO or "
		   "FROM clause", &code->loc);
      break;
    case EXEC_OMP_ATOMIC:
      resolve_omp_clauses (code, code->block->ext.omp_clauses, NULL);
      resolve_omp_atomic (code);
      break;
    case EXEC_OMP_CRITICAL:
      resolve_omp_clauses (code, code->ext.omp_clauses, NULL);
      if (!code->ext.omp_clauses->critical_name
	  && code->ext.omp_clauses->hint
	  && code->ext.omp_clauses->hint->ts.type == BT_INTEGER
	  && code->ext.omp_clauses->hint->expr_type == EXPR_CONSTANT
	  && mpz_sgn (code->ext.omp_clauses->hint->value.integer) != 0)
	gfc_error ("OMP CRITICAL at %L with HINT clause requires a NAME, "
		   "except when omp_sync_hint_none is used", &code->loc);
      break;
    case EXEC_OMP_SCAN:
      /* Flag is only used to checking, hence, it is unset afterwards.  */
      if (!code->ext.omp_clauses->if_present)
	gfc_error ("Unexpected !$OMP SCAN at %L outside loop construct with "
		   "%<inscan%> REDUCTION clause", &code->loc);
      code->ext.omp_clauses->if_present = false;
      resolve_omp_clauses (code, code->ext.omp_clauses, ns);
      break;
    case EXEC_OMP_DISPATCH:
      if (code->ext.omp_clauses)
	resolve_omp_clauses (code, code->ext.omp_clauses, ns);
      resolve_omp_dispatch (code);
      break;
    case EXEC_OMP_METADIRECTIVE:
      resolve_omp_metadirective (code, ns);
      break;
    default:
      break;
    }
}

/* Resolve !$omp declare {variant|simd} constructs in NS.
   Note that !$omp declare target is resolved in resolve_symbol.  */

void
gfc_resolve_omp_declare (gfc_namespace *ns)
{
  gfc_omp_declare_simd *ods;
  for (ods = ns->omp_declare_simd; ods; ods = ods->next)
    {
      if (ods->proc_name != NULL
	  && ods->proc_name != ns->proc_name)
	gfc_error ("!$OMP DECLARE SIMD should refer to containing procedure "
		   "%qs at %L", ns->proc_name->name, &ods->where);
      if (ods->clauses)
	resolve_omp_clauses (NULL, ods->clauses, ns);
    }

  gfc_omp_declare_variant *odv;
  gfc_omp_namelist *range_begin = NULL;
  for (odv = ns->omp_declare_variant; odv; odv = odv->next)
    for (gfc_omp_namelist *n = odv->adjust_args_list; n != NULL; n = n->next)
      {
	if ((n->expr == NULL
	     && (range_begin
		 || n->u.adj_args.range_start
		 || n->u.adj_args.omp_num_args_plus
		 || n->u.adj_args.omp_num_args_minus))
	    || n->u.adj_args.error_p)
	  {
	  }
	else if (range_begin
		 || n->u.adj_args.range_start
		 || n->u.adj_args.omp_num_args_plus
		 || n->u.adj_args.omp_num_args_minus)
	  {
	    if (!n->expr
		|| !gfc_resolve_expr (n->expr)
		|| n->expr->expr_type != EXPR_CONSTANT
		|| n->expr->ts.type != BT_INTEGER
		|| n->expr->rank != 0
		|| mpz_sgn (n->expr->value.integer) < 0
		|| ((n->u.adj_args.omp_num_args_plus
		     || n->u.adj_args.omp_num_args_minus)
		    && mpz_sgn (n->expr->value.integer) == 0))
	      {
		if (n->u.adj_args.omp_num_args_plus
		    || n->u.adj_args.omp_num_args_minus)
		  gfc_error ("Expected constant non-negative scalar integer "
			     "offset expression at %L", &n->where);
		else
		  gfc_error ("For range-based %<adjust_args%>, a constant "
			     "positive scalar integer expression is required "
			     "at %L", &n->where);
	      }
	  }
	else if (n->expr
		 && n->expr->expr_type == EXPR_CONSTANT
		 && n->expr->ts.type == BT_INTEGER
		 && mpz_sgn (n->expr->value.integer) > 0)
	  {
	  }
	else if (!n->expr
		 || !gfc_resolve_expr (n->expr)
		 || n->expr->expr_type != EXPR_VARIABLE)
	  gfc_error ("Expected dummy parameter name or a positive integer "
		     "at %L", &n->where);
	else if (n->expr->expr_type == EXPR_VARIABLE)
	  n->sym = n->expr->symtree->n.sym;

	range_begin = n->u.adj_args.range_start ? n : NULL;
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
