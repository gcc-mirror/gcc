/* Declaration statement matcher
   Copyright (C) 2002-2017 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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
#include "options.h"
#include "tree.h"
#include "gfortran.h"
#include "stringpool.h"
#include "match.h"
#include "parse.h"
#include "constructor.h"

/* Macros to access allocate memory for gfc_data_variable,
   gfc_data_value and gfc_data.  */
#define gfc_get_data_variable() XCNEW (gfc_data_variable)
#define gfc_get_data_value() XCNEW (gfc_data_value)
#define gfc_get_data() XCNEW (gfc_data)


static bool set_binding_label (const char **, const char *, int);


/* This flag is set if an old-style length selector is matched
   during a type-declaration statement.  */

static int old_char_selector;

/* When variables acquire types and attributes from a declaration
   statement, they get them from the following static variables.  The
   first part of a declaration sets these variables and the second
   part copies these into symbol structures.  */

static gfc_typespec current_ts;

static symbol_attribute current_attr;
static gfc_array_spec *current_as;
static int colon_seen;
static int attr_seen;

/* The current binding label (if any).  */
static const char* curr_binding_label;
/* Need to know how many identifiers are on the current data declaration
   line in case we're given the BIND(C) attribute with a NAME= specifier.  */
static int num_idents_on_line;
/* Need to know if a NAME= specifier was found during gfc_match_bind_c so we
   can supply a name if the curr_binding_label is nil and NAME= was not.  */
static int has_name_equals = 0;

/* Initializer of the previous enumerator.  */

static gfc_expr *last_initializer;

/* History of all the enumerators is maintained, so that
   kind values of all the enumerators could be updated depending
   upon the maximum initialized value.  */

typedef struct enumerator_history
{
  gfc_symbol *sym;
  gfc_expr *initializer;
  struct enumerator_history *next;
}
enumerator_history;

/* Header of enum history chain.  */

static enumerator_history *enum_history = NULL;

/* Pointer of enum history node containing largest initializer.  */

static enumerator_history *max_enum = NULL;

/* gfc_new_block points to the symbol of a newly matched block.  */

gfc_symbol *gfc_new_block;

bool gfc_matching_function;

/* If a kind expression of a component of a parameterized derived type is
   parameterized, temporarily store the expression here.  */
static gfc_expr *saved_kind_expr = NULL;

/* Used to store the parameter list arising in a PDT declaration and
   in the typespec of a PDT variable or component.  */
static gfc_actual_arglist *decl_type_param_list;
static gfc_actual_arglist *type_param_spec_list;


/********************* DATA statement subroutines *********************/

static bool in_match_data = false;

bool
gfc_in_match_data (void)
{
  return in_match_data;
}

static void
set_in_match_data (bool set_value)
{
  in_match_data = set_value;
}

/* Free a gfc_data_variable structure and everything beneath it.  */

static void
free_variable (gfc_data_variable *p)
{
  gfc_data_variable *q;

  for (; p; p = q)
    {
      q = p->next;
      gfc_free_expr (p->expr);
      gfc_free_iterator (&p->iter, 0);
      free_variable (p->list);
      free (p);
    }
}


/* Free a gfc_data_value structure and everything beneath it.  */

static void
free_value (gfc_data_value *p)
{
  gfc_data_value *q;

  for (; p; p = q)
    {
      q = p->next;
      mpz_clear (p->repeat);
      gfc_free_expr (p->expr);
      free (p);
    }
}


/* Free a list of gfc_data structures.  */

void
gfc_free_data (gfc_data *p)
{
  gfc_data *q;

  for (; p; p = q)
    {
      q = p->next;
      free_variable (p->var);
      free_value (p->value);
      free (p);
    }
}


/* Free all data in a namespace.  */

static void
gfc_free_data_all (gfc_namespace *ns)
{
  gfc_data *d;

  for (;ns->data;)
    {
      d = ns->data->next;
      free (ns->data);
      ns->data = d;
    }
}

/* Reject data parsed since the last restore point was marked.  */

void
gfc_reject_data (gfc_namespace *ns)
{
  gfc_data *d;

  while (ns->data && ns->data != ns->old_data)
    {
      d = ns->data->next;
      free (ns->data);
      ns->data = d;
    }
}

static match var_element (gfc_data_variable *);

/* Match a list of variables terminated by an iterator and a right
   parenthesis.  */

static match
var_list (gfc_data_variable *parent)
{
  gfc_data_variable *tail, var;
  match m;

  m = var_element (&var);
  if (m == MATCH_ERROR)
    return MATCH_ERROR;
  if (m == MATCH_NO)
    goto syntax;

  tail = gfc_get_data_variable ();
  *tail = var;

  parent->list = tail;

  for (;;)
    {
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;

      m = gfc_match_iterator (&parent->iter, 1);
      if (m == MATCH_YES)
	break;
      if (m == MATCH_ERROR)
	return MATCH_ERROR;

      m = var_element (&var);
      if (m == MATCH_ERROR)
	return MATCH_ERROR;
      if (m == MATCH_NO)
	goto syntax;

      tail->next = gfc_get_data_variable ();
      tail = tail->next;

      *tail = var;
    }

  if (gfc_match_char (')') != MATCH_YES)
    goto syntax;
  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_DATA);
  return MATCH_ERROR;
}


/* Match a single element in a data variable list, which can be a
   variable-iterator list.  */

static match
var_element (gfc_data_variable *new_var)
{
  match m;
  gfc_symbol *sym;

  memset (new_var, 0, sizeof (gfc_data_variable));

  if (gfc_match_char ('(') == MATCH_YES)
    return var_list (new_var);

  m = gfc_match_variable (&new_var->expr, 0);
  if (m != MATCH_YES)
    return m;

  sym = new_var->expr->symtree->n.sym;

  /* Symbol should already have an associated type.  */
  if (!gfc_check_symbol_typed (sym, gfc_current_ns, false, gfc_current_locus))
    return MATCH_ERROR;

  if (!sym->attr.function && gfc_current_ns->parent
      && gfc_current_ns->parent == sym->ns)
    {
      gfc_error ("Host associated variable %qs may not be in the DATA "
		 "statement at %C", sym->name);
      return MATCH_ERROR;
    }

  if (gfc_current_state () != COMP_BLOCK_DATA
      && sym->attr.in_common
      && !gfc_notify_std (GFC_STD_GNU, "initialization of "
			  "common block variable %qs in DATA statement at %C",
			  sym->name))
    return MATCH_ERROR;

  if (!gfc_add_data (&sym->attr, sym->name, &new_var->expr->where))
    return MATCH_ERROR;

  return MATCH_YES;
}


/* Match the top-level list of data variables.  */

static match
top_var_list (gfc_data *d)
{
  gfc_data_variable var, *tail, *new_var;
  match m;

  tail = NULL;

  for (;;)
    {
      m = var_element (&var);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	return MATCH_ERROR;

      new_var = gfc_get_data_variable ();
      *new_var = var;

      if (tail == NULL)
	d->var = new_var;
      else
	tail->next = new_var;

      tail = new_var;

      if (gfc_match_char ('/') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_DATA);
  gfc_free_data_all (gfc_current_ns);
  return MATCH_ERROR;
}


static match
match_data_constant (gfc_expr **result)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym, *dt_sym = NULL;
  gfc_expr *expr;
  match m;
  locus old_loc;

  m = gfc_match_literal_constant (&expr, 1);
  if (m == MATCH_YES)
    {
      *result = expr;
      return MATCH_YES;
    }

  if (m == MATCH_ERROR)
    return MATCH_ERROR;

  m = gfc_match_null (result);
  if (m != MATCH_NO)
    return m;

  old_loc = gfc_current_locus;

  /* Should this be a structure component, try to match it
     before matching a name.  */
  m = gfc_match_rvalue (result);
  if (m == MATCH_ERROR)
    return m;

  if (m == MATCH_YES && (*result)->expr_type == EXPR_STRUCTURE)
    {
      if (!gfc_simplify_expr (*result, 0))
	m = MATCH_ERROR;
      return m;
    }
  else if (m == MATCH_YES)
    gfc_free_expr (*result);

  gfc_current_locus = old_loc;

  m = gfc_match_name (name);
  if (m != MATCH_YES)
    return m;

  if (gfc_find_symbol (name, NULL, 1, &sym))
    return MATCH_ERROR;

  if (sym && sym->attr.generic)
    dt_sym = gfc_find_dt_in_generic (sym);

  if (sym == NULL
      || (sym->attr.flavor != FL_PARAMETER
	  && (!dt_sym || !gfc_fl_struct (dt_sym->attr.flavor))))
    {
      gfc_error ("Symbol %qs must be a PARAMETER in DATA statement at %C",
		 name);
      *result = NULL;
      return MATCH_ERROR;
    }
  else if (dt_sym && gfc_fl_struct (dt_sym->attr.flavor))
    return gfc_match_structure_constructor (dt_sym, result);

  /* Check to see if the value is an initialization array expression.  */
  if (sym->value->expr_type == EXPR_ARRAY)
    {
      gfc_current_locus = old_loc;

      m = gfc_match_init_expr (result);
      if (m == MATCH_ERROR)
	return m;

      if (m == MATCH_YES)
	{
	  if (!gfc_simplify_expr (*result, 0))
	    m = MATCH_ERROR;

	  if ((*result)->expr_type == EXPR_CONSTANT)
	    return m;
          else
	    {
	      gfc_error ("Invalid initializer %s in Data statement at %C", name);
	      return MATCH_ERROR;
	    }
	}
    }

  *result = gfc_copy_expr (sym->value);
  return MATCH_YES;
}


/* Match a list of values in a DATA statement.  The leading '/' has
   already been seen at this point.  */

static match
top_val_list (gfc_data *data)
{
  gfc_data_value *new_val, *tail;
  gfc_expr *expr;
  match m;

  tail = NULL;

  for (;;)
    {
      m = match_data_constant (&expr);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	return MATCH_ERROR;

      new_val = gfc_get_data_value ();
      mpz_init (new_val->repeat);

      if (tail == NULL)
	data->value = new_val;
      else
	tail->next = new_val;

      tail = new_val;

      if (expr->ts.type != BT_INTEGER || gfc_match_char ('*') != MATCH_YES)
	{
	  tail->expr = expr;
	  mpz_set_ui (tail->repeat, 1);
	}
      else
	{
	  mpz_set (tail->repeat, expr->value.integer);
	  gfc_free_expr (expr);

	  m = match_data_constant (&tail->expr);
	  if (m == MATCH_NO)
	    goto syntax;
	  if (m == MATCH_ERROR)
	    return MATCH_ERROR;
	}

      if (gfc_match_char ('/') == MATCH_YES)
	break;
      if (gfc_match_char (',') == MATCH_NO)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_DATA);
  gfc_free_data_all (gfc_current_ns);
  return MATCH_ERROR;
}


/* Matches an old style initialization.  */

static match
match_old_style_init (const char *name)
{
  match m;
  gfc_symtree *st;
  gfc_symbol *sym;
  gfc_data *newdata;

  /* Set up data structure to hold initializers.  */
  gfc_find_sym_tree (name, NULL, 0, &st);
  sym = st->n.sym;

  newdata = gfc_get_data ();
  newdata->var = gfc_get_data_variable ();
  newdata->var->expr = gfc_get_variable_expr (st);
  newdata->where = gfc_current_locus;

  /* Match initial value list. This also eats the terminal '/'.  */
  m = top_val_list (newdata);
  if (m != MATCH_YES)
    {
      free (newdata);
      return m;
    }

  if (gfc_pure (NULL))
    {
      gfc_error ("Initialization at %C is not allowed in a PURE procedure");
      free (newdata);
      return MATCH_ERROR;
    }
  gfc_unset_implicit_pure (gfc_current_ns->proc_name);

  /* Mark the variable as having appeared in a data statement.  */
  if (!gfc_add_data (&sym->attr, sym->name, &sym->declared_at))
    {
      free (newdata);
      return MATCH_ERROR;
    }

  /* Chain in namespace list of DATA initializers.  */
  newdata->next = gfc_current_ns->data;
  gfc_current_ns->data = newdata;

  return m;
}


/* Match the stuff following a DATA statement. If ERROR_FLAG is set,
   we are matching a DATA statement and are therefore issuing an error
   if we encounter something unexpected, if not, we're trying to match
   an old-style initialization expression of the form INTEGER I /2/.  */

match
gfc_match_data (void)
{
  gfc_data *new_data;
  match m;

  /* Before parsing the rest of a DATA statement, check F2008:c1206.  */
  if ((gfc_current_state () == COMP_FUNCTION
       || gfc_current_state () == COMP_SUBROUTINE)
      && gfc_state_stack->previous->state == COMP_INTERFACE)
    {
      gfc_error ("DATA statement at %C cannot appear within an INTERFACE");
      return MATCH_ERROR;
    }

  set_in_match_data (true);

  for (;;)
    {
      new_data = gfc_get_data ();
      new_data->where = gfc_current_locus;

      m = top_var_list (new_data);
      if (m != MATCH_YES)
	goto cleanup;

      m = top_val_list (new_data);
      if (m != MATCH_YES)
	goto cleanup;

      new_data->next = gfc_current_ns->data;
      gfc_current_ns->data = new_data;

      if (gfc_match_eos () == MATCH_YES)
	break;

      gfc_match_char (',');	/* Optional comma */
    }

  set_in_match_data (false);

  if (gfc_pure (NULL))
    {
      gfc_error ("DATA statement at %C is not allowed in a PURE procedure");
      return MATCH_ERROR;
    }
  gfc_unset_implicit_pure (gfc_current_ns->proc_name);

  return MATCH_YES;

cleanup:
  set_in_match_data (false);
  gfc_free_data (new_data);
  return MATCH_ERROR;
}


/************************ Declaration statements *********************/


/* Like gfc_match_init_expr, but matches a 'clist' (old-style initialization
   list). The difference here is the expression is a list of constants
   and is surrounded by '/'.
   The typespec ts must match the typespec of the variable which the
   clist is initializing.
   The arrayspec tells whether this should match a list of constants
   corresponding to array elements or a scalar (as == NULL).  */

static match
match_clist_expr (gfc_expr **result, gfc_typespec *ts, gfc_array_spec *as)
{
  gfc_constructor_base array_head = NULL;
  gfc_expr *expr = NULL;
  match m;
  locus where;
  mpz_t repeat, size;
  bool scalar;
  int cmp;

  gcc_assert (ts);

  mpz_init_set_ui (repeat, 0);
  mpz_init (size);
  scalar = !as || !as->rank;

  /* We have already matched '/' - now look for a constant list, as with
     top_val_list from decl.c, but append the result to an array.  */
  if (gfc_match ("/") == MATCH_YES)
    {
      gfc_error ("Empty old style initializer list at %C");
      goto cleanup;
    }

  where = gfc_current_locus;
  for (;;)
    {
      m = match_data_constant (&expr);
      if (m != MATCH_YES)
        expr = NULL; /* match_data_constant may set expr to garbage */
      if (m == MATCH_NO)
        goto syntax;
      if (m == MATCH_ERROR)
        goto cleanup;

      /* Found r in repeat spec r*c; look for the constant to repeat.  */
      if ( gfc_match_char ('*') == MATCH_YES)
        {
          if (scalar)
            {
              gfc_error ("Repeat spec invalid in scalar initializer at %C");
              goto cleanup;
            }
          if (expr->ts.type != BT_INTEGER)
            {
              gfc_error ("Repeat spec must be an integer at %C");
              goto cleanup;
            }
          mpz_set (repeat, expr->value.integer);
          gfc_free_expr (expr);
          expr = NULL;

          m = match_data_constant (&expr);
          if (m == MATCH_NO)
            gfc_error ("Expected data constant after repeat spec at %C");
          if (m != MATCH_YES)
            goto cleanup;
        }
      /* No repeat spec, we matched the data constant itself. */
      else
        mpz_set_ui (repeat, 1);

      if (!scalar)
        {
          /* Add the constant initializer as many times as repeated. */
          for (; mpz_cmp_ui (repeat, 0) > 0; mpz_sub_ui (repeat, repeat, 1))
            {
              /* Make sure types of elements match */
              if(ts && !gfc_compare_types (&expr->ts, ts)
                    && !gfc_convert_type (expr, ts, 1))
                goto cleanup;

              gfc_constructor_append_expr (&array_head,
                  gfc_copy_expr (expr), &gfc_current_locus);
            }

          gfc_free_expr (expr);
          expr = NULL;
        }

      /* For scalar initializers quit after one element.  */
      else
        {
          if(gfc_match_char ('/') != MATCH_YES)
            {
              gfc_error ("End of scalar initializer expected at %C");
              goto cleanup;
            }
          break;
        }

      if (gfc_match_char ('/') == MATCH_YES)
        break;
      if (gfc_match_char (',') == MATCH_NO)
        goto syntax;
    }

  /* Set up expr as an array constructor. */
  if (!scalar)
    {
      expr = gfc_get_array_expr (ts->type, ts->kind, &where);
      expr->ts = *ts;
      expr->value.constructor = array_head;

      expr->rank = as->rank;
      expr->shape = gfc_get_shape (expr->rank);

      /* Validate sizes. */
      gcc_assert (gfc_array_size (expr, &size));
      gcc_assert (spec_size (as, &repeat));
      cmp = mpz_cmp (size, repeat);
      if (cmp < 0)
        gfc_error ("Not enough elements in array initializer at %C");
      else if (cmp > 0)
        gfc_error ("Too many elements in array initializer at %C");
      if (cmp)
        goto cleanup;
    }

  /* Make sure scalar types match. */
  else if (!gfc_compare_types (&expr->ts, ts)
           && !gfc_convert_type (expr, ts, 1))
    goto cleanup;

  if (expr->ts.u.cl)
    expr->ts.u.cl->length_from_typespec = 1;

  *result = expr;
  mpz_clear (size);
  mpz_clear (repeat);
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in old style initializer list at %C");

cleanup:
  if (expr)
    expr->value.constructor = NULL;
  gfc_free_expr (expr);
  gfc_constructor_free (array_head);
  mpz_clear (size);
  mpz_clear (repeat);
  return MATCH_ERROR;
}


/* Auxiliary function to merge DIMENSION and CODIMENSION array specs.  */

static bool
merge_array_spec (gfc_array_spec *from, gfc_array_spec *to, bool copy)
{
  int i;

  if ((from->type == AS_ASSUMED_RANK && to->corank)
      || (to->type == AS_ASSUMED_RANK && from->corank))
    {
      gfc_error ("The assumed-rank array at %C shall not have a codimension");
      return false;
    }

  if (to->rank == 0 && from->rank > 0)
    {
      to->rank = from->rank;
      to->type = from->type;
      to->cray_pointee = from->cray_pointee;
      to->cp_was_assumed = from->cp_was_assumed;

      for (i = 0; i < to->corank; i++)
	{
	  to->lower[from->rank + i] = to->lower[i];
	  to->upper[from->rank + i] = to->upper[i];
	}
      for (i = 0; i < from->rank; i++)
	{
	  if (copy)
	    {
	      to->lower[i] = gfc_copy_expr (from->lower[i]);
	      to->upper[i] = gfc_copy_expr (from->upper[i]);
	    }
	  else
	    {
	      to->lower[i] = from->lower[i];
	      to->upper[i] = from->upper[i];
	    }
	}
    }
  else if (to->corank == 0 && from->corank > 0)
    {
      to->corank = from->corank;
      to->cotype = from->cotype;

      for (i = 0; i < from->corank; i++)
	{
	  if (copy)
	    {
	      to->lower[to->rank + i] = gfc_copy_expr (from->lower[i]);
	      to->upper[to->rank + i] = gfc_copy_expr (from->upper[i]);
	    }
	  else
	    {
	      to->lower[to->rank + i] = from->lower[i];
	      to->upper[to->rank + i] = from->upper[i];
	    }
	}
    }

  return true;
}


/* Match an intent specification.  Since this can only happen after an
   INTENT word, a legal intent-spec must follow.  */

static sym_intent
match_intent_spec (void)
{

  if (gfc_match (" ( in out )") == MATCH_YES)
    return INTENT_INOUT;
  if (gfc_match (" ( in )") == MATCH_YES)
    return INTENT_IN;
  if (gfc_match (" ( out )") == MATCH_YES)
    return INTENT_OUT;

  gfc_error ("Bad INTENT specification at %C");
  return INTENT_UNKNOWN;
}


/* Matches a character length specification, which is either a
   specification expression, '*', or ':'.  */

static match
char_len_param_value (gfc_expr **expr, bool *deferred)
{
  match m;

  *expr = NULL;
  *deferred = false;

  if (gfc_match_char ('*') == MATCH_YES)
    return MATCH_YES;

  if (gfc_match_char (':') == MATCH_YES)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "deferred type parameter at %C"))
	return MATCH_ERROR;

      *deferred = true;

      return MATCH_YES;
    }

  m = gfc_match_expr (expr);

  if (m == MATCH_NO || m == MATCH_ERROR)
    return m;

  if (!gfc_expr_check_typed (*expr, gfc_current_ns, false))
    return MATCH_ERROR;

  if ((*expr)->expr_type == EXPR_FUNCTION)
    {
      if ((*expr)->ts.type == BT_INTEGER
	  || ((*expr)->ts.type == BT_UNKNOWN
	      && strcmp((*expr)->symtree->name, "null") != 0))
	return MATCH_YES;

      goto syntax;
    }
  else if ((*expr)->expr_type == EXPR_CONSTANT)
    {
      /* F2008, 4.4.3.1:  The length is a type parameter; its kind is
	 processor dependent and its value is greater than or equal to zero.
	 F2008, 4.4.3.2:  If the character length parameter value evaluates
	 to a negative value, the length of character entities declared
	 is zero.  */

      if ((*expr)->ts.type == BT_INTEGER)
	{
	  if (mpz_cmp_si ((*expr)->value.integer, 0) < 0)
	    mpz_set_si ((*expr)->value.integer, 0);
	}
      else
	goto syntax;
    }
  else if ((*expr)->expr_type == EXPR_ARRAY)
    goto syntax;
  else if ((*expr)->expr_type == EXPR_VARIABLE)
    {
      bool t;
      gfc_expr *e;

      e = gfc_copy_expr (*expr);

      /* This catches the invalid code "[character(m(2:3)) :: 'x', 'y']",
	 which causes an ICE if gfc_reduce_init_expr() is called.  */
      if (e->ref && e->ref->type == REF_ARRAY
	  && e->ref->u.ar.type == AR_UNKNOWN
	  && e->ref->u.ar.dimen_type[0] == DIMEN_RANGE)
	goto syntax;

      t = gfc_reduce_init_expr (e);

      if (!t && e->ts.type == BT_UNKNOWN
	  && e->symtree->n.sym->attr.untyped == 1
	  && (flag_implicit_none
	      || e->symtree->n.sym->ns->seen_implicit_none == 1
	      || e->symtree->n.sym->ns->parent->seen_implicit_none == 1))
	{
	  gfc_free_expr (e);
	  goto syntax;
	}

      if ((e->ref && e->ref->type == REF_ARRAY
	   && e->ref->u.ar.type != AR_ELEMENT)
	  || (!e->ref && e->expr_type == EXPR_ARRAY))
	{
	  gfc_free_expr (e);
	  goto syntax;
	}

      gfc_free_expr (e);
    }

  return m;

syntax:
  gfc_error ("Scalar INTEGER expression expected at %L", &(*expr)->where);
  return MATCH_ERROR;
}


/* A character length is a '*' followed by a literal integer or a
   char_len_param_value in parenthesis.  */

static match
match_char_length (gfc_expr **expr, bool *deferred, bool obsolescent_check)
{
  int length;
  match m;

  *deferred = false;
  m = gfc_match_char ('*');
  if (m != MATCH_YES)
    return m;

  m = gfc_match_small_literal_int (&length, NULL);
  if (m == MATCH_ERROR)
    return m;

  if (m == MATCH_YES)
    {
      if (obsolescent_check
	  && !gfc_notify_std (GFC_STD_F95_OBS, "Old-style character length at %C"))
	return MATCH_ERROR;
      *expr = gfc_get_int_expr (gfc_default_integer_kind, NULL, length);
      return m;
    }

  if (gfc_match_char ('(') == MATCH_NO)
    goto syntax;

  m = char_len_param_value (expr, deferred);
  if (m != MATCH_YES && gfc_matching_function)
    {
      gfc_undo_symbols ();
      m = MATCH_YES;
    }

  if (m == MATCH_ERROR)
    return m;
  if (m == MATCH_NO)
    goto syntax;

  if (gfc_match_char (')') == MATCH_NO)
    {
      gfc_free_expr (*expr);
      *expr = NULL;
      goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in character length specification at %C");
  return MATCH_ERROR;
}


/* Special subroutine for finding a symbol.  Check if the name is found
   in the current name space.  If not, and we're compiling a function or
   subroutine and the parent compilation unit is an interface, then check
   to see if the name we've been given is the name of the interface
   (located in another namespace).  */

static int
find_special (const char *name, gfc_symbol **result, bool allow_subroutine)
{
  gfc_state_data *s;
  gfc_symtree *st;
  int i;

  i = gfc_get_sym_tree (name, NULL, &st, allow_subroutine);
  if (i == 0)
    {
      *result = st ? st->n.sym : NULL;
      goto end;
    }

  if (gfc_current_state () != COMP_SUBROUTINE
      && gfc_current_state () != COMP_FUNCTION)
    goto end;

  s = gfc_state_stack->previous;
  if (s == NULL)
    goto end;

  if (s->state != COMP_INTERFACE)
    goto end;
  if (s->sym == NULL)
    goto end;		  /* Nameless interface.  */

  if (strcmp (name, s->sym->name) == 0)
    {
      *result = s->sym;
      return 0;
    }

end:
  return i;
}


/* Special subroutine for getting a symbol node associated with a
   procedure name, used in SUBROUTINE and FUNCTION statements.  The
   symbol is created in the parent using with symtree node in the
   child unit pointing to the symbol.  If the current namespace has no
   parent, then the symbol is just created in the current unit.  */

static int
get_proc_name (const char *name, gfc_symbol **result, bool module_fcn_entry)
{
  gfc_symtree *st;
  gfc_symbol *sym;
  int rc = 0;

  /* Module functions have to be left in their own namespace because
     they have potentially (almost certainly!) already been referenced.
     In this sense, they are rather like external functions.  This is
     fixed up in resolve.c(resolve_entries), where the symbol name-
     space is set to point to the master function, so that the fake
     result mechanism can work.  */
  if (module_fcn_entry)
    {
      /* Present if entry is declared to be a module procedure.  */
      rc = gfc_find_symbol (name, gfc_current_ns->parent, 0, result);

      if (*result == NULL)
	rc = gfc_get_symbol (name, NULL, result);
      else if (!gfc_get_symbol (name, NULL, &sym) && sym
		 && (*result)->ts.type == BT_UNKNOWN
		 && sym->attr.flavor == FL_UNKNOWN)
	/* Pick up the typespec for the entry, if declared in the function
	   body.  Note that this symbol is FL_UNKNOWN because it will
	   only have appeared in a type declaration.  The local symtree
	   is set to point to the module symbol and a unique symtree
	   to the local version.  This latter ensures a correct clearing
	   of the symbols.  */
	{
	  /* If the ENTRY proceeds its specification, we need to ensure
	     that this does not raise a "has no IMPLICIT type" error.  */
	  if (sym->ts.type == BT_UNKNOWN)
	    sym->attr.untyped = 1;

	  (*result)->ts = sym->ts;

	  /* Put the symbol in the procedure namespace so that, should
	     the ENTRY precede its specification, the specification
	     can be applied.  */
	  (*result)->ns = gfc_current_ns;

	  gfc_find_sym_tree (name, gfc_current_ns, 0, &st);
	  st->n.sym = *result;
	  st = gfc_get_unique_symtree (gfc_current_ns);
	  sym->refs++;
	  st->n.sym = sym;
	}
    }
  else
    rc = gfc_get_symbol (name, gfc_current_ns->parent, result);

  if (rc)
    return rc;

  sym = *result;
  if (sym->attr.proc == PROC_ST_FUNCTION)
    return rc;

  if (sym->attr.module_procedure
      && sym->attr.if_source == IFSRC_IFBODY)
    {
      /* Create a partially populated interface symbol to carry the
	 characteristics of the procedure and the result.  */
      sym->tlink = gfc_new_symbol (name, sym->ns);
      gfc_add_type (sym->tlink, &(sym->ts),
		    &gfc_current_locus);
      gfc_copy_attr (&sym->tlink->attr, &sym->attr, NULL);
      if (sym->attr.dimension)
	sym->tlink->as = gfc_copy_array_spec (sym->as);

      /* Ideally, at this point, a copy would be made of the formal
	 arguments and their namespace. However, this does not appear
	 to be necessary, albeit at the expense of not being able to
	 use gfc_compare_interfaces directly.  */

      if (sym->result && sym->result != sym)
	{
	  sym->tlink->result = sym->result;
	  sym->result = NULL;
	}
      else if (sym->result)
	{
	  sym->tlink->result = sym->tlink;
	}
    }
  else if (sym && !sym->gfc_new
	   && gfc_current_state () != COMP_INTERFACE)
    {
      /* Trap another encompassed procedure with the same name.  All
	 these conditions are necessary to avoid picking up an entry
	 whose name clashes with that of the encompassing procedure;
	 this is handled using gsymbols to register unique, globally
	 accessible names.  */
      if (sym->attr.flavor != 0
	  && sym->attr.proc != 0
	  && (sym->attr.subroutine || sym->attr.function)
	  && sym->attr.if_source != IFSRC_UNKNOWN)
	gfc_error_now ("Procedure %qs at %C is already defined at %L",
		       name, &sym->declared_at);

      /* Trap a procedure with a name the same as interface in the
	 encompassing scope.  */
      if (sym->attr.generic != 0
	  && (sym->attr.subroutine || sym->attr.function)
	  && !sym->attr.mod_proc)
	gfc_error_now ("Name %qs at %C is already defined"
		       " as a generic interface at %L",
		       name, &sym->declared_at);

      /* Trap declarations of attributes in encompassing scope.  The
	 signature for this is that ts.kind is set.  Legitimate
	 references only set ts.type.  */
      if (sym->ts.kind != 0
	  && !sym->attr.implicit_type
	  && sym->attr.proc == 0
	  && gfc_current_ns->parent != NULL
	  && sym->attr.access == 0
	  && !module_fcn_entry)
	gfc_error_now ("Procedure %qs at %C has an explicit interface "
		       "and must not have attributes declared at %L",
		       name, &sym->declared_at);
    }

  if (gfc_current_ns->parent == NULL || *result == NULL)
    return rc;

  /* Module function entries will already have a symtree in
     the current namespace but will need one at module level.  */
  if (module_fcn_entry)
    {
      /* Present if entry is declared to be a module procedure.  */
      rc = gfc_find_sym_tree (name, gfc_current_ns->parent, 0, &st);
      if (st == NULL)
	st = gfc_new_symtree (&gfc_current_ns->parent->sym_root, name);
    }
  else
    st = gfc_new_symtree (&gfc_current_ns->sym_root, name);

  st->n.sym = sym;
  sym->refs++;

  /* See if the procedure should be a module procedure.  */

  if (((sym->ns->proc_name != NULL
		&& sym->ns->proc_name->attr.flavor == FL_MODULE
		&& sym->attr.proc != PROC_MODULE)
	    || (module_fcn_entry && sym->attr.proc != PROC_MODULE))
	&& !gfc_add_procedure (&sym->attr, PROC_MODULE, sym->name, NULL))
    rc = 2;

  return rc;
}


/* Verify that the given symbol representing a parameter is C
   interoperable, by checking to see if it was marked as such after
   its declaration.  If the given symbol is not interoperable, a
   warning is reported, thus removing the need to return the status to
   the calling function.  The standard does not require the user use
   one of the iso_c_binding named constants to declare an
   interoperable parameter, but we can't be sure if the param is C
   interop or not if the user doesn't.  For example, integer(4) may be
   legal Fortran, but doesn't have meaning in C.  It may interop with
   a number of the C types, which causes a problem because the
   compiler can't know which one.  This code is almost certainly not
   portable, and the user will get what they deserve if the C type
   across platforms isn't always interoperable with integer(4).  If
   the user had used something like integer(c_int) or integer(c_long),
   the compiler could have automatically handled the varying sizes
   across platforms.  */

bool
gfc_verify_c_interop_param (gfc_symbol *sym)
{
  int is_c_interop = 0;
  bool retval = true;

  /* We check implicitly typed variables in symbol.c:gfc_set_default_type().
     Don't repeat the checks here.  */
  if (sym->attr.implicit_type)
    return true;

  /* For subroutines or functions that are passed to a BIND(C) procedure,
     they're interoperable if they're BIND(C) and their params are all
     interoperable.  */
  if (sym->attr.flavor == FL_PROCEDURE)
    {
      if (sym->attr.is_bind_c == 0)
        {
          gfc_error_now ("Procedure %qs at %L must have the BIND(C) "
			 "attribute to be C interoperable", sym->name,
			 &(sym->declared_at));
          return false;
        }
      else
        {
          if (sym->attr.is_c_interop == 1)
            /* We've already checked this procedure; don't check it again.  */
            return true;
          else
            return verify_bind_c_sym (sym, &(sym->ts), sym->attr.in_common,
                                      sym->common_block);
        }
    }

  /* See if we've stored a reference to a procedure that owns sym.  */
  if (sym->ns != NULL && sym->ns->proc_name != NULL)
    {
      if (sym->ns->proc_name->attr.is_bind_c == 1)
	{
	  is_c_interop = (gfc_verify_c_interop(&(sym->ts)) ? 1 : 0);

	  if (is_c_interop != 1)
	    {
	      /* Make personalized messages to give better feedback.  */
	      if (sym->ts.type == BT_DERIVED)
		gfc_error ("Variable %qs at %L is a dummy argument to the "
			   "BIND(C) procedure %qs but is not C interoperable "
			   "because derived type %qs is not C interoperable",
			   sym->name, &(sym->declared_at),
			   sym->ns->proc_name->name,
			   sym->ts.u.derived->name);
	      else if (sym->ts.type == BT_CLASS)
		gfc_error ("Variable %qs at %L is a dummy argument to the "
			   "BIND(C) procedure %qs but is not C interoperable "
			   "because it is polymorphic",
			   sym->name, &(sym->declared_at),
			   sym->ns->proc_name->name);
	      else if (warn_c_binding_type)
		gfc_warning (OPT_Wc_binding_type,
			     "Variable %qs at %L is a dummy argument of the "
			     "BIND(C) procedure %qs but may not be C "
			     "interoperable",
			     sym->name, &(sym->declared_at),
			     sym->ns->proc_name->name);
	    }

          /* Character strings are only C interoperable if they have a
             length of 1.  */
          if (sym->ts.type == BT_CHARACTER)
	    {
	      gfc_charlen *cl = sym->ts.u.cl;
	      if (!cl || !cl->length || cl->length->expr_type != EXPR_CONSTANT
                  || mpz_cmp_si (cl->length->value.integer, 1) != 0)
		{
		  gfc_error ("Character argument %qs at %L "
			     "must be length 1 because "
                             "procedure %qs is BIND(C)",
			     sym->name, &sym->declared_at,
                             sym->ns->proc_name->name);
		  retval = false;
		}
	    }

	  /* We have to make sure that any param to a bind(c) routine does
	     not have the allocatable, pointer, or optional attributes,
	     according to J3/04-007, section 5.1.  */
	  if (sym->attr.allocatable == 1
	      && !gfc_notify_std (GFC_STD_F2008_TS, "Variable %qs at %L with "
				  "ALLOCATABLE attribute in procedure %qs "
				  "with BIND(C)", sym->name,
				  &(sym->declared_at),
				  sym->ns->proc_name->name))
	    retval = false;

	  if (sym->attr.pointer == 1
	      && !gfc_notify_std (GFC_STD_F2008_TS, "Variable %qs at %L with "
				  "POINTER attribute in procedure %qs "
				  "with BIND(C)", sym->name,
				  &(sym->declared_at),
				  sym->ns->proc_name->name))
	    retval = false;

	  if ((sym->attr.allocatable || sym->attr.pointer) && !sym->as)
	    {
	      gfc_error ("Scalar variable %qs at %L with POINTER or "
			 "ALLOCATABLE in procedure %qs with BIND(C) is not yet"
			 " supported", sym->name, &(sym->declared_at),
			 sym->ns->proc_name->name);
	      retval = false;
	    }

	  if (sym->attr.optional == 1 && sym->attr.value)
	    {
	      gfc_error ("Variable %qs at %L cannot have both the OPTIONAL "
			 "and the VALUE attribute because procedure %qs "
			 "is BIND(C)", sym->name, &(sym->declared_at),
			 sym->ns->proc_name->name);
	      retval = false;
	    }
	  else if (sym->attr.optional == 1
		   && !gfc_notify_std (GFC_STD_F2008_TS, "Variable %qs "
				       "at %L with OPTIONAL attribute in "
				       "procedure %qs which is BIND(C)",
				       sym->name, &(sym->declared_at),
				       sym->ns->proc_name->name))
	    retval = false;

          /* Make sure that if it has the dimension attribute, that it is
	     either assumed size or explicit shape. Deferred shape is already
	     covered by the pointer/allocatable attribute.  */
	  if (sym->as != NULL && sym->as->type == AS_ASSUMED_SHAPE
	      && !gfc_notify_std (GFC_STD_F2008_TS, "Assumed-shape array %qs "
				  "at %L as dummy argument to the BIND(C) "
				  "procedure %qs at %L", sym->name,
				  &(sym->declared_at),
				  sym->ns->proc_name->name,
				  &(sym->ns->proc_name->declared_at)))
	    retval = false;
	}
    }

  return retval;
}



/* Function called by variable_decl() that adds a name to the symbol table.  */

static bool
build_sym (const char *name, gfc_charlen *cl, bool cl_deferred,
	   gfc_array_spec **as, locus *var_locus)
{
  symbol_attribute attr;
  gfc_symbol *sym;
  int upper;
  gfc_symtree *st;

  /* Symbols in a submodule are host associated from the parent module or
     submodules. Therefore, they can be overridden by declarations in the
     submodule scope. Deal with this by attaching the existing symbol to
     a new symtree and recycling the old symtree with a new symbol...  */
  st = gfc_find_symtree (gfc_current_ns->sym_root, name);
  if (st != NULL && gfc_state_stack->state == COMP_SUBMODULE
      && st->n.sym != NULL
      && st->n.sym->attr.host_assoc && st->n.sym->attr.used_in_submodule)
    {
      gfc_symtree *s = gfc_get_unique_symtree (gfc_current_ns);
      s->n.sym = st->n.sym;
      sym = gfc_new_symbol (name, gfc_current_ns);


      st->n.sym = sym;
      sym->refs++;
      gfc_set_sym_referenced (sym);
    }
  /* ...Otherwise generate a new symtree and new symbol.  */
  else if (gfc_get_symbol (name, NULL, &sym))
    return false;

  /* Check if the name has already been defined as a type.  The
     first letter of the symtree will be in upper case then.  Of
     course, this is only necessary if the upper case letter is
     actually different.  */

  upper = TOUPPER(name[0]);
  if (upper != name[0])
    {
      char u_name[GFC_MAX_SYMBOL_LEN + 1];
      gfc_symtree *st;
      int nlen;

      nlen = strlen(name);
      gcc_assert (nlen <= GFC_MAX_SYMBOL_LEN);
      strncpy (u_name, name, nlen + 1);
      u_name[0] = upper;

      st = gfc_find_symtree (gfc_current_ns->sym_root, u_name);

      /* STRUCTURE types can alias symbol names */
      if (st != 0 && st->n.sym->attr.flavor != FL_STRUCT)
	{
	  gfc_error ("Symbol %qs at %C also declared as a type at %L", name,
		     &st->n.sym->declared_at);
	  return false;
	}
    }

  /* Start updating the symbol table.  Add basic type attribute if present.  */
  if (current_ts.type != BT_UNKNOWN
      && (sym->attr.implicit_type == 0
	  || !gfc_compare_types (&sym->ts, &current_ts))
      && !gfc_add_type (sym, &current_ts, var_locus))
    return false;

  if (sym->ts.type == BT_CHARACTER)
    {
      sym->ts.u.cl = cl;
      sym->ts.deferred = cl_deferred;
    }

  /* Add dimension attribute if present.  */
  if (!gfc_set_array_spec (sym, *as, var_locus))
    return false;
  *as = NULL;

  /* Add attribute to symbol.  The copy is so that we can reset the
     dimension attribute.  */
  attr = current_attr;
  attr.dimension = 0;
  attr.codimension = 0;

  if (!gfc_copy_attr (&sym->attr, &attr, var_locus))
    return false;

  /* Finish any work that may need to be done for the binding label,
     if it's a bind(c).  The bind(c) attr is found before the symbol
     is made, and before the symbol name (for data decls), so the
     current_ts is holding the binding label, or nothing if the
     name= attr wasn't given.  Therefore, test here if we're dealing
     with a bind(c) and make sure the binding label is set correctly.  */
  if (sym->attr.is_bind_c == 1)
    {
      if (!sym->binding_label)
        {
	  /* Set the binding label and verify that if a NAME= was specified
	     then only one identifier was in the entity-decl-list.  */
	  if (!set_binding_label (&sym->binding_label, sym->name,
				  num_idents_on_line))
            return false;
        }
    }

  /* See if we know we're in a common block, and if it's a bind(c)
     common then we need to make sure we're an interoperable type.  */
  if (sym->attr.in_common == 1)
    {
      /* Test the common block object.  */
      if (sym->common_block != NULL && sym->common_block->is_bind_c == 1
          && sym->ts.is_c_interop != 1)
        {
          gfc_error_now ("Variable %qs in common block %qs at %C "
                         "must be declared with a C interoperable "
                         "kind since common block %qs is BIND(C)",
                         sym->name, sym->common_block->name,
                         sym->common_block->name);
          gfc_clear_error ();
        }
    }

  sym->attr.implied_index = 0;

  /* Use the parameter expressions for a parameterized derived type.  */
  if ((sym->ts.type == BT_DERIVED || sym->ts.type == BT_CLASS)
      && sym->ts.u.derived->attr.pdt_type && type_param_spec_list)
    sym->param_list = gfc_copy_actual_arglist (type_param_spec_list);

  if (sym->ts.type == BT_CLASS)
    return gfc_build_class_symbol (&sym->ts, &sym->attr, &sym->as);

  return true;
}


/* Set character constant to the given length. The constant will be padded or
   truncated.  If we're inside an array constructor without a typespec, we
   additionally check that all elements have the same length; check_len -1
   means no checking.  */

void
gfc_set_constant_character_len (int len, gfc_expr *expr, int check_len)
{
  gfc_char_t *s;
  int slen;

  if (expr->ts.type != BT_CHARACTER)
    return;

  if (expr->expr_type != EXPR_CONSTANT)
    {
      gfc_error_now ("CHARACTER length must be a constant at %L", &expr->where);
      return;
    }

  slen = expr->value.character.length;
  if (len != slen)
    {
      s = gfc_get_wide_string (len + 1);
      memcpy (s, expr->value.character.string,
	      MIN (len, slen) * sizeof (gfc_char_t));
      if (len > slen)
	gfc_wide_memset (&s[slen], ' ', len - slen);

      if (warn_character_truncation && slen > len)
	gfc_warning_now (OPT_Wcharacter_truncation,
			 "CHARACTER expression at %L is being truncated "
			 "(%d/%d)", &expr->where, slen, len);

      /* Apply the standard by 'hand' otherwise it gets cleared for
	 initializers.  */
      if (check_len != -1 && slen != check_len
          && !(gfc_option.allow_std & GFC_STD_GNU))
	gfc_error_now ("The CHARACTER elements of the array constructor "
		       "at %L must have the same length (%d/%d)",
			&expr->where, slen, check_len);

      s[len] = '\0';
      free (expr->value.character.string);
      expr->value.character.string = s;
      expr->value.character.length = len;
    }
}


/* Function to create and update the enumerator history
   using the information passed as arguments.
   Pointer "max_enum" is also updated, to point to
   enum history node containing largest initializer.

   SYM points to the symbol node of enumerator.
   INIT points to its enumerator value.  */

static void
create_enum_history (gfc_symbol *sym, gfc_expr *init)
{
  enumerator_history *new_enum_history;
  gcc_assert (sym != NULL && init != NULL);

  new_enum_history = XCNEW (enumerator_history);

  new_enum_history->sym = sym;
  new_enum_history->initializer = init;
  new_enum_history->next = NULL;

  if (enum_history == NULL)
    {
      enum_history = new_enum_history;
      max_enum = enum_history;
    }
  else
    {
      new_enum_history->next = enum_history;
      enum_history = new_enum_history;

      if (mpz_cmp (max_enum->initializer->value.integer,
		   new_enum_history->initializer->value.integer) < 0)
	max_enum = new_enum_history;
    }
}


/* Function to free enum kind history.  */

void
gfc_free_enum_history (void)
{
  enumerator_history *current = enum_history;
  enumerator_history *next;

  while (current != NULL)
    {
      next = current->next;
      free (current);
      current = next;
    }
  max_enum = NULL;
  enum_history = NULL;
}


/* Function called by variable_decl() that adds an initialization
   expression to a symbol.  */

static bool
add_init_expr_to_sym (const char *name, gfc_expr **initp, locus *var_locus)
{
  symbol_attribute attr;
  gfc_symbol *sym;
  gfc_expr *init;

  init = *initp;
  if (find_special (name, &sym, false))
    return false;

  attr = sym->attr;

  /* If this symbol is confirming an implicit parameter type,
     then an initialization expression is not allowed.  */
  if (attr.flavor == FL_PARAMETER
      && sym->value != NULL
      && *initp != NULL)
    {
      gfc_error ("Initializer not allowed for PARAMETER %qs at %C",
		 sym->name);
      return false;
    }

  if (init == NULL)
    {
      /* An initializer is required for PARAMETER declarations.  */
      if (attr.flavor == FL_PARAMETER)
	{
	  gfc_error ("PARAMETER at %L is missing an initializer", var_locus);
	  return false;
	}
    }
  else
    {
      /* If a variable appears in a DATA block, it cannot have an
	 initializer.  */
      if (sym->attr.data)
	{
	  gfc_error ("Variable %qs at %C with an initializer already "
		     "appears in a DATA statement", sym->name);
	  return false;
	}

      /* Check if the assignment can happen. This has to be put off
	 until later for derived type variables and procedure pointers.  */
      if (!gfc_bt_struct (sym->ts.type) && !gfc_bt_struct (init->ts.type)
	  && sym->ts.type != BT_CLASS && init->ts.type != BT_CLASS
	  && !sym->attr.proc_pointer
	  && !gfc_check_assign_symbol (sym, NULL, init))
	return false;

      if (sym->ts.type == BT_CHARACTER && sym->ts.u.cl
	    && init->ts.type == BT_CHARACTER)
	{
	  /* Update symbol character length according initializer.  */
	  if (!gfc_check_assign_symbol (sym, NULL, init))
	    return false;

	  if (sym->ts.u.cl->length == NULL)
	    {
	      int clen;
	      /* If there are multiple CHARACTER variables declared on the
		 same line, we don't want them to share the same length.  */
	      sym->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);

	      if (sym->attr.flavor == FL_PARAMETER)
		{
		  if (init->expr_type == EXPR_CONSTANT)
		    {
		      clen = init->value.character.length;
		      sym->ts.u.cl->length
				= gfc_get_int_expr (gfc_default_integer_kind,
						    NULL, clen);
		    }
		  else if (init->expr_type == EXPR_ARRAY)
		    {
		      if (init->ts.u.cl)
			{
			  const gfc_expr *length = init->ts.u.cl->length;
			  if (length->expr_type != EXPR_CONSTANT)
			    {
			      gfc_error ("Cannot initialize parameter array "
					 "at %L "
					 "with variable length elements",
					 &sym->declared_at);
			      return false;
			    }
			  clen = mpz_get_si (length->value.integer);
			}
		      else if (init->value.constructor)
			{
			  gfc_constructor *c;
	                  c = gfc_constructor_first (init->value.constructor);
	                  clen = c->expr->value.character.length;
			}
		      else
			  gcc_unreachable ();
		      sym->ts.u.cl->length
				= gfc_get_int_expr (gfc_default_integer_kind,
						    NULL, clen);
		    }
		  else if (init->ts.u.cl && init->ts.u.cl->length)
		    sym->ts.u.cl->length =
				gfc_copy_expr (sym->value->ts.u.cl->length);
		}
	    }
	  /* Update initializer character length according symbol.  */
	  else if (sym->ts.u.cl->length->expr_type == EXPR_CONSTANT)
	    {
	      int len;

	      if (!gfc_specification_expr (sym->ts.u.cl->length))
		return false;

	      len = mpz_get_si (sym->ts.u.cl->length->value.integer);

	      if (init->expr_type == EXPR_CONSTANT)
		gfc_set_constant_character_len (len, init, -1);
	      else if (init->expr_type == EXPR_ARRAY)
		{
		  gfc_constructor *c;

		  /* Build a new charlen to prevent simplification from
		     deleting the length before it is resolved.  */
		  init->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);
		  init->ts.u.cl->length = gfc_copy_expr (sym->ts.u.cl->length);

		  for (c = gfc_constructor_first (init->value.constructor);
		       c; c = gfc_constructor_next (c))
		    gfc_set_constant_character_len (len, c->expr, -1);
		}
	    }
	}

      /* If sym is implied-shape, set its upper bounds from init.  */
      if (sym->attr.flavor == FL_PARAMETER && sym->attr.dimension
	  && sym->as->type == AS_IMPLIED_SHAPE)
	{
	  int dim;

	  if (init->rank == 0)
	    {
	      gfc_error ("Can't initialize implied-shape array at %L"
			 " with scalar", &sym->declared_at);
	      return false;
	    }

	  /* Shape should be present, we get an initialization expression.  */
	  gcc_assert (init->shape);

	  for (dim = 0; dim < sym->as->rank; ++dim)
	    {
	      int k;
	      gfc_expr *e, *lower;

	      lower = sym->as->lower[dim];

	      /* If the lower bound is an array element from another
		 parameterized array, then it is marked with EXPR_VARIABLE and
		 is an initialization expression.  Try to reduce it.  */
	      if (lower->expr_type == EXPR_VARIABLE)
		gfc_reduce_init_expr (lower);

	      if (lower->expr_type == EXPR_CONSTANT)
		{
		  /* All dimensions must be without upper bound.  */
		  gcc_assert (!sym->as->upper[dim]);

		  k = lower->ts.kind;
		  e = gfc_get_constant_expr (BT_INTEGER, k, &sym->declared_at);
		  mpz_add (e->value.integer, lower->value.integer,
			   init->shape[dim]);
		  mpz_sub_ui (e->value.integer, e->value.integer, 1);
		  sym->as->upper[dim] = e;
		}
	      else
		{
		  gfc_error ("Non-constant lower bound in implied-shape"
			     " declaration at %L", &lower->where);
		  return false;
		}
	    }

	  sym->as->type = AS_EXPLICIT;
	}

      /* Need to check if the expression we initialized this
	 to was one of the iso_c_binding named constants.  If so,
	 and we're a parameter (constant), let it be iso_c.
	 For example:
	 integer(c_int), parameter :: my_int = c_int
	 integer(my_int) :: my_int_2
	 If we mark my_int as iso_c (since we can see it's value
	 is equal to one of the named constants), then my_int_2
	 will be considered C interoperable.  */
      if (sym->ts.type != BT_CHARACTER && !gfc_bt_struct (sym->ts.type))
	{
	  sym->ts.is_iso_c |= init->ts.is_iso_c;
	  sym->ts.is_c_interop |= init->ts.is_c_interop;
	  /* attr bits needed for module files.  */
	  sym->attr.is_iso_c |= init->ts.is_iso_c;
	  sym->attr.is_c_interop |= init->ts.is_c_interop;
	  if (init->ts.is_iso_c)
	    sym->ts.f90_type = init->ts.f90_type;
	}

      /* Add initializer.  Make sure we keep the ranks sane.  */
      if (sym->attr.dimension && init->rank == 0)
	{
	  mpz_t size;
	  gfc_expr *array;
	  int n;
	  if (sym->attr.flavor == FL_PARAMETER
		&& init->expr_type == EXPR_CONSTANT
		&& spec_size (sym->as, &size)
		&& mpz_cmp_si (size, 0) > 0)
	    {
	      array = gfc_get_array_expr (init->ts.type, init->ts.kind,
					  &init->where);
	      for (n = 0; n < (int)mpz_get_si (size); n++)
		gfc_constructor_append_expr (&array->value.constructor,
					     n == 0
						? init
						: gfc_copy_expr (init),
					     &init->where);

	      array->shape = gfc_get_shape (sym->as->rank);
	      for (n = 0; n < sym->as->rank; n++)
		spec_dimen_size (sym->as, n, &array->shape[n]);

	      init = array;
	      mpz_clear (size);
	    }
	  init->rank = sym->as->rank;
	}

      sym->value = init;
      if (sym->attr.save == SAVE_NONE)
	sym->attr.save = SAVE_IMPLICIT;
      *initp = NULL;
    }

  return true;
}


/* Function called by variable_decl() that adds a name to a structure
   being built.  */

static bool
build_struct (const char *name, gfc_charlen *cl, gfc_expr **init,
	      gfc_array_spec **as)
{
  gfc_state_data *s;
  gfc_component *c;

  /* F03:C438/C439. If the current symbol is of the same derived type that we're
     constructing, it must have the pointer attribute.  */
  if ((current_ts.type == BT_DERIVED || current_ts.type == BT_CLASS)
      && current_ts.u.derived == gfc_current_block ()
      && current_attr.pointer == 0)
    {
      if (current_attr.allocatable
	  && !gfc_notify_std(GFC_STD_F2008, "Component at %C "
			     "must have the POINTER attribute"))
	{
	  return false;
	}
      else if (current_attr.allocatable == 0)
	{
	  gfc_error ("Component at %C must have the POINTER attribute");
	  return false;
	}
    }

  /* F03:C437.  */
  if (current_ts.type == BT_CLASS
      && !(current_attr.pointer || current_attr.allocatable))
    {
      gfc_error ("Component %qs with CLASS at %C must be allocatable "
                 "or pointer", name);
      return false;
    }

  if (gfc_current_block ()->attr.pointer && (*as)->rank != 0)
    {
      if ((*as)->type != AS_DEFERRED && (*as)->type != AS_EXPLICIT)
	{
	  gfc_error ("Array component of structure at %C must have explicit "
		     "or deferred shape");
	  return false;
	}
    }

  /* If we are in a nested union/map definition, gfc_add_component will not
     properly find repeated components because:
       (i) gfc_add_component does a flat search, where components of unions
           and maps are implicity chained so nested components may conflict.
      (ii) Unions and maps are not linked as components of their parent
           structures until after they are parsed.
     For (i) we use gfc_find_component which searches recursively, and for (ii)
     we search each block directly from the parse stack until we find the top
     level structure.  */

  s = gfc_state_stack;
  if (s->state == COMP_UNION || s->state == COMP_MAP)
    {
      while (s->state == COMP_UNION || gfc_comp_struct (s->state))
        {
          c = gfc_find_component (s->sym, name, true, true, NULL);
          if (c != NULL)
            {
              gfc_error_now ("Component %qs at %C already declared at %L",
                             name, &c->loc);
              return false;
            }
          /* Break after we've searched the entire chain.  */
          if (s->state == COMP_DERIVED || s->state == COMP_STRUCTURE)
            break;
          s = s->previous;
        }
    }

  if (!gfc_add_component (gfc_current_block(), name, &c))
    return false;

  c->ts = current_ts;
  if (c->ts.type == BT_CHARACTER)
    c->ts.u.cl = cl;

  if (c->ts.type != BT_CLASS && c->ts.type != BT_DERIVED
      && c->ts.kind == 0 && saved_kind_expr != NULL)
    c->kind_expr = gfc_copy_expr (saved_kind_expr);

  c->attr = current_attr;

  c->initializer = *init;
  *init = NULL;

  c->as = *as;
  if (c->as != NULL)
    {
      if (c->as->corank)
	c->attr.codimension = 1;
      if (c->as->rank)
	c->attr.dimension = 1;
    }
  *as = NULL;

  gfc_apply_init (&c->ts, &c->attr, c->initializer);

  /* Check array components.  */
  if (!c->attr.dimension)
    goto scalar;

  if (c->attr.pointer)
    {
      if (c->as->type != AS_DEFERRED)
	{
	  gfc_error ("Pointer array component of structure at %C must have a "
		     "deferred shape");
	  return false;
	}
    }
  else if (c->attr.allocatable)
    {
      if (c->as->type != AS_DEFERRED)
	{
	  gfc_error ("Allocatable component of structure at %C must have a "
		     "deferred shape");
	  return false;
	}
    }
  else
    {
      if (c->as->type != AS_EXPLICIT)
	{
	  gfc_error ("Array component of structure at %C must have an "
		     "explicit shape");
	  return false;
	}
    }

scalar:
  if (c->ts.type == BT_CLASS)
    return gfc_build_class_symbol (&c->ts, &c->attr, &c->as);

  if (c->attr.pdt_kind || c->attr.pdt_len)
    {
      gfc_symbol *sym;
      gfc_find_symbol (c->name, gfc_current_block ()->f2k_derived,
		       0, &sym);
      if (sym == NULL)
	{
	  gfc_error ("Type parameter %qs at %C has no corresponding entry "
		     "in the type parameter name list at %L",
		     c->name, &gfc_current_block ()->declared_at);
	  return false;
	}
      sym->ts = c->ts;
      sym->attr.pdt_kind = c->attr.pdt_kind;
      sym->attr.pdt_len = c->attr.pdt_len;
      if (c->initializer)
	sym->value = gfc_copy_expr (c->initializer);
      sym->attr.flavor = FL_VARIABLE;
    }

  if ((c->ts.type == BT_DERIVED || c->ts.type == BT_CLASS)
      && c->ts.u.derived && c->ts.u.derived->attr.pdt_template
      && decl_type_param_list)
    c->param_list = gfc_copy_actual_arglist (decl_type_param_list);

  return true;
}


/* Match a 'NULL()', and possibly take care of some side effects.  */

match
gfc_match_null (gfc_expr **result)
{
  gfc_symbol *sym;
  match m, m2 = MATCH_NO;

  if ((m = gfc_match (" null ( )")) == MATCH_ERROR)
    return MATCH_ERROR;

  if (m == MATCH_NO)
    {
      locus old_loc;
      char name[GFC_MAX_SYMBOL_LEN + 1];

      if ((m2 = gfc_match (" null (")) != MATCH_YES)
	return m2;

      old_loc = gfc_current_locus;
      if ((m2 = gfc_match (" %n ) ", name)) == MATCH_ERROR)
	return MATCH_ERROR;
      if (m2 != MATCH_YES
	  && ((m2 = gfc_match (" mold = %n )", name)) == MATCH_ERROR))
	return MATCH_ERROR;
      if (m2 == MATCH_NO)
	{
	  gfc_current_locus = old_loc;
	  return MATCH_NO;
	}
    }

  /* The NULL symbol now has to be/become an intrinsic function.  */
  if (gfc_get_symbol ("null", NULL, &sym))
    {
      gfc_error ("NULL() initialization at %C is ambiguous");
      return MATCH_ERROR;
    }

  gfc_intrinsic_symbol (sym);

  if (sym->attr.proc != PROC_INTRINSIC
      && !(sym->attr.use_assoc && sym->attr.intrinsic)
      && (!gfc_add_procedure(&sym->attr, PROC_INTRINSIC, sym->name, NULL)
	  || !gfc_add_function (&sym->attr, sym->name, NULL)))
    return MATCH_ERROR;

  *result = gfc_get_null_expr (&gfc_current_locus);

  /* Invalid per F2008, C512.  */
  if (m2 == MATCH_YES)
    {
      gfc_error ("NULL() initialization at %C may not have MOLD");
      return MATCH_ERROR;
    }

  return MATCH_YES;
}


/* Match the initialization expr for a data pointer or procedure pointer.  */

static match
match_pointer_init (gfc_expr **init, int procptr)
{
  match m;

  if (gfc_pure (NULL) && !gfc_comp_struct (gfc_state_stack->state))
    {
      gfc_error ("Initialization of pointer at %C is not allowed in "
		 "a PURE procedure");
      return MATCH_ERROR;
    }
  gfc_unset_implicit_pure (gfc_current_ns->proc_name);

  /* Match NULL() initialization.  */
  m = gfc_match_null (init);
  if (m != MATCH_NO)
    return m;

  /* Match non-NULL initialization.  */
  gfc_matching_ptr_assignment = !procptr;
  gfc_matching_procptr_assignment = procptr;
  m = gfc_match_rvalue (init);
  gfc_matching_ptr_assignment = 0;
  gfc_matching_procptr_assignment = 0;
  if (m == MATCH_ERROR)
    return MATCH_ERROR;
  else if (m == MATCH_NO)
    {
      gfc_error ("Error in pointer initialization at %C");
      return MATCH_ERROR;
    }

  if (!procptr && !gfc_resolve_expr (*init))
    return MATCH_ERROR;

  if (!gfc_notify_std (GFC_STD_F2008, "non-NULL pointer "
		       "initialization at %C"))
    return MATCH_ERROR;

  return MATCH_YES;
}


static bool
check_function_name (char *name)
{
  /* In functions that have a RESULT variable defined, the function name always
     refers to function calls.  Therefore, the name is not allowed to appear in
     specification statements. When checking this, be careful about
     'hidden' procedure pointer results ('ppr@').  */

  if (gfc_current_state () == COMP_FUNCTION)
    {
      gfc_symbol *block = gfc_current_block ();
      if (block && block->result && block->result != block
	  && strcmp (block->result->name, "ppr@") != 0
	  && strcmp (block->name, name) == 0)
	{
	  gfc_error ("Function name %qs not allowed at %C", name);
	  return false;
	}
    }

  return true;
}


/* Match a variable name with an optional initializer.  When this
   subroutine is called, a variable is expected to be parsed next.
   Depending on what is happening at the moment, updates either the
   symbol table or the current interface.  */

static match
variable_decl (int elem)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  static unsigned int fill_id = 0;
  gfc_expr *initializer, *char_len;
  gfc_array_spec *as;
  gfc_array_spec *cp_as; /* Extra copy for Cray Pointees.  */
  gfc_charlen *cl;
  bool cl_deferred;
  locus var_locus;
  match m;
  bool t;
  gfc_symbol *sym;

  initializer = NULL;
  as = NULL;
  cp_as = NULL;

  /* When we get here, we've just matched a list of attributes and
     maybe a type and a double colon.  The next thing we expect to see
     is the name of the symbol.  */

  /* If we are parsing a structure with legacy support, we allow the symbol
     name to be '%FILL' which gives it an anonymous (inaccessible) name.  */
  m = MATCH_NO;
  gfc_gobble_whitespace ();
  if (gfc_peek_ascii_char () == '%')
    {
      gfc_next_ascii_char ();
      m = gfc_match ("fill");
    }

  if (m != MATCH_YES)
    {
      m = gfc_match_name (name);
      if (m != MATCH_YES)
	goto cleanup;
    }

  else
    {
      m = MATCH_ERROR;
      if (gfc_current_state () != COMP_STRUCTURE)
	{
	  if (flag_dec_structure)
	    gfc_error ("%qs not allowed outside STRUCTURE at %C", "%FILL");
	  else
	    gfc_error ("%qs at %C is a DEC extension, enable with "
		       "%<-fdec-structure%>", "%FILL");
	  goto cleanup;
	}

      if (attr_seen)
	{
	  gfc_error ("%qs entity cannot have attributes at %C", "%FILL");
	  goto cleanup;
	}

      /* %FILL components are given invalid fortran names.  */
      snprintf (name, GFC_MAX_SYMBOL_LEN + 1, "%%FILL%u", fill_id++);
      m = MATCH_YES;
    }

  var_locus = gfc_current_locus;

  /* Now we could see the optional array spec. or character length.  */
  m = gfc_match_array_spec (&as, true, true);
  if (m == MATCH_ERROR)
    goto cleanup;

  if (m == MATCH_NO)
    as = gfc_copy_array_spec (current_as);
  else if (current_as
	   && !merge_array_spec (current_as, as, true))
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  if (flag_cray_pointer)
    cp_as = gfc_copy_array_spec (as);

  /* At this point, we know for sure if the symbol is PARAMETER and can thus
     determine (and check) whether it can be implied-shape.  If it
     was parsed as assumed-size, change it because PARAMETERs can not
     be assumed-size.  */
  if (as)
    {
      if (as->type == AS_IMPLIED_SHAPE && current_attr.flavor != FL_PARAMETER)
	{
	  m = MATCH_ERROR;
	  gfc_error ("Non-PARAMETER symbol %qs at %L can't be implied-shape",
		     name, &var_locus);
	  goto cleanup;
	}

      if (as->type == AS_ASSUMED_SIZE && as->rank == 1
	  && current_attr.flavor == FL_PARAMETER)
	as->type = AS_IMPLIED_SHAPE;

      if (as->type == AS_IMPLIED_SHAPE
	  && !gfc_notify_std (GFC_STD_F2008, "Implied-shape array at %L",
			      &var_locus))
	{
	  m = MATCH_ERROR;
	  goto cleanup;
	}
    }

  char_len = NULL;
  cl = NULL;
  cl_deferred = false;

  if (current_ts.type == BT_CHARACTER)
    {
      switch (match_char_length (&char_len, &cl_deferred, false))
	{
	case MATCH_YES:
	  cl = gfc_new_charlen (gfc_current_ns, NULL);

	  cl->length = char_len;
	  break;

	/* Non-constant lengths need to be copied after the first
	   element.  Also copy assumed lengths.  */
	case MATCH_NO:
	  if (elem > 1
	      && (current_ts.u.cl->length == NULL
		  || current_ts.u.cl->length->expr_type != EXPR_CONSTANT))
	    {
	      cl = gfc_new_charlen (gfc_current_ns, NULL);
	      cl->length = gfc_copy_expr (current_ts.u.cl->length);
	    }
	  else
	    cl = current_ts.u.cl;

	  cl_deferred = current_ts.deferred;

	  break;

	case MATCH_ERROR:
	  goto cleanup;
	}
    }

  /* The dummy arguments and result of the abreviated form of MODULE
     PROCEDUREs, used in SUBMODULES should not be redefined.  */
  if (gfc_current_ns->proc_name
      && gfc_current_ns->proc_name->abr_modproc_decl)
    {
      gfc_find_symbol (name, gfc_current_ns, 1, &sym);
      if (sym != NULL && (sym->attr.dummy || sym->attr.result))
	{
	  m = MATCH_ERROR;
	  gfc_error ("%qs at %C is a redefinition of the declaration "
		     "in the corresponding interface for MODULE "
		     "PROCEDURE %qs", sym->name,
		     gfc_current_ns->proc_name->name);
	  goto cleanup;
	}
    }

  /* %FILL components may not have initializers.  */
  if (strncmp (name, "%FILL", 5) == 0 && gfc_match_eos () != MATCH_YES)
    {
      gfc_error ("%qs entity cannot have an initializer at %C", "%FILL");
      m = MATCH_ERROR;
      goto cleanup;
    }

  /*  If this symbol has already shown up in a Cray Pointer declaration,
      and this is not a component declaration,
      then we want to set the type & bail out.  */
  if (flag_cray_pointer && !gfc_comp_struct (gfc_current_state ()))
    {
      gfc_find_symbol (name, gfc_current_ns, 1, &sym);
      if (sym != NULL && sym->attr.cray_pointee)
	{
	  sym->ts.type = current_ts.type;
	  sym->ts.kind = current_ts.kind;
	  sym->ts.u.cl = cl;
	  sym->ts.u.derived = current_ts.u.derived;
	  sym->ts.is_c_interop = current_ts.is_c_interop;
	  sym->ts.is_iso_c = current_ts.is_iso_c;
	  m = MATCH_YES;

	  /* Check to see if we have an array specification.  */
	  if (cp_as != NULL)
	    {
	      if (sym->as != NULL)
		{
		  gfc_error ("Duplicate array spec for Cray pointee at %C");
		  gfc_free_array_spec (cp_as);
		  m = MATCH_ERROR;
		  goto cleanup;
		}
	      else
		{
		  if (!gfc_set_array_spec (sym, cp_as, &var_locus))
		    gfc_internal_error ("Couldn't set pointee array spec.");

		  /* Fix the array spec.  */
		  m = gfc_mod_pointee_as (sym->as);
		  if (m == MATCH_ERROR)
		    goto cleanup;
		}
	    }
	  goto cleanup;
	}
      else
	{
	  gfc_free_array_spec (cp_as);
	}
    }

  /* Procedure pointer as function result.  */
  if (gfc_current_state () == COMP_FUNCTION
      && strcmp ("ppr@", gfc_current_block ()->name) == 0
      && strcmp (name, gfc_current_block ()->ns->proc_name->name) == 0)
    strcpy (name, "ppr@");

  if (gfc_current_state () == COMP_FUNCTION
      && strcmp (name, gfc_current_block ()->name) == 0
      && gfc_current_block ()->result
      && strcmp ("ppr@", gfc_current_block ()->result->name) == 0)
    strcpy (name, "ppr@");

  /* OK, we've successfully matched the declaration.  Now put the
     symbol in the current namespace, because it might be used in the
     optional initialization expression for this symbol, e.g. this is
     perfectly legal:

     integer, parameter :: i = huge(i)

     This is only true for parameters or variables of a basic type.
     For components of derived types, it is not true, so we don't
     create a symbol for those yet.  If we fail to create the symbol,
     bail out.  */
  if (!gfc_comp_struct (gfc_current_state ())
      && !build_sym (name, cl, cl_deferred, &as, &var_locus))
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  if (!check_function_name (name))
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  /* We allow old-style initializations of the form
       integer i /2/, j(4) /3*3, 1/
     (if no colon has been seen). These are different from data
     statements in that initializers are only allowed to apply to the
     variable immediately preceding, i.e.
       integer i, j /1, 2/
     is not allowed. Therefore we have to do some work manually, that
     could otherwise be left to the matchers for DATA statements.  */

  if (!colon_seen && gfc_match (" /") == MATCH_YES)
    {
      if (!gfc_notify_std (GFC_STD_GNU, "Old-style "
			   "initialization at %C"))
	return MATCH_ERROR;

      /* Allow old style initializations for components of STRUCTUREs and MAPs
         but not components of derived types.  */
      else if (gfc_current_state () == COMP_DERIVED)
	{
	  gfc_error ("Invalid old style initialization for derived type "
		     "component at %C");
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      /* For structure components, read the initializer as a special
         expression and let the rest of this function apply the initializer
         as usual.  */
      else if (gfc_comp_struct (gfc_current_state ()))
        {
          m = match_clist_expr (&initializer, &current_ts, as);
          if (m == MATCH_NO)
            gfc_error ("Syntax error in old style initialization of %s at %C",
                       name);
          if (m != MATCH_YES)
            goto cleanup;
        }

      /* Otherwise we treat the old style initialization just like a
         DATA declaration for the current variable.  */
      else
        return match_old_style_init (name);
    }

  /* The double colon must be present in order to have initializers.
     Otherwise the statement is ambiguous with an assignment statement.  */
  if (colon_seen)
    {
      if (gfc_match (" =>") == MATCH_YES)
	{
	  if (!current_attr.pointer)
	    {
	      gfc_error ("Initialization at %C isn't for a pointer variable");
	      m = MATCH_ERROR;
	      goto cleanup;
	    }

	  m = match_pointer_init (&initializer, 0);
	  if (m != MATCH_YES)
	    goto cleanup;
	}
      else if (gfc_match_char ('=') == MATCH_YES)
	{
	  if (current_attr.pointer)
	    {
	      gfc_error ("Pointer initialization at %C requires %<=>%>, "
			 "not %<=%>");
	      m = MATCH_ERROR;
	      goto cleanup;
	    }

	  m = gfc_match_init_expr (&initializer);
	  if (m == MATCH_NO)
	    {
	      gfc_error ("Expected an initialization expression at %C");
	      m = MATCH_ERROR;
	    }

	  if (current_attr.flavor != FL_PARAMETER && gfc_pure (NULL)
	      && !gfc_comp_struct (gfc_state_stack->state))
	    {
	      gfc_error ("Initialization of variable at %C is not allowed in "
			 "a PURE procedure");
	      m = MATCH_ERROR;
	    }

	  if (current_attr.flavor != FL_PARAMETER
	      && !gfc_comp_struct (gfc_state_stack->state))
	    gfc_unset_implicit_pure (gfc_current_ns->proc_name);

	  if (m != MATCH_YES)
	    goto cleanup;
	}
    }

  if (initializer != NULL && current_attr.allocatable
	&& gfc_comp_struct (gfc_current_state ()))
    {
      gfc_error ("Initialization of allocatable component at %C is not "
		 "allowed");
      m = MATCH_ERROR;
      goto cleanup;
    }

  if (gfc_current_state () == COMP_DERIVED
      && gfc_current_block ()->attr.pdt_template)
    {
      gfc_symbol *param;
      gfc_find_symbol (name, gfc_current_block ()->f2k_derived,
		       0, &param);
      if (!param && (current_attr.pdt_kind || current_attr.pdt_len))
	{
	  gfc_error ("The component with KIND or LEN attribute at %C does not "
		     "not appear in the type parameter list at %L",
		     &gfc_current_block ()->declared_at);
	  m = MATCH_ERROR;
	  goto cleanup;
	}
      else if (param && !(current_attr.pdt_kind || current_attr.pdt_len))
	{
	  gfc_error ("The component at %C that appears in the type parameter "
		     "list at %L has neither the KIND nor LEN attribute",
		     &gfc_current_block ()->declared_at);
	  m = MATCH_ERROR;
	  goto cleanup;
	}
      else if (as && (current_attr.pdt_kind || current_attr.pdt_len))
	{
	  gfc_error ("The component at %C which is a type parameter must be "
		     "a scalar");
	  m = MATCH_ERROR;
	  goto cleanup;
	}
      else if (param && initializer)
	param->value = gfc_copy_expr (initializer);
    }

  /* Add the initializer.  Note that it is fine if initializer is
     NULL here, because we sometimes also need to check if a
     declaration *must* have an initialization expression.  */
  if (!gfc_comp_struct (gfc_current_state ()))
    t = add_init_expr_to_sym (name, &initializer, &var_locus);
  else
    {
      if (current_ts.type == BT_DERIVED
	  && !current_attr.pointer && !initializer)
	initializer = gfc_default_initializer (&current_ts);
      t = build_struct (name, cl, &initializer, &as);

      /* If we match a nested structure definition we expect to see the
       * body even if the variable declarations blow up, so we need to keep
       * the structure declaration around.  */
      if (gfc_new_block && gfc_new_block->attr.flavor == FL_STRUCT)
        gfc_commit_symbol (gfc_new_block);
    }

  m = (t) ? MATCH_YES : MATCH_ERROR;

cleanup:
  /* Free stuff up and return.  */
  gfc_free_expr (initializer);
  gfc_free_array_spec (as);

  return m;
}


/* Match an extended-f77 "TYPESPEC*bytesize"-style kind specification.
   This assumes that the byte size is equal to the kind number for
   non-COMPLEX types, and equal to twice the kind number for COMPLEX.  */

match
gfc_match_old_kind_spec (gfc_typespec *ts)
{
  match m;
  int original_kind;

  if (gfc_match_char ('*') != MATCH_YES)
    return MATCH_NO;

  m = gfc_match_small_literal_int (&ts->kind, NULL);
  if (m != MATCH_YES)
    return MATCH_ERROR;

  original_kind = ts->kind;

  /* Massage the kind numbers for complex types.  */
  if (ts->type == BT_COMPLEX)
    {
      if (ts->kind % 2)
	{
	  gfc_error ("Old-style type declaration %s*%d not supported at %C",
		     gfc_basic_typename (ts->type), original_kind);
	  return MATCH_ERROR;
	}
      ts->kind /= 2;

    }

  if (ts->type == BT_INTEGER && ts->kind == 4 && flag_integer4_kind == 8)
    ts->kind = 8;

  if (ts->type == BT_REAL || ts->type == BT_COMPLEX)
    {
      if (ts->kind == 4)
	{
	  if (flag_real4_kind == 8)
	    ts->kind =  8;
	  if (flag_real4_kind == 10)
	    ts->kind = 10;
	  if (flag_real4_kind == 16)
	    ts->kind = 16;
	}

      if (ts->kind == 8)
	{
	  if (flag_real8_kind == 4)
	    ts->kind = 4;
	  if (flag_real8_kind == 10)
	    ts->kind = 10;
	  if (flag_real8_kind == 16)
	    ts->kind = 16;
	}
    }

  if (gfc_validate_kind (ts->type, ts->kind, true) < 0)
    {
      gfc_error ("Old-style type declaration %s*%d not supported at %C",
		 gfc_basic_typename (ts->type), original_kind);
      return MATCH_ERROR;
    }

  if (!gfc_notify_std (GFC_STD_GNU,
		       "Nonstandard type declaration %s*%d at %C",
		       gfc_basic_typename(ts->type), original_kind))
    return MATCH_ERROR;

  return MATCH_YES;
}


/* Match a kind specification.  Since kinds are generally optional, we
   usually return MATCH_NO if something goes wrong.  If a "kind="
   string is found, then we know we have an error.  */

match
gfc_match_kind_spec (gfc_typespec *ts, bool kind_expr_only)
{
  locus where, loc;
  gfc_expr *e;
  match m, n;
  char c;

  m = MATCH_NO;
  n = MATCH_YES;
  e = NULL;
  saved_kind_expr = NULL;

  where = loc = gfc_current_locus;

  if (kind_expr_only)
    goto kind_expr;

  if (gfc_match_char ('(') == MATCH_NO)
    return MATCH_NO;

  /* Also gobbles optional text.  */
  if (gfc_match (" kind = ") == MATCH_YES)
    m = MATCH_ERROR;

  loc = gfc_current_locus;

kind_expr:

  n = gfc_match_init_expr (&e);

  if (gfc_derived_parameter_expr (e))
    {
      ts->kind = 0;
      saved_kind_expr = gfc_copy_expr (e);
      goto close_brackets;
    }

  if (n != MATCH_YES)
    {
      if (gfc_matching_function)
	{
	  /* The function kind expression might include use associated or
	     imported parameters and try again after the specification
	     expressions.....  */
	  if (gfc_match_char (')') != MATCH_YES)
	    {
	      gfc_error ("Missing right parenthesis at %C");
	      m = MATCH_ERROR;
	      goto no_match;
	    }

	  gfc_free_expr (e);
	  gfc_undo_symbols ();
	  return MATCH_YES;
	}
      else
	{
	  /* ....or else, the match is real.  */
	  if (n == MATCH_NO)
	    gfc_error ("Expected initialization expression at %C");
	  if (n != MATCH_YES)
	    return MATCH_ERROR;
	}
    }

  if (e->rank != 0)
    {
      gfc_error ("Expected scalar initialization expression at %C");
      m = MATCH_ERROR;
      goto no_match;
    }

  if (gfc_extract_int (e, &ts->kind, 1))
    {
      m = MATCH_ERROR;
      goto no_match;
    }

  /* Before throwing away the expression, let's see if we had a
     C interoperable kind (and store the fact).	 */
  if (e->ts.is_c_interop == 1)
    {
      /* Mark this as C interoperable if being declared with one
	 of the named constants from iso_c_binding.  */
      ts->is_c_interop = e->ts.is_iso_c;
      ts->f90_type = e->ts.f90_type;
      if (e->symtree)
	ts->interop_kind = e->symtree->n.sym;
    }

  gfc_free_expr (e);
  e = NULL;

  /* Ignore errors to this point, if we've gotten here.  This means
     we ignore the m=MATCH_ERROR from above.  */
  if (gfc_validate_kind (ts->type, ts->kind, true) < 0)
    {
      gfc_error ("Kind %d not supported for type %s at %C", ts->kind,
		 gfc_basic_typename (ts->type));
      gfc_current_locus = where;
      return MATCH_ERROR;
    }

  /* Warn if, e.g., c_int is used for a REAL variable, but not
     if, e.g., c_double is used for COMPLEX as the standard
     explicitly says that the kind type parameter for complex and real
     variable is the same, i.e. c_float == c_float_complex.  */
  if (ts->f90_type != BT_UNKNOWN && ts->f90_type != ts->type
      && !((ts->f90_type == BT_REAL && ts->type == BT_COMPLEX)
	   || (ts->f90_type == BT_COMPLEX && ts->type == BT_REAL)))
    gfc_warning_now (0, "C kind type parameter is for type %s but type at %L "
		     "is %s", gfc_basic_typename (ts->f90_type), &where,
		     gfc_basic_typename (ts->type));

close_brackets:

  gfc_gobble_whitespace ();
  if ((c = gfc_next_ascii_char ()) != ')'
      && (ts->type != BT_CHARACTER || c != ','))
    {
      if (ts->type == BT_CHARACTER)
	gfc_error ("Missing right parenthesis or comma at %C");
      else
	gfc_error ("Missing right parenthesis at %C");
      m = MATCH_ERROR;
    }
  else
     /* All tests passed.  */
     m = MATCH_YES;

  if(m == MATCH_ERROR)
     gfc_current_locus = where;

  if (ts->type == BT_INTEGER && ts->kind == 4 && flag_integer4_kind == 8)
    ts->kind =  8;

  if (ts->type == BT_REAL || ts->type == BT_COMPLEX)
    {
      if (ts->kind == 4)
	{
	  if (flag_real4_kind == 8)
	    ts->kind =  8;
	  if (flag_real4_kind == 10)
	    ts->kind = 10;
	  if (flag_real4_kind == 16)
	    ts->kind = 16;
	}

      if (ts->kind == 8)
	{
	  if (flag_real8_kind == 4)
	    ts->kind = 4;
	  if (flag_real8_kind == 10)
	    ts->kind = 10;
	  if (flag_real8_kind == 16)
	    ts->kind = 16;
	}
    }

  /* Return what we know from the test(s).  */
  return m;

no_match:
  gfc_free_expr (e);
  gfc_current_locus = where;
  return m;
}


static match
match_char_kind (int * kind, int * is_iso_c)
{
  locus where;
  gfc_expr *e;
  match m, n;
  bool fail;

  m = MATCH_NO;
  e = NULL;
  where = gfc_current_locus;

  n = gfc_match_init_expr (&e);

  if (n != MATCH_YES && gfc_matching_function)
    {
      /* The expression might include use-associated or imported
	 parameters and try again after the specification
	 expressions.  */
      gfc_free_expr (e);
      gfc_undo_symbols ();
      return MATCH_YES;
    }

  if (n == MATCH_NO)
    gfc_error ("Expected initialization expression at %C");
  if (n != MATCH_YES)
    return MATCH_ERROR;

  if (e->rank != 0)
    {
      gfc_error ("Expected scalar initialization expression at %C");
      m = MATCH_ERROR;
      goto no_match;
    }

  if (gfc_derived_parameter_expr (e))
    {
      saved_kind_expr = e;
      *kind = 0;
      return MATCH_YES;
    }

  fail = gfc_extract_int (e, kind, 1);
  *is_iso_c = e->ts.is_iso_c;
  if (fail)
    {
      m = MATCH_ERROR;
      goto no_match;
    }

  gfc_free_expr (e);

  /* Ignore errors to this point, if we've gotten here.  This means
     we ignore the m=MATCH_ERROR from above.  */
  if (gfc_validate_kind (BT_CHARACTER, *kind, true) < 0)
    {
      gfc_error ("Kind %d is not supported for CHARACTER at %C", *kind);
      m = MATCH_ERROR;
    }
  else
     /* All tests passed.  */
     m = MATCH_YES;

  if (m == MATCH_ERROR)
     gfc_current_locus = where;

  /* Return what we know from the test(s).  */
  return m;

no_match:
  gfc_free_expr (e);
  gfc_current_locus = where;
  return m;
}


/* Match the various kind/length specifications in a CHARACTER
   declaration.  We don't return MATCH_NO.  */

match
gfc_match_char_spec (gfc_typespec *ts)
{
  int kind, seen_length, is_iso_c;
  gfc_charlen *cl;
  gfc_expr *len;
  match m;
  bool deferred;

  len = NULL;
  seen_length = 0;
  kind = 0;
  is_iso_c = 0;
  deferred = false;

  /* Try the old-style specification first.  */
  old_char_selector = 0;

  m = match_char_length (&len, &deferred, true);
  if (m != MATCH_NO)
    {
      if (m == MATCH_YES)
	old_char_selector = 1;
      seen_length = 1;
      goto done;
    }

  m = gfc_match_char ('(');
  if (m != MATCH_YES)
    {
      m = MATCH_YES;	/* Character without length is a single char.  */
      goto done;
    }

  /* Try the weird case:  ( KIND = <int> [ , LEN = <len-param> ] ).  */
  if (gfc_match (" kind =") == MATCH_YES)
    {
      m = match_char_kind (&kind, &is_iso_c);

      if (m == MATCH_ERROR)
	goto done;
      if (m == MATCH_NO)
	goto syntax;

      if (gfc_match (" , len =") == MATCH_NO)
	goto rparen;

      m = char_len_param_value (&len, &deferred);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto done;
      seen_length = 1;

      goto rparen;
    }

  /* Try to match "LEN = <len-param>" or "LEN = <len-param>, KIND = <int>".  */
  if (gfc_match (" len =") == MATCH_YES)
    {
      m = char_len_param_value (&len, &deferred);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto done;
      seen_length = 1;

      if (gfc_match_char (')') == MATCH_YES)
	goto done;

      if (gfc_match (" , kind =") != MATCH_YES)
	goto syntax;

      if (match_char_kind (&kind, &is_iso_c) == MATCH_ERROR)
	goto done;

      goto rparen;
    }

  /* Try to match ( <len-param> ) or ( <len-param> , [ KIND = ] <int> ).  */
  m = char_len_param_value (&len, &deferred);
  if (m == MATCH_NO)
    goto syntax;
  if (m == MATCH_ERROR)
    goto done;
  seen_length = 1;

  m = gfc_match_char (')');
  if (m == MATCH_YES)
    goto done;

  if (gfc_match_char (',') != MATCH_YES)
    goto syntax;

  gfc_match (" kind =");	/* Gobble optional text.  */

  m = match_char_kind (&kind, &is_iso_c);
  if (m == MATCH_ERROR)
    goto done;
  if (m == MATCH_NO)
    goto syntax;

rparen:
  /* Require a right-paren at this point.  */
  m = gfc_match_char (')');
  if (m == MATCH_YES)
    goto done;

syntax:
  gfc_error ("Syntax error in CHARACTER declaration at %C");
  m = MATCH_ERROR;
  gfc_free_expr (len);
  return m;

done:
  /* Deal with character functions after USE and IMPORT statements.  */
  if (gfc_matching_function)
    {
      gfc_free_expr (len);
      gfc_undo_symbols ();
      return MATCH_YES;
    }

  if (m != MATCH_YES)
    {
      gfc_free_expr (len);
      return m;
    }

  /* Do some final massaging of the length values.  */
  cl = gfc_new_charlen (gfc_current_ns, NULL);

  if (seen_length == 0)
    cl->length = gfc_get_int_expr (gfc_default_integer_kind, NULL, 1);
  else
    cl->length = len;

  ts->u.cl = cl;
  ts->kind = kind == 0 ? gfc_default_character_kind : kind;
  ts->deferred = deferred;

  /* We have to know if it was a C interoperable kind so we can
     do accurate type checking of bind(c) procs, etc.  */
  if (kind != 0)
    /* Mark this as C interoperable if being declared with one
       of the named constants from iso_c_binding.  */
    ts->is_c_interop = is_iso_c;
  else if (len != NULL)
    /* Here, we might have parsed something such as: character(c_char)
       In this case, the parsing code above grabs the c_char when
       looking for the length (line 1690, roughly).  it's the last
       testcase for parsing the kind params of a character variable.
       However, it's not actually the length.	 this seems like it
       could be an error.
       To see if the user used a C interop kind, test the expr
       of the so called length, and see if it's C interoperable.  */
    ts->is_c_interop = len->ts.is_iso_c;

  return MATCH_YES;
}


/* Matches a RECORD declaration. */

static match
match_record_decl (char *name)
{
    locus old_loc;
    old_loc = gfc_current_locus;
    match m;

    m = gfc_match (" record /");
    if (m == MATCH_YES)
      {
          if (!flag_dec_structure)
            {
                gfc_current_locus = old_loc;
                gfc_error ("RECORD at %C is an extension, enable it with "
                           "-fdec-structure");
                return MATCH_ERROR;
            }
          m = gfc_match (" %n/", name);
          if (m == MATCH_YES)
            return MATCH_YES;
      }

  gfc_current_locus = old_loc;
  if (flag_dec_structure
      && (gfc_match (" record% ") == MATCH_YES
          || gfc_match (" record%t") == MATCH_YES))
    gfc_error ("Structure name expected after RECORD at %C");
  if (m == MATCH_NO)
    return MATCH_NO;

  return MATCH_ERROR;
}


/* This function uses the gfc_actual_arglist 'type_param_spec_list' as a source
   of expressions to substitute into the possibly parameterized expression
   'e'. Using a list is inefficient but should not be too bad since the
   number of type parameters is not likely to be large.  */
static bool
insert_parameter_exprs (gfc_expr* e, gfc_symbol* sym ATTRIBUTE_UNUSED,
			int* f)
{
  gfc_actual_arglist *param;
  gfc_expr *copy;

  if (e->expr_type != EXPR_VARIABLE)
    return false;

  gcc_assert (e->symtree);
  if (e->symtree->n.sym->attr.pdt_kind
      || (*f != 0 && e->symtree->n.sym->attr.pdt_len))
    {
      for (param = type_param_spec_list; param; param = param->next)
	if (strcmp (e->symtree->n.sym->name, param->name) == 0)
	  break;

      if (param)
	{
	  copy = gfc_copy_expr (param->expr);
	  *e = *copy;
	  free (copy);
	}
    }

  return false;
}


bool
gfc_insert_kind_parameter_exprs (gfc_expr *e)
{
  return gfc_traverse_expr (e, NULL, &insert_parameter_exprs, 0);
}


bool
gfc_insert_parameter_exprs (gfc_expr *e, gfc_actual_arglist *param_list)
{
  gfc_actual_arglist *old_param_spec_list = type_param_spec_list;
  type_param_spec_list = param_list;
  return gfc_traverse_expr (e, NULL, &insert_parameter_exprs, 1);
  type_param_spec_list = NULL;
  type_param_spec_list = old_param_spec_list;
}

/* Determines the instance of a parameterized derived type to be used by
   matching determining the values of the kind parameters and using them
   in the name of the instance. If the instance exists, it is used, otherwise
   a new derived type is created.  */
match
gfc_get_pdt_instance (gfc_actual_arglist *param_list, gfc_symbol **sym,
		      gfc_actual_arglist **ext_param_list)
{
  /* The PDT template symbol.  */
  gfc_symbol *pdt = *sym;
  /* The symbol for the parameter in the template f2k_namespace.  */
  gfc_symbol *param;
  /* The hoped for instance of the PDT.  */
  gfc_symbol *instance;
  /* The list of parameters appearing in the PDT declaration.  */
  gfc_formal_arglist *type_param_name_list;
  /* Used to store the parameter specification list during recursive calls.  */
  gfc_actual_arglist *old_param_spec_list;
  /* Pointers to the parameter specification being used.  */
  gfc_actual_arglist *actual_param;
  gfc_actual_arglist *tail = NULL;
  /* Used to build up the name of the PDT instance. The prefix uses 4
     characters and each KIND parameter 2 more.  Allow 8 of the latter. */
  char name[GFC_MAX_SYMBOL_LEN + 21];

  bool name_seen = (param_list == NULL);
  bool assumed_seen = false;
  bool deferred_seen = false;
  bool spec_error = false;
  int kind_value, i;
  gfc_expr *kind_expr;
  gfc_component *c1, *c2;
  match m;

  type_param_spec_list = NULL;

  type_param_name_list = pdt->formal;
  actual_param = param_list;
  sprintf (name, "Pdt%s", pdt->name);

  /* Run through the parameter name list and pick up the actual
     parameter values or use the default values in the PDT declaration.  */
  for (; type_param_name_list;
       type_param_name_list = type_param_name_list->next)
    {
      if (actual_param && actual_param->spec_type != SPEC_EXPLICIT)
	{
	  if (actual_param->spec_type == SPEC_ASSUMED)
	    spec_error = deferred_seen;
	  else
	    spec_error = assumed_seen;

	  if (spec_error)
	    {
	      gfc_error ("The type parameter spec list at %C cannot contain "
			 "both ASSUMED and DEFERRED parameters");
	      goto error_return;
	    }
	}

      if (actual_param && actual_param->name)
	name_seen = true;
      param = type_param_name_list->sym;

      c1 = gfc_find_component (pdt, param->name, false, true, NULL);
      /* An error should already have been thrown in resolve.c
	 (resolve_fl_derived0).  */
      if (!pdt->attr.use_assoc && !c1)
	goto error_return;

      kind_expr = NULL;
      if (!name_seen)
	{
	  if (!actual_param && !(c1 && c1->initializer))
	    {
	      gfc_error ("The type parameter spec list at %C does not contain "
			 "enough parameter expressions");
	      goto error_return;
	    }
	  else if (!actual_param && c1 && c1->initializer)
	    kind_expr = gfc_copy_expr (c1->initializer);
	  else if (actual_param && actual_param->spec_type == SPEC_EXPLICIT)
	    kind_expr = gfc_copy_expr (actual_param->expr);
	}
      else
	{
	  actual_param = param_list;
	  for (;actual_param; actual_param = actual_param->next)
	    if (actual_param->name
	        && strcmp (actual_param->name, param->name) == 0)
	      break;
	  if (actual_param && actual_param->spec_type == SPEC_EXPLICIT)
	    kind_expr = gfc_copy_expr (actual_param->expr);
	  else
	    {
	      if (c1->initializer)
		kind_expr = gfc_copy_expr (c1->initializer);
	      else if (!(actual_param && param->attr.pdt_len))
		{
		  gfc_error ("The derived parameter '%qs' at %C does not "
			     "have a default value", param->name);
		  goto error_return;
		}
	    }
	}

      /* Store the current parameter expressions in a temporary actual
	 arglist 'list' so that they can be substituted in the corresponding
	 expressions in the PDT instance.  */
      if (type_param_spec_list == NULL)
	{
	  type_param_spec_list = gfc_get_actual_arglist ();
	  tail = type_param_spec_list;
	}
      else
	{
	  tail->next = gfc_get_actual_arglist ();
	  tail = tail->next;
	}
      tail->name = param->name;

      if (kind_expr)
	{
	  /* Try simplification even for LEN expressions.  */
	  gfc_resolve_expr (kind_expr);
	  gfc_simplify_expr (kind_expr, 1);
	  /* Variable expressions seem to default to BT_PROCEDURE.
	     TODO find out why this is and fix it.  */
	  if (kind_expr->ts.type != BT_INTEGER
	      && kind_expr->ts.type != BT_PROCEDURE)
	    {
	      gfc_error ("The parameter expression at %C must be of "
		         "INTEGER type and not %s type",
			 gfc_basic_typename (kind_expr->ts.type));
	      goto error_return;
	    }

	  tail->expr = gfc_copy_expr (kind_expr);
	}

      if (actual_param)
	tail->spec_type = actual_param->spec_type;

      if (!param->attr.pdt_kind)
	{
	  if (!name_seen && actual_param)
	    actual_param = actual_param->next;
	  if (kind_expr)
	    {
	      gfc_free_expr (kind_expr);
	      kind_expr = NULL;
	    }
	  continue;
	}

      if (actual_param
	  && (actual_param->spec_type == SPEC_ASSUMED
	      || actual_param->spec_type == SPEC_DEFERRED))
	{
	  gfc_error ("The KIND parameter '%qs' at %C cannot either be "
		     "ASSUMED or DEFERRED", param->name);
	  goto error_return;
	}

      if (!kind_expr || !gfc_is_constant_expr (kind_expr))
	{
	  gfc_error ("The value for the KIND parameter '%qs' at %C does not "
		     "reduce to a constant expression", param->name);
	  goto error_return;
	}

      gfc_extract_int (kind_expr, &kind_value);
      sprintf (name, "%s_%d", name, kind_value);

      if (!name_seen && actual_param)
	actual_param = actual_param->next;
      gfc_free_expr (kind_expr);
    }

  if (!name_seen && actual_param)
    {
      gfc_error ("The type parameter spec list at %C contains too many "
		 "parameter expressions");
      goto error_return;
    }

  /* Now we search for the PDT instance 'name'. If it doesn't exist, we
     build it, using 'pdt' as a template.  */
  if (gfc_get_symbol (name, pdt->ns, &instance))
    {
      gfc_error ("Parameterized derived type at %C is ambiguous");
      goto error_return;
    }

  m = MATCH_YES;

  if (instance->attr.flavor == FL_DERIVED
      && instance->attr.pdt_type)
    {
      instance->refs++;
      if (ext_param_list)
        *ext_param_list = type_param_spec_list;
      *sym = instance;
      gfc_commit_symbols ();
      return m;
    }

  /* Start building the new instance of the parameterized type.  */
  gfc_copy_attr (&instance->attr, &pdt->attr, &pdt->declared_at);
  instance->attr.pdt_template = 0;
  instance->attr.pdt_type = 1;
  instance->declared_at = gfc_current_locus;

  /* Add the components, replacing the parameters in all expressions
     with the expressions for their values in 'type_param_spec_list'.  */
  c1 = pdt->components;
  tail = type_param_spec_list;
  for (; c1; c1 = c1->next)
    {
      gfc_add_component (instance, c1->name, &c2);
      c2->ts = c1->ts;
      c2->attr = c1->attr;

      /* Deal with type extension by recursively calling this function
	 to obtain the instance of the extended type.  */
      if (gfc_current_state () != COMP_DERIVED
	  && c1 == pdt->components
	  && (c1->ts.type == BT_DERIVED || c1->ts.type == BT_CLASS)
	  && c1->ts.u.derived && c1->ts.u.derived->attr.pdt_template
	  && gfc_get_derived_super_type (*sym) == c2->ts.u.derived)
	{
	  gfc_formal_arglist *f;

	  old_param_spec_list = type_param_spec_list;

	  /* Obtain a spec list appropriate to the extended type..*/
	  actual_param = gfc_copy_actual_arglist (type_param_spec_list);
	  type_param_spec_list = actual_param;
	  for (f = c1->ts.u.derived->formal; f && f->next; f = f->next)
	    actual_param = actual_param->next;
	  if (actual_param)
	    {
	      gfc_free_actual_arglist (actual_param->next);
	      actual_param->next = NULL;
	    }

	  /* Now obtain the PDT instance for the extended type.  */
	  c2->param_list = type_param_spec_list;
	  m = gfc_get_pdt_instance (type_param_spec_list, &c2->ts.u.derived,
				    NULL);
	  type_param_spec_list = old_param_spec_list;

	  c2->ts.u.derived->refs++;
	  gfc_set_sym_referenced (c2->ts.u.derived);

	  /* Set extension level.  */
	  if (c2->ts.u.derived->attr.extension == 255)
	    {
	      /* Since the extension field is 8 bit wide, we can only have
		 up to 255 extension levels.  */
	      gfc_error ("Maximum extension level reached with type %qs at %L",
			 c2->ts.u.derived->name,
			 &c2->ts.u.derived->declared_at);
	      goto error_return;
	    }
	  instance->attr.extension = c2->ts.u.derived->attr.extension + 1;

	  /* Advance the position in the spec list by the number of
	     parameters in the extended type.  */
	  tail = type_param_spec_list;
	  for (f = c1->ts.u.derived->formal; f && f->next; f = f->next)
	    tail = tail->next;

	  continue;
	}

      /* Set the component kind using the parameterized expression.  */
      if (c1->ts.kind == 0 && c1->kind_expr != NULL)
	{
	  gfc_expr *e = gfc_copy_expr (c1->kind_expr);
	  gfc_insert_kind_parameter_exprs (e);
	  gfc_simplify_expr (e, 1);
	  gfc_extract_int (e, &c2->ts.kind);
	  gfc_free_expr (e);
	  if (gfc_validate_kind (c2->ts.type, c2->ts.kind, true) < 0)
	    {
	      gfc_error ("Kind %d not supported for type %s at %C",
			 c2->ts.kind, gfc_basic_typename (c2->ts.type));
	      goto error_return;
	    }
	}

      /* Similarly, set the string length if parameterized.  */
      if (c1->ts.type == BT_CHARACTER
	  && c1->ts.u.cl->length
	  && gfc_derived_parameter_expr (c1->ts.u.cl->length))
	{
	  gfc_expr *e;
	  e = gfc_copy_expr (c1->ts.u.cl->length);
	  gfc_insert_kind_parameter_exprs (e);
	  gfc_simplify_expr (e, 1);
	  c2->ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);
	  c2->ts.u.cl->length = e;
	  c2->attr.pdt_string = 1;
	}

      /* Set up either the KIND/LEN initializer, if constant,
	 or the parameterized expression. Use the template
	 initializer if one is not already set in this instance.  */
      if (c2->attr.pdt_kind || c2->attr.pdt_len)
	{
	  if (tail && tail->expr && gfc_is_constant_expr (tail->expr))
	    c2->initializer = gfc_copy_expr (tail->expr);
	  else if (tail && tail->expr)
	    {
	      c2->param_list = gfc_get_actual_arglist ();
	      c2->param_list->name = tail->name;
	      c2->param_list->expr = gfc_copy_expr (tail->expr);
	      c2->param_list->next = NULL;
	    }

	  if (!c2->initializer && c1->initializer)
	    c2->initializer = gfc_copy_expr (c1->initializer);

	  tail = tail->next;
	}

      /* Copy the array spec.  */
      c2->as = gfc_copy_array_spec (c1->as);
      if (c1->ts.type == BT_CLASS)
	CLASS_DATA (c2)->as = gfc_copy_array_spec (CLASS_DATA (c1)->as);

      /* Determine if an array spec is parameterized. If so, substitute
	 in the parameter expressions for the bounds and set the pdt_array
	 attribute. Notice that this attribute must be unconditionally set
	 if this is an array of parameterized character length.  */
      if (c1->as && c1->as->type == AS_EXPLICIT)
	{
	  bool pdt_array = false;

	  /* Are the bounds of the array parameterized?  */
	  for (i = 0; i < c1->as->rank; i++)
	    {
	      if (gfc_derived_parameter_expr (c1->as->lower[i]))
		pdt_array = true;
	      if (gfc_derived_parameter_expr (c1->as->upper[i]))
		pdt_array = true;
	    }

	  /* If they are, free the expressions for the bounds and
	     replace them with the template expressions with substitute
	     values.  */
	  for (i = 0; pdt_array && i < c1->as->rank; i++)
	    {
	      gfc_expr *e;
	      e = gfc_copy_expr (c1->as->lower[i]);
	      gfc_insert_kind_parameter_exprs (e);
	      gfc_simplify_expr (e, 1);
	      gfc_free_expr (c2->as->lower[i]);
	      c2->as->lower[i] = e;
	      e = gfc_copy_expr (c1->as->upper[i]);
	      gfc_insert_kind_parameter_exprs (e);
	      gfc_simplify_expr (e, 1);
	      gfc_free_expr (c2->as->upper[i]);
	      c2->as->upper[i] = e;
	    }
	  c2->attr.pdt_array = pdt_array ? 1 : c2->attr.pdt_string;
	}

      /* Recurse into this function for PDT components.  */
      if ((c1->ts.type == BT_DERIVED || c1->ts.type == BT_CLASS)
	  && c1->ts.u.derived && c1->ts.u.derived->attr.pdt_template)
	{
	  gfc_actual_arglist *params;
	  /* The component in the template has a list of specification
	     expressions derived from its declaration.  */
	  params = gfc_copy_actual_arglist (c1->param_list);
	  actual_param = params;
	  /* Substitute the template parameters with the expressions
	     from the specification list.  */
	  for (;actual_param; actual_param = actual_param->next)
	    gfc_insert_parameter_exprs (actual_param->expr,
					type_param_spec_list);

	  /* Now obtain the PDT instance for the component.  */
	  old_param_spec_list = type_param_spec_list;
	  m = gfc_get_pdt_instance (params, &c2->ts.u.derived, NULL);
	  type_param_spec_list = old_param_spec_list;

	  c2->param_list = params;
	  if (!(c2->attr.pointer || c2->attr.allocatable))
	    c2->initializer = gfc_default_initializer (&c2->ts);

	  if (c2->attr.allocatable)
	    instance->attr.alloc_comp = 1;
	}
    }

  gfc_commit_symbol (instance);
  if (ext_param_list)
    *ext_param_list = type_param_spec_list;
  *sym = instance;
  return m;

error_return:
  gfc_free_actual_arglist (type_param_spec_list);
  return MATCH_ERROR;
}


/* Matches a declaration-type-spec (F03:R502).  If successful, sets the ts
   structure to the matched specification.  This is necessary for FUNCTION and
   IMPLICIT statements.

   If implicit_flag is nonzero, then we don't check for the optional
   kind specification.  Not doing so is needed for matching an IMPLICIT
   statement correctly.  */

match
gfc_match_decl_type_spec (gfc_typespec *ts, int implicit_flag)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym, *dt_sym;
  match m;
  char c;
  bool seen_deferred_kind, matched_type;
  const char *dt_name;

  decl_type_param_list = NULL;

  /* A belt and braces check that the typespec is correctly being treated
     as a deferred characteristic association.  */
  seen_deferred_kind = (gfc_current_state () == COMP_FUNCTION)
			  && (gfc_current_block ()->result->ts.kind == -1)
			  && (ts->kind == -1);
  gfc_clear_ts (ts);
  if (seen_deferred_kind)
    ts->kind = -1;

  /* Clear the current binding label, in case one is given.  */
  curr_binding_label = NULL;

  if (gfc_match (" byte") == MATCH_YES)
    {
      if (!gfc_notify_std (GFC_STD_GNU, "BYTE type at %C"))
	return MATCH_ERROR;

      if (gfc_validate_kind (BT_INTEGER, 1, true) < 0)
	{
	  gfc_error ("BYTE type used at %C "
		     "is not available on the target machine");
	  return MATCH_ERROR;
	}

      ts->type = BT_INTEGER;
      ts->kind = 1;
      return MATCH_YES;
    }


  m = gfc_match (" type (");
  matched_type = (m == MATCH_YES);
  if (matched_type)
    {
      gfc_gobble_whitespace ();
      if (gfc_peek_ascii_char () == '*')
	{
	  if ((m = gfc_match ("*)")) != MATCH_YES)
	    return m;
	  if (gfc_comp_struct (gfc_current_state ()))
	    {
	      gfc_error ("Assumed type at %C is not allowed for components");
	      return MATCH_ERROR;
	    }
	  if (!gfc_notify_std (GFC_STD_F2008_TS, "Assumed type "
			       "at %C"))
	    return MATCH_ERROR;
	  ts->type = BT_ASSUMED;
	  return MATCH_YES;
	}

      m = gfc_match ("%n", name);
      matched_type = (m == MATCH_YES);
    }

  if ((matched_type && strcmp ("integer", name) == 0)
      || (!matched_type && gfc_match (" integer") == MATCH_YES))
    {
      ts->type = BT_INTEGER;
      ts->kind = gfc_default_integer_kind;
      goto get_kind;
    }

  if ((matched_type && strcmp ("character", name) == 0)
      || (!matched_type && gfc_match (" character") == MATCH_YES))
    {
      if (matched_type
	  && !gfc_notify_std (GFC_STD_F2008, "TYPE with "
			      "intrinsic-type-spec at %C"))
	return MATCH_ERROR;

      ts->type = BT_CHARACTER;
      if (implicit_flag == 0)
	m = gfc_match_char_spec (ts);
      else
	m = MATCH_YES;

      if (matched_type && m == MATCH_YES && gfc_match_char (')') != MATCH_YES)
	m = MATCH_ERROR;

      return m;
    }

  if ((matched_type && strcmp ("real", name) == 0)
      || (!matched_type && gfc_match (" real") == MATCH_YES))
    {
      ts->type = BT_REAL;
      ts->kind = gfc_default_real_kind;
      goto get_kind;
    }

  if ((matched_type
       && (strcmp ("doubleprecision", name) == 0
	   || (strcmp ("double", name) == 0
	       && gfc_match (" precision") == MATCH_YES)))
      || (!matched_type && gfc_match (" double precision") == MATCH_YES))
    {
      if (matched_type
	  && !gfc_notify_std (GFC_STD_F2008, "TYPE with "
			      "intrinsic-type-spec at %C"))
	return MATCH_ERROR;
      if (matched_type && gfc_match_char (')') != MATCH_YES)
	return MATCH_ERROR;

      ts->type = BT_REAL;
      ts->kind = gfc_default_double_kind;
      return MATCH_YES;
    }

  if ((matched_type && strcmp ("complex", name) == 0)
      || (!matched_type && gfc_match (" complex") == MATCH_YES))
    {
      ts->type = BT_COMPLEX;
      ts->kind = gfc_default_complex_kind;
      goto get_kind;
    }

  if ((matched_type
       && (strcmp ("doublecomplex", name) == 0
	   || (strcmp ("double", name) == 0
	       && gfc_match (" complex") == MATCH_YES)))
      || (!matched_type && gfc_match (" double complex") == MATCH_YES))
    {
      if (!gfc_notify_std (GFC_STD_GNU, "DOUBLE COMPLEX at %C"))
	return MATCH_ERROR;

      if (matched_type
	  && !gfc_notify_std (GFC_STD_F2008, "TYPE with "
			      "intrinsic-type-spec at %C"))
	return MATCH_ERROR;

      if (matched_type && gfc_match_char (')') != MATCH_YES)
	return MATCH_ERROR;

      ts->type = BT_COMPLEX;
      ts->kind = gfc_default_double_kind;
      return MATCH_YES;
    }

  if ((matched_type && strcmp ("logical", name) == 0)
      || (!matched_type && gfc_match (" logical") == MATCH_YES))
    {
      ts->type = BT_LOGICAL;
      ts->kind = gfc_default_logical_kind;
      goto get_kind;
    }

  if (matched_type)
    {
      m = gfc_match_actual_arglist (1, &decl_type_param_list, true);
      if (m == MATCH_ERROR)
	return m;

    m = gfc_match_char (')');
    }

  if (m != MATCH_YES)
    m = match_record_decl (name);

  if (matched_type || m == MATCH_YES)
    {
      ts->type = BT_DERIVED;
      /* We accept record/s/ or type(s) where s is a structure, but we
       * don't need all the extra derived-type stuff for structures.  */
      if (gfc_find_symbol (gfc_dt_upper_string (name), NULL, 1, &sym))
        {
          gfc_error ("Type name %qs at %C is ambiguous", name);
          return MATCH_ERROR;
        }

      if (sym && sym->attr.flavor == FL_DERIVED
	  && sym->attr.pdt_template
	  && gfc_current_state () != COMP_DERIVED)
	{
	  m = gfc_get_pdt_instance (decl_type_param_list, &sym,  NULL);
	  if (m != MATCH_YES)
	    return m;
	  gcc_assert (!sym->attr.pdt_template && sym->attr.pdt_type);
	  ts->u.derived = sym;
	  strcpy (name, gfc_dt_lower_string (sym->name));
	}

      if (sym && sym->attr.flavor == FL_STRUCT)
        {
          ts->u.derived = sym;
          return MATCH_YES;
        }
      /* Actually a derived type.  */
    }

  else
    {
      /* Match nested STRUCTURE declarations; only valid within another
	 structure declaration.  */
      if (flag_dec_structure
	  && (gfc_current_state () == COMP_STRUCTURE
	      || gfc_current_state () == COMP_MAP))
	{
	  m = gfc_match (" structure");
	  if (m == MATCH_YES)
	    {
	      m = gfc_match_structure_decl ();
	      if (m == MATCH_YES)
		{
		  /* gfc_new_block is updated by match_structure_decl.  */
		  ts->type = BT_DERIVED;
		  ts->u.derived = gfc_new_block;
		  return MATCH_YES;
		}
	    }
	  if (m == MATCH_ERROR)
	    return MATCH_ERROR;
	}

      /* Match CLASS declarations.  */
      m = gfc_match (" class ( * )");
      if (m == MATCH_ERROR)
	return MATCH_ERROR;
      else if (m == MATCH_YES)
	{
	  gfc_symbol *upe;
	  gfc_symtree *st;
	  ts->type = BT_CLASS;
	  gfc_find_symbol ("STAR", gfc_current_ns, 1, &upe);
	  if (upe == NULL)
	    {
	      upe = gfc_new_symbol ("STAR", gfc_current_ns);
	      st = gfc_new_symtree (&gfc_current_ns->sym_root, "STAR");
	      st->n.sym = upe;
	      gfc_set_sym_referenced (upe);
	      upe->refs++;
	      upe->ts.type = BT_VOID;
	      upe->attr.unlimited_polymorphic = 1;
	      /* This is essential to force the construction of
		 unlimited polymorphic component class containers.  */
	      upe->attr.zero_comp = 1;
	      if (!gfc_add_flavor (&upe->attr, FL_DERIVED, NULL,
				   &gfc_current_locus))
	      return MATCH_ERROR;
	    }
	  else
	    {
	      st = gfc_get_tbp_symtree (&gfc_current_ns->sym_root, "STAR");
	      st->n.sym = upe;
	      upe->refs++;
	    }
	  ts->u.derived = upe;
	  return m;
	}

      m = gfc_match (" class (");

      if (m == MATCH_YES)
	m = gfc_match ("%n", name);
      else
	return m;

      if (m != MATCH_YES)
	return m;
      ts->type = BT_CLASS;

      if (!gfc_notify_std (GFC_STD_F2003, "CLASS statement at %C"))
	return MATCH_ERROR;

      m = gfc_match_actual_arglist (1, &decl_type_param_list, true);
      if (m == MATCH_ERROR)
	return m;

      m = gfc_match_char (')');
      if (m != MATCH_YES)
	return m;
    }

  /* Defer association of the derived type until the end of the
     specification block.  However, if the derived type can be
     found, add it to the typespec.  */
  if (gfc_matching_function)
    {
      ts->u.derived = NULL;
      if (gfc_current_state () != COMP_INTERFACE
	    && !gfc_find_symbol (name, NULL, 1, &sym) && sym)
	{
	  sym = gfc_find_dt_in_generic (sym);
	  ts->u.derived = sym;
	}
      return MATCH_YES;
    }

  /* Search for the name but allow the components to be defined later.  If
     type = -1, this typespec has been seen in a function declaration but
     the type could not be accessed at that point.  The actual derived type is
     stored in a symtree with the first letter of the name capitalized; the
     symtree with the all lower-case name contains the associated
     generic function.  */
  dt_name = gfc_dt_upper_string (name);
  sym = NULL;
  dt_sym = NULL;
  if (ts->kind != -1)
    {
      gfc_get_ha_symbol (name, &sym);
      if (sym->generic && gfc_find_symbol (dt_name, NULL, 0, &dt_sym))
	{
	  gfc_error ("Type name %qs at %C is ambiguous", name);
	  return MATCH_ERROR;
	}
      if (sym->generic && !dt_sym)
	dt_sym = gfc_find_dt_in_generic (sym);

      /* Host associated PDTs can get confused with their constructors
	 because they ar instantiated in the template's namespace.  */
      if (!dt_sym)
	{
	  if (gfc_find_symbol (dt_name, NULL, 1, &dt_sym))
	    {
	      gfc_error ("Type name %qs at %C is ambiguous", name);
	      return MATCH_ERROR;
	    }
	  if (dt_sym && !dt_sym->attr.pdt_type)
	    dt_sym = NULL;
	}
    }
  else if (ts->kind == -1)
    {
      int iface = gfc_state_stack->previous->state != COMP_INTERFACE
		    || gfc_current_ns->has_import_set;
      gfc_find_symbol (name, NULL, iface, &sym);
      if (sym && sym->generic && gfc_find_symbol (dt_name, NULL, 1, &dt_sym))
	{
	  gfc_error ("Type name %qs at %C is ambiguous", name);
	  return MATCH_ERROR;
	}
      if (sym && sym->generic && !dt_sym)
	dt_sym = gfc_find_dt_in_generic (sym);

      ts->kind = 0;
      if (sym == NULL)
	return MATCH_NO;
    }

  if ((sym->attr.flavor != FL_UNKNOWN && sym->attr.flavor != FL_STRUCT
       && !(sym->attr.flavor == FL_PROCEDURE && sym->attr.generic))
      || sym->attr.subroutine)
    {
      gfc_error ("Type name %qs at %C conflicts with previously declared "
		 "entity at %L, which has the same name", name,
		 &sym->declared_at);
      return MATCH_ERROR;
    }

  if (sym && sym->attr.flavor == FL_DERIVED
      && sym->attr.pdt_template
      && gfc_current_state () != COMP_DERIVED)
    {
      m = gfc_get_pdt_instance (decl_type_param_list, &sym, NULL);
      if (m != MATCH_YES)
	return m;
      gcc_assert (!sym->attr.pdt_template && sym->attr.pdt_type);
      ts->u.derived = sym;
      strcpy (name, gfc_dt_lower_string (sym->name));
    }

  gfc_save_symbol_data (sym);
  gfc_set_sym_referenced (sym);
  if (!sym->attr.generic
      && !gfc_add_generic (&sym->attr, sym->name, NULL))
    return MATCH_ERROR;

  if (!sym->attr.function
      && !gfc_add_function (&sym->attr, sym->name, NULL))
    return MATCH_ERROR;

  if (dt_sym && dt_sym->attr.flavor == FL_DERIVED
      && dt_sym->attr.pdt_template
      && gfc_current_state () != COMP_DERIVED)
    {
      m = gfc_get_pdt_instance (decl_type_param_list, &dt_sym, NULL);
      if (m != MATCH_YES)
	return m;
      gcc_assert (!dt_sym->attr.pdt_template && dt_sym->attr.pdt_type);
    }

  if (!dt_sym)
    {
      gfc_interface *intr, *head;

      /* Use upper case to save the actual derived-type symbol.  */
      gfc_get_symbol (dt_name, NULL, &dt_sym);
      dt_sym->name = gfc_get_string ("%s", sym->name);
      head = sym->generic;
      intr = gfc_get_interface ();
      intr->sym = dt_sym;
      intr->where = gfc_current_locus;
      intr->next = head;
      sym->generic = intr;
      sym->attr.if_source = IFSRC_DECL;
    }
  else
    gfc_save_symbol_data (dt_sym);

  gfc_set_sym_referenced (dt_sym);

  if (dt_sym->attr.flavor != FL_DERIVED && dt_sym->attr.flavor != FL_STRUCT
      && !gfc_add_flavor (&dt_sym->attr, FL_DERIVED, sym->name, NULL))
    return MATCH_ERROR;

  ts->u.derived = dt_sym;

  return MATCH_YES;

get_kind:
  if (matched_type
      && !gfc_notify_std (GFC_STD_F2008, "TYPE with "
			  "intrinsic-type-spec at %C"))
    return MATCH_ERROR;

  /* For all types except double, derived and character, look for an
     optional kind specifier.  MATCH_NO is actually OK at this point.  */
  if (implicit_flag == 1)
    {
	if (matched_type && gfc_match_char (')') != MATCH_YES)
	  return MATCH_ERROR;

	return MATCH_YES;
    }

  if (gfc_current_form == FORM_FREE)
    {
      c = gfc_peek_ascii_char ();
      if (!gfc_is_whitespace (c) && c != '*' && c != '('
	  && c != ':' && c != ',')
        {
	  if (matched_type && c == ')')
	    {
	      gfc_next_ascii_char ();
	      return MATCH_YES;
	    }
	  return MATCH_NO;
	}
    }

  m = gfc_match_kind_spec (ts, false);
  if (m == MATCH_NO && ts->type != BT_CHARACTER)
    {
      m = gfc_match_old_kind_spec (ts);
      if (gfc_validate_kind (ts->type, ts->kind, true) == -1)
         return MATCH_ERROR;
    }

  if (matched_type && gfc_match_char (')') != MATCH_YES)
    return MATCH_ERROR;

  /* Defer association of the KIND expression of function results
     until after USE and IMPORT statements.  */
  if ((gfc_current_state () == COMP_NONE && gfc_error_flag_test ())
	 || gfc_matching_function)
    return MATCH_YES;

  if (m == MATCH_NO)
    m = MATCH_YES;		/* No kind specifier found.  */

  return m;
}


/* Match an IMPLICIT NONE statement.  Actually, this statement is
   already matched in parse.c, or we would not end up here in the
   first place.  So the only thing we need to check, is if there is
   trailing garbage.  If not, the match is successful.  */

match
gfc_match_implicit_none (void)
{
  char c;
  match m;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  bool type = false;
  bool external = false;
  locus cur_loc = gfc_current_locus;

  if (gfc_current_ns->seen_implicit_none
      || gfc_current_ns->has_implicit_none_export)
    {
      gfc_error ("Duplicate IMPLICIT NONE statement at %C");
      return MATCH_ERROR;
    }

  gfc_gobble_whitespace ();
  c = gfc_peek_ascii_char ();
  if (c == '(')
    {
      (void) gfc_next_ascii_char ();
      if (!gfc_notify_std (GFC_STD_F2015, "IMPORT NONE with spec list at %C"))
	return MATCH_ERROR;

      gfc_gobble_whitespace ();
      if (gfc_peek_ascii_char () == ')')
	{
	  (void) gfc_next_ascii_char ();
	  type = true;
	}
      else
	for(;;)
	  {
	    m = gfc_match (" %n", name);
	    if (m != MATCH_YES)
	      return MATCH_ERROR;

	    if (strcmp (name, "type") == 0)
	      type = true;
	    else if (strcmp (name, "external") == 0)
	      external = true;
	    else
	      return MATCH_ERROR;

	    gfc_gobble_whitespace ();
	    c = gfc_next_ascii_char ();
	    if (c == ',')
	      continue;
	    if (c == ')')
	      break;
	    return MATCH_ERROR;
	  }
    }
  else
    type = true;

  if (gfc_match_eos () != MATCH_YES)
    return MATCH_ERROR;

  gfc_set_implicit_none (type, external, &cur_loc);

  return MATCH_YES;
}


/* Match the letter range(s) of an IMPLICIT statement.  */

static match
match_implicit_range (void)
{
  char c, c1, c2;
  int inner;
  locus cur_loc;

  cur_loc = gfc_current_locus;

  gfc_gobble_whitespace ();
  c = gfc_next_ascii_char ();
  if (c != '(')
    {
      gfc_error ("Missing character range in IMPLICIT at %C");
      goto bad;
    }

  inner = 1;
  while (inner)
    {
      gfc_gobble_whitespace ();
      c1 = gfc_next_ascii_char ();
      if (!ISALPHA (c1))
	goto bad;

      gfc_gobble_whitespace ();
      c = gfc_next_ascii_char ();

      switch (c)
	{
	case ')':
	  inner = 0;		/* Fall through.  */

	case ',':
	  c2 = c1;
	  break;

	case '-':
	  gfc_gobble_whitespace ();
	  c2 = gfc_next_ascii_char ();
	  if (!ISALPHA (c2))
	    goto bad;

	  gfc_gobble_whitespace ();
	  c = gfc_next_ascii_char ();

	  if ((c != ',') && (c != ')'))
	    goto bad;
	  if (c == ')')
	    inner = 0;

	  break;

	default:
	  goto bad;
	}

      if (c1 > c2)
	{
	  gfc_error ("Letters must be in alphabetic order in "
		     "IMPLICIT statement at %C");
	  goto bad;
	}

      /* See if we can add the newly matched range to the pending
	 implicits from this IMPLICIT statement.  We do not check for
	 conflicts with whatever earlier IMPLICIT statements may have
	 set.  This is done when we've successfully finished matching
	 the current one.  */
      if (!gfc_add_new_implicit_range (c1, c2))
	goto bad;
    }

  return MATCH_YES;

bad:
  gfc_syntax_error (ST_IMPLICIT);

  gfc_current_locus = cur_loc;
  return MATCH_ERROR;
}


/* Match an IMPLICIT statement, storing the types for
   gfc_set_implicit() if the statement is accepted by the parser.
   There is a strange looking, but legal syntactic construction
   possible.  It looks like:

     IMPLICIT INTEGER (a-b) (c-d)

   This is legal if "a-b" is a constant expression that happens to
   equal one of the legal kinds for integers.  The real problem
   happens with an implicit specification that looks like:

     IMPLICIT INTEGER (a-b)

   In this case, a typespec matcher that is "greedy" (as most of the
   matchers are) gobbles the character range as a kindspec, leaving
   nothing left.  We therefore have to go a bit more slowly in the
   matching process by inhibiting the kindspec checking during
   typespec matching and checking for a kind later.  */

match
gfc_match_implicit (void)
{
  gfc_typespec ts;
  locus cur_loc;
  char c;
  match m;

  if (gfc_current_ns->seen_implicit_none)
    {
      gfc_error ("IMPLICIT statement at %C following an IMPLICIT NONE (type) "
		 "statement");
      return MATCH_ERROR;
    }

  gfc_clear_ts (&ts);

  /* We don't allow empty implicit statements.  */
  if (gfc_match_eos () == MATCH_YES)
    {
      gfc_error ("Empty IMPLICIT statement at %C");
      return MATCH_ERROR;
    }

  do
    {
      /* First cleanup.  */
      gfc_clear_new_implicit ();

      /* A basic type is mandatory here.  */
      m = gfc_match_decl_type_spec (&ts, 1);
      if (m == MATCH_ERROR)
	goto error;
      if (m == MATCH_NO)
	goto syntax;

      cur_loc = gfc_current_locus;
      m = match_implicit_range ();

      if (m == MATCH_YES)
	{
	  /* We may have <TYPE> (<RANGE>).  */
	  gfc_gobble_whitespace ();
          c = gfc_peek_ascii_char ();
	  if (c == ',' || c == '\n' || c == ';' || c == '!')
	    {
	      /* Check for CHARACTER with no length parameter.  */
	      if (ts.type == BT_CHARACTER && !ts.u.cl)
		{
		  ts.kind = gfc_default_character_kind;
		  ts.u.cl = gfc_new_charlen (gfc_current_ns, NULL);
		  ts.u.cl->length = gfc_get_int_expr (gfc_default_integer_kind,
						      NULL, 1);
		}

	      /* Record the Successful match.  */
	      if (!gfc_merge_new_implicit (&ts))
		return MATCH_ERROR;
	      if (c == ',')
		c = gfc_next_ascii_char ();
	      else if (gfc_match_eos () == MATCH_ERROR)
		goto error;
	      continue;
	    }

	  gfc_current_locus = cur_loc;
	}

      /* Discard the (incorrectly) matched range.  */
      gfc_clear_new_implicit ();

      /* Last chance -- check <TYPE> <SELECTOR> (<RANGE>).  */
      if (ts.type == BT_CHARACTER)
	m = gfc_match_char_spec (&ts);
      else
	{
	  m = gfc_match_kind_spec (&ts, false);
	  if (m == MATCH_NO)
	    {
	      m = gfc_match_old_kind_spec (&ts);
	      if (m == MATCH_ERROR)
		goto error;
	      if (m == MATCH_NO)
		goto syntax;
	    }
	}
      if (m == MATCH_ERROR)
	goto error;

      m = match_implicit_range ();
      if (m == MATCH_ERROR)
	goto error;
      if (m == MATCH_NO)
	goto syntax;

      gfc_gobble_whitespace ();
      c = gfc_next_ascii_char ();
      if (c != ',' && gfc_match_eos () != MATCH_YES)
	goto syntax;

      if (!gfc_merge_new_implicit (&ts))
	return MATCH_ERROR;
    }
  while (c == ',');

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_IMPLICIT);

error:
  return MATCH_ERROR;
}


match
gfc_match_import (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  match m;
  gfc_symbol *sym;
  gfc_symtree *st;

  if (gfc_current_ns->proc_name == NULL
      || gfc_current_ns->proc_name->attr.if_source != IFSRC_IFBODY)
    {
      gfc_error ("IMPORT statement at %C only permitted in "
		 "an INTERFACE body");
      return MATCH_ERROR;
    }

  if (gfc_current_ns->proc_name->attr.module_procedure)
    {
      gfc_error ("F2008: C1210 IMPORT statement at %C is not permitted "
		 "in a module procedure interface body");
      return MATCH_ERROR;
    }

  if (!gfc_notify_std (GFC_STD_F2003, "IMPORT statement at %C"))
    return MATCH_ERROR;

  if (gfc_match_eos () == MATCH_YES)
    {
      /* All host variables should be imported.  */
      gfc_current_ns->has_import_set = 1;
      return MATCH_YES;
    }

  if (gfc_match (" ::") == MATCH_YES)
    {
      if (gfc_match_eos () == MATCH_YES)
	{
	   gfc_error ("Expecting list of named entities at %C");
	   return MATCH_ERROR;
	}
    }

  for(;;)
    {
      sym = NULL;
      m = gfc_match (" %n", name);
      switch (m)
	{
	case MATCH_YES:
	  if (gfc_current_ns->parent !=  NULL
	      && gfc_find_symbol (name, gfc_current_ns->parent, 1, &sym))
	    {
	       gfc_error ("Type name %qs at %C is ambiguous", name);
	       return MATCH_ERROR;
	    }
	  else if (!sym && gfc_current_ns->proc_name->ns->parent !=  NULL
		   && gfc_find_symbol (name,
				       gfc_current_ns->proc_name->ns->parent,
				       1, &sym))
	    {
	       gfc_error ("Type name %qs at %C is ambiguous", name);
	       return MATCH_ERROR;
	    }

	  if (sym == NULL)
	    {
	      gfc_error ("Cannot IMPORT %qs from host scoping unit "
			 "at %C - does not exist.", name);
	      return MATCH_ERROR;
	    }

	  if (gfc_find_symtree (gfc_current_ns->sym_root, name))
	    {
	      gfc_warning (0, "%qs is already IMPORTed from host scoping unit "
			   "at %C", name);
	      goto next_item;
	    }

	  st = gfc_new_symtree (&gfc_current_ns->sym_root, name);
	  st->n.sym = sym;
	  sym->refs++;
	  sym->attr.imported = 1;

	  if (sym->attr.generic && (sym = gfc_find_dt_in_generic (sym)))
	    {
	      /* The actual derived type is stored in a symtree with the first
		 letter of the name capitalized; the symtree with the all
		 lower-case name contains the associated generic function.  */
	      st = gfc_new_symtree (&gfc_current_ns->sym_root,
                                    gfc_dt_upper_string (name));
	      st->n.sym = sym;
	      sym->refs++;
	      sym->attr.imported = 1;
	    }

	  goto next_item;

	case MATCH_NO:
	  break;

	case MATCH_ERROR:
	  return MATCH_ERROR;
	}

    next_item:
      if (gfc_match_eos () == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in IMPORT statement at %C");
  return MATCH_ERROR;
}


/* A minimal implementation of gfc_match without whitespace, escape
   characters or variable arguments.  Returns true if the next
   characters match the TARGET template exactly.  */

static bool
match_string_p (const char *target)
{
  const char *p;

  for (p = target; *p; p++)
    if ((char) gfc_next_ascii_char () != *p)
      return false;
  return true;
}

/* Matches an attribute specification including array specs.  If
   successful, leaves the variables current_attr and current_as
   holding the specification.  Also sets the colon_seen variable for
   later use by matchers associated with initializations.

   This subroutine is a little tricky in the sense that we don't know
   if we really have an attr-spec until we hit the double colon.
   Until that time, we can only return MATCH_NO.  This forces us to
   check for duplicate specification at this level.  */

static match
match_attr_spec (void)
{
  /* Modifiers that can exist in a type statement.  */
  enum
  { GFC_DECL_BEGIN = 0,
    DECL_ALLOCATABLE = GFC_DECL_BEGIN, DECL_DIMENSION, DECL_EXTERNAL,
    DECL_IN, DECL_OUT, DECL_INOUT, DECL_INTRINSIC, DECL_OPTIONAL,
    DECL_PARAMETER, DECL_POINTER, DECL_PROTECTED, DECL_PRIVATE,
    DECL_STATIC, DECL_AUTOMATIC,
    DECL_PUBLIC, DECL_SAVE, DECL_TARGET, DECL_VALUE, DECL_VOLATILE,
    DECL_IS_BIND_C, DECL_CODIMENSION, DECL_ASYNCHRONOUS, DECL_CONTIGUOUS,
    DECL_LEN, DECL_KIND, DECL_NONE, GFC_DECL_END /* Sentinel */
  };

/* GFC_DECL_END is the sentinel, index starts at 0.  */
#define NUM_DECL GFC_DECL_END

  locus start, seen_at[NUM_DECL];
  int seen[NUM_DECL];
  unsigned int d;
  const char *attr;
  match m;
  bool t;

  gfc_clear_attr (&current_attr);
  start = gfc_current_locus;

  current_as = NULL;
  colon_seen = 0;
  attr_seen = 0;

  /* See if we get all of the keywords up to the final double colon.  */
  for (d = GFC_DECL_BEGIN; d != GFC_DECL_END; d++)
    seen[d] = 0;

  for (;;)
    {
      char ch;

      d = DECL_NONE;
      gfc_gobble_whitespace ();

      ch = gfc_next_ascii_char ();
      if (ch == ':')
	{
	  /* This is the successful exit condition for the loop.  */
	  if (gfc_next_ascii_char () == ':')
	    break;
	}
      else if (ch == ',')
	{
	  gfc_gobble_whitespace ();
	  switch (gfc_peek_ascii_char ())
	    {
	    case 'a':
	      gfc_next_ascii_char ();
	      switch (gfc_next_ascii_char ())
		{
		case 'l':
		  if (match_string_p ("locatable"))
		    {
		      /* Matched "allocatable".  */
		      d = DECL_ALLOCATABLE;
		    }
		  break;

		case 's':
		  if (match_string_p ("ynchronous"))
		    {
		      /* Matched "asynchronous".  */
		      d = DECL_ASYNCHRONOUS;
		    }
		  break;

		case 'u':
		  if (match_string_p ("tomatic"))
		    {
		      /* Matched "automatic".  */
		      d = DECL_AUTOMATIC;
		    }
		  break;
		}
	      break;

	    case 'b':
	      /* Try and match the bind(c).  */
	      m = gfc_match_bind_c (NULL, true);
	      if (m == MATCH_YES)
		d = DECL_IS_BIND_C;
	      else if (m == MATCH_ERROR)
		goto cleanup;
	      break;

	    case 'c':
	      gfc_next_ascii_char ();
	      if ('o' != gfc_next_ascii_char ())
		break;
	      switch (gfc_next_ascii_char ())
		{
		case 'd':
		  if (match_string_p ("imension"))
		    {
		      d = DECL_CODIMENSION;
		      break;
		    }
		  /* FALLTHRU */
		case 'n':
		  if (match_string_p ("tiguous"))
		    {
		      d = DECL_CONTIGUOUS;
		      break;
		    }
		}
	      break;

	    case 'd':
	      if (match_string_p ("dimension"))
		d = DECL_DIMENSION;
	      break;

	    case 'e':
	      if (match_string_p ("external"))
		d = DECL_EXTERNAL;
	      break;

	    case 'i':
	      if (match_string_p ("int"))
		{
		  ch = gfc_next_ascii_char ();
		  if (ch == 'e')
		    {
		      if (match_string_p ("nt"))
			{
			  /* Matched "intent".  */
			  /* TODO: Call match_intent_spec from here.  */
			  if (gfc_match (" ( in out )") == MATCH_YES)
			    d = DECL_INOUT;
			  else if (gfc_match (" ( in )") == MATCH_YES)
			    d = DECL_IN;
			  else if (gfc_match (" ( out )") == MATCH_YES)
			    d = DECL_OUT;
			}
		    }
		  else if (ch == 'r')
		    {
		      if (match_string_p ("insic"))
			{
			  /* Matched "intrinsic".  */
			  d = DECL_INTRINSIC;
			}
		    }
		}
	      break;

	    case 'k':
	      if (match_string_p ("kind"))
		d = DECL_KIND;
	      break;

	    case 'l':
	      if (match_string_p ("len"))
		d = DECL_LEN;
	      break;

	    case 'o':
	      if (match_string_p ("optional"))
		d = DECL_OPTIONAL;
	      break;

	    case 'p':
	      gfc_next_ascii_char ();
	      switch (gfc_next_ascii_char ())
		{
		case 'a':
		  if (match_string_p ("rameter"))
		    {
		      /* Matched "parameter".  */
		      d = DECL_PARAMETER;
		    }
		  break;

		case 'o':
		  if (match_string_p ("inter"))
		    {
		      /* Matched "pointer".  */
		      d = DECL_POINTER;
		    }
		  break;

		case 'r':
		  ch = gfc_next_ascii_char ();
		  if (ch == 'i')
		    {
		      if (match_string_p ("vate"))
			{
			  /* Matched "private".  */
			  d = DECL_PRIVATE;
			}
		    }
		  else if (ch == 'o')
		    {
		      if (match_string_p ("tected"))
			{
			  /* Matched "protected".  */
			  d = DECL_PROTECTED;
			}
		    }
		  break;

		case 'u':
		  if (match_string_p ("blic"))
		    {
		      /* Matched "public".  */
		      d = DECL_PUBLIC;
		    }
		  break;
		}
	      break;

	    case 's':
	      gfc_next_ascii_char ();
	      switch (gfc_next_ascii_char ())
		{
		  case 'a':
		    if (match_string_p ("ve"))
		      {
			/* Matched "save".  */
			d = DECL_SAVE;
		      }
		    break;

		  case 't':
		    if (match_string_p ("atic"))
		      {
			/* Matched "static".  */
			d = DECL_STATIC;
		      }
		    break;
		}
	      break;

	    case 't':
	      if (match_string_p ("target"))
		d = DECL_TARGET;
	      break;

	    case 'v':
	      gfc_next_ascii_char ();
	      ch = gfc_next_ascii_char ();
	      if (ch == 'a')
		{
		  if (match_string_p ("lue"))
		    {
		      /* Matched "value".  */
		      d = DECL_VALUE;
		    }
		}
	      else if (ch == 'o')
		{
		  if (match_string_p ("latile"))
		    {
		      /* Matched "volatile".  */
		      d = DECL_VOLATILE;
		    }
		}
	      break;
	    }
	}

      /* No double colon and no recognizable decl_type, so assume that
	 we've been looking at something else the whole time.  */
      if (d == DECL_NONE)
	{
	  m = MATCH_NO;
	  goto cleanup;
	}

      /* Check to make sure any parens are paired up correctly.  */
      if (gfc_match_parens () == MATCH_ERROR)
	{
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      seen[d]++;
      seen_at[d] = gfc_current_locus;

      if (d == DECL_DIMENSION || d == DECL_CODIMENSION)
	{
	  gfc_array_spec *as = NULL;

	  m = gfc_match_array_spec (&as, d == DECL_DIMENSION,
				    d == DECL_CODIMENSION);

	  if (current_as == NULL)
	    current_as = as;
	  else if (m == MATCH_YES)
	    {
	      if (!merge_array_spec (as, current_as, false))
		m = MATCH_ERROR;
	      free (as);
	    }

	  if (m == MATCH_NO)
	    {
	      if (d == DECL_CODIMENSION)
		gfc_error ("Missing codimension specification at %C");
	      else
		gfc_error ("Missing dimension specification at %C");
	      m = MATCH_ERROR;
	    }

	  if (m == MATCH_ERROR)
	    goto cleanup;
	}
    }

  /* Since we've seen a double colon, we have to be looking at an
     attr-spec.  This means that we can now issue errors.  */
  for (d = GFC_DECL_BEGIN; d != GFC_DECL_END; d++)
    if (seen[d] > 1)
      {
	switch (d)
	  {
	  case DECL_ALLOCATABLE:
	    attr = "ALLOCATABLE";
	    break;
	  case DECL_ASYNCHRONOUS:
	    attr = "ASYNCHRONOUS";
	    break;
	  case DECL_CODIMENSION:
	    attr = "CODIMENSION";
	    break;
	  case DECL_CONTIGUOUS:
	    attr = "CONTIGUOUS";
	    break;
	  case DECL_DIMENSION:
	    attr = "DIMENSION";
	    break;
	  case DECL_EXTERNAL:
	    attr = "EXTERNAL";
	    break;
	  case DECL_IN:
	    attr = "INTENT (IN)";
	    break;
	  case DECL_OUT:
	    attr = "INTENT (OUT)";
	    break;
	  case DECL_INOUT:
	    attr = "INTENT (IN OUT)";
	    break;
	  case DECL_INTRINSIC:
	    attr = "INTRINSIC";
	    break;
	  case DECL_OPTIONAL:
	    attr = "OPTIONAL";
	    break;
	  case DECL_KIND:
	    attr = "KIND";
	    break;
	  case DECL_LEN:
	    attr = "LEN";
	    break;
	  case DECL_PARAMETER:
	    attr = "PARAMETER";
	    break;
	  case DECL_POINTER:
	    attr = "POINTER";
	    break;
	  case DECL_PROTECTED:
	    attr = "PROTECTED";
	    break;
	  case DECL_PRIVATE:
	    attr = "PRIVATE";
	    break;
	  case DECL_PUBLIC:
	    attr = "PUBLIC";
	    break;
	  case DECL_SAVE:
	    attr = "SAVE";
	    break;
	  case DECL_STATIC:
	    attr = "STATIC";
	    break;
	  case DECL_AUTOMATIC:
	    attr = "AUTOMATIC";
	    break;
	  case DECL_TARGET:
	    attr = "TARGET";
	    break;
          case DECL_IS_BIND_C:
            attr = "IS_BIND_C";
            break;
          case DECL_VALUE:
            attr = "VALUE";
            break;
	  case DECL_VOLATILE:
	    attr = "VOLATILE";
	    break;
	  default:
	    attr = NULL;	/* This shouldn't happen.  */
	  }

	gfc_error ("Duplicate %s attribute at %L", attr, &seen_at[d]);
	m = MATCH_ERROR;
	goto cleanup;
      }

  /* Now that we've dealt with duplicate attributes, add the attributes
     to the current attribute.  */
  for (d = GFC_DECL_BEGIN; d != GFC_DECL_END; d++)
    {
      if (seen[d] == 0)
	continue;
      else
        attr_seen = 1;

      if ((d == DECL_STATIC || d == DECL_AUTOMATIC)
	  && !flag_dec_static)
	{
	  gfc_error ("%s at %L is a DEC extension, enable with "
		     "%<-fdec-static%>",
		     d == DECL_STATIC ? "STATIC" : "AUTOMATIC", &seen_at[d]);
	  m = MATCH_ERROR;
	  goto cleanup;
	}
      /* Allow SAVE with STATIC, but don't complain.  */
      if (d == DECL_STATIC && seen[DECL_SAVE])
	continue;

      if (gfc_current_state () == COMP_DERIVED
	  && d != DECL_DIMENSION && d != DECL_CODIMENSION
	  && d != DECL_POINTER   && d != DECL_PRIVATE
	  && d != DECL_PUBLIC && d != DECL_CONTIGUOUS && d != DECL_NONE)
	{
	  if (d == DECL_ALLOCATABLE)
	    {
	      if (!gfc_notify_std (GFC_STD_F2003, "ALLOCATABLE "
				   "attribute at %C in a TYPE definition"))
		{
		  m = MATCH_ERROR;
		  goto cleanup;
		}
	    }
	  else if (d == DECL_KIND)
	    {
	      if (!gfc_notify_std (GFC_STD_F2003, "KIND "
				   "attribute at %C in a TYPE definition"))
		{
		  m = MATCH_ERROR;
		  goto cleanup;
		}
	      if (current_ts.type != BT_INTEGER)
		{
		  gfc_error ("Component with KIND attribute at %C must be "
			     "INTEGER");
		  m = MATCH_ERROR;
		  goto cleanup;
		}
	      if (current_ts.kind != gfc_default_integer_kind)
		{
		  gfc_error ("Component with KIND attribute at %C must be "
			     "default integer kind (%d)",
			      gfc_default_integer_kind);
		  m = MATCH_ERROR;
		  goto cleanup;
		}
	    }
	  else if (d == DECL_LEN)
	    {
	      if (!gfc_notify_std (GFC_STD_F2003, "LEN "
				   "attribute at %C in a TYPE definition"))
		{
		  m = MATCH_ERROR;
		  goto cleanup;
		}
	      if (current_ts.type != BT_INTEGER)
		{
		  gfc_error ("Component with LEN attribute at %C must be "
			     "INTEGER");
		  m = MATCH_ERROR;
		  goto cleanup;
		}
	      if (current_ts.kind != gfc_default_integer_kind)
		{
		  gfc_error ("Component with LEN attribute at %C must be "
			     "default integer kind (%d)",
			      gfc_default_integer_kind);
		  m = MATCH_ERROR;
		  goto cleanup;
		}
	    }
	  else
	    {
	      gfc_error ("Attribute at %L is not allowed in a TYPE definition",
			 &seen_at[d]);
	      m = MATCH_ERROR;
	      goto cleanup;
	    }
	}

      if ((d == DECL_PRIVATE || d == DECL_PUBLIC)
	  && gfc_current_state () != COMP_MODULE)
	{
	  if (d == DECL_PRIVATE)
	    attr = "PRIVATE";
	  else
	    attr = "PUBLIC";
	  if (gfc_current_state () == COMP_DERIVED
	      && gfc_state_stack->previous
	      && gfc_state_stack->previous->state == COMP_MODULE)
	    {
	      if (!gfc_notify_std (GFC_STD_F2003, "Attribute %s "
				   "at %L in a TYPE definition", attr,
				   &seen_at[d]))
		{
		  m = MATCH_ERROR;
		  goto cleanup;
		}
	    }
	  else
	    {
	      gfc_error ("%s attribute at %L is not allowed outside of the "
			 "specification part of a module", attr, &seen_at[d]);
	      m = MATCH_ERROR;
	      goto cleanup;
	    }
	}

      if (gfc_current_state () != COMP_DERIVED
	  && (d == DECL_KIND || d == DECL_LEN))
	{
	  gfc_error ("Attribute at %L is not allowed outside a TYPE "
		     "definition", &seen_at[d]);
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      switch (d)
	{
	case DECL_ALLOCATABLE:
	  t = gfc_add_allocatable (&current_attr, &seen_at[d]);
	  break;

	case DECL_ASYNCHRONOUS:
	  if (!gfc_notify_std (GFC_STD_F2003, "ASYNCHRONOUS attribute at %C"))
	    t = false;
	  else
	    t = gfc_add_asynchronous (&current_attr, NULL, &seen_at[d]);
	  break;

	case DECL_CODIMENSION:
	  t = gfc_add_codimension (&current_attr, NULL, &seen_at[d]);
	  break;

	case DECL_CONTIGUOUS:
	  if (!gfc_notify_std (GFC_STD_F2008, "CONTIGUOUS attribute at %C"))
	    t = false;
	  else
	    t = gfc_add_contiguous (&current_attr, NULL, &seen_at[d]);
	  break;

	case DECL_DIMENSION:
	  t = gfc_add_dimension (&current_attr, NULL, &seen_at[d]);
	  break;

	case DECL_EXTERNAL:
	  t = gfc_add_external (&current_attr, &seen_at[d]);
	  break;

	case DECL_IN:
	  t = gfc_add_intent (&current_attr, INTENT_IN, &seen_at[d]);
	  break;

	case DECL_OUT:
	  t = gfc_add_intent (&current_attr, INTENT_OUT, &seen_at[d]);
	  break;

	case DECL_INOUT:
	  t = gfc_add_intent (&current_attr, INTENT_INOUT, &seen_at[d]);
	  break;

	case DECL_INTRINSIC:
	  t = gfc_add_intrinsic (&current_attr, &seen_at[d]);
	  break;

	case DECL_OPTIONAL:
	  t = gfc_add_optional (&current_attr, &seen_at[d]);
	  break;

	case DECL_KIND:
	  t = gfc_add_kind (&current_attr, &seen_at[d]);
	  break;

	case DECL_LEN:
	  t = gfc_add_len (&current_attr, &seen_at[d]);
	  break;

	case DECL_PARAMETER:
	  t = gfc_add_flavor (&current_attr, FL_PARAMETER, NULL, &seen_at[d]);
	  break;

	case DECL_POINTER:
	  t = gfc_add_pointer (&current_attr, &seen_at[d]);
	  break;

	case DECL_PROTECTED:
	  if (gfc_current_state () != COMP_MODULE
	      || (gfc_current_ns->proc_name
		  && gfc_current_ns->proc_name->attr.flavor != FL_MODULE))
	    {
	       gfc_error ("PROTECTED at %C only allowed in specification "
			  "part of a module");
	       t = false;
	       break;
	    }

	  if (!gfc_notify_std (GFC_STD_F2003, "PROTECTED attribute at %C"))
	    t = false;
	  else
	    t = gfc_add_protected (&current_attr, NULL, &seen_at[d]);
	  break;

	case DECL_PRIVATE:
	  t = gfc_add_access (&current_attr, ACCESS_PRIVATE, NULL,
			      &seen_at[d]);
	  break;

	case DECL_PUBLIC:
	  t = gfc_add_access (&current_attr, ACCESS_PUBLIC, NULL,
			      &seen_at[d]);
	  break;

	case DECL_STATIC:
	case DECL_SAVE:
	  t = gfc_add_save (&current_attr, SAVE_EXPLICIT, NULL, &seen_at[d]);
	  break;

	case DECL_AUTOMATIC:
	  t = gfc_add_automatic (&current_attr, NULL, &seen_at[d]);
	  break;

	case DECL_TARGET:
	  t = gfc_add_target (&current_attr, &seen_at[d]);
	  break;

        case DECL_IS_BIND_C:
           t = gfc_add_is_bind_c(&current_attr, NULL, &seen_at[d], 0);
           break;

	case DECL_VALUE:
	  if (!gfc_notify_std (GFC_STD_F2003, "VALUE attribute at %C"))
	    t = false;
	  else
	    t = gfc_add_value (&current_attr, NULL, &seen_at[d]);
	  break;

	case DECL_VOLATILE:
	  if (!gfc_notify_std (GFC_STD_F2003, "VOLATILE attribute at %C"))
	    t = false;
	  else
	    t = gfc_add_volatile (&current_attr, NULL, &seen_at[d]);
	  break;

	default:
	  gfc_internal_error ("match_attr_spec(): Bad attribute");
	}

      if (!t)
	{
	  m = MATCH_ERROR;
	  goto cleanup;
	}
    }

  /* Since Fortran 2008 module variables implicitly have the SAVE attribute.  */
  if ((gfc_current_state () == COMP_MODULE
       || gfc_current_state () == COMP_SUBMODULE)
      && !current_attr.save
      && (gfc_option.allow_std & GFC_STD_F2008) != 0)
    current_attr.save = SAVE_IMPLICIT;

  colon_seen = 1;
  return MATCH_YES;

cleanup:
  gfc_current_locus = start;
  gfc_free_array_spec (current_as);
  current_as = NULL;
  attr_seen = 0;
  return m;
}


/* Set the binding label, dest_label, either with the binding label
   stored in the given gfc_typespec, ts, or if none was provided, it
   will be the symbol name in all lower case, as required by the draft
   (J3/04-007, section 15.4.1).  If a binding label was given and
   there is more than one argument (num_idents), it is an error.  */

static bool
set_binding_label (const char **dest_label, const char *sym_name,
		   int num_idents)
{
  if (num_idents > 1 && has_name_equals)
    {
      gfc_error ("Multiple identifiers provided with "
		 "single NAME= specifier at %C");
      return false;
    }

  if (curr_binding_label)
    /* Binding label given; store in temp holder till have sym.  */
    *dest_label = curr_binding_label;
  else
    {
      /* No binding label given, and the NAME= specifier did not exist,
         which means there was no NAME="".  */
      if (sym_name != NULL && has_name_equals == 0)
        *dest_label = IDENTIFIER_POINTER (get_identifier (sym_name));
    }

  return true;
}


/* Set the status of the given common block as being BIND(C) or not,
   depending on the given parameter, is_bind_c.  */

void
set_com_block_bind_c (gfc_common_head *com_block, int is_bind_c)
{
  com_block->is_bind_c = is_bind_c;
  return;
}


/* Verify that the given gfc_typespec is for a C interoperable type.  */

bool
gfc_verify_c_interop (gfc_typespec *ts)
{
  if (ts->type == BT_DERIVED && ts->u.derived != NULL)
    return (ts->u.derived->ts.is_c_interop || ts->u.derived->attr.is_bind_c)
	   ? true : false;
  else if (ts->type == BT_CLASS)
    return false;
  else if (ts->is_c_interop != 1 && ts->type != BT_ASSUMED)
    return false;

  return true;
}


/* Verify that the variables of a given common block, which has been
   defined with the attribute specifier bind(c), to be of a C
   interoperable type.  Errors will be reported here, if
   encountered.  */

bool
verify_com_block_vars_c_interop (gfc_common_head *com_block)
{
  gfc_symbol *curr_sym = NULL;
  bool retval = true;

  curr_sym = com_block->head;

  /* Make sure we have at least one symbol.  */
  if (curr_sym == NULL)
    return retval;

  /* Here we know we have a symbol, so we'll execute this loop
     at least once.  */
  do
    {
      /* The second to last param, 1, says this is in a common block.  */
      retval = verify_bind_c_sym (curr_sym, &(curr_sym->ts), 1, com_block);
      curr_sym = curr_sym->common_next;
    } while (curr_sym != NULL);

  return retval;
}


/* Verify that a given BIND(C) symbol is C interoperable.  If it is not,
   an appropriate error message is reported.  */

bool
verify_bind_c_sym (gfc_symbol *tmp_sym, gfc_typespec *ts,
                   int is_in_common, gfc_common_head *com_block)
{
  bool bind_c_function = false;
  bool retval = true;

  if (tmp_sym->attr.function && tmp_sym->attr.is_bind_c)
    bind_c_function = true;

  if (tmp_sym->attr.function && tmp_sym->result != NULL)
    {
      tmp_sym = tmp_sym->result;
      /* Make sure it wasn't an implicitly typed result.  */
      if (tmp_sym->attr.implicit_type && warn_c_binding_type)
	{
	  gfc_warning (OPT_Wc_binding_type,
		       "Implicitly declared BIND(C) function %qs at "
                       "%L may not be C interoperable", tmp_sym->name,
                       &tmp_sym->declared_at);
	  tmp_sym->ts.f90_type = tmp_sym->ts.type;
	  /* Mark it as C interoperable to prevent duplicate warnings.	*/
	  tmp_sym->ts.is_c_interop = 1;
	  tmp_sym->attr.is_c_interop = 1;
	}
    }

  /* Here, we know we have the bind(c) attribute, so if we have
     enough type info, then verify that it's a C interop kind.
     The info could be in the symbol already, or possibly still in
     the given ts (current_ts), so look in both.  */
  if (tmp_sym->ts.type != BT_UNKNOWN || ts->type != BT_UNKNOWN)
    {
      if (!gfc_verify_c_interop (&(tmp_sym->ts)))
	{
	  /* See if we're dealing with a sym in a common block or not.	*/
	  if (is_in_common == 1 && warn_c_binding_type)
	    {
	      gfc_warning (OPT_Wc_binding_type,
			   "Variable %qs in common block %qs at %L "
                           "may not be a C interoperable "
                           "kind though common block %qs is BIND(C)",
                           tmp_sym->name, com_block->name,
                           &(tmp_sym->declared_at), com_block->name);
	    }
	  else
	    {
              if (tmp_sym->ts.type == BT_DERIVED || ts->type == BT_DERIVED)
                gfc_error ("Type declaration %qs at %L is not C "
                           "interoperable but it is BIND(C)",
                           tmp_sym->name, &(tmp_sym->declared_at));
              else if (warn_c_binding_type)
                gfc_warning (OPT_Wc_binding_type, "Variable %qs at %L "
                             "may not be a C interoperable "
                             "kind but it is BIND(C)",
                             tmp_sym->name, &(tmp_sym->declared_at));
	    }
	}

      /* Variables declared w/in a common block can't be bind(c)
	 since there's no way for C to see these variables, so there's
	 semantically no reason for the attribute.  */
      if (is_in_common == 1 && tmp_sym->attr.is_bind_c == 1)
	{
	  gfc_error ("Variable %qs in common block %qs at "
		     "%L cannot be declared with BIND(C) "
		     "since it is not a global",
		     tmp_sym->name, com_block->name,
		     &(tmp_sym->declared_at));
	  retval = false;
	}

      /* Scalar variables that are bind(c) can not have the pointer
	 or allocatable attributes.  */
      if (tmp_sym->attr.is_bind_c == 1)
	{
	  if (tmp_sym->attr.pointer == 1)
	    {
	      gfc_error ("Variable %qs at %L cannot have both the "
			 "POINTER and BIND(C) attributes",
			 tmp_sym->name, &(tmp_sym->declared_at));
	      retval = false;
	    }

	  if (tmp_sym->attr.allocatable == 1)
	    {
	      gfc_error ("Variable %qs at %L cannot have both the "
			 "ALLOCATABLE and BIND(C) attributes",
			 tmp_sym->name, &(tmp_sym->declared_at));
	      retval = false;
	    }

        }

      /* If it is a BIND(C) function, make sure the return value is a
	 scalar value.  The previous tests in this function made sure
	 the type is interoperable.  */
      if (bind_c_function && tmp_sym->as != NULL)
	gfc_error ("Return type of BIND(C) function %qs at %L cannot "
		   "be an array", tmp_sym->name, &(tmp_sym->declared_at));

      /* BIND(C) functions can not return a character string.  */
      if (bind_c_function && tmp_sym->ts.type == BT_CHARACTER)
	if (tmp_sym->ts.u.cl == NULL || tmp_sym->ts.u.cl->length == NULL
	    || tmp_sym->ts.u.cl->length->expr_type != EXPR_CONSTANT
	    || mpz_cmp_si (tmp_sym->ts.u.cl->length->value.integer, 1) != 0)
	  gfc_error ("Return type of BIND(C) function %qs at %L cannot "
			 "be a character string", tmp_sym->name,
			 &(tmp_sym->declared_at));
    }

  /* See if the symbol has been marked as private.  If it has, make sure
     there is no binding label and warn the user if there is one.  */
  if (tmp_sym->attr.access == ACCESS_PRIVATE
      && tmp_sym->binding_label)
      /* Use gfc_warning_now because we won't say that the symbol fails
	 just because of this.	*/
      gfc_warning_now (0, "Symbol %qs at %L is marked PRIVATE but has been "
		       "given the binding label %qs", tmp_sym->name,
		       &(tmp_sym->declared_at), tmp_sym->binding_label);

  return retval;
}


/* Set the appropriate fields for a symbol that's been declared as
   BIND(C) (the is_bind_c flag and the binding label), and verify that
   the type is C interoperable.  Errors are reported by the functions
   used to set/test these fields.  */

bool
set_verify_bind_c_sym (gfc_symbol *tmp_sym, int num_idents)
{
  bool retval = true;

  /* TODO: Do we need to make sure the vars aren't marked private?  */

  /* Set the is_bind_c bit in symbol_attribute.  */
  gfc_add_is_bind_c (&(tmp_sym->attr), tmp_sym->name, &gfc_current_locus, 0);

  if (!set_binding_label (&tmp_sym->binding_label, tmp_sym->name, num_idents))
    return false;

  return retval;
}


/* Set the fields marking the given common block as BIND(C), including
   a binding label, and report any errors encountered.  */

bool
set_verify_bind_c_com_block (gfc_common_head *com_block, int num_idents)
{
  bool retval = true;

  /* destLabel, common name, typespec (which may have binding label).  */
  if (!set_binding_label (&com_block->binding_label, com_block->name,
			  num_idents))
    return false;

  /* Set the given common block (com_block) to being bind(c) (1).  */
  set_com_block_bind_c (com_block, 1);

  return retval;
}


/* Retrieve the list of one or more identifiers that the given bind(c)
   attribute applies to.  */

bool
get_bind_c_idents (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  int num_idents = 0;
  gfc_symbol *tmp_sym = NULL;
  match found_id;
  gfc_common_head *com_block = NULL;

  if (gfc_match_name (name) == MATCH_YES)
    {
      found_id = MATCH_YES;
      gfc_get_ha_symbol (name, &tmp_sym);
    }
  else if (match_common_name (name) == MATCH_YES)
    {
      found_id = MATCH_YES;
      com_block = gfc_get_common (name, 0);
    }
  else
    {
      gfc_error ("Need either entity or common block name for "
		 "attribute specification statement at %C");
      return false;
    }

  /* Save the current identifier and look for more.  */
  do
    {
      /* Increment the number of identifiers found for this spec stmt.  */
      num_idents++;

      /* Make sure we have a sym or com block, and verify that it can
	 be bind(c).  Set the appropriate field(s) and look for more
	 identifiers.  */
      if (tmp_sym != NULL || com_block != NULL)
        {
	  if (tmp_sym != NULL)
	    {
	      if (!set_verify_bind_c_sym (tmp_sym, num_idents))
		return false;
	    }
	  else
	    {
	      if (!set_verify_bind_c_com_block (com_block, num_idents))
		return false;
	    }

	  /* Look to see if we have another identifier.  */
	  tmp_sym = NULL;
	  if (gfc_match_eos () == MATCH_YES)
	    found_id = MATCH_NO;
	  else if (gfc_match_char (',') != MATCH_YES)
	    found_id = MATCH_NO;
	  else if (gfc_match_name (name) == MATCH_YES)
	    {
	      found_id = MATCH_YES;
	      gfc_get_ha_symbol (name, &tmp_sym);
	    }
	  else if (match_common_name (name) == MATCH_YES)
	    {
	      found_id = MATCH_YES;
	      com_block = gfc_get_common (name, 0);
	    }
	  else
	    {
	      gfc_error ("Missing entity or common block name for "
			 "attribute specification statement at %C");
	      return false;
	    }
	}
      else
	{
	  gfc_internal_error ("Missing symbol");
	}
    } while (found_id == MATCH_YES);

  /* if we get here we were successful */
  return true;
}


/* Try and match a BIND(C) attribute specification statement.  */

match
gfc_match_bind_c_stmt (void)
{
  match found_match = MATCH_NO;
  gfc_typespec *ts;

  ts = &current_ts;

  /* This may not be necessary.  */
  gfc_clear_ts (ts);
  /* Clear the temporary binding label holder.  */
  curr_binding_label = NULL;

  /* Look for the bind(c).  */
  found_match = gfc_match_bind_c (NULL, true);

  if (found_match == MATCH_YES)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "BIND(C) statement at %C"))
	return MATCH_ERROR;

      /* Look for the :: now, but it is not required.  */
      gfc_match (" :: ");

      /* Get the identifier(s) that needs to be updated.  This may need to
	 change to hand the flag(s) for the attr specified so all identifiers
	 found can have all appropriate parts updated (assuming that the same
	 spec stmt can have multiple attrs, such as both bind(c) and
	 allocatable...).  */
      if (!get_bind_c_idents ())
	/* Error message should have printed already.  */
	return MATCH_ERROR;
    }

  return found_match;
}


/* Match a data declaration statement.  */

match
gfc_match_data_decl (void)
{
  gfc_symbol *sym;
  match m;
  int elem;

  type_param_spec_list = NULL;
  decl_type_param_list = NULL;

  num_idents_on_line = 0;

  m = gfc_match_decl_type_spec (&current_ts, 0);
  if (m != MATCH_YES)
    return m;

  if ((current_ts.type == BT_DERIVED || current_ts.type == BT_CLASS)
	&& !gfc_comp_struct (gfc_current_state ()))
    {
      sym = gfc_use_derived (current_ts.u.derived);

      if (sym == NULL)
	{
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      current_ts.u.derived = sym;
    }

  m = match_attr_spec ();
  if (m == MATCH_ERROR)
    {
      m = MATCH_NO;
      goto cleanup;
    }

  if (current_ts.type == BT_CLASS
	&& current_ts.u.derived->attr.unlimited_polymorphic)
    goto ok;

  if ((current_ts.type == BT_DERIVED || current_ts.type == BT_CLASS)
      && current_ts.u.derived->components == NULL
      && !current_ts.u.derived->attr.zero_comp)
    {

      if (current_attr.pointer && gfc_comp_struct (gfc_current_state ()))
	goto ok;

      if (current_attr.allocatable && gfc_current_state () == COMP_DERIVED
	  && current_ts.u.derived == gfc_current_block ())
	goto ok;

      gfc_find_symbol (current_ts.u.derived->name,
		       current_ts.u.derived->ns, 1, &sym);

      /* Any symbol that we find had better be a type definition
	 which has its components defined, or be a structure definition
         actively being parsed.  */
      if (sym != NULL && gfc_fl_struct (sym->attr.flavor)
	  && (current_ts.u.derived->components != NULL
	      || current_ts.u.derived->attr.zero_comp
	      || current_ts.u.derived == gfc_new_block))
	goto ok;

      gfc_error ("Derived type at %C has not been previously defined "
		 "and so cannot appear in a derived type definition");
      m = MATCH_ERROR;
      goto cleanup;
    }

ok:
  /* If we have an old-style character declaration, and no new-style
     attribute specifications, then there a comma is optional between
     the type specification and the variable list.  */
  if (m == MATCH_NO && current_ts.type == BT_CHARACTER && old_char_selector)
    gfc_match_char (',');

  /* Give the types/attributes to symbols that follow. Give the element
     a number so that repeat character length expressions can be copied.  */
  elem = 1;
  for (;;)
    {
      num_idents_on_line++;
      m = variable_decl (elem++);
      if (m == MATCH_ERROR)
	goto cleanup;
      if (m == MATCH_NO)
	break;

      if (gfc_match_eos () == MATCH_YES)
	goto cleanup;
      if (gfc_match_char (',') != MATCH_YES)
	break;
    }

  if (!gfc_error_flag_test ())
    {
      /* An anonymous structure declaration is unambiguous; if we matched one
	 according to gfc_match_structure_decl, we need to return MATCH_YES
	 here to avoid confusing the remaining matchers, even if there was an
	 error during variable_decl.  We must flush any such errors.  Note this
	 causes the parser to gracefully continue parsing the remaining input
	 as a structure body, which likely follows.  */
      if (current_ts.type == BT_DERIVED && current_ts.u.derived
	  && gfc_fl_struct (current_ts.u.derived->attr.flavor))
	{
	  gfc_error_now ("Syntax error in anonymous structure declaration"
			 " at %C");
	  /* Skip the bad variable_decl and line up for the start of the
	     structure body.  */
	  gfc_error_recovery ();
	  m = MATCH_YES;
	  goto cleanup;
	}

      gfc_error ("Syntax error in data declaration at %C");
    }

  m = MATCH_ERROR;

  gfc_free_data_all (gfc_current_ns);

cleanup:
  if (saved_kind_expr)
    gfc_free_expr (saved_kind_expr);
  if (type_param_spec_list)
    gfc_free_actual_arglist (type_param_spec_list);
  if (decl_type_param_list)
    gfc_free_actual_arglist (decl_type_param_list);
  saved_kind_expr = NULL;
  gfc_free_array_spec (current_as);
  current_as = NULL;
  return m;
}


/* Match a prefix associated with a function or subroutine
   declaration.  If the typespec pointer is nonnull, then a typespec
   can be matched.  Note that if nothing matches, MATCH_YES is
   returned (the null string was matched).  */

match
gfc_match_prefix (gfc_typespec *ts)
{
  bool seen_type;
  bool seen_impure;
  bool found_prefix;

  gfc_clear_attr (&current_attr);
  seen_type = false;
  seen_impure = false;

  gcc_assert (!gfc_matching_prefix);
  gfc_matching_prefix = true;

  do
    {
      found_prefix = false;

      /* MODULE is a prefix like PURE, ELEMENTAL, etc., having a
	 corresponding attribute seems natural and distinguishes these
	 procedures from procedure types of PROC_MODULE, which these are
	 as well.  */
      if (gfc_match ("module% ") == MATCH_YES)
	{
	  if (!gfc_notify_std (GFC_STD_F2008, "MODULE prefix at %C"))
	    goto error;

	  current_attr.module_procedure = 1;
	  found_prefix = true;
	}

      if (!seen_type && ts != NULL
	  && gfc_match_decl_type_spec (ts, 0) == MATCH_YES
	  && gfc_match_space () == MATCH_YES)
	{

	  seen_type = true;
	  found_prefix = true;
	}

      if (gfc_match ("elemental% ") == MATCH_YES)
	{
	  if (!gfc_add_elemental (&current_attr, NULL))
	    goto error;

	  found_prefix = true;
	}

      if (gfc_match ("pure% ") == MATCH_YES)
	{
	  if (!gfc_add_pure (&current_attr, NULL))
	    goto error;

	  found_prefix = true;
	}

      if (gfc_match ("recursive% ") == MATCH_YES)
	{
	  if (!gfc_add_recursive (&current_attr, NULL))
	    goto error;

	  found_prefix = true;
	}

      /* IMPURE is a somewhat special case, as it needs not set an actual
	 attribute but rather only prevents ELEMENTAL routines from being
	 automatically PURE.  */
      if (gfc_match ("impure% ") == MATCH_YES)
	{
	  if (!gfc_notify_std (GFC_STD_F2008, "IMPURE procedure at %C"))
	    goto error;

	  seen_impure = true;
	  found_prefix = true;
	}
    }
  while (found_prefix);

  /* IMPURE and PURE must not both appear, of course.  */
  if (seen_impure && current_attr.pure)
    {
      gfc_error ("PURE and IMPURE must not appear both at %C");
      goto error;
    }

  /* If IMPURE it not seen but the procedure is ELEMENTAL, mark it as PURE.  */
  if (!seen_impure && current_attr.elemental && !current_attr.pure)
    {
      if (!gfc_add_pure (&current_attr, NULL))
	goto error;
    }

  /* At this point, the next item is not a prefix.  */
  gcc_assert (gfc_matching_prefix);

  gfc_matching_prefix = false;
  return MATCH_YES;

error:
  gcc_assert (gfc_matching_prefix);
  gfc_matching_prefix = false;
  return MATCH_ERROR;
}


/* Copy attributes matched by gfc_match_prefix() to attributes on a symbol.  */

static bool
copy_prefix (symbol_attribute *dest, locus *where)
{
  if (dest->module_procedure)
    {
      if (current_attr.elemental)
	dest->elemental = 1;

      if (current_attr.pure)
	dest->pure = 1;

      if (current_attr.recursive)
	dest->recursive = 1;

      /* Module procedures are unusual in that the 'dest' is copied from
	 the interface declaration. However, this is an oportunity to
	 check that the submodule declaration is compliant with the
	 interface.  */
      if (dest->elemental && !current_attr.elemental)
	{
	  gfc_error ("ELEMENTAL prefix in MODULE PROCEDURE interface is "
		     "missing at %L", where);
	  return false;
	}

      if (dest->pure && !current_attr.pure)
	{
	  gfc_error ("PURE prefix in MODULE PROCEDURE interface is "
		     "missing at %L", where);
	  return false;
	}

      if (dest->recursive && !current_attr.recursive)
	{
	  gfc_error ("RECURSIVE prefix in MODULE PROCEDURE interface is "
		     "missing at %L", where);
	  return false;
	}

      return true;
    }

  if (current_attr.elemental && !gfc_add_elemental (dest, where))
    return false;

  if (current_attr.pure && !gfc_add_pure (dest, where))
    return false;

  if (current_attr.recursive && !gfc_add_recursive (dest, where))
    return false;

  return true;
}


/* Match a formal argument list or, if typeparam is true, a
   type_param_name_list.  */

match
gfc_match_formal_arglist (gfc_symbol *progname, int st_flag,
			  int null_flag, bool typeparam)
{
  gfc_formal_arglist *head, *tail, *p, *q;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  match m;
  gfc_formal_arglist *formal = NULL;

  head = tail = NULL;

  /* Keep the interface formal argument list and null it so that the
     matching for the new declaration can be done.  The numbers and
     names of the arguments are checked here. The interface formal
     arguments are retained in formal_arglist and the characteristics
     are compared in resolve.c(resolve_fl_procedure).  See the remark
     in get_proc_name about the eventual need to copy the formal_arglist
     and populate the formal namespace of the interface symbol.  */
  if (progname->attr.module_procedure
      && progname->attr.host_assoc)
    {
      formal = progname->formal;
      progname->formal = NULL;
    }

  if (gfc_match_char ('(') != MATCH_YES)
    {
      if (null_flag)
	goto ok;
      return MATCH_NO;
    }

  if (gfc_match_char (')') == MATCH_YES)
    goto ok;

  for (;;)
    {
      if (gfc_match_char ('*') == MATCH_YES)
	{
	  sym = NULL;
	  if (!gfc_notify_std (GFC_STD_F95_OBS, "Alternate-return argument "
			       "at %C"))
	    {
	      m = MATCH_ERROR;
	      goto cleanup;
	    }
	}
      else
	{
	  m = gfc_match_name (name);
	  if (m != MATCH_YES)
	    goto cleanup;

	  if (!typeparam && gfc_get_symbol (name, NULL, &sym))
	    goto cleanup;
	  else if (typeparam
		   && gfc_get_symbol (name, progname->f2k_derived, &sym))
	    goto cleanup;
	}

      p = gfc_get_formal_arglist ();

      if (head == NULL)
	head = tail = p;
      else
	{
	  tail->next = p;
	  tail = p;
	}

      tail->sym = sym;

      /* We don't add the VARIABLE flavor because the name could be a
	 dummy procedure.  We don't apply these attributes to formal
	 arguments of statement functions.  */
      if (sym != NULL && !st_flag
	  && (!gfc_add_dummy(&sym->attr, sym->name, NULL)
	      || !gfc_missing_attr (&sym->attr, NULL)))
	{
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      /* The name of a program unit can be in a different namespace,
	 so check for it explicitly.  After the statement is accepted,
	 the name is checked for especially in gfc_get_symbol().  */
      if (gfc_new_block != NULL && sym != NULL && !typeparam
	  && strcmp (sym->name, gfc_new_block->name) == 0)
	{
	  gfc_error ("Name %qs at %C is the name of the procedure",
		     sym->name);
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      if (gfc_match_char (')') == MATCH_YES)
	goto ok;

      m = gfc_match_char (',');
      if (m != MATCH_YES)
	{
	  if (typeparam)
	    gfc_error_now ("Expected parameter list in type declaration "
			   "at %C");
	  else
	    gfc_error ("Unexpected junk in formal argument list at %C");
	  goto cleanup;
	}
    }

ok:
  /* Check for duplicate symbols in the formal argument list.  */
  if (head != NULL)
    {
      for (p = head; p->next; p = p->next)
	{
	  if (p->sym == NULL)
	    continue;

	  for (q = p->next; q; q = q->next)
	    if (p->sym == q->sym)
	      {
		if (typeparam)
		  gfc_error_now ("Duplicate name %qs in parameter "
				 "list at %C", p->sym->name);
		else
		  gfc_error ("Duplicate symbol %qs in formal argument "
			     "list at %C", p->sym->name);

		m = MATCH_ERROR;
		goto cleanup;
	      }
	}
    }

  if (!gfc_add_explicit_interface (progname, IFSRC_DECL, head, NULL))
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  /* gfc_error_now used in following and return with MATCH_YES because
     doing otherwise results in a cascade of extraneous errors and in
     some cases an ICE in symbol.c(gfc_release_symbol).  */
  if (progname->attr.module_procedure && progname->attr.host_assoc)
    {
      bool arg_count_mismatch = false;

      if (!formal && head)
	arg_count_mismatch = true;

      /* Abbreviated module procedure declaration is not meant to have any
	 formal arguments!  */
      if (!progname->abr_modproc_decl && formal && !head)
	arg_count_mismatch = true;

      for (p = formal, q = head; p && q; p = p->next, q = q->next)
	{
	  if ((p->next != NULL && q->next == NULL)
	      || (p->next == NULL && q->next != NULL))
	    arg_count_mismatch = true;
	  else if ((p->sym == NULL && q->sym == NULL)
		    || strcmp (p->sym->name, q->sym->name) == 0)
	    continue;
	  else
	    gfc_error_now ("Mismatch in MODULE PROCEDURE formal "
			   "argument names (%s/%s) at %C",
			   p->sym->name, q->sym->name);
	}

      if (arg_count_mismatch)
	gfc_error_now ("Mismatch in number of MODULE PROCEDURE "
		       "formal arguments at %C");
    }

  return MATCH_YES;

cleanup:
  gfc_free_formal_arglist (head);
  return m;
}


/* Match a RESULT specification following a function declaration or
   ENTRY statement.  Also matches the end-of-statement.  */

static match
match_result (gfc_symbol *function, gfc_symbol **result)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *r;
  match m;

  if (gfc_match (" result (") != MATCH_YES)
    return MATCH_NO;

  m = gfc_match_name (name);
  if (m != MATCH_YES)
    return m;

  /* Get the right paren, and that's it because there could be the
     bind(c) attribute after the result clause.  */
  if (gfc_match_char (')') != MATCH_YES)
    {
     /* TODO: should report the missing right paren here.  */
      return MATCH_ERROR;
    }

  if (strcmp (function->name, name) == 0)
    {
      gfc_error ("RESULT variable at %C must be different than function name");
      return MATCH_ERROR;
    }

  if (gfc_get_symbol (name, NULL, &r))
    return MATCH_ERROR;

  if (!gfc_add_result (&r->attr, r->name, NULL))
    return MATCH_ERROR;

  *result = r;

  return MATCH_YES;
}


/* Match a function suffix, which could be a combination of a result
   clause and BIND(C), either one, or neither.  The draft does not
   require them to come in a specific order.  */

match
gfc_match_suffix (gfc_symbol *sym, gfc_symbol **result)
{
  match is_bind_c;   /* Found bind(c).  */
  match is_result;   /* Found result clause.  */
  match found_match; /* Status of whether we've found a good match.  */
  char peek_char;    /* Character we're going to peek at.  */
  bool allow_binding_name;

  /* Initialize to having found nothing.  */
  found_match = MATCH_NO;
  is_bind_c = MATCH_NO;
  is_result = MATCH_NO;

  /* Get the next char to narrow between result and bind(c).  */
  gfc_gobble_whitespace ();
  peek_char = gfc_peek_ascii_char ();

  /* C binding names are not allowed for internal procedures.  */
  if (gfc_current_state () == COMP_CONTAINS
      && sym->ns->proc_name->attr.flavor != FL_MODULE)
    allow_binding_name = false;
  else
    allow_binding_name = true;

  switch (peek_char)
    {
    case 'r':
      /* Look for result clause.  */
      is_result = match_result (sym, result);
      if (is_result == MATCH_YES)
	{
	  /* Now see if there is a bind(c) after it.  */
	  is_bind_c = gfc_match_bind_c (sym, allow_binding_name);
	  /* We've found the result clause and possibly bind(c).  */
	  found_match = MATCH_YES;
	}
      else
	/* This should only be MATCH_ERROR.  */
	found_match = is_result;
      break;
    case 'b':
      /* Look for bind(c) first.  */
      is_bind_c = gfc_match_bind_c (sym, allow_binding_name);
      if (is_bind_c == MATCH_YES)
	{
	  /* Now see if a result clause followed it.  */
	  is_result = match_result (sym, result);
	  found_match = MATCH_YES;
	}
      else
	{
	  /* Should only be a MATCH_ERROR if we get here after seeing 'b'.  */
	  found_match = MATCH_ERROR;
	}
      break;
    default:
      gfc_error ("Unexpected junk after function declaration at %C");
      found_match = MATCH_ERROR;
      break;
    }

  if (is_bind_c == MATCH_YES)
    {
      /* Fortran 2008 draft allows BIND(C) for internal procedures.  */
      if (gfc_current_state () == COMP_CONTAINS
	  && sym->ns->proc_name->attr.flavor != FL_MODULE
	  && !gfc_notify_std (GFC_STD_F2008, "BIND(C) attribute "
			      "at %L may not be specified for an internal "
			      "procedure", &gfc_current_locus))
	return MATCH_ERROR;

      if (!gfc_add_is_bind_c (&(sym->attr), sym->name, &gfc_current_locus, 1))
     	return MATCH_ERROR;
    }

  return found_match;
}


/* Procedure pointer return value without RESULT statement:
   Add "hidden" result variable named "ppr@".  */

static bool
add_hidden_procptr_result (gfc_symbol *sym)
{
  bool case1,case2;

  if (gfc_notification_std (GFC_STD_F2003) == ERROR)
    return false;

  /* First usage case: PROCEDURE and EXTERNAL statements.  */
  case1 = gfc_current_state () == COMP_FUNCTION && gfc_current_block ()
	  && strcmp (gfc_current_block ()->name, sym->name) == 0
	  && sym->attr.external;
  /* Second usage case: INTERFACE statements.  */
  case2 = gfc_current_state () == COMP_INTERFACE && gfc_state_stack->previous
	  && gfc_state_stack->previous->state == COMP_FUNCTION
	  && strcmp (gfc_state_stack->previous->sym->name, sym->name) == 0;

  if (case1 || case2)
    {
      gfc_symtree *stree;
      if (case1)
	gfc_get_sym_tree ("ppr@", gfc_current_ns, &stree, false);
      else if (case2)
	{
	  gfc_symtree *st2;
	  gfc_get_sym_tree ("ppr@", gfc_current_ns->parent, &stree, false);
	  st2 = gfc_new_symtree (&gfc_current_ns->sym_root, "ppr@");
	  st2->n.sym = stree->n.sym;
	  stree->n.sym->refs++;
	}
      sym->result = stree->n.sym;

      sym->result->attr.proc_pointer = sym->attr.proc_pointer;
      sym->result->attr.pointer = sym->attr.pointer;
      sym->result->attr.external = sym->attr.external;
      sym->result->attr.referenced = sym->attr.referenced;
      sym->result->ts = sym->ts;
      sym->attr.proc_pointer = 0;
      sym->attr.pointer = 0;
      sym->attr.external = 0;
      if (sym->result->attr.external && sym->result->attr.pointer)
	{
	  sym->result->attr.pointer = 0;
	  sym->result->attr.proc_pointer = 1;
	}

      return gfc_add_result (&sym->result->attr, sym->result->name, NULL);
    }
  /* POINTER after PROCEDURE/EXTERNAL/INTERFACE statement.  */
  else if (sym->attr.function && !sym->attr.external && sym->attr.pointer
	   && sym->result && sym->result != sym && sym->result->attr.external
	   && sym == gfc_current_ns->proc_name
	   && sym == sym->result->ns->proc_name
	   && strcmp ("ppr@", sym->result->name) == 0)
    {
      sym->result->attr.proc_pointer = 1;
      sym->attr.pointer = 0;
      return true;
    }
  else
    return false;
}


/* Match the interface for a PROCEDURE declaration,
   including brackets (R1212).  */

static match
match_procedure_interface (gfc_symbol **proc_if)
{
  match m;
  gfc_symtree *st;
  locus old_loc, entry_loc;
  gfc_namespace *old_ns = gfc_current_ns;
  char name[GFC_MAX_SYMBOL_LEN + 1];

  old_loc = entry_loc = gfc_current_locus;
  gfc_clear_ts (&current_ts);

  if (gfc_match (" (") != MATCH_YES)
    {
      gfc_current_locus = entry_loc;
      return MATCH_NO;
    }

  /* Get the type spec. for the procedure interface.  */
  old_loc = gfc_current_locus;
  m = gfc_match_decl_type_spec (&current_ts, 0);
  gfc_gobble_whitespace ();
  if (m == MATCH_YES || (m == MATCH_NO && gfc_peek_ascii_char () == ')'))
    goto got_ts;

  if (m == MATCH_ERROR)
    return m;

  /* Procedure interface is itself a procedure.  */
  gfc_current_locus = old_loc;
  m = gfc_match_name (name);

  /* First look to see if it is already accessible in the current
     namespace because it is use associated or contained.  */
  st = NULL;
  if (gfc_find_sym_tree (name, NULL, 0, &st))
    return MATCH_ERROR;

  /* If it is still not found, then try the parent namespace, if it
     exists and create the symbol there if it is still not found.  */
  if (gfc_current_ns->parent)
    gfc_current_ns = gfc_current_ns->parent;
  if (st == NULL && gfc_get_ha_sym_tree (name, &st))
    return MATCH_ERROR;

  gfc_current_ns = old_ns;
  *proc_if = st->n.sym;

  if (*proc_if)
    {
      (*proc_if)->refs++;
      /* Resolve interface if possible. That way, attr.procedure is only set
	 if it is declared by a later procedure-declaration-stmt, which is
	 invalid per F08:C1216 (cf. resolve_procedure_interface).  */
      while ((*proc_if)->ts.interface
	     && *proc_if != (*proc_if)->ts.interface)
	*proc_if = (*proc_if)->ts.interface;

      if ((*proc_if)->attr.flavor == FL_UNKNOWN
	  && (*proc_if)->ts.type == BT_UNKNOWN
	  && !gfc_add_flavor (&(*proc_if)->attr, FL_PROCEDURE,
			      (*proc_if)->name, NULL))
	return MATCH_ERROR;
    }

got_ts:
  if (gfc_match (" )") != MATCH_YES)
    {
      gfc_current_locus = entry_loc;
      return MATCH_NO;
    }

  return MATCH_YES;
}


/* Match a PROCEDURE declaration (R1211).  */

static match
match_procedure_decl (void)
{
  match m;
  gfc_symbol *sym, *proc_if = NULL;
  int num;
  gfc_expr *initializer = NULL;

  /* Parse interface (with brackets).  */
  m = match_procedure_interface (&proc_if);
  if (m != MATCH_YES)
    return m;

  /* Parse attributes (with colons).  */
  m = match_attr_spec();
  if (m == MATCH_ERROR)
    return MATCH_ERROR;

  if (proc_if && proc_if->attr.is_bind_c && !current_attr.is_bind_c)
    {
      current_attr.is_bind_c = 1;
      has_name_equals = 0;
      curr_binding_label = NULL;
    }

  /* Get procedure symbols.  */
  for(num=1;;num++)
    {
      m = gfc_match_symbol (&sym, 0);
      if (m == MATCH_NO)
	goto syntax;
      else if (m == MATCH_ERROR)
	return m;

      /* Add current_attr to the symbol attributes.  */
      if (!gfc_copy_attr (&sym->attr, &current_attr, NULL))
	return MATCH_ERROR;

      if (sym->attr.is_bind_c)
	{
	  /* Check for C1218.  */
	  if (!proc_if || !proc_if->attr.is_bind_c)
	    {
	      gfc_error ("BIND(C) attribute at %C requires "
			"an interface with BIND(C)");
	      return MATCH_ERROR;
	    }
	  /* Check for C1217.  */
	  if (has_name_equals && sym->attr.pointer)
	    {
	      gfc_error ("BIND(C) procedure with NAME may not have "
			"POINTER attribute at %C");
	      return MATCH_ERROR;
	    }
	  if (has_name_equals && sym->attr.dummy)
	    {
	      gfc_error ("Dummy procedure at %C may not have "
			"BIND(C) attribute with NAME");
	      return MATCH_ERROR;
	    }
	  /* Set binding label for BIND(C).  */
	  if (!set_binding_label (&sym->binding_label, sym->name, num))
	    return MATCH_ERROR;
	}

      if (!gfc_add_external (&sym->attr, NULL))
	return MATCH_ERROR;

      if (add_hidden_procptr_result (sym))
	sym = sym->result;

      if (!gfc_add_proc (&sym->attr, sym->name, NULL))
	return MATCH_ERROR;

      /* Set interface.  */
      if (proc_if != NULL)
	{
          if (sym->ts.type != BT_UNKNOWN)
	    {
	      gfc_error ("Procedure %qs at %L already has basic type of %s",
			 sym->name, &gfc_current_locus,
			 gfc_basic_typename (sym->ts.type));
	      return MATCH_ERROR;
	    }
	  sym->ts.interface = proc_if;
	  sym->attr.untyped = 1;
	  sym->attr.if_source = IFSRC_IFBODY;
	}
      else if (current_ts.type != BT_UNKNOWN)
	{
	  if (!gfc_add_type (sym, &current_ts, &gfc_current_locus))
	    return MATCH_ERROR;
	  sym->ts.interface = gfc_new_symbol ("", gfc_current_ns);
	  sym->ts.interface->ts = current_ts;
	  sym->ts.interface->attr.flavor = FL_PROCEDURE;
	  sym->ts.interface->attr.function = 1;
	  sym->attr.function = 1;
	  sym->attr.if_source = IFSRC_UNKNOWN;
	}

      if (gfc_match (" =>") == MATCH_YES)
	{
	  if (!current_attr.pointer)
	    {
	      gfc_error ("Initialization at %C isn't for a pointer variable");
	      m = MATCH_ERROR;
	      goto cleanup;
	    }

	  m = match_pointer_init (&initializer, 1);
	  if (m != MATCH_YES)
	    goto cleanup;

	  if (!add_init_expr_to_sym (sym->name, &initializer, &gfc_current_locus))
	    goto cleanup;

	}

      if (gfc_match_eos () == MATCH_YES)
	return MATCH_YES;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

syntax:
  gfc_error ("Syntax error in PROCEDURE statement at %C");
  return MATCH_ERROR;

cleanup:
  /* Free stuff up and return.  */
  gfc_free_expr (initializer);
  return m;
}


static match
match_binding_attributes (gfc_typebound_proc* ba, bool generic, bool ppc);


/* Match a procedure pointer component declaration (R445).  */

static match
match_ppc_decl (void)
{
  match m;
  gfc_symbol *proc_if = NULL;
  gfc_typespec ts;
  int num;
  gfc_component *c;
  gfc_expr *initializer = NULL;
  gfc_typebound_proc* tb;
  char name[GFC_MAX_SYMBOL_LEN + 1];

  /* Parse interface (with brackets).  */
  m = match_procedure_interface (&proc_if);
  if (m != MATCH_YES)
    goto syntax;

  /* Parse attributes.  */
  tb = XCNEW (gfc_typebound_proc);
  tb->where = gfc_current_locus;
  m = match_binding_attributes (tb, false, true);
  if (m == MATCH_ERROR)
    return m;

  gfc_clear_attr (&current_attr);
  current_attr.procedure = 1;
  current_attr.proc_pointer = 1;
  current_attr.access = tb->access;
  current_attr.flavor = FL_PROCEDURE;

  /* Match the colons (required).  */
  if (gfc_match (" ::") != MATCH_YES)
    {
      gfc_error ("Expected %<::%> after binding-attributes at %C");
      return MATCH_ERROR;
    }

  /* Check for C450.  */
  if (!tb->nopass && proc_if == NULL)
    {
      gfc_error("NOPASS or explicit interface required at %C");
      return MATCH_ERROR;
    }

  if (!gfc_notify_std (GFC_STD_F2003, "Procedure pointer component at %C"))
    return MATCH_ERROR;

  /* Match PPC names.  */
  ts = current_ts;
  for(num=1;;num++)
    {
      m = gfc_match_name (name);
      if (m == MATCH_NO)
	goto syntax;
      else if (m == MATCH_ERROR)
	return m;

      if (!gfc_add_component (gfc_current_block(), name, &c))
	return MATCH_ERROR;

      /* Add current_attr to the symbol attributes.  */
      if (!gfc_copy_attr (&c->attr, &current_attr, NULL))
	return MATCH_ERROR;

      if (!gfc_add_external (&c->attr, NULL))
	return MATCH_ERROR;

      if (!gfc_add_proc (&c->attr, name, NULL))
	return MATCH_ERROR;

      if (num == 1)
	c->tb = tb;
      else
	{
	  c->tb = XCNEW (gfc_typebound_proc);
	  c->tb->where = gfc_current_locus;
	  *c->tb = *tb;
	}

      /* Set interface.  */
      if (proc_if != NULL)
	{
	  c->ts.interface = proc_if;
	  c->attr.untyped = 1;
	  c->attr.if_source = IFSRC_IFBODY;
	}
      else if (ts.type != BT_UNKNOWN)
	{
	  c->ts = ts;
	  c->ts.interface = gfc_new_symbol ("", gfc_current_ns);
	  c->ts.interface->result = c->ts.interface;
	  c->ts.interface->ts = ts;
	  c->ts.interface->attr.flavor = FL_PROCEDURE;
	  c->ts.interface->attr.function = 1;
	  c->attr.function = 1;
	  c->attr.if_source = IFSRC_UNKNOWN;
	}

      if (gfc_match (" =>") == MATCH_YES)
	{
	  m = match_pointer_init (&initializer, 1);
	  if (m != MATCH_YES)
	    {
	      gfc_free_expr (initializer);
	      return m;
	    }
	  c->initializer = initializer;
	}

      if (gfc_match_eos () == MATCH_YES)
	return MATCH_YES;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

syntax:
  gfc_error ("Syntax error in procedure pointer component at %C");
  return MATCH_ERROR;
}


/* Match a PROCEDURE declaration inside an interface (R1206).  */

static match
match_procedure_in_interface (void)
{
  match m;
  gfc_symbol *sym;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  locus old_locus;

  if (current_interface.type == INTERFACE_NAMELESS
      || current_interface.type == INTERFACE_ABSTRACT)
    {
      gfc_error ("PROCEDURE at %C must be in a generic interface");
      return MATCH_ERROR;
    }

  /* Check if the F2008 optional double colon appears.  */
  gfc_gobble_whitespace ();
  old_locus = gfc_current_locus;
  if (gfc_match ("::") == MATCH_YES)
    {
      if (!gfc_notify_std (GFC_STD_F2008, "double colon in "
			   "MODULE PROCEDURE statement at %L", &old_locus))
	return MATCH_ERROR;
    }
  else
    gfc_current_locus = old_locus;

  for(;;)
    {
      m = gfc_match_name (name);
      if (m == MATCH_NO)
	goto syntax;
      else if (m == MATCH_ERROR)
	return m;
      if (gfc_get_symbol (name, gfc_current_ns->parent, &sym))
	return MATCH_ERROR;

      if (!gfc_add_interface (sym))
	return MATCH_ERROR;

      if (gfc_match_eos () == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in PROCEDURE statement at %C");
  return MATCH_ERROR;
}


/* General matcher for PROCEDURE declarations.  */

static match match_procedure_in_type (void);

match
gfc_match_procedure (void)
{
  match m;

  switch (gfc_current_state ())
    {
    case COMP_NONE:
    case COMP_PROGRAM:
    case COMP_MODULE:
    case COMP_SUBMODULE:
    case COMP_SUBROUTINE:
    case COMP_FUNCTION:
    case COMP_BLOCK:
      m = match_procedure_decl ();
      break;
    case COMP_INTERFACE:
      m = match_procedure_in_interface ();
      break;
    case COMP_DERIVED:
      m = match_ppc_decl ();
      break;
    case COMP_DERIVED_CONTAINS:
      m = match_procedure_in_type ();
      break;
    default:
      return MATCH_NO;
    }

  if (m != MATCH_YES)
    return m;

  if (!gfc_notify_std (GFC_STD_F2003, "PROCEDURE statement at %C"))
    return MATCH_ERROR;

  return m;
}


/* Warn if a matched procedure has the same name as an intrinsic; this is
   simply a wrapper around gfc_warn_intrinsic_shadow that interprets the current
   parser-state-stack to find out whether we're in a module.  */

static void
do_warn_intrinsic_shadow (const gfc_symbol* sym, bool func)
{
  bool in_module;

  in_module = (gfc_state_stack->previous
	       && (gfc_state_stack->previous->state == COMP_MODULE
		   || gfc_state_stack->previous->state == COMP_SUBMODULE));

  gfc_warn_intrinsic_shadow (sym, in_module, func);
}


/* Match a function declaration.  */

match
gfc_match_function_decl (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym, *result;
  locus old_loc;
  match m;
  match suffix_match;
  match found_match; /* Status returned by match func.  */

  if (gfc_current_state () != COMP_NONE
      && gfc_current_state () != COMP_INTERFACE
      && gfc_current_state () != COMP_CONTAINS)
    return MATCH_NO;

  gfc_clear_ts (&current_ts);

  old_loc = gfc_current_locus;

  m = gfc_match_prefix (&current_ts);
  if (m != MATCH_YES)
    {
      gfc_current_locus = old_loc;
      return m;
    }

  if (gfc_match ("function% %n", name) != MATCH_YES)
    {
      gfc_current_locus = old_loc;
      return MATCH_NO;
    }

  if (get_proc_name (name, &sym, false))
    return MATCH_ERROR;

  if (add_hidden_procptr_result (sym))
    sym = sym->result;

  if (current_attr.module_procedure)
    sym->attr.module_procedure = 1;

  gfc_new_block = sym;

  m = gfc_match_formal_arglist (sym, 0, 0);
  if (m == MATCH_NO)
    {
      gfc_error ("Expected formal argument list in function "
		 "definition at %C");
      m = MATCH_ERROR;
      goto cleanup;
    }
  else if (m == MATCH_ERROR)
    goto cleanup;

  result = NULL;

  /* According to the draft, the bind(c) and result clause can
     come in either order after the formal_arg_list (i.e., either
     can be first, both can exist together or by themselves or neither
     one).  Therefore, the match_result can't match the end of the
     string, and check for the bind(c) or result clause in either order.  */
  found_match = gfc_match_eos ();

  /* Make sure that it isn't already declared as BIND(C).  If it is, it
     must have been marked BIND(C) with a BIND(C) attribute and that is
     not allowed for procedures.  */
  if (sym->attr.is_bind_c == 1)
    {
      sym->attr.is_bind_c = 0;
      if (sym->old_symbol != NULL)
        gfc_error_now ("BIND(C) attribute at %L can only be used for "
                       "variables or common blocks",
                       &(sym->old_symbol->declared_at));
      else
        gfc_error_now ("BIND(C) attribute at %L can only be used for "
                       "variables or common blocks", &gfc_current_locus);
    }

  if (found_match != MATCH_YES)
    {
      /* If we haven't found the end-of-statement, look for a suffix.  */
      suffix_match = gfc_match_suffix (sym, &result);
      if (suffix_match == MATCH_YES)
        /* Need to get the eos now.  */
        found_match = gfc_match_eos ();
      else
	found_match = suffix_match;
    }

  if(found_match != MATCH_YES)
    m = MATCH_ERROR;
  else
    {
      /* Make changes to the symbol.  */
      m = MATCH_ERROR;

      if (!gfc_add_function (&sym->attr, sym->name, NULL))
	goto cleanup;

      if (!gfc_missing_attr (&sym->attr, NULL))
	goto cleanup;

      if (!copy_prefix (&sym->attr, &sym->declared_at))
	{
	  if(!sym->attr.module_procedure)
	goto cleanup;
	  else
	    gfc_error_check ();
	}

      /* Delay matching the function characteristics until after the
	 specification block by signalling kind=-1.  */
      sym->declared_at = old_loc;
      if (current_ts.type != BT_UNKNOWN)
	current_ts.kind = -1;
      else
	current_ts.kind = 0;

      if (result == NULL)
	{
          if (current_ts.type != BT_UNKNOWN
	      && !gfc_add_type (sym, &current_ts, &gfc_current_locus))
	    goto cleanup;
	  sym->result = sym;
	}
      else
	{
          if (current_ts.type != BT_UNKNOWN
	      && !gfc_add_type (result, &current_ts, &gfc_current_locus))
	    goto cleanup;
	  sym->result = result;
	}

      /* Warn if this procedure has the same name as an intrinsic.  */
      do_warn_intrinsic_shadow (sym, true);

      return MATCH_YES;
    }

cleanup:
  gfc_current_locus = old_loc;
  return m;
}


/* This is mostly a copy of parse.c(add_global_procedure) but modified to
   pass the name of the entry, rather than the gfc_current_block name, and
   to return false upon finding an existing global entry.  */

static bool
add_global_entry (const char *name, const char *binding_label, bool sub,
		  locus *where)
{
  gfc_gsymbol *s;
  enum gfc_symbol_type type;

  type = sub ? GSYM_SUBROUTINE : GSYM_FUNCTION;

  /* Only in Fortran 2003: For procedures with a binding label also the Fortran
     name is a global identifier.  */
  if (!binding_label || gfc_notification_std (GFC_STD_F2008))
    {
      s = gfc_get_gsymbol (name);

      if (s->defined || (s->type != GSYM_UNKNOWN && s->type != type))
	{
	  gfc_global_used (s, where);
	  return false;
	}
      else
	{
	  s->type = type;
	  s->sym_name = name;
	  s->where = *where;
	  s->defined = 1;
	  s->ns = gfc_current_ns;
	}
    }

  /* Don't add the symbol multiple times.  */
  if (binding_label
      && (!gfc_notification_std (GFC_STD_F2008)
	  || strcmp (name, binding_label) != 0))
    {
      s = gfc_get_gsymbol (binding_label);

      if (s->defined || (s->type != GSYM_UNKNOWN && s->type != type))
	{
	  gfc_global_used (s, where);
	  return false;
	}
      else
	{
	  s->type = type;
	  s->sym_name = name;
	  s->binding_label = binding_label;
	  s->where = *where;
	  s->defined = 1;
	  s->ns = gfc_current_ns;
	}
    }

  return true;
}


/* Match an ENTRY statement.  */

match
gfc_match_entry (void)
{
  gfc_symbol *proc;
  gfc_symbol *result;
  gfc_symbol *entry;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_compile_state state;
  match m;
  gfc_entry_list *el;
  locus old_loc;
  bool module_procedure;
  char peek_char;
  match is_bind_c;

  m = gfc_match_name (name);
  if (m != MATCH_YES)
    return m;

  if (!gfc_notify_std (GFC_STD_F2008_OBS, "ENTRY statement at %C"))
    return MATCH_ERROR;

  state = gfc_current_state ();
  if (state != COMP_SUBROUTINE && state != COMP_FUNCTION)
    {
      switch (state)
	{
	  case COMP_PROGRAM:
	    gfc_error ("ENTRY statement at %C cannot appear within a PROGRAM");
	    break;
	  case COMP_MODULE:
	    gfc_error ("ENTRY statement at %C cannot appear within a MODULE");
	    break;
	  case COMP_SUBMODULE:
	    gfc_error ("ENTRY statement at %C cannot appear within a SUBMODULE");
	    break;
	  case COMP_BLOCK_DATA:
	    gfc_error ("ENTRY statement at %C cannot appear within "
		       "a BLOCK DATA");
	    break;
	  case COMP_INTERFACE:
	    gfc_error ("ENTRY statement at %C cannot appear within "
		       "an INTERFACE");
	    break;
          case COMP_STRUCTURE:
            gfc_error ("ENTRY statement at %C cannot appear within "
                       "a STRUCTURE block");
            break;
	  case COMP_DERIVED:
	    gfc_error ("ENTRY statement at %C cannot appear within "
		       "a DERIVED TYPE block");
	    break;
	  case COMP_IF:
	    gfc_error ("ENTRY statement at %C cannot appear within "
		       "an IF-THEN block");
	    break;
	  case COMP_DO:
	  case COMP_DO_CONCURRENT:
	    gfc_error ("ENTRY statement at %C cannot appear within "
		       "a DO block");
	    break;
	  case COMP_SELECT:
	    gfc_error ("ENTRY statement at %C cannot appear within "
		       "a SELECT block");
	    break;
	  case COMP_FORALL:
	    gfc_error ("ENTRY statement at %C cannot appear within "
		       "a FORALL block");
	    break;
	  case COMP_WHERE:
	    gfc_error ("ENTRY statement at %C cannot appear within "
		       "a WHERE block");
	    break;
	  case COMP_CONTAINS:
	    gfc_error ("ENTRY statement at %C cannot appear within "
		       "a contained subprogram");
	    break;
	  default:
	    gfc_error ("Unexpected ENTRY statement at %C");
	}
      return MATCH_ERROR;
    }

  if ((state == COMP_SUBROUTINE || state == COMP_FUNCTION)
      && gfc_state_stack->previous->state == COMP_INTERFACE)
    {
      gfc_error ("ENTRY statement at %C cannot appear within an INTERFACE");
      return MATCH_ERROR;
    }

  module_procedure = gfc_current_ns->parent != NULL
		   && gfc_current_ns->parent->proc_name
		   && gfc_current_ns->parent->proc_name->attr.flavor
		      == FL_MODULE;

  if (gfc_current_ns->parent != NULL
      && gfc_current_ns->parent->proc_name
      && !module_procedure)
    {
      gfc_error("ENTRY statement at %C cannot appear in a "
		"contained procedure");
      return MATCH_ERROR;
    }

  /* Module function entries need special care in get_proc_name
     because previous references within the function will have
     created symbols attached to the current namespace.  */
  if (get_proc_name (name, &entry,
		     gfc_current_ns->parent != NULL
		     && module_procedure))
    return MATCH_ERROR;

  proc = gfc_current_block ();

  /* Make sure that it isn't already declared as BIND(C).  If it is, it
     must have been marked BIND(C) with a BIND(C) attribute and that is
     not allowed for procedures.  */
  if (entry->attr.is_bind_c == 1)
    {
      entry->attr.is_bind_c = 0;
      if (entry->old_symbol != NULL)
        gfc_error_now ("BIND(C) attribute at %L can only be used for "
                       "variables or common blocks",
                       &(entry->old_symbol->declared_at));
      else
        gfc_error_now ("BIND(C) attribute at %L can only be used for "
                       "variables or common blocks", &gfc_current_locus);
    }

  /* Check what next non-whitespace character is so we can tell if there
     is the required parens if we have a BIND(C).  */
  old_loc = gfc_current_locus;
  gfc_gobble_whitespace ();
  peek_char = gfc_peek_ascii_char ();

  if (state == COMP_SUBROUTINE)
    {
      m = gfc_match_formal_arglist (entry, 0, 1);
      if (m != MATCH_YES)
	return MATCH_ERROR;

      /* Call gfc_match_bind_c with allow_binding_name = true as ENTRY can
	 never be an internal procedure.  */
      is_bind_c = gfc_match_bind_c (entry, true);
      if (is_bind_c == MATCH_ERROR)
	return MATCH_ERROR;
      if (is_bind_c == MATCH_YES)
	{
	  if (peek_char != '(')
	    {
	      gfc_error ("Missing required parentheses before BIND(C) at %C");
	      return MATCH_ERROR;
	    }
	    if (!gfc_add_is_bind_c (&(entry->attr), entry->name,
				    &(entry->declared_at), 1))
	      return MATCH_ERROR;
	}

      if (!gfc_current_ns->parent
	  && !add_global_entry (name, entry->binding_label, true,
				&old_loc))
	return MATCH_ERROR;

      /* An entry in a subroutine.  */
      if (!gfc_add_entry (&entry->attr, entry->name, NULL)
	  || !gfc_add_subroutine (&entry->attr, entry->name, NULL))
	return MATCH_ERROR;
    }
  else
    {
      /* An entry in a function.
	 We need to take special care because writing
	    ENTRY f()
	 as
	    ENTRY f
	 is allowed, whereas
	    ENTRY f() RESULT (r)
	 can't be written as
	    ENTRY f RESULT (r).  */
      if (gfc_match_eos () == MATCH_YES)
	{
	  gfc_current_locus = old_loc;
	  /* Match the empty argument list, and add the interface to
	     the symbol.  */
	  m = gfc_match_formal_arglist (entry, 0, 1);
	}
      else
	m = gfc_match_formal_arglist (entry, 0, 0);

      if (m != MATCH_YES)
	return MATCH_ERROR;

      result = NULL;

      if (gfc_match_eos () == MATCH_YES)
	{
	  if (!gfc_add_entry (&entry->attr, entry->name, NULL)
	      || !gfc_add_function (&entry->attr, entry->name, NULL))
	    return MATCH_ERROR;

	  entry->result = entry;
	}
      else
	{
	  m = gfc_match_suffix (entry, &result);
	  if (m == MATCH_NO)
	    gfc_syntax_error (ST_ENTRY);
	  if (m != MATCH_YES)
	    return MATCH_ERROR;

          if (result)
	    {
	      if (!gfc_add_result (&result->attr, result->name, NULL)
		  || !gfc_add_entry (&entry->attr, result->name, NULL)
		  || !gfc_add_function (&entry->attr, result->name, NULL))
	        return MATCH_ERROR;
	      entry->result = result;
	    }
	  else
	    {
	      if (!gfc_add_entry (&entry->attr, entry->name, NULL)
		  || !gfc_add_function (&entry->attr, entry->name, NULL))
		return MATCH_ERROR;
	      entry->result = entry;
	    }
	}

      if (!gfc_current_ns->parent
	  && !add_global_entry (name, entry->binding_label, false,
				&old_loc))
	return MATCH_ERROR;
    }

  if (gfc_match_eos () != MATCH_YES)
    {
      gfc_syntax_error (ST_ENTRY);
      return MATCH_ERROR;
    }

  entry->attr.recursive = proc->attr.recursive;
  entry->attr.elemental = proc->attr.elemental;
  entry->attr.pure = proc->attr.pure;

  el = gfc_get_entry_list ();
  el->sym = entry;
  el->next = gfc_current_ns->entries;
  gfc_current_ns->entries = el;
  if (el->next)
    el->id = el->next->id + 1;
  else
    el->id = 1;

  new_st.op = EXEC_ENTRY;
  new_st.ext.entry = el;

  return MATCH_YES;
}


/* Match a subroutine statement, including optional prefixes.  */

match
gfc_match_subroutine (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  match m;
  match is_bind_c;
  char peek_char;
  bool allow_binding_name;

  if (gfc_current_state () != COMP_NONE
      && gfc_current_state () != COMP_INTERFACE
      && gfc_current_state () != COMP_CONTAINS)
    return MATCH_NO;

  m = gfc_match_prefix (NULL);
  if (m != MATCH_YES)
    return m;

  m = gfc_match ("subroutine% %n", name);
  if (m != MATCH_YES)
    return m;

  if (get_proc_name (name, &sym, false))
    return MATCH_ERROR;

  /* Set declared_at as it might point to, e.g., a PUBLIC statement, if
     the symbol existed before.  */
  sym->declared_at = gfc_current_locus;

  if (current_attr.module_procedure)
    sym->attr.module_procedure = 1;

  if (add_hidden_procptr_result (sym))
    sym = sym->result;

  gfc_new_block = sym;

  /* Check what next non-whitespace character is so we can tell if there
     is the required parens if we have a BIND(C).  */
  gfc_gobble_whitespace ();
  peek_char = gfc_peek_ascii_char ();

  if (!gfc_add_subroutine (&sym->attr, sym->name, NULL))
    return MATCH_ERROR;

  if (gfc_match_formal_arglist (sym, 0, 1) != MATCH_YES)
    return MATCH_ERROR;

  /* Make sure that it isn't already declared as BIND(C).  If it is, it
     must have been marked BIND(C) with a BIND(C) attribute and that is
     not allowed for procedures.  */
  if (sym->attr.is_bind_c == 1)
    {
      sym->attr.is_bind_c = 0;
      if (sym->old_symbol != NULL)
        gfc_error_now ("BIND(C) attribute at %L can only be used for "
                       "variables or common blocks",
                       &(sym->old_symbol->declared_at));
      else
        gfc_error_now ("BIND(C) attribute at %L can only be used for "
                       "variables or common blocks", &gfc_current_locus);
    }

  /* C binding names are not allowed for internal procedures.  */
  if (gfc_current_state () == COMP_CONTAINS
      && sym->ns->proc_name->attr.flavor != FL_MODULE)
    allow_binding_name = false;
  else
    allow_binding_name = true;

  /* Here, we are just checking if it has the bind(c) attribute, and if
     so, then we need to make sure it's all correct.  If it doesn't,
     we still need to continue matching the rest of the subroutine line.  */
  is_bind_c = gfc_match_bind_c (sym, allow_binding_name);
  if (is_bind_c == MATCH_ERROR)
    {
      /* There was an attempt at the bind(c), but it was wrong.	 An
	 error message should have been printed w/in the gfc_match_bind_c
	 so here we'll just return the MATCH_ERROR.  */
      return MATCH_ERROR;
    }

  if (is_bind_c == MATCH_YES)
    {
      /* The following is allowed in the Fortran 2008 draft.  */
      if (gfc_current_state () == COMP_CONTAINS
	  && sym->ns->proc_name->attr.flavor != FL_MODULE
	  && !gfc_notify_std (GFC_STD_F2008, "BIND(C) attribute "
			      "at %L may not be specified for an internal "
			      "procedure", &gfc_current_locus))
	return MATCH_ERROR;

      if (peek_char != '(')
        {
          gfc_error ("Missing required parentheses before BIND(C) at %C");
          return MATCH_ERROR;
        }
      if (!gfc_add_is_bind_c (&(sym->attr), sym->name,
			      &(sym->declared_at), 1))
        return MATCH_ERROR;
    }

  if (gfc_match_eos () != MATCH_YES)
    {
      gfc_syntax_error (ST_SUBROUTINE);
      return MATCH_ERROR;
    }

  if (!copy_prefix (&sym->attr, &sym->declared_at))
    {
      if(!sym->attr.module_procedure)
	return MATCH_ERROR;
      else
	gfc_error_check ();
    }

  /* Warn if it has the same name as an intrinsic.  */
  do_warn_intrinsic_shadow (sym, false);

  return MATCH_YES;
}


/* Check that the NAME identifier in a BIND attribute or statement
   is conform to C identifier rules.  */

match
check_bind_name_identifier (char **name)
{
  char *n = *name, *p;

  /* Remove leading spaces.  */
  while (*n == ' ')
    n++;

  /* On an empty string, free memory and set name to NULL.  */
  if (*n == '\0')
    {
      free (*name);
      *name = NULL;
      return MATCH_YES;
    }

  /* Remove trailing spaces.  */
  p = n + strlen(n) - 1;
  while (*p == ' ')
    *(p--) = '\0';

  /* Insert the identifier into the symbol table.  */
  p = xstrdup (n);
  free (*name);
  *name = p;

  /* Now check that identifier is valid under C rules.  */
  if (ISDIGIT (*p))
    {
      gfc_error ("Invalid C identifier in NAME= specifier at %C");
      return MATCH_ERROR;
    }

  for (; *p; p++)
    if (!(ISALNUM (*p) || *p == '_' || *p == '$'))
      {
        gfc_error ("Invalid C identifier in NAME= specifier at %C");
	return MATCH_ERROR;
      }

  return MATCH_YES;
}


/* Match a BIND(C) specifier, with the optional 'name=' specifier if
   given, and set the binding label in either the given symbol (if not
   NULL), or in the current_ts.  The symbol may be NULL because we may
   encounter the BIND(C) before the declaration itself.  Return
   MATCH_NO if what we're looking at isn't a BIND(C) specifier,
   MATCH_ERROR if it is a BIND(C) clause but an error was encountered,
   or MATCH_YES if the specifier was correct and the binding label and
   bind(c) fields were set correctly for the given symbol or the
   current_ts. If allow_binding_name is false, no binding name may be
   given.  */

match
gfc_match_bind_c (gfc_symbol *sym, bool allow_binding_name)
{
  char *binding_label = NULL;
  gfc_expr *e = NULL;

  /* Initialize the flag that specifies whether we encountered a NAME=
     specifier or not.  */
  has_name_equals = 0;

  /* This much we have to be able to match, in this order, if
     there is a bind(c) label.	*/
  if (gfc_match (" bind ( c ") != MATCH_YES)
    return MATCH_NO;

  /* Now see if there is a binding label, or if we've reached the
     end of the bind(c) attribute without one.	*/
  if (gfc_match_char (',') == MATCH_YES)
    {
      if (gfc_match (" name = ") != MATCH_YES)
        {
          gfc_error ("Syntax error in NAME= specifier for binding label "
                     "at %C");
          /* should give an error message here */
          return MATCH_ERROR;
        }

      has_name_equals = 1;

      if (gfc_match_init_expr (&e) != MATCH_YES)
	{
	  gfc_free_expr (e);
	  return MATCH_ERROR;
	}

      if (!gfc_simplify_expr(e, 0))
	{
	  gfc_error ("NAME= specifier at %C should be a constant expression");
	  gfc_free_expr (e);
	  return MATCH_ERROR;
	}

      if (e->expr_type != EXPR_CONSTANT || e->ts.type != BT_CHARACTER
	  || e->ts.kind != gfc_default_character_kind || e->rank != 0)
	{
	  gfc_error ("NAME= specifier at %C should be a scalar of "
	             "default character kind");
	  gfc_free_expr(e);
	  return MATCH_ERROR;
	}

      // Get a C string from the Fortran string constant
      binding_label = gfc_widechar_to_char (e->value.character.string,
					    e->value.character.length);
      gfc_free_expr(e);

      // Check that it is valid (old gfc_match_name_C)
      if (check_bind_name_identifier (&binding_label) != MATCH_YES)
	return MATCH_ERROR;
    }

  /* Get the required right paren.  */
  if (gfc_match_char (')') != MATCH_YES)
    {
      gfc_error ("Missing closing paren for binding label at %C");
      return MATCH_ERROR;
    }

  if (has_name_equals && !allow_binding_name)
    {
      gfc_error ("No binding name is allowed in BIND(C) at %C");
      return MATCH_ERROR;
    }

  if (has_name_equals && sym != NULL && sym->attr.dummy)
    {
      gfc_error ("For dummy procedure %s, no binding name is "
		 "allowed in BIND(C) at %C", sym->name);
      return MATCH_ERROR;
    }


  /* Save the binding label to the symbol.  If sym is null, we're
     probably matching the typespec attributes of a declaration and
     haven't gotten the name yet, and therefore, no symbol yet.	 */
  if (binding_label)
    {
      if (sym != NULL)
	sym->binding_label = binding_label;
      else
	curr_binding_label = binding_label;
    }
  else if (allow_binding_name)
    {
      /* No binding label, but if symbol isn't null, we
	 can set the label for it here.
	 If name="" or allow_binding_name is false, no C binding name is
	 created.  */
      if (sym != NULL && sym->name != NULL && has_name_equals == 0)
	sym->binding_label = IDENTIFIER_POINTER (get_identifier (sym->name));
    }

  if (has_name_equals && gfc_current_state () == COMP_INTERFACE
      && current_interface.type == INTERFACE_ABSTRACT)
    {
      gfc_error ("NAME not allowed on BIND(C) for ABSTRACT INTERFACE at %C");
      return MATCH_ERROR;
    }

  return MATCH_YES;
}


/* Return nonzero if we're currently compiling a contained procedure.  */

static int
contained_procedure (void)
{
  gfc_state_data *s = gfc_state_stack;

  if ((s->state == COMP_SUBROUTINE || s->state == COMP_FUNCTION)
      && s->previous != NULL && s->previous->state == COMP_CONTAINS)
    return 1;

  return 0;
}

/* Set the kind of each enumerator.  The kind is selected such that it is
   interoperable with the corresponding C enumeration type, making
   sure that -fshort-enums is honored.  */

static void
set_enum_kind(void)
{
  enumerator_history *current_history = NULL;
  int kind;
  int i;

  if (max_enum == NULL || enum_history == NULL)
    return;

  if (!flag_short_enums)
    return;

  i = 0;
  do
    {
      kind = gfc_integer_kinds[i++].kind;
    }
  while (kind < gfc_c_int_kind
	 && gfc_check_integer_range (max_enum->initializer->value.integer,
				     kind) != ARITH_OK);

  current_history = enum_history;
  while (current_history != NULL)
    {
      current_history->sym->ts.kind = kind;
      current_history = current_history->next;
    }
}


/* Match any of the various end-block statements.  Returns the type of
   END to the caller.  The END INTERFACE, END IF, END DO, END SELECT
   and END BLOCK statements cannot be replaced by a single END statement.  */

match
gfc_match_end (gfc_statement *st)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_compile_state state;
  locus old_loc;
  const char *block_name;
  const char *target;
  int eos_ok;
  match m;
  gfc_namespace *parent_ns, *ns, *prev_ns;
  gfc_namespace **nsp;
  bool abreviated_modproc_decl = false;
  bool got_matching_end = false;

  old_loc = gfc_current_locus;
  if (gfc_match ("end") != MATCH_YES)
    return MATCH_NO;

  state = gfc_current_state ();
  block_name = gfc_current_block () == NULL
	     ? NULL : gfc_current_block ()->name;

  switch (state)
    {
    case COMP_ASSOCIATE:
    case COMP_BLOCK:
      if (!strncmp (block_name, "block@", strlen("block@")))
	block_name = NULL;
      break;

    case COMP_CONTAINS:
    case COMP_DERIVED_CONTAINS:
      state = gfc_state_stack->previous->state;
      block_name = gfc_state_stack->previous->sym == NULL
		 ? NULL : gfc_state_stack->previous->sym->name;
      abreviated_modproc_decl = gfc_state_stack->previous->sym
		&& gfc_state_stack->previous->sym->abr_modproc_decl;
      break;

    default:
      break;
    }

  if (!abreviated_modproc_decl)
    abreviated_modproc_decl = gfc_current_block ()
			      && gfc_current_block ()->abr_modproc_decl;

  switch (state)
    {
    case COMP_NONE:
    case COMP_PROGRAM:
      *st = ST_END_PROGRAM;
      target = " program";
      eos_ok = 1;
      break;

    case COMP_SUBROUTINE:
      *st = ST_END_SUBROUTINE;
      if (!abreviated_modproc_decl)
      target = " subroutine";
      else
	target = " procedure";
      eos_ok = !contained_procedure ();
      break;

    case COMP_FUNCTION:
      *st = ST_END_FUNCTION;
      if (!abreviated_modproc_decl)
      target = " function";
      else
	target = " procedure";
      eos_ok = !contained_procedure ();
      break;

    case COMP_BLOCK_DATA:
      *st = ST_END_BLOCK_DATA;
      target = " block data";
      eos_ok = 1;
      break;

    case COMP_MODULE:
      *st = ST_END_MODULE;
      target = " module";
      eos_ok = 1;
      break;

    case COMP_SUBMODULE:
      *st = ST_END_SUBMODULE;
      target = " submodule";
      eos_ok = 1;
      break;

    case COMP_INTERFACE:
      *st = ST_END_INTERFACE;
      target = " interface";
      eos_ok = 0;
      break;

    case COMP_MAP:
      *st = ST_END_MAP;
      target = " map";
      eos_ok = 0;
      break;

    case COMP_UNION:
      *st = ST_END_UNION;
      target = " union";
      eos_ok = 0;
      break;

    case COMP_STRUCTURE:
      *st = ST_END_STRUCTURE;
      target = " structure";
      eos_ok = 0;
      break;

    case COMP_DERIVED:
    case COMP_DERIVED_CONTAINS:
      *st = ST_END_TYPE;
      target = " type";
      eos_ok = 0;
      break;

    case COMP_ASSOCIATE:
      *st = ST_END_ASSOCIATE;
      target = " associate";
      eos_ok = 0;
      break;

    case COMP_BLOCK:
      *st = ST_END_BLOCK;
      target = " block";
      eos_ok = 0;
      break;

    case COMP_IF:
      *st = ST_ENDIF;
      target = " if";
      eos_ok = 0;
      break;

    case COMP_DO:
    case COMP_DO_CONCURRENT:
      *st = ST_ENDDO;
      target = " do";
      eos_ok = 0;
      break;

    case COMP_CRITICAL:
      *st = ST_END_CRITICAL;
      target = " critical";
      eos_ok = 0;
      break;

    case COMP_SELECT:
    case COMP_SELECT_TYPE:
      *st = ST_END_SELECT;
      target = " select";
      eos_ok = 0;
      break;

    case COMP_FORALL:
      *st = ST_END_FORALL;
      target = " forall";
      eos_ok = 0;
      break;

    case COMP_WHERE:
      *st = ST_END_WHERE;
      target = " where";
      eos_ok = 0;
      break;

    case COMP_ENUM:
      *st = ST_END_ENUM;
      target = " enum";
      eos_ok = 0;
      last_initializer = NULL;
      set_enum_kind ();
      gfc_free_enum_history ();
      break;

    default:
      gfc_error ("Unexpected END statement at %C");
      goto cleanup;
    }

  old_loc = gfc_current_locus;
  if (gfc_match_eos () == MATCH_YES)
    {
      if (!eos_ok && (*st == ST_END_SUBROUTINE || *st == ST_END_FUNCTION))
	{
	  if (!gfc_notify_std (GFC_STD_F2008, "END statement "
			       "instead of %s statement at %L",
			       abreviated_modproc_decl ? "END PROCEDURE"
			       : gfc_ascii_statement(*st), &old_loc))
	    goto cleanup;
	}
      else if (!eos_ok)
	{
	  /* We would have required END [something].  */
	  gfc_error ("%s statement expected at %L",
		     gfc_ascii_statement (*st), &old_loc);
	  goto cleanup;
	}

      return MATCH_YES;
    }

  /* Verify that we've got the sort of end-block that we're expecting.  */
  if (gfc_match (target) != MATCH_YES)
    {
      gfc_error ("Expecting %s statement at %L", abreviated_modproc_decl
		 ? "END PROCEDURE" : gfc_ascii_statement(*st), &old_loc);
      goto cleanup;
    }
  else
    got_matching_end = true;

  old_loc = gfc_current_locus;
  /* If we're at the end, make sure a block name wasn't required.  */
  if (gfc_match_eos () == MATCH_YES)
    {

      if (*st != ST_ENDDO && *st != ST_ENDIF && *st != ST_END_SELECT
	  && *st != ST_END_FORALL && *st != ST_END_WHERE && *st != ST_END_BLOCK
	  && *st != ST_END_ASSOCIATE && *st != ST_END_CRITICAL)
	return MATCH_YES;

      if (!block_name)
	return MATCH_YES;

      gfc_error ("Expected block name of %qs in %s statement at %L",
		 block_name, gfc_ascii_statement (*st), &old_loc);

      return MATCH_ERROR;
    }

  /* END INTERFACE has a special handler for its several possible endings.  */
  if (*st == ST_END_INTERFACE)
    return gfc_match_end_interface ();

  /* We haven't hit the end of statement, so what is left must be an
     end-name.  */
  m = gfc_match_space ();
  if (m == MATCH_YES)
    m = gfc_match_name (name);

  if (m == MATCH_NO)
    gfc_error ("Expected terminating name at %C");
  if (m != MATCH_YES)
    goto cleanup;

  if (block_name == NULL)
    goto syntax;

  /* We have to pick out the declared submodule name from the composite
     required by F2008:11.2.3 para 2, which ends in the declared name.  */
  if (state == COMP_SUBMODULE)
    block_name = strchr (block_name, '.') + 1;

  if (strcmp (name, block_name) != 0 && strcmp (block_name, "ppr@") != 0)
    {
      gfc_error ("Expected label %qs for %s statement at %C", block_name,
		 gfc_ascii_statement (*st));
      goto cleanup;
    }
  /* Procedure pointer as function result.  */
  else if (strcmp (block_name, "ppr@") == 0
	   && strcmp (name, gfc_current_block ()->ns->proc_name->name) != 0)
    {
      gfc_error ("Expected label %qs for %s statement at %C",
		 gfc_current_block ()->ns->proc_name->name,
		 gfc_ascii_statement (*st));
      goto cleanup;
    }

  if (gfc_match_eos () == MATCH_YES)
    return MATCH_YES;

syntax:
  gfc_syntax_error (*st);

cleanup:
  gfc_current_locus = old_loc;

  /* If we are missing an END BLOCK, we created a half-ready namespace.
     Remove it from the parent namespace's sibling list.  */

  while (state == COMP_BLOCK && !got_matching_end)
    {
      parent_ns = gfc_current_ns->parent;

      nsp = &(gfc_state_stack->previous->tail->ext.block.ns);

      prev_ns = NULL;
      ns = *nsp;
      while (ns)
	{
	  if (ns == gfc_current_ns)
	    {
	      if (prev_ns == NULL)
		*nsp = NULL;
	      else
		prev_ns->sibling = ns->sibling;
	    }
	  prev_ns = ns;
	  ns = ns->sibling;
	}

      gfc_free_namespace (gfc_current_ns);
      gfc_current_ns = parent_ns;
      gfc_state_stack = gfc_state_stack->previous;
      state = gfc_current_state ();
    }

  return MATCH_ERROR;
}



/***************** Attribute declaration statements ****************/

/* Set the attribute of a single variable.  */

static match
attr_decl1 (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_array_spec *as;

  /* Workaround -Wmaybe-uninitialized false positive during
     profiledbootstrap by initializing them.  */
  gfc_symbol *sym = NULL;
  locus var_locus;
  match m;

  as = NULL;

  m = gfc_match_name (name);
  if (m != MATCH_YES)
    goto cleanup;

  if (find_special (name, &sym, false))
    return MATCH_ERROR;

  if (!check_function_name (name))
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  var_locus = gfc_current_locus;

  /* Deal with possible array specification for certain attributes.  */
  if (current_attr.dimension
      || current_attr.codimension
      || current_attr.allocatable
      || current_attr.pointer
      || current_attr.target)
    {
      m = gfc_match_array_spec (&as, !current_attr.codimension,
				!current_attr.dimension
				&& !current_attr.pointer
				&& !current_attr.target);
      if (m == MATCH_ERROR)
	goto cleanup;

      if (current_attr.dimension && m == MATCH_NO)
	{
	  gfc_error ("Missing array specification at %L in DIMENSION "
		     "statement", &var_locus);
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      if (current_attr.dimension && sym->value)
	{
	  gfc_error ("Dimensions specified for %s at %L after its "
		     "initialization", sym->name, &var_locus);
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      if (current_attr.codimension && m == MATCH_NO)
	{
	  gfc_error ("Missing array specification at %L in CODIMENSION "
		     "statement", &var_locus);
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      if ((current_attr.allocatable || current_attr.pointer)
	  && (m == MATCH_YES) && (as->type != AS_DEFERRED))
	{
	  gfc_error ("Array specification must be deferred at %L", &var_locus);
	  m = MATCH_ERROR;
	  goto cleanup;
	}
    }

  /* Update symbol table.  DIMENSION attribute is set in
     gfc_set_array_spec().  For CLASS variables, this must be applied
     to the first component, or '_data' field.  */
  if (sym->ts.type == BT_CLASS && sym->ts.u.derived->attr.is_class)
    {
      if (!gfc_copy_attr (&CLASS_DATA(sym)->attr, &current_attr, &var_locus))
	{
	  m = MATCH_ERROR;
	  goto cleanup;
	}
    }
  else
    {
      if (current_attr.dimension == 0 && current_attr.codimension == 0
	  && !gfc_copy_attr (&sym->attr, &current_attr, &var_locus))
	{
	  m = MATCH_ERROR;
	  goto cleanup;
	}
    }

  if (sym->ts.type == BT_CLASS
      && !gfc_build_class_symbol (&sym->ts, &sym->attr, &sym->as))
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  if (!gfc_set_array_spec (sym, as, &var_locus))
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  if (sym->attr.cray_pointee && sym->as != NULL)
    {
      /* Fix the array spec.  */
      m = gfc_mod_pointee_as (sym->as);
      if (m == MATCH_ERROR)
	goto cleanup;
    }

  if (!gfc_add_attribute (&sym->attr, &var_locus))
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  if ((current_attr.external || current_attr.intrinsic)
      && sym->attr.flavor != FL_PROCEDURE
      && !gfc_add_flavor (&sym->attr, FL_PROCEDURE, sym->name, NULL))
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  add_hidden_procptr_result (sym);

  return MATCH_YES;

cleanup:
  gfc_free_array_spec (as);
  return m;
}


/* Generic attribute declaration subroutine.  Used for attributes that
   just have a list of names.  */

static match
attr_decl (void)
{
  match m;

  /* Gobble the optional double colon, by simply ignoring the result
     of gfc_match().  */
  gfc_match (" ::");

  for (;;)
    {
      m = attr_decl1 ();
      if (m != MATCH_YES)
	break;

      if (gfc_match_eos () == MATCH_YES)
	{
	  m = MATCH_YES;
	  break;
	}

      if (gfc_match_char (',') != MATCH_YES)
	{
	  gfc_error ("Unexpected character in variable list at %C");
	  m = MATCH_ERROR;
	  break;
	}
    }

  return m;
}


/* This routine matches Cray Pointer declarations of the form:
   pointer ( <pointer>, <pointee> )
   or
   pointer ( <pointer1>, <pointee1> ), ( <pointer2>, <pointee2> ), ...
   The pointer, if already declared, should be an integer.  Otherwise, we
   set it as BT_INTEGER with kind gfc_index_integer_kind.  The pointee may
   be either a scalar, or an array declaration.  No space is allocated for
   the pointee.  For the statement
   pointer (ipt, ar(10))
   any subsequent uses of ar will be translated (in C-notation) as
   ar(i) => ((<type> *) ipt)(i)
   After gimplification, pointee variable will disappear in the code.  */

static match
cray_pointer_decl (void)
{
  match m;
  gfc_array_spec *as = NULL;
  gfc_symbol *cptr; /* Pointer symbol.  */
  gfc_symbol *cpte; /* Pointee symbol.  */
  locus var_locus;
  bool done = false;

  while (!done)
    {
      if (gfc_match_char ('(') != MATCH_YES)
	{
	  gfc_error ("Expected %<(%> at %C");
	  return MATCH_ERROR;
	}

      /* Match pointer.  */
      var_locus = gfc_current_locus;
      gfc_clear_attr (&current_attr);
      gfc_add_cray_pointer (&current_attr, &var_locus);
      current_ts.type = BT_INTEGER;
      current_ts.kind = gfc_index_integer_kind;

      m = gfc_match_symbol (&cptr, 0);
      if (m != MATCH_YES)
	{
	  gfc_error ("Expected variable name at %C");
	  return m;
	}

      if (!gfc_add_cray_pointer (&cptr->attr, &var_locus))
	return MATCH_ERROR;

      gfc_set_sym_referenced (cptr);

      if (cptr->ts.type == BT_UNKNOWN) /* Override the type, if necessary.  */
	{
	  cptr->ts.type = BT_INTEGER;
	  cptr->ts.kind = gfc_index_integer_kind;
	}
      else if (cptr->ts.type != BT_INTEGER)
	{
	  gfc_error ("Cray pointer at %C must be an integer");
	  return MATCH_ERROR;
	}
      else if (cptr->ts.kind < gfc_index_integer_kind)
	gfc_warning (0, "Cray pointer at %C has %d bytes of precision;"
		     " memory addresses require %d bytes",
		     cptr->ts.kind, gfc_index_integer_kind);

      if (gfc_match_char (',') != MATCH_YES)
	{
	  gfc_error ("Expected \",\" at %C");
	  return MATCH_ERROR;
	}

      /* Match Pointee.  */
      var_locus = gfc_current_locus;
      gfc_clear_attr (&current_attr);
      gfc_add_cray_pointee (&current_attr, &var_locus);
      current_ts.type = BT_UNKNOWN;
      current_ts.kind = 0;

      m = gfc_match_symbol (&cpte, 0);
      if (m != MATCH_YES)
	{
	  gfc_error ("Expected variable name at %C");
	  return m;
	}

      /* Check for an optional array spec.  */
      m = gfc_match_array_spec (&as, true, false);
      if (m == MATCH_ERROR)
	{
	  gfc_free_array_spec (as);
	  return m;
	}
      else if (m == MATCH_NO)
	{
	  gfc_free_array_spec (as);
	  as = NULL;
	}

      if (!gfc_add_cray_pointee (&cpte->attr, &var_locus))
	return MATCH_ERROR;

      gfc_set_sym_referenced (cpte);

      if (cpte->as == NULL)
	{
	  if (!gfc_set_array_spec (cpte, as, &var_locus))
	    gfc_internal_error ("Couldn't set Cray pointee array spec.");
	}
      else if (as != NULL)
	{
	  gfc_error ("Duplicate array spec for Cray pointee at %C");
	  gfc_free_array_spec (as);
	  return MATCH_ERROR;
	}

      as = NULL;

      if (cpte->as != NULL)
	{
	  /* Fix array spec.  */
	  m = gfc_mod_pointee_as (cpte->as);
	  if (m == MATCH_ERROR)
	    return m;
	}

      /* Point the Pointee at the Pointer.  */
      cpte->cp_pointer = cptr;

      if (gfc_match_char (')') != MATCH_YES)
	{
	  gfc_error ("Expected \")\" at %C");
	  return MATCH_ERROR;
	}
      m = gfc_match_char (',');
      if (m != MATCH_YES)
	done = true; /* Stop searching for more declarations.  */

    }

  if (m == MATCH_ERROR /* Failed when trying to find ',' above.  */
      || gfc_match_eos () != MATCH_YES)
    {
      gfc_error ("Expected %<,%> or end of statement at %C");
      return MATCH_ERROR;
    }
  return MATCH_YES;
}


match
gfc_match_external (void)
{

  gfc_clear_attr (&current_attr);
  current_attr.external = 1;

  return attr_decl ();
}


match
gfc_match_intent (void)
{
  sym_intent intent;

  /* This is not allowed within a BLOCK construct!  */
  if (gfc_current_state () == COMP_BLOCK)
    {
      gfc_error ("INTENT is not allowed inside of BLOCK at %C");
      return MATCH_ERROR;
    }

  intent = match_intent_spec ();
  if (intent == INTENT_UNKNOWN)
    return MATCH_ERROR;

  gfc_clear_attr (&current_attr);
  current_attr.intent = intent;

  return attr_decl ();
}


match
gfc_match_intrinsic (void)
{

  gfc_clear_attr (&current_attr);
  current_attr.intrinsic = 1;

  return attr_decl ();
}


match
gfc_match_optional (void)
{
  /* This is not allowed within a BLOCK construct!  */
  if (gfc_current_state () == COMP_BLOCK)
    {
      gfc_error ("OPTIONAL is not allowed inside of BLOCK at %C");
      return MATCH_ERROR;
    }

  gfc_clear_attr (&current_attr);
  current_attr.optional = 1;

  return attr_decl ();
}


match
gfc_match_pointer (void)
{
  gfc_gobble_whitespace ();
  if (gfc_peek_ascii_char () == '(')
    {
      if (!flag_cray_pointer)
	{
	  gfc_error ("Cray pointer declaration at %C requires -fcray-pointer "
		     "flag");
	  return MATCH_ERROR;
	}
      return cray_pointer_decl ();
    }
  else
    {
      gfc_clear_attr (&current_attr);
      current_attr.pointer = 1;

      return attr_decl ();
    }
}


match
gfc_match_allocatable (void)
{
  gfc_clear_attr (&current_attr);
  current_attr.allocatable = 1;

  return attr_decl ();
}


match
gfc_match_codimension (void)
{
  gfc_clear_attr (&current_attr);
  current_attr.codimension = 1;

  return attr_decl ();
}


match
gfc_match_contiguous (void)
{
  if (!gfc_notify_std (GFC_STD_F2008, "CONTIGUOUS statement at %C"))
    return MATCH_ERROR;

  gfc_clear_attr (&current_attr);
  current_attr.contiguous = 1;

  return attr_decl ();
}


match
gfc_match_dimension (void)
{
  gfc_clear_attr (&current_attr);
  current_attr.dimension = 1;

  return attr_decl ();
}


match
gfc_match_target (void)
{
  gfc_clear_attr (&current_attr);
  current_attr.target = 1;

  return attr_decl ();
}


/* Match the list of entities being specified in a PUBLIC or PRIVATE
   statement.  */

static match
access_attr_decl (gfc_statement st)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  interface_type type;
  gfc_user_op *uop;
  gfc_symbol *sym, *dt_sym;
  gfc_intrinsic_op op;
  match m;

  if (gfc_match (" ::") == MATCH_NO && gfc_match_space () == MATCH_NO)
    goto done;

  for (;;)
    {
      m = gfc_match_generic_spec (&type, name, &op);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	return MATCH_ERROR;

      switch (type)
	{
	case INTERFACE_NAMELESS:
	case INTERFACE_ABSTRACT:
	  goto syntax;

	case INTERFACE_GENERIC:
	case INTERFACE_DTIO:

	  if (gfc_get_symbol (name, NULL, &sym))
	    goto done;

	  if (type == INTERFACE_DTIO
	      && gfc_current_ns->proc_name
	      && gfc_current_ns->proc_name->attr.flavor == FL_MODULE
	      && sym->attr.flavor == FL_UNKNOWN)
	    sym->attr.flavor = FL_PROCEDURE;

	  if (!gfc_add_access (&sym->attr,
			       (st == ST_PUBLIC)
			       ? ACCESS_PUBLIC : ACCESS_PRIVATE,
			       sym->name, NULL))
	    return MATCH_ERROR;

	  if (sym->attr.generic && (dt_sym = gfc_find_dt_in_generic (sym))
	      && !gfc_add_access (&dt_sym->attr,
				  (st == ST_PUBLIC)
				  ? ACCESS_PUBLIC : ACCESS_PRIVATE,
				  sym->name, NULL))
	    return MATCH_ERROR;

	  break;

	case INTERFACE_INTRINSIC_OP:
	  if (gfc_current_ns->operator_access[op] == ACCESS_UNKNOWN)
	    {
	      gfc_intrinsic_op other_op;

	      gfc_current_ns->operator_access[op] =
		(st == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE;

	      /* Handle the case if there is another op with the same
		 function, for INTRINSIC_EQ vs. INTRINSIC_EQ_OS and so on.  */
	      other_op = gfc_equivalent_op (op);

	      if (other_op != INTRINSIC_NONE)
		gfc_current_ns->operator_access[other_op] =
		  (st == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE;

	    }
	  else
	    {
	      gfc_error ("Access specification of the %s operator at %C has "
			 "already been specified", gfc_op2string (op));
	      goto done;
	    }

	  break;

	case INTERFACE_USER_OP:
	  uop = gfc_get_uop (name);

	  if (uop->access == ACCESS_UNKNOWN)
	    {
	      uop->access = (st == ST_PUBLIC)
			  ? ACCESS_PUBLIC : ACCESS_PRIVATE;
	    }
	  else
	    {
	      gfc_error ("Access specification of the .%s. operator at %C "
			 "has already been specified", sym->name);
	      goto done;
	    }

	  break;
	}

      if (gfc_match_char (',') == MATCH_NO)
	break;
    }

  if (gfc_match_eos () != MATCH_YES)
    goto syntax;
  return MATCH_YES;

syntax:
  gfc_syntax_error (st);

done:
  return MATCH_ERROR;
}


match
gfc_match_protected (void)
{
  gfc_symbol *sym;
  match m;

  if (!gfc_current_ns->proc_name
      || gfc_current_ns->proc_name->attr.flavor != FL_MODULE)
    {
       gfc_error ("PROTECTED at %C only allowed in specification "
		  "part of a module");
       return MATCH_ERROR;

    }

  if (!gfc_notify_std (GFC_STD_F2003, "PROTECTED statement at %C"))
    return MATCH_ERROR;

  if (gfc_match (" ::") == MATCH_NO && gfc_match_space () == MATCH_NO)
    {
      return MATCH_ERROR;
    }

  if (gfc_match_eos () == MATCH_YES)
    goto syntax;

  for(;;)
    {
      m = gfc_match_symbol (&sym, 0);
      switch (m)
	{
	case MATCH_YES:
	  if (!gfc_add_protected (&sym->attr, sym->name, &gfc_current_locus))
	    return MATCH_ERROR;
	  goto next_item;

	case MATCH_NO:
	  break;

	case MATCH_ERROR:
	  return MATCH_ERROR;
	}

    next_item:
      if (gfc_match_eos () == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in PROTECTED statement at %C");
  return MATCH_ERROR;
}


/* The PRIVATE statement is a bit weird in that it can be an attribute
   declaration, but also works as a standalone statement inside of a
   type declaration or a module.  */

match
gfc_match_private (gfc_statement *st)
{

  if (gfc_match ("private") != MATCH_YES)
    return MATCH_NO;

  if (gfc_current_state () != COMP_MODULE
      && !(gfc_current_state () == COMP_DERIVED
	   && gfc_state_stack->previous
	   && gfc_state_stack->previous->state == COMP_MODULE)
      && !(gfc_current_state () == COMP_DERIVED_CONTAINS
	   && gfc_state_stack->previous && gfc_state_stack->previous->previous
	   && gfc_state_stack->previous->previous->state == COMP_MODULE))
    {
      gfc_error ("PRIVATE statement at %C is only allowed in the "
		 "specification part of a module");
      return MATCH_ERROR;
    }

  if (gfc_current_state () == COMP_DERIVED)
    {
      if (gfc_match_eos () == MATCH_YES)
	{
	  *st = ST_PRIVATE;
	  return MATCH_YES;
	}

      gfc_syntax_error (ST_PRIVATE);
      return MATCH_ERROR;
    }

  if (gfc_match_eos () == MATCH_YES)
    {
      *st = ST_PRIVATE;
      return MATCH_YES;
    }

  *st = ST_ATTR_DECL;
  return access_attr_decl (ST_PRIVATE);
}


match
gfc_match_public (gfc_statement *st)
{

  if (gfc_match ("public") != MATCH_YES)
    return MATCH_NO;

  if (gfc_current_state () != COMP_MODULE)
    {
      gfc_error ("PUBLIC statement at %C is only allowed in the "
		 "specification part of a module");
      return MATCH_ERROR;
    }

  if (gfc_match_eos () == MATCH_YES)
    {
      *st = ST_PUBLIC;
      return MATCH_YES;
    }

  *st = ST_ATTR_DECL;
  return access_attr_decl (ST_PUBLIC);
}


/* Workhorse for gfc_match_parameter.  */

static match
do_parm (void)
{
  gfc_symbol *sym;
  gfc_expr *init;
  match m;
  bool t;

  m = gfc_match_symbol (&sym, 0);
  if (m == MATCH_NO)
    gfc_error ("Expected variable name at %C in PARAMETER statement");

  if (m != MATCH_YES)
    return m;

  if (gfc_match_char ('=') == MATCH_NO)
    {
      gfc_error ("Expected = sign in PARAMETER statement at %C");
      return MATCH_ERROR;
    }

  m = gfc_match_init_expr (&init);
  if (m == MATCH_NO)
    gfc_error ("Expected expression at %C in PARAMETER statement");
  if (m != MATCH_YES)
    return m;

  if (sym->ts.type == BT_UNKNOWN
      && !gfc_set_default_type (sym, 1, NULL))
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  if (!gfc_check_assign_symbol (sym, NULL, init)
      || !gfc_add_flavor (&sym->attr, FL_PARAMETER, sym->name, NULL))
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  if (sym->value)
    {
      gfc_error ("Initializing already initialized variable at %C");
      m = MATCH_ERROR;
      goto cleanup;
    }

  t = add_init_expr_to_sym (sym->name, &init, &gfc_current_locus);
  return (t) ? MATCH_YES : MATCH_ERROR;

cleanup:
  gfc_free_expr (init);
  return m;
}


/* Match a parameter statement, with the weird syntax that these have.  */

match
gfc_match_parameter (void)
{
  const char *term = " )%t";
  match m;

  if (gfc_match_char ('(') == MATCH_NO)
    {
      /* With legacy PARAMETER statements, don't expect a terminating ')'.  */
      if (!gfc_notify_std (GFC_STD_LEGACY, "PARAMETER without '()' at %C"))
	return MATCH_NO;
      term = " %t";
    }

  for (;;)
    {
      m = do_parm ();
      if (m != MATCH_YES)
	break;

      if (gfc_match (term) == MATCH_YES)
	break;

      if (gfc_match_char (',') != MATCH_YES)
	{
	  gfc_error ("Unexpected characters in PARAMETER statement at %C");
	  m = MATCH_ERROR;
	  break;
	}
    }

  return m;
}


match
gfc_match_automatic (void)
{
  gfc_symbol *sym;
  match m;
  bool seen_symbol = false;

  if (!flag_dec_static)
    {
      gfc_error ("%s at %C is a DEC extension, enable with "
		 "%<-fdec-static%>",
		 "AUTOMATIC"
		 );
      return MATCH_ERROR;
    }

  gfc_match (" ::");

  for (;;)
    {
      m = gfc_match_symbol (&sym, 0);
      switch (m)
      {
      case MATCH_NO:
        break;

      case MATCH_ERROR:
	return MATCH_ERROR;

      case MATCH_YES:
	if (!gfc_add_automatic (&sym->attr, sym->name, &gfc_current_locus))
	  return MATCH_ERROR;
	seen_symbol = true;
	break;
      }

      if (gfc_match_eos () == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  if (!seen_symbol)
    {
      gfc_error ("Expected entity-list in AUTOMATIC statement at %C");
      return MATCH_ERROR;
    }

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in AUTOMATIC statement at %C");
  return MATCH_ERROR;
}


match
gfc_match_static (void)
{
  gfc_symbol *sym;
  match m;
  bool seen_symbol = false;

  if (!flag_dec_static)
    {
      gfc_error ("%s at %C is a DEC extension, enable with "
		 "%<-fdec-static%>",
		 "STATIC");
      return MATCH_ERROR;
    }

  gfc_match (" ::");

  for (;;)
    {
      m = gfc_match_symbol (&sym, 0);
      switch (m)
      {
      case MATCH_NO:
        break;

      case MATCH_ERROR:
	return MATCH_ERROR;

      case MATCH_YES:
	if (!gfc_add_save (&sym->attr, SAVE_EXPLICIT, sym->name,
			  &gfc_current_locus))
	  return MATCH_ERROR;
	seen_symbol = true;
	break;
      }

      if (gfc_match_eos () == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  if (!seen_symbol)
    {
      gfc_error ("Expected entity-list in STATIC statement at %C");
      return MATCH_ERROR;
    }

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in STATIC statement at %C");
  return MATCH_ERROR;
}


/* Save statements have a special syntax.  */

match
gfc_match_save (void)
{
  char n[GFC_MAX_SYMBOL_LEN+1];
  gfc_common_head *c;
  gfc_symbol *sym;
  match m;

  if (gfc_match_eos () == MATCH_YES)
    {
      if (gfc_current_ns->seen_save)
	{
	  if (!gfc_notify_std (GFC_STD_LEGACY, "Blanket SAVE statement at %C "
			       "follows previous SAVE statement"))
	    return MATCH_ERROR;
	}

      gfc_current_ns->save_all = gfc_current_ns->seen_save = 1;
      return MATCH_YES;
    }

  if (gfc_current_ns->save_all)
    {
      if (!gfc_notify_std (GFC_STD_LEGACY, "SAVE statement at %C follows "
			   "blanket SAVE statement"))
	return MATCH_ERROR;
    }

  gfc_match (" ::");

  for (;;)
    {
      m = gfc_match_symbol (&sym, 0);
      switch (m)
	{
	case MATCH_YES:
	  if (!gfc_add_save (&sym->attr, SAVE_EXPLICIT, sym->name,
			     &gfc_current_locus))
	    return MATCH_ERROR;
	  goto next_item;

	case MATCH_NO:
	  break;

	case MATCH_ERROR:
	  return MATCH_ERROR;
	}

      m = gfc_match (" / %n /", &n);
      if (m == MATCH_ERROR)
	return MATCH_ERROR;
      if (m == MATCH_NO)
	goto syntax;

      c = gfc_get_common (n, 0);
      c->saved = 1;

      gfc_current_ns->seen_save = 1;

    next_item:
      if (gfc_match_eos () == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in SAVE statement at %C");
  return MATCH_ERROR;
}


match
gfc_match_value (void)
{
  gfc_symbol *sym;
  match m;

  /* This is not allowed within a BLOCK construct!  */
  if (gfc_current_state () == COMP_BLOCK)
    {
      gfc_error ("VALUE is not allowed inside of BLOCK at %C");
      return MATCH_ERROR;
    }

  if (!gfc_notify_std (GFC_STD_F2003, "VALUE statement at %C"))
    return MATCH_ERROR;

  if (gfc_match (" ::") == MATCH_NO && gfc_match_space () == MATCH_NO)
    {
      return MATCH_ERROR;
    }

  if (gfc_match_eos () == MATCH_YES)
    goto syntax;

  for(;;)
    {
      m = gfc_match_symbol (&sym, 0);
      switch (m)
	{
	case MATCH_YES:
	  if (!gfc_add_value (&sym->attr, sym->name, &gfc_current_locus))
	    return MATCH_ERROR;
	  goto next_item;

	case MATCH_NO:
	  break;

	case MATCH_ERROR:
	  return MATCH_ERROR;
	}

    next_item:
      if (gfc_match_eos () == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in VALUE statement at %C");
  return MATCH_ERROR;
}


match
gfc_match_volatile (void)
{
  gfc_symbol *sym;
  match m;

  if (!gfc_notify_std (GFC_STD_F2003, "VOLATILE statement at %C"))
    return MATCH_ERROR;

  if (gfc_match (" ::") == MATCH_NO && gfc_match_space () == MATCH_NO)
    {
      return MATCH_ERROR;
    }

  if (gfc_match_eos () == MATCH_YES)
    goto syntax;

  for(;;)
    {
      /* VOLATILE is special because it can be added to host-associated
	 symbols locally.  Except for coarrays.  */
      m = gfc_match_symbol (&sym, 1);
      switch (m)
	{
	case MATCH_YES:
	  /* F2008, C560+C561. VOLATILE for host-/use-associated variable or
	     for variable in a BLOCK which is defined outside of the BLOCK.  */
	  if (sym->ns != gfc_current_ns && sym->attr.codimension)
	    {
	      gfc_error ("Specifying VOLATILE for coarray variable %qs at "
			 "%C, which is use-/host-associated", sym->name);
	      return MATCH_ERROR;
	    }
	  if (!gfc_add_volatile (&sym->attr, sym->name, &gfc_current_locus))
	    return MATCH_ERROR;
	  goto next_item;

	case MATCH_NO:
	  break;

	case MATCH_ERROR:
	  return MATCH_ERROR;
	}

    next_item:
      if (gfc_match_eos () == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in VOLATILE statement at %C");
  return MATCH_ERROR;
}


match
gfc_match_asynchronous (void)
{
  gfc_symbol *sym;
  match m;

  if (!gfc_notify_std (GFC_STD_F2003, "ASYNCHRONOUS statement at %C"))
    return MATCH_ERROR;

  if (gfc_match (" ::") == MATCH_NO && gfc_match_space () == MATCH_NO)
    {
      return MATCH_ERROR;
    }

  if (gfc_match_eos () == MATCH_YES)
    goto syntax;

  for(;;)
    {
      /* ASYNCHRONOUS is special because it can be added to host-associated
	 symbols locally.  */
      m = gfc_match_symbol (&sym, 1);
      switch (m)
	{
	case MATCH_YES:
	  if (!gfc_add_asynchronous (&sym->attr, sym->name, &gfc_current_locus))
	    return MATCH_ERROR;
	  goto next_item;

	case MATCH_NO:
	  break;

	case MATCH_ERROR:
	  return MATCH_ERROR;
	}

    next_item:
      if (gfc_match_eos () == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in ASYNCHRONOUS statement at %C");
  return MATCH_ERROR;
}


/* Match a module procedure statement in a submodule.  */

match
gfc_match_submod_proc (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym, *fsym;
  match m;
  gfc_formal_arglist *formal, *head, *tail;

  if (gfc_current_state () != COMP_CONTAINS
      || !(gfc_state_stack->previous
	   && (gfc_state_stack->previous->state == COMP_SUBMODULE
	       || gfc_state_stack->previous->state == COMP_MODULE)))
    return MATCH_NO;

  m = gfc_match (" module% procedure% %n", name);
  if (m != MATCH_YES)
    return m;

  if (!gfc_notify_std (GFC_STD_F2008, "MODULE PROCEDURE declaration "
				      "at %C"))
    return MATCH_ERROR;

  if (get_proc_name (name, &sym, false))
    return MATCH_ERROR;

  /* Make sure that the result field is appropriately filled, even though
     the result symbol will be replaced later on.  */
  if (sym->tlink && sym->tlink->attr.function)
    {
      if (sym->tlink->result
	  && sym->tlink->result != sym->tlink)
	sym->result= sym->tlink->result;
      else
	sym->result = sym;
    }

  /* Set declared_at as it might point to, e.g., a PUBLIC statement, if
     the symbol existed before.  */
  sym->declared_at = gfc_current_locus;

  if (!sym->attr.module_procedure)
    return MATCH_ERROR;

  /* Signal match_end to expect "end procedure".  */
  sym->abr_modproc_decl = 1;

  /* Change from IFSRC_IFBODY coming from the interface declaration.  */
  sym->attr.if_source = IFSRC_DECL;

  gfc_new_block = sym;

  /* Make a new formal arglist with the symbols in the procedure
      namespace.  */
  head = tail = NULL;
  for (formal = sym->formal; formal && formal->sym; formal = formal->next)
    {
      if (formal == sym->formal)
	head = tail = gfc_get_formal_arglist ();
      else
	{
	  tail->next = gfc_get_formal_arglist ();
	  tail = tail->next;
	}

      if (gfc_copy_dummy_sym (&fsym, formal->sym, 0))
	goto cleanup;

      tail->sym = fsym;
      gfc_set_sym_referenced (fsym);
    }

  /* The dummy symbols get cleaned up, when the formal_namespace of the
     interface declaration is cleared.  This allows us to add the
     explicit interface as is done for other type of procedure.  */
  if (!gfc_add_explicit_interface (sym, IFSRC_DECL, head,
				   &gfc_current_locus))
    return MATCH_ERROR;

  if (gfc_match_eos () != MATCH_YES)
    {
      gfc_syntax_error (ST_MODULE_PROC);
      return MATCH_ERROR;
    }

  return MATCH_YES;

cleanup:
  gfc_free_formal_arglist (head);
  return MATCH_ERROR;
}


/* Match a module procedure statement.  Note that we have to modify
   symbols in the parent's namespace because the current one was there
   to receive symbols that are in an interface's formal argument list.  */

match
gfc_match_modproc (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  match m;
  locus old_locus;
  gfc_namespace *module_ns;
  gfc_interface *old_interface_head, *interface;

  if (gfc_state_stack->state != COMP_INTERFACE
      || gfc_state_stack->previous == NULL
      || current_interface.type == INTERFACE_NAMELESS
      || current_interface.type == INTERFACE_ABSTRACT)
    {
      gfc_error ("MODULE PROCEDURE at %C must be in a generic module "
		 "interface");
      return MATCH_ERROR;
    }

  module_ns = gfc_current_ns->parent;
  for (; module_ns; module_ns = module_ns->parent)
    if (module_ns->proc_name->attr.flavor == FL_MODULE
	|| module_ns->proc_name->attr.flavor == FL_PROGRAM
	|| (module_ns->proc_name->attr.flavor == FL_PROCEDURE
	    && !module_ns->proc_name->attr.contained))
      break;

  if (module_ns == NULL)
    return MATCH_ERROR;

  /* Store the current state of the interface. We will need it if we
     end up with a syntax error and need to recover.  */
  old_interface_head = gfc_current_interface_head ();

  /* Check if the F2008 optional double colon appears.  */
  gfc_gobble_whitespace ();
  old_locus = gfc_current_locus;
  if (gfc_match ("::") == MATCH_YES)
    {
      if (!gfc_notify_std (GFC_STD_F2008, "double colon in "
			   "MODULE PROCEDURE statement at %L", &old_locus))
	return MATCH_ERROR;
    }
  else
    gfc_current_locus = old_locus;

  for (;;)
    {
      bool last = false;
      old_locus = gfc_current_locus;

      m = gfc_match_name (name);
      if (m == MATCH_NO)
	goto syntax;
      if (m != MATCH_YES)
	return MATCH_ERROR;

      /* Check for syntax error before starting to add symbols to the
	 current namespace.  */
      if (gfc_match_eos () == MATCH_YES)
	last = true;

      if (!last && gfc_match_char (',') != MATCH_YES)
	goto syntax;

      /* Now we're sure the syntax is valid, we process this item
	 further.  */
      if (gfc_get_symbol (name, module_ns, &sym))
	return MATCH_ERROR;

      if (sym->attr.intrinsic)
	{
	  gfc_error ("Intrinsic procedure at %L cannot be a MODULE "
		     "PROCEDURE", &old_locus);
	  return MATCH_ERROR;
	}

      if (sym->attr.proc != PROC_MODULE
	  && !gfc_add_procedure (&sym->attr, PROC_MODULE, sym->name, NULL))
	return MATCH_ERROR;

      if (!gfc_add_interface (sym))
	return MATCH_ERROR;

      sym->attr.mod_proc = 1;
      sym->declared_at = old_locus;

      if (last)
	break;
    }

  return MATCH_YES;

syntax:
  /* Restore the previous state of the interface.  */
  interface = gfc_current_interface_head ();
  gfc_set_current_interface_head (old_interface_head);

  /* Free the new interfaces.  */
  while (interface != old_interface_head)
  {
    gfc_interface *i = interface->next;
    free (interface);
    interface = i;
  }

  /* And issue a syntax error.  */
  gfc_syntax_error (ST_MODULE_PROC);
  return MATCH_ERROR;
}


/* Check a derived type that is being extended.  */

static gfc_symbol*
check_extended_derived_type (char *name)
{
  gfc_symbol *extended;

  if (gfc_find_symbol (name, gfc_current_ns, 1, &extended))
    {
      gfc_error ("Ambiguous symbol in TYPE definition at %C");
      return NULL;
    }

  extended = gfc_find_dt_in_generic (extended);

  /* F08:C428.  */
  if (!extended)
    {
      gfc_error ("Symbol %qs at %C has not been previously defined", name);
      return NULL;
    }

  if (extended->attr.flavor != FL_DERIVED)
    {
      gfc_error ("%qs in EXTENDS expression at %C is not a "
		 "derived type", name);
      return NULL;
    }

  if (extended->attr.is_bind_c)
    {
      gfc_error ("%qs cannot be extended at %C because it "
		 "is BIND(C)", extended->name);
      return NULL;
    }

  if (extended->attr.sequence)
    {
      gfc_error ("%qs cannot be extended at %C because it "
		 "is a SEQUENCE type", extended->name);
      return NULL;
    }

  return extended;
}


/* Match the optional attribute specifiers for a type declaration.
   Return MATCH_ERROR if an error is encountered in one of the handled
   attributes (public, private, bind(c)), MATCH_NO if what's found is
   not a handled attribute, and MATCH_YES otherwise.  TODO: More error
   checking on attribute conflicts needs to be done.  */

match
gfc_get_type_attr_spec (symbol_attribute *attr, char *name)
{
  /* See if the derived type is marked as private.  */
  if (gfc_match (" , private") == MATCH_YES)
    {
      if (gfc_current_state () != COMP_MODULE)
	{
	  gfc_error ("Derived type at %C can only be PRIVATE in the "
		     "specification part of a module");
	  return MATCH_ERROR;
	}

      if (!gfc_add_access (attr, ACCESS_PRIVATE, NULL, NULL))
	return MATCH_ERROR;
    }
  else if (gfc_match (" , public") == MATCH_YES)
    {
      if (gfc_current_state () != COMP_MODULE)
	{
	  gfc_error ("Derived type at %C can only be PUBLIC in the "
		     "specification part of a module");
	  return MATCH_ERROR;
	}

      if (!gfc_add_access (attr, ACCESS_PUBLIC, NULL, NULL))
	return MATCH_ERROR;
    }
  else if (gfc_match (" , bind ( c )") == MATCH_YES)
    {
      /* If the type is defined to be bind(c) it then needs to make
	 sure that all fields are interoperable.  This will
	 need to be a semantic check on the finished derived type.
	 See 15.2.3 (lines 9-12) of F2003 draft.  */
      if (!gfc_add_is_bind_c (attr, NULL, &gfc_current_locus, 0))
	return MATCH_ERROR;

      /* TODO: attr conflicts need to be checked, probably in symbol.c.  */
    }
  else if (gfc_match (" , abstract") == MATCH_YES)
    {
      if (!gfc_notify_std (GFC_STD_F2003, "ABSTRACT type at %C"))
	return MATCH_ERROR;

      if (!gfc_add_abstract (attr, &gfc_current_locus))
	return MATCH_ERROR;
    }
  else if (name && gfc_match (" , extends ( %n )", name) == MATCH_YES)
    {
      if (!gfc_add_extension (attr, &gfc_current_locus))
	return MATCH_ERROR;
    }
  else
    return MATCH_NO;

  /* If we get here, something matched.  */
  return MATCH_YES;
}


/* Common function for type declaration blocks similar to derived types, such
   as STRUCTURES and MAPs. Unlike derived types, a structure type
   does NOT have a generic symbol matching the name given by the user.
   STRUCTUREs can share names with variables and PARAMETERs so we must allow
   for the creation of an independent symbol.
   Other parameters are a message to prefix errors with, the name of the new
   type to be created, and the flavor to add to the resulting symbol. */

static bool
get_struct_decl (const char *name, sym_flavor fl, locus *decl,
                 gfc_symbol **result)
{
  gfc_symbol *sym;
  locus where;

  gcc_assert (name[0] == (char) TOUPPER (name[0]));

  if (decl)
    where = *decl;
  else
    where = gfc_current_locus;

  if (gfc_get_symbol (name, NULL, &sym))
    return false;

  if (!sym)
    {
      gfc_internal_error ("Failed to create structure type '%s' at %C", name);
      return false;
    }

  if (sym->components != NULL || sym->attr.zero_comp)
    {
      gfc_error ("Type definition of %qs at %C was already defined at %L",
                 sym->name, &sym->declared_at);
      return false;
    }

  sym->declared_at = where;

  if (sym->attr.flavor != fl
      && !gfc_add_flavor (&sym->attr, fl, sym->name, NULL))
    return false;

  if (!sym->hash_value)
      /* Set the hash for the compound name for this type.  */
    sym->hash_value = gfc_hash_value (sym);

  /* Normally the type is expected to have been completely parsed by the time
     a field declaration with this type is seen. For unions, maps, and nested
     structure declarations, we need to indicate that it is okay that we
     haven't seen any components yet. This will be updated after the structure
     is fully parsed. */
  sym->attr.zero_comp = 0;

  /* Structures always act like derived-types with the SEQUENCE attribute */
  gfc_add_sequence (&sym->attr, sym->name, NULL);

  if (result) *result = sym;

  return true;
}


/* Match the opening of a MAP block. Like a struct within a union in C;
   behaves identical to STRUCTURE blocks.  */

match
gfc_match_map (void)
{
  /* Counter used to give unique internal names to map structures. */
  static unsigned int gfc_map_id = 0;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  locus old_loc;

  old_loc = gfc_current_locus;

  if (gfc_match_eos () != MATCH_YES)
    {
	gfc_error ("Junk after MAP statement at %C");
	gfc_current_locus = old_loc;
	return MATCH_ERROR;
    }

  /* Map blocks are anonymous so we make up unique names for the symbol table
     which are invalid Fortran identifiers.  */
  snprintf (name, GFC_MAX_SYMBOL_LEN + 1, "MM$%u", gfc_map_id++);

  if (!get_struct_decl (name, FL_STRUCT, &old_loc, &sym))
    return MATCH_ERROR;

  gfc_new_block = sym;

  return MATCH_YES;
}


/* Match the opening of a UNION block.  */

match
gfc_match_union (void)
{
  /* Counter used to give unique internal names to union types. */
  static unsigned int gfc_union_id = 0;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  locus old_loc;

  old_loc = gfc_current_locus;

  if (gfc_match_eos () != MATCH_YES)
    {
	gfc_error ("Junk after UNION statement at %C");
	gfc_current_locus = old_loc;
	return MATCH_ERROR;
    }

  /* Unions are anonymous so we make up unique names for the symbol table
     which are invalid Fortran identifiers.  */
  snprintf (name, GFC_MAX_SYMBOL_LEN + 1, "UU$%u", gfc_union_id++);

  if (!get_struct_decl (name, FL_UNION, &old_loc, &sym))
    return MATCH_ERROR;

  gfc_new_block = sym;

  return MATCH_YES;
}


/* Match the beginning of a STRUCTURE declaration. This is similar to
   matching the beginning of a derived type declaration with a few
   twists. The resulting type symbol has no access control or other
   interesting attributes.  */

match
gfc_match_structure_decl (void)
{
  /* Counter used to give unique internal names to anonymous structures.  */
  static unsigned int gfc_structure_id = 0;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  match m;
  locus where;

  if (!flag_dec_structure)
    {
      gfc_error ("%s at %C is a DEC extension, enable with "
		 "%<-fdec-structure%>",
		 "STRUCTURE");
      return MATCH_ERROR;
    }

  name[0] = '\0';

  m = gfc_match (" /%n/", name);
  if (m != MATCH_YES)
    {
      /* Non-nested structure declarations require a structure name.  */
      if (!gfc_comp_struct (gfc_current_state ()))
	{
	    gfc_error ("Structure name expected in non-nested structure "
		       "declaration at %C");
	    return MATCH_ERROR;
	}
      /* This is an anonymous structure; make up a unique name for it
	 (upper-case letters never make it to symbol names from the source).
	 The important thing is initializing the type variable
	 and setting gfc_new_symbol, which is immediately used by
	 parse_structure () and variable_decl () to add components of
	 this type.  */
      snprintf (name, GFC_MAX_SYMBOL_LEN + 1, "SS$%u", gfc_structure_id++);
    }

  where = gfc_current_locus;
  /* No field list allowed after non-nested structure declaration.  */
  if (!gfc_comp_struct (gfc_current_state ())
      && gfc_match_eos () != MATCH_YES)
    {
      gfc_error ("Junk after non-nested STRUCTURE statement at %C");
      return MATCH_ERROR;
    }

  /* Make sure the name is not the name of an intrinsic type.  */
  if (gfc_is_intrinsic_typename (name))
    {
      gfc_error ("Structure name %qs at %C cannot be the same as an"
		 " intrinsic type", name);
      return MATCH_ERROR;
    }

  /* Store the actual type symbol for the structure with an upper-case first
     letter (an invalid Fortran identifier).  */

  if (!get_struct_decl (gfc_dt_upper_string (name), FL_STRUCT, &where, &sym))
    return MATCH_ERROR;

  gfc_new_block = sym;
  return MATCH_YES;
}


/* This function does some work to determine which matcher should be used to
 * match a statement beginning with "TYPE". This is used to disambiguate TYPE
 * as an alias for PRINT from derived type declarations, TYPE IS statements,
 * and derived type data declarations.  */

match
gfc_match_type (gfc_statement *st)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  match m;
  locus old_loc;

  /* Requires -fdec.  */
  if (!flag_dec)
    return MATCH_NO;

  m = gfc_match ("type");
  if (m != MATCH_YES)
    return m;
  /* If we already have an error in the buffer, it is probably from failing to
   * match a derived type data declaration. Let it happen.  */
  else if (gfc_error_flag_test ())
    return MATCH_NO;

  old_loc = gfc_current_locus;
  *st = ST_NONE;

  /* If we see an attribute list before anything else it's definitely a derived
   * type declaration.  */
  if (gfc_match (" ,") == MATCH_YES || gfc_match (" ::") == MATCH_YES)
    {
      gfc_current_locus = old_loc;
      *st = ST_DERIVED_DECL;
      return gfc_match_derived_decl ();
    }

  /* By now "TYPE" has already been matched. If we do not see a name, this may
   * be something like "TYPE *" or "TYPE <fmt>".  */
  m = gfc_match_name (name);
  if (m != MATCH_YES)
    {
      /* Let print match if it can, otherwise throw an error from
       * gfc_match_derived_decl.  */
      gfc_current_locus = old_loc;
      if (gfc_match_print () == MATCH_YES)
	{
	  *st = ST_WRITE;
	  return MATCH_YES;
	}
      gfc_current_locus = old_loc;
      *st = ST_DERIVED_DECL;
      return gfc_match_derived_decl ();
    }

  /* A derived type declaration requires an EOS. Without it, assume print.  */
  m = gfc_match_eos ();
  if (m == MATCH_NO)
    {
      /* Check manually for TYPE IS (... - this is invalid print syntax.  */
      if (strncmp ("is", name, 3) == 0
	  && gfc_match (" (", name) == MATCH_YES)
	{
	  gfc_current_locus = old_loc;
	  gcc_assert (gfc_match (" is") == MATCH_YES);
	  *st = ST_TYPE_IS;
	  return gfc_match_type_is ();
	}
      gfc_current_locus = old_loc;
      *st = ST_WRITE;
      return gfc_match_print ();
    }
  else
    {
      /* By now we have "TYPE <name> <EOS>". Check first if the name is an
       * intrinsic typename - if so let gfc_match_derived_decl dump an error.
       * Otherwise if gfc_match_derived_decl fails it's probably an existing
       * symbol which can be printed.  */
      gfc_current_locus = old_loc;
      m = gfc_match_derived_decl ();
      if (gfc_is_intrinsic_typename (name) || m == MATCH_YES)
	{
	  *st = ST_DERIVED_DECL;
	  return m;
	}
      gfc_current_locus = old_loc;
      *st = ST_WRITE;
      return gfc_match_print ();
    }

  return MATCH_NO;
}


/* Match the beginning of a derived type declaration.  If a type name
   was the result of a function, then it is possible to have a symbol
   already to be known as a derived type yet have no components.  */

match
gfc_match_derived_decl (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  char parent[GFC_MAX_SYMBOL_LEN + 1];
  symbol_attribute attr;
  gfc_symbol *sym, *gensym;
  gfc_symbol *extended;
  match m;
  match is_type_attr_spec = MATCH_NO;
  bool seen_attr = false;
  gfc_interface *intr = NULL, *head;
  bool parameterized_type = false;
  bool seen_colons = false;

  if (gfc_comp_struct (gfc_current_state ()))
    return MATCH_NO;

  name[0] = '\0';
  parent[0] = '\0';
  gfc_clear_attr (&attr);
  extended = NULL;

  do
    {
      is_type_attr_spec = gfc_get_type_attr_spec (&attr, parent);
      if (is_type_attr_spec == MATCH_ERROR)
	return MATCH_ERROR;
      if (is_type_attr_spec == MATCH_YES)
	seen_attr = true;
    } while (is_type_attr_spec == MATCH_YES);

  /* Deal with derived type extensions.  The extension attribute has
     been added to 'attr' but now the parent type must be found and
     checked.  */
  if (parent[0])
    extended = check_extended_derived_type (parent);

  if (parent[0] && !extended)
    return MATCH_ERROR;

  m = gfc_match (" ::");
  if (m == MATCH_YES)
    {
      seen_colons = true;
    }
  else if (seen_attr)
    {
      gfc_error ("Expected :: in TYPE definition at %C");
      return MATCH_ERROR;
    }

  m = gfc_match (" %n ", name);
  if (m != MATCH_YES)
    return m;

  /* Make sure that we don't identify TYPE IS (...) as a parameterized
     derived type named 'is'.
     TODO Expand the check, when 'name' = "is" by matching " (tname) "
     and checking if this is a(n intrinsic) typename. his picks up
     misplaced TYPE IS statements such as in select_type_1.f03.  */
  if (gfc_peek_ascii_char () == '(')
    {
      if (gfc_current_state () == COMP_SELECT_TYPE
	  || (!seen_colons && !strcmp (name, "is")))
	return MATCH_NO;
      parameterized_type = true;
    }

  m = gfc_match_eos ();
  if (m != MATCH_YES && !parameterized_type)
    return m;

  /* Make sure the name is not the name of an intrinsic type.  */
  if (gfc_is_intrinsic_typename (name))
    {
      gfc_error ("Type name %qs at %C cannot be the same as an intrinsic "
		 "type", name);
      return MATCH_ERROR;
    }

  if (gfc_get_symbol (name, NULL, &gensym))
    return MATCH_ERROR;

  if (!gensym->attr.generic && gensym->ts.type != BT_UNKNOWN)
    {
      gfc_error ("Derived type name %qs at %C already has a basic type "
		 "of %s", gensym->name, gfc_typename (&gensym->ts));
      return MATCH_ERROR;
    }

  if (!gensym->attr.generic
      && !gfc_add_generic (&gensym->attr, gensym->name, NULL))
    return MATCH_ERROR;

  if (!gensym->attr.function
      && !gfc_add_function (&gensym->attr, gensym->name, NULL))
    return MATCH_ERROR;

  sym = gfc_find_dt_in_generic (gensym);

  if (sym && (sym->components != NULL || sym->attr.zero_comp))
    {
      gfc_error ("Derived type definition of %qs at %C has already been "
                 "defined", sym->name);
      return MATCH_ERROR;
    }

  if (!sym)
    {
      /* Use upper case to save the actual derived-type symbol.  */
      gfc_get_symbol (gfc_dt_upper_string (gensym->name), NULL, &sym);
      sym->name = gfc_get_string ("%s", gensym->name);
      head = gensym->generic;
      intr = gfc_get_interface ();
      intr->sym = sym;
      intr->where = gfc_current_locus;
      intr->sym->declared_at = gfc_current_locus;
      intr->next = head;
      gensym->generic = intr;
      gensym->attr.if_source = IFSRC_DECL;
    }

  /* The symbol may already have the derived attribute without the
     components.  The ways this can happen is via a function
     definition, an INTRINSIC statement or a subtype in another
     derived type that is a pointer.  The first part of the AND clause
     is true if the symbol is not the return value of a function.  */
  if (sym->attr.flavor != FL_DERIVED
      && !gfc_add_flavor (&sym->attr, FL_DERIVED, sym->name, NULL))
    return MATCH_ERROR;

  if (attr.access != ACCESS_UNKNOWN
      && !gfc_add_access (&sym->attr, attr.access, sym->name, NULL))
    return MATCH_ERROR;
  else if (sym->attr.access == ACCESS_UNKNOWN
	   && gensym->attr.access != ACCESS_UNKNOWN
	   && !gfc_add_access (&sym->attr, gensym->attr.access,
			       sym->name, NULL))
    return MATCH_ERROR;

  if (sym->attr.access != ACCESS_UNKNOWN
      && gensym->attr.access == ACCESS_UNKNOWN)
    gensym->attr.access = sym->attr.access;

  /* See if the derived type was labeled as bind(c).  */
  if (attr.is_bind_c != 0)
    sym->attr.is_bind_c = attr.is_bind_c;

  /* Construct the f2k_derived namespace if it is not yet there.  */
  if (!sym->f2k_derived)
    sym->f2k_derived = gfc_get_namespace (NULL, 0);

  if (parameterized_type)
    {
      /* Ignore error or mismatches to avoid the component declarations
	 causing problems later.  */
      gfc_match_formal_arglist (sym, 0, 0, true);
      m = gfc_match_eos ();
      if (m != MATCH_YES)
	return m;
      sym->attr.pdt_template = 1;
    }

  if (extended && !sym->components)
    {
      gfc_component *p;
      gfc_formal_arglist *f, *g, *h;

      /* Add the extended derived type as the first component.  */
      gfc_add_component (sym, parent, &p);
      extended->refs++;
      gfc_set_sym_referenced (extended);

      p->ts.type = BT_DERIVED;
      p->ts.u.derived = extended;
      p->initializer = gfc_default_initializer (&p->ts);

      /* Set extension level.  */
      if (extended->attr.extension == 255)
	{
	  /* Since the extension field is 8 bit wide, we can only have
	     up to 255 extension levels.  */
	  gfc_error ("Maximum extension level reached with type %qs at %L",
		     extended->name, &extended->declared_at);
	  return MATCH_ERROR;
	}
      sym->attr.extension = extended->attr.extension + 1;

      /* Provide the links between the extended type and its extension.  */
      if (!extended->f2k_derived)
	extended->f2k_derived = gfc_get_namespace (NULL, 0);

      /* Copy the extended type-param-name-list from the extended type,
	 append those of the extension and add the whole lot to the
	 extension.  */
      if (extended->attr.pdt_template)
	{
	  g = h = NULL;
	  sym->attr.pdt_template = 1;
	  for (f = extended->formal; f; f = f->next)
	    {
	      if (f == extended->formal)
		{
		  g = gfc_get_formal_arglist ();
		  h = g;
		}
	      else
		{
		  g->next = gfc_get_formal_arglist ();
		  g = g->next;
		}
	      g->sym = f->sym;
	    }
	  g->next = sym->formal;
	  sym->formal = h;
	}
    }

  if (!sym->hash_value)
    /* Set the hash for the compound name for this type.  */
    sym->hash_value = gfc_hash_value (sym);

  /* Take over the ABSTRACT attribute.  */
  sym->attr.abstract = attr.abstract;

  gfc_new_block = sym;

  return MATCH_YES;
}


/* Cray Pointees can be declared as:
      pointer (ipt, a (n,m,...,*))  */

match
gfc_mod_pointee_as (gfc_array_spec *as)
{
  as->cray_pointee = true; /* This will be useful to know later.  */
  if (as->type == AS_ASSUMED_SIZE)
    as->cp_was_assumed = true;
  else if (as->type == AS_ASSUMED_SHAPE)
    {
      gfc_error ("Cray Pointee at %C cannot be assumed shape array");
      return MATCH_ERROR;
    }
  return MATCH_YES;
}


/* Match the enum definition statement, here we are trying to match
   the first line of enum definition statement.
   Returns MATCH_YES if match is found.  */

match
gfc_match_enum (void)
{
  match m;

  m = gfc_match_eos ();
  if (m != MATCH_YES)
    return m;

  if (!gfc_notify_std (GFC_STD_F2003, "ENUM and ENUMERATOR at %C"))
    return MATCH_ERROR;

  return MATCH_YES;
}


/* Returns an initializer whose value is one higher than the value of the
   LAST_INITIALIZER argument.  If the argument is NULL, the
   initializers value will be set to zero.  The initializer's kind
   will be set to gfc_c_int_kind.

   If -fshort-enums is given, the appropriate kind will be selected
   later after all enumerators have been parsed.  A warning is issued
   here if an initializer exceeds gfc_c_int_kind.  */

static gfc_expr *
enum_initializer (gfc_expr *last_initializer, locus where)
{
  gfc_expr *result;
  result = gfc_get_constant_expr (BT_INTEGER, gfc_c_int_kind, &where);

  mpz_init (result->value.integer);

  if (last_initializer != NULL)
    {
      mpz_add_ui (result->value.integer, last_initializer->value.integer, 1);
      result->where = last_initializer->where;

      if (gfc_check_integer_range (result->value.integer,
	     gfc_c_int_kind) != ARITH_OK)
	{
	  gfc_error ("Enumerator exceeds the C integer type at %C");
	  return NULL;
	}
    }
  else
    {
      /* Control comes here, if it's the very first enumerator and no
	 initializer has been given.  It will be initialized to zero.  */
      mpz_set_si (result->value.integer, 0);
    }

  return result;
}


/* Match a variable name with an optional initializer.  When this
   subroutine is called, a variable is expected to be parsed next.
   Depending on what is happening at the moment, updates either the
   symbol table or the current interface.  */

static match
enumerator_decl (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_expr *initializer;
  gfc_array_spec *as = NULL;
  gfc_symbol *sym;
  locus var_locus;
  match m;
  bool t;
  locus old_locus;

  initializer = NULL;
  old_locus = gfc_current_locus;

  /* When we get here, we've just matched a list of attributes and
     maybe a type and a double colon.  The next thing we expect to see
     is the name of the symbol.  */
  m = gfc_match_name (name);
  if (m != MATCH_YES)
    goto cleanup;

  var_locus = gfc_current_locus;

  /* OK, we've successfully matched the declaration.  Now put the
     symbol in the current namespace. If we fail to create the symbol,
     bail out.  */
  if (!build_sym (name, NULL, false, &as, &var_locus))
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  /* The double colon must be present in order to have initializers.
     Otherwise the statement is ambiguous with an assignment statement.  */
  if (colon_seen)
    {
      if (gfc_match_char ('=') == MATCH_YES)
	{
	  m = gfc_match_init_expr (&initializer);
	  if (m == MATCH_NO)
	    {
	      gfc_error ("Expected an initialization expression at %C");
	      m = MATCH_ERROR;
	    }

	  if (m != MATCH_YES)
	    goto cleanup;
	}
    }

  /* If we do not have an initializer, the initialization value of the
     previous enumerator (stored in last_initializer) is incremented
     by 1 and is used to initialize the current enumerator.  */
  if (initializer == NULL)
    initializer = enum_initializer (last_initializer, old_locus);

  if (initializer == NULL || initializer->ts.type != BT_INTEGER)
    {
      gfc_error ("ENUMERATOR %L not initialized with integer expression",
		 &var_locus);
      m = MATCH_ERROR;
      goto cleanup;
    }

  /* Store this current initializer, for the next enumerator variable
     to be parsed.  add_init_expr_to_sym() zeros initializer, so we
     use last_initializer below.  */
  last_initializer = initializer;
  t = add_init_expr_to_sym (name, &initializer, &var_locus);

  /* Maintain enumerator history.  */
  gfc_find_symbol (name, NULL, 0, &sym);
  create_enum_history (sym, last_initializer);

  return (t) ? MATCH_YES : MATCH_ERROR;

cleanup:
  /* Free stuff up and return.  */
  gfc_free_expr (initializer);

  return m;
}


/* Match the enumerator definition statement.  */

match
gfc_match_enumerator_def (void)
{
  match m;
  bool t;

  gfc_clear_ts (&current_ts);

  m = gfc_match (" enumerator");
  if (m != MATCH_YES)
    return m;

  m = gfc_match (" :: ");
  if (m == MATCH_ERROR)
    return m;

  colon_seen = (m == MATCH_YES);

  if (gfc_current_state () != COMP_ENUM)
    {
      gfc_error ("ENUM definition statement expected before %C");
      gfc_free_enum_history ();
      return MATCH_ERROR;
    }

  (&current_ts)->type = BT_INTEGER;
  (&current_ts)->kind = gfc_c_int_kind;

  gfc_clear_attr (&current_attr);
  t = gfc_add_flavor (&current_attr, FL_PARAMETER, NULL, NULL);
  if (!t)
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  for (;;)
    {
      m = enumerator_decl ();
      if (m == MATCH_ERROR)
	{
	  gfc_free_enum_history ();
	  goto cleanup;
	}
      if (m == MATCH_NO)
	break;

      if (gfc_match_eos () == MATCH_YES)
	goto cleanup;
      if (gfc_match_char (',') != MATCH_YES)
	break;
    }

  if (gfc_current_state () == COMP_ENUM)
    {
      gfc_free_enum_history ();
      gfc_error ("Syntax error in ENUMERATOR definition at %C");
      m = MATCH_ERROR;
    }

cleanup:
  gfc_free_array_spec (current_as);
  current_as = NULL;
  return m;

}


/* Match binding attributes.  */

static match
match_binding_attributes (gfc_typebound_proc* ba, bool generic, bool ppc)
{
  bool found_passing = false;
  bool seen_ptr = false;
  match m = MATCH_YES;

  /* Initialize to defaults.  Do so even before the MATCH_NO check so that in
     this case the defaults are in there.  */
  ba->access = ACCESS_UNKNOWN;
  ba->pass_arg = NULL;
  ba->pass_arg_num = 0;
  ba->nopass = 0;
  ba->non_overridable = 0;
  ba->deferred = 0;
  ba->ppc = ppc;

  /* If we find a comma, we believe there are binding attributes.  */
  m = gfc_match_char (',');
  if (m == MATCH_NO)
    goto done;

  do
    {
      /* Access specifier.  */

      m = gfc_match (" public");
      if (m == MATCH_ERROR)
	goto error;
      if (m == MATCH_YES)
	{
	  if (ba->access != ACCESS_UNKNOWN)
	    {
	      gfc_error ("Duplicate access-specifier at %C");
	      goto error;
	    }

	  ba->access = ACCESS_PUBLIC;
	  continue;
	}

      m = gfc_match (" private");
      if (m == MATCH_ERROR)
	goto error;
      if (m == MATCH_YES)
	{
	  if (ba->access != ACCESS_UNKNOWN)
	    {
	      gfc_error ("Duplicate access-specifier at %C");
	      goto error;
	    }

	  ba->access = ACCESS_PRIVATE;
	  continue;
	}

      /* If inside GENERIC, the following is not allowed.  */
      if (!generic)
	{

	  /* NOPASS flag.  */
	  m = gfc_match (" nopass");
	  if (m == MATCH_ERROR)
	    goto error;
	  if (m == MATCH_YES)
	    {
	      if (found_passing)
		{
		  gfc_error ("Binding attributes already specify passing,"
			     " illegal NOPASS at %C");
		  goto error;
		}

	      found_passing = true;
	      ba->nopass = 1;
	      continue;
	    }

	  /* PASS possibly including argument.  */
	  m = gfc_match (" pass");
	  if (m == MATCH_ERROR)
	    goto error;
	  if (m == MATCH_YES)
	    {
	      char arg[GFC_MAX_SYMBOL_LEN + 1];

	      if (found_passing)
		{
		  gfc_error ("Binding attributes already specify passing,"
			     " illegal PASS at %C");
		  goto error;
		}

	      m = gfc_match (" ( %n )", arg);
	      if (m == MATCH_ERROR)
		goto error;
	      if (m == MATCH_YES)
		ba->pass_arg = gfc_get_string ("%s", arg);
	      gcc_assert ((m == MATCH_YES) == (ba->pass_arg != NULL));

	      found_passing = true;
	      ba->nopass = 0;
	      continue;
	    }

	  if (ppc)
	    {
	      /* POINTER flag.  */
	      m = gfc_match (" pointer");
	      if (m == MATCH_ERROR)
		goto error;
	      if (m == MATCH_YES)
		{
		  if (seen_ptr)
		    {
		      gfc_error ("Duplicate POINTER attribute at %C");
		      goto error;
		    }

		  seen_ptr = true;
        	  continue;
		}
	    }
	  else
	    {
	      /* NON_OVERRIDABLE flag.  */
	      m = gfc_match (" non_overridable");
	      if (m == MATCH_ERROR)
		goto error;
	      if (m == MATCH_YES)
		{
		  if (ba->non_overridable)
		    {
		      gfc_error ("Duplicate NON_OVERRIDABLE at %C");
		      goto error;
		    }

		  ba->non_overridable = 1;
		  continue;
		}

	      /* DEFERRED flag.  */
	      m = gfc_match (" deferred");
	      if (m == MATCH_ERROR)
		goto error;
	      if (m == MATCH_YES)
		{
		  if (ba->deferred)
		    {
		      gfc_error ("Duplicate DEFERRED at %C");
		      goto error;
		    }

		  ba->deferred = 1;
		  continue;
		}
	    }

	}

      /* Nothing matching found.  */
      if (generic)
	gfc_error ("Expected access-specifier at %C");
      else
	gfc_error ("Expected binding attribute at %C");
      goto error;
    }
  while (gfc_match_char (',') == MATCH_YES);

  /* NON_OVERRIDABLE and DEFERRED exclude themselves.  */
  if (ba->non_overridable && ba->deferred)
    {
      gfc_error ("NON_OVERRIDABLE and DEFERRED can't both appear at %C");
      goto error;
    }

  m = MATCH_YES;

done:
  if (ba->access == ACCESS_UNKNOWN)
    ba->access = gfc_typebound_default_access;

  if (ppc && !seen_ptr)
    {
      gfc_error ("POINTER attribute is required for procedure pointer component"
                 " at %C");
      goto error;
    }

  return m;

error:
  return MATCH_ERROR;
}


/* Match a PROCEDURE specific binding inside a derived type.  */

static match
match_procedure_in_type (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  char target_buf[GFC_MAX_SYMBOL_LEN + 1];
  char* target = NULL, *ifc = NULL;
  gfc_typebound_proc tb;
  bool seen_colons;
  bool seen_attrs;
  match m;
  gfc_symtree* stree;
  gfc_namespace* ns;
  gfc_symbol* block;
  int num;

  /* Check current state.  */
  gcc_assert (gfc_state_stack->state == COMP_DERIVED_CONTAINS);
  block = gfc_state_stack->previous->sym;
  gcc_assert (block);

  /* Try to match PROCEDURE(interface).  */
  if (gfc_match (" (") == MATCH_YES)
    {
      m = gfc_match_name (target_buf);
      if (m == MATCH_ERROR)
	return m;
      if (m != MATCH_YES)
	{
	  gfc_error ("Interface-name expected after %<(%> at %C");
	  return MATCH_ERROR;
	}

      if (gfc_match (" )") != MATCH_YES)
	{
	  gfc_error ("%<)%> expected at %C");
	  return MATCH_ERROR;
	}

      ifc = target_buf;
    }

  /* Construct the data structure.  */
  memset (&tb, 0, sizeof (tb));
  tb.where = gfc_current_locus;

  /* Match binding attributes.  */
  m = match_binding_attributes (&tb, false, false);
  if (m == MATCH_ERROR)
    return m;
  seen_attrs = (m == MATCH_YES);

  /* Check that attribute DEFERRED is given if an interface is specified.  */
  if (tb.deferred && !ifc)
    {
      gfc_error ("Interface must be specified for DEFERRED binding at %C");
      return MATCH_ERROR;
    }
  if (ifc && !tb.deferred)
    {
      gfc_error ("PROCEDURE(interface) at %C should be declared DEFERRED");
      return MATCH_ERROR;
    }

  /* Match the colons.  */
  m = gfc_match (" ::");
  if (m == MATCH_ERROR)
    return m;
  seen_colons = (m == MATCH_YES);
  if (seen_attrs && !seen_colons)
    {
      gfc_error ("Expected %<::%> after binding-attributes at %C");
      return MATCH_ERROR;
    }

  /* Match the binding names.  */
  for(num=1;;num++)
    {
      m = gfc_match_name (name);
      if (m == MATCH_ERROR)
	return m;
      if (m == MATCH_NO)
	{
	  gfc_error ("Expected binding name at %C");
	  return MATCH_ERROR;
	}

      if (num>1 && !gfc_notify_std (GFC_STD_F2008, "PROCEDURE list at %C"))
	return MATCH_ERROR;

      /* Try to match the '=> target', if it's there.  */
      target = ifc;
      m = gfc_match (" =>");
      if (m == MATCH_ERROR)
	return m;
      if (m == MATCH_YES)
	{
	  if (tb.deferred)
	    {
	      gfc_error ("%<=> target%> is invalid for DEFERRED binding at %C");
	      return MATCH_ERROR;
	    }

	  if (!seen_colons)
	    {
	      gfc_error ("%<::%> needed in PROCEDURE binding with explicit target"
			 " at %C");
	      return MATCH_ERROR;
	    }

	  m = gfc_match_name (target_buf);
	  if (m == MATCH_ERROR)
	    return m;
	  if (m == MATCH_NO)
	    {
	      gfc_error ("Expected binding target after %<=>%> at %C");
	      return MATCH_ERROR;
	    }
	  target = target_buf;
	}

      /* If no target was found, it has the same name as the binding.  */
      if (!target)
	target = name;

      /* Get the namespace to insert the symbols into.  */
      ns = block->f2k_derived;
      gcc_assert (ns);

      /* If the binding is DEFERRED, check that the containing type is ABSTRACT.  */
      if (tb.deferred && !block->attr.abstract)
	{
	  gfc_error ("Type %qs containing DEFERRED binding at %C "
		     "is not ABSTRACT", block->name);
	  return MATCH_ERROR;
	}

      /* See if we already have a binding with this name in the symtree which
	 would be an error.  If a GENERIC already targeted this binding, it may
	 be already there but then typebound is still NULL.  */
      stree = gfc_find_symtree (ns->tb_sym_root, name);
      if (stree && stree->n.tb)
	{
	  gfc_error ("There is already a procedure with binding name %qs for "
		     "the derived type %qs at %C", name, block->name);
	  return MATCH_ERROR;
	}

      /* Insert it and set attributes.  */

      if (!stree)
	{
	  stree = gfc_new_symtree (&ns->tb_sym_root, name);
	  gcc_assert (stree);
	}
      stree->n.tb = gfc_get_typebound_proc (&tb);

      if (gfc_get_sym_tree (target, gfc_current_ns, &stree->n.tb->u.specific,
			    false))
	return MATCH_ERROR;
      gfc_set_sym_referenced (stree->n.tb->u.specific->n.sym);
      gfc_add_flavor(&stree->n.tb->u.specific->n.sym->attr, FL_PROCEDURE,
		     target, &stree->n.tb->u.specific->n.sym->declared_at);

      if (gfc_match_eos () == MATCH_YES)
	return MATCH_YES;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

syntax:
  gfc_error ("Syntax error in PROCEDURE statement at %C");
  return MATCH_ERROR;
}


/* Match a GENERIC procedure binding inside a derived type.  */

match
gfc_match_generic (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  char bind_name[GFC_MAX_SYMBOL_LEN + 16]; /* Allow space for OPERATOR(...).  */
  gfc_symbol* block;
  gfc_typebound_proc tbattr; /* Used for match_binding_attributes.  */
  gfc_typebound_proc* tb;
  gfc_namespace* ns;
  interface_type op_type;
  gfc_intrinsic_op op;
  match m;

  /* Check current state.  */
  if (gfc_current_state () == COMP_DERIVED)
    {
      gfc_error ("GENERIC at %C must be inside a derived-type CONTAINS");
      return MATCH_ERROR;
    }
  if (gfc_current_state () != COMP_DERIVED_CONTAINS)
    return MATCH_NO;
  block = gfc_state_stack->previous->sym;
  ns = block->f2k_derived;
  gcc_assert (block && ns);

  memset (&tbattr, 0, sizeof (tbattr));
  tbattr.where = gfc_current_locus;

  /* See if we get an access-specifier.  */
  m = match_binding_attributes (&tbattr, true, false);
  if (m == MATCH_ERROR)
    goto error;

  /* Now the colons, those are required.  */
  if (gfc_match (" ::") != MATCH_YES)
    {
      gfc_error ("Expected %<::%> at %C");
      goto error;
    }

  /* Match the binding name; depending on type (operator / generic) format
     it for future error messages into bind_name.  */

  m = gfc_match_generic_spec (&op_type, name, &op);
  if (m == MATCH_ERROR)
    return MATCH_ERROR;
  if (m == MATCH_NO)
    {
      gfc_error ("Expected generic name or operator descriptor at %C");
      goto error;
    }

  switch (op_type)
    {
    case INTERFACE_GENERIC:
    case INTERFACE_DTIO:
      snprintf (bind_name, sizeof (bind_name), "%s", name);
      break;

    case INTERFACE_USER_OP:
      snprintf (bind_name, sizeof (bind_name), "OPERATOR(.%s.)", name);
      break;

    case INTERFACE_INTRINSIC_OP:
      snprintf (bind_name, sizeof (bind_name), "OPERATOR(%s)",
		gfc_op2string (op));
      break;

    case INTERFACE_NAMELESS:
      gfc_error ("Malformed GENERIC statement at %C");
      goto error;
      break;

    default:
      gcc_unreachable ();
    }

  /* Match the required =>.  */
  if (gfc_match (" =>") != MATCH_YES)
    {
      gfc_error ("Expected %<=>%> at %C");
      goto error;
    }

  /* Try to find existing GENERIC binding with this name / for this operator;
     if there is something, check that it is another GENERIC and then extend
     it rather than building a new node.  Otherwise, create it and put it
     at the right position.  */

  switch (op_type)
    {
    case INTERFACE_DTIO:
    case INTERFACE_USER_OP:
    case INTERFACE_GENERIC:
      {
	const bool is_op = (op_type == INTERFACE_USER_OP);
	gfc_symtree* st;

	st = gfc_find_symtree (is_op ? ns->tb_uop_root : ns->tb_sym_root, name);
	tb = st ? st->n.tb : NULL;
	break;
      }

    case INTERFACE_INTRINSIC_OP:
      tb = ns->tb_op[op];
      break;

    default:
      gcc_unreachable ();
    }

  if (tb)
    {
      if (!tb->is_generic)
	{
	  gcc_assert (op_type == INTERFACE_GENERIC);
	  gfc_error ("There's already a non-generic procedure with binding name"
		     " %qs for the derived type %qs at %C",
		     bind_name, block->name);
	  goto error;
	}

      if (tb->access != tbattr.access)
	{
	  gfc_error ("Binding at %C must have the same access as already"
		     " defined binding %qs", bind_name);
	  goto error;
	}
    }
  else
    {
      tb = gfc_get_typebound_proc (NULL);
      tb->where = gfc_current_locus;
      tb->access = tbattr.access;
      tb->is_generic = 1;
      tb->u.generic = NULL;

      switch (op_type)
	{
	case INTERFACE_DTIO:
	case INTERFACE_GENERIC:
	case INTERFACE_USER_OP:
	  {
	    const bool is_op = (op_type == INTERFACE_USER_OP);
	    gfc_symtree* st = gfc_get_tbp_symtree (is_op ? &ns->tb_uop_root :
						   &ns->tb_sym_root, name);
	    gcc_assert (st);
	    st->n.tb = tb;

	    break;
	  }

	case INTERFACE_INTRINSIC_OP:
	  ns->tb_op[op] = tb;
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  /* Now, match all following names as specific targets.  */
  do
    {
      gfc_symtree* target_st;
      gfc_tbp_generic* target;

      m = gfc_match_name (name);
      if (m == MATCH_ERROR)
	goto error;
      if (m == MATCH_NO)
	{
	  gfc_error ("Expected specific binding name at %C");
	  goto error;
	}

      target_st = gfc_get_tbp_symtree (&ns->tb_sym_root, name);

      /* See if this is a duplicate specification.  */
      for (target = tb->u.generic; target; target = target->next)
	if (target_st == target->specific_st)
	  {
	    gfc_error ("%qs already defined as specific binding for the"
		       " generic %qs at %C", name, bind_name);
	    goto error;
	  }

      target = gfc_get_tbp_generic ();
      target->specific_st = target_st;
      target->specific = NULL;
      target->next = tb->u.generic;
      target->is_operator = ((op_type == INTERFACE_USER_OP)
			     || (op_type == INTERFACE_INTRINSIC_OP));
      tb->u.generic = target;
    }
  while (gfc_match (" ,") == MATCH_YES);

  /* Here should be the end.  */
  if (gfc_match_eos () != MATCH_YES)
    {
      gfc_error ("Junk after GENERIC binding at %C");
      goto error;
    }

  return MATCH_YES;

error:
  return MATCH_ERROR;
}


/* Match a FINAL declaration inside a derived type.  */

match
gfc_match_final_decl (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol* sym;
  match m;
  gfc_namespace* module_ns;
  bool first, last;
  gfc_symbol* block;

  if (gfc_current_form == FORM_FREE)
    {
      char c = gfc_peek_ascii_char ();
      if (!gfc_is_whitespace (c) && c != ':')
	return MATCH_NO;
    }

  if (gfc_state_stack->state != COMP_DERIVED_CONTAINS)
    {
      if (gfc_current_form == FORM_FIXED)
	return MATCH_NO;

      gfc_error ("FINAL declaration at %C must be inside a derived type "
		 "CONTAINS section");
      return MATCH_ERROR;
    }

  block = gfc_state_stack->previous->sym;
  gcc_assert (block);

  if (!gfc_state_stack->previous || !gfc_state_stack->previous->previous
      || gfc_state_stack->previous->previous->state != COMP_MODULE)
    {
      gfc_error ("Derived type declaration with FINAL at %C must be in the"
		 " specification part of a MODULE");
      return MATCH_ERROR;
    }

  module_ns = gfc_current_ns;
  gcc_assert (module_ns);
  gcc_assert (module_ns->proc_name->attr.flavor == FL_MODULE);

  /* Match optional ::, don't care about MATCH_YES or MATCH_NO.  */
  if (gfc_match (" ::") == MATCH_ERROR)
    return MATCH_ERROR;

  /* Match the sequence of procedure names.  */
  first = true;
  last = false;
  do
    {
      gfc_finalizer* f;

      if (first && gfc_match_eos () == MATCH_YES)
	{
	  gfc_error ("Empty FINAL at %C");
	  return MATCH_ERROR;
	}

      m = gfc_match_name (name);
      if (m == MATCH_NO)
	{
	  gfc_error ("Expected module procedure name at %C");
	  return MATCH_ERROR;
	}
      else if (m != MATCH_YES)
	return MATCH_ERROR;

      if (gfc_match_eos () == MATCH_YES)
	last = true;
      if (!last && gfc_match_char (',') != MATCH_YES)
	{
	  gfc_error ("Expected %<,%> at %C");
	  return MATCH_ERROR;
	}

      if (gfc_get_symbol (name, module_ns, &sym))
	{
	  gfc_error ("Unknown procedure name %qs at %C", name);
	  return MATCH_ERROR;
	}

      /* Mark the symbol as module procedure.  */
      if (sym->attr.proc != PROC_MODULE
	  && !gfc_add_procedure (&sym->attr, PROC_MODULE, sym->name, NULL))
	return MATCH_ERROR;

      /* Check if we already have this symbol in the list, this is an error.  */
      for (f = block->f2k_derived->finalizers; f; f = f->next)
	if (f->proc_sym == sym)
	  {
	    gfc_error ("%qs at %C is already defined as FINAL procedure",
		       name);
	    return MATCH_ERROR;
	  }

      /* Add this symbol to the list of finalizers.  */
      gcc_assert (block->f2k_derived);
      sym->refs++;
      f = XCNEW (gfc_finalizer);
      f->proc_sym = sym;
      f->proc_tree = NULL;
      f->where = gfc_current_locus;
      f->next = block->f2k_derived->finalizers;
      block->f2k_derived->finalizers = f;

      first = false;
    }
  while (!last);

  return MATCH_YES;
}


const ext_attr_t ext_attr_list[] = {
  { "dllimport",    EXT_ATTR_DLLIMPORT,    "dllimport" },
  { "dllexport",    EXT_ATTR_DLLEXPORT,    "dllexport" },
  { "cdecl",        EXT_ATTR_CDECL,        "cdecl"     },
  { "stdcall",      EXT_ATTR_STDCALL,      "stdcall"   },
  { "fastcall",     EXT_ATTR_FASTCALL,     "fastcall"  },
  { "no_arg_check", EXT_ATTR_NO_ARG_CHECK, NULL        },
  { NULL,           EXT_ATTR_LAST,         NULL        }
};

/* Match a !GCC$ ATTRIBUTES statement of the form:
      !GCC$ ATTRIBUTES attribute-list :: var-name [, var-name] ...
   When we come here, we have already matched the !GCC$ ATTRIBUTES string.

   TODO: We should support all GCC attributes using the same syntax for
   the attribute list, i.e. the list in C
      __attributes(( attribute-list ))
   matches then
      !GCC$ ATTRIBUTES attribute-list ::
   Cf. c-parser.c's c_parser_attributes; the data can then directly be
   saved into a TREE.

   As there is absolutely no risk of confusion, we should never return
   MATCH_NO.  */
match
gfc_match_gcc_attributes (void)
{
  symbol_attribute attr;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  unsigned id;
  gfc_symbol *sym;
  match m;

  gfc_clear_attr (&attr);
  for(;;)
    {
      char ch;

      if (gfc_match_name (name) != MATCH_YES)
	return MATCH_ERROR;

      for (id = 0; id < EXT_ATTR_LAST; id++)
	if (strcmp (name, ext_attr_list[id].name) == 0)
	  break;

      if (id == EXT_ATTR_LAST)
	{
	  gfc_error ("Unknown attribute in !GCC$ ATTRIBUTES statement at %C");
	  return MATCH_ERROR;
	}

      if (!gfc_add_ext_attribute (&attr, (ext_attr_id_t)id, &gfc_current_locus))
	return MATCH_ERROR;

      gfc_gobble_whitespace ();
      ch = gfc_next_ascii_char ();
      if (ch == ':')
        {
          /* This is the successful exit condition for the loop.  */
          if (gfc_next_ascii_char () == ':')
            break;
        }

      if (ch == ',')
	continue;

      goto syntax;
    }

  if (gfc_match_eos () == MATCH_YES)
    goto syntax;

  for(;;)
    {
      m = gfc_match_name (name);
      if (m != MATCH_YES)
	return m;

      if (find_special (name, &sym, true))
	return MATCH_ERROR;

      sym->attr.ext_attr |= attr.ext_attr;

      if (gfc_match_eos () == MATCH_YES)
	break;

      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in !GCC$ ATTRIBUTES statement at %C");
  return MATCH_ERROR;
}
