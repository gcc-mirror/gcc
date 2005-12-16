/* Declaration statement matcher
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */


#include "config.h"
#include "system.h"
#include "gfortran.h"
#include "match.h"
#include "parse.h"


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


/********************* DATA statement subroutines *********************/

/* Free a gfc_data_variable structure and everything beneath it.  */

static void
free_variable (gfc_data_variable * p)
{
  gfc_data_variable *q;

  for (; p; p = q)
    {
      q = p->next;
      gfc_free_expr (p->expr);
      gfc_free_iterator (&p->iter, 0);
      free_variable (p->list);

      gfc_free (p);
    }
}


/* Free a gfc_data_value structure and everything beneath it.  */

static void
free_value (gfc_data_value * p)
{
  gfc_data_value *q;

  for (; p; p = q)
    {
      q = p->next;
      gfc_free_expr (p->expr);
      gfc_free (p);
    }
}


/* Free a list of gfc_data structures.  */

void
gfc_free_data (gfc_data * p)
{
  gfc_data *q;

  for (; p; p = q)
    {
      q = p->next;

      free_variable (p->var);
      free_value (p->value);

      gfc_free (p);
    }
}


static match var_element (gfc_data_variable *);

/* Match a list of variables terminated by an iterator and a right
   parenthesis.  */

static match
var_list (gfc_data_variable * parent)
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
var_element (gfc_data_variable * new)
{
  match m;
  gfc_symbol *sym;

  memset (new, 0, sizeof (gfc_data_variable));

  if (gfc_match_char ('(') == MATCH_YES)
    return var_list (new);

  m = gfc_match_variable (&new->expr, 0);
  if (m != MATCH_YES)
    return m;

  sym = new->expr->symtree->n.sym;

  if (!sym->attr.function && gfc_current_ns->parent && gfc_current_ns->parent == sym->ns)
    {
      gfc_error ("Host associated variable '%s' may not be in the DATA "
		 "statement at %C.", sym->name);
      return MATCH_ERROR;
    }

  if (gfc_current_state () != COMP_BLOCK_DATA
	&& sym->attr.in_common
	&& gfc_notify_std (GFC_STD_GNU, "Extension: initialization of "
			   "common block variable '%s' in DATA statement at %C",
			   sym->name) == FAILURE)
    return MATCH_ERROR;

  if (gfc_add_data (&sym->attr, sym->name, &new->expr->where) == FAILURE)
    return MATCH_ERROR;

  return MATCH_YES;
}


/* Match the top-level list of data variables.  */

static match
top_var_list (gfc_data * d)
{
  gfc_data_variable var, *tail, *new;
  match m;

  tail = NULL;

  for (;;)
    {
      m = var_element (&var);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	return MATCH_ERROR;

      new = gfc_get_data_variable ();
      *new = var;

      if (tail == NULL)
	d->var = new;
      else
	tail->next = new;

      tail = new;

      if (gfc_match_char ('/') == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_DATA);
  return MATCH_ERROR;
}


static match
match_data_constant (gfc_expr ** result)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  gfc_expr *expr;
  match m;

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

  m = gfc_match_name (name);
  if (m != MATCH_YES)
    return m;

  if (gfc_find_symbol (name, NULL, 1, &sym))
    return MATCH_ERROR;

  if (sym == NULL
      || (sym->attr.flavor != FL_PARAMETER && sym->attr.flavor != FL_DERIVED))
    {
      gfc_error ("Symbol '%s' must be a PARAMETER in DATA statement at %C",
		 name);
      return MATCH_ERROR;
    }
  else if (sym->attr.flavor == FL_DERIVED)
    return gfc_match_structure_constructor (sym, result);

  *result = gfc_copy_expr (sym->value);
  return MATCH_YES;
}


/* Match a list of values in a DATA statement.  The leading '/' has
   already been seen at this point.  */

static match
top_val_list (gfc_data * data)
{
  gfc_data_value *new, *tail;
  gfc_expr *expr;
  const char *msg;
  match m;

  tail = NULL;

  for (;;)
    {
      m = match_data_constant (&expr);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	return MATCH_ERROR;

      new = gfc_get_data_value ();

      if (tail == NULL)
	data->value = new;
      else
	tail->next = new;

      tail = new;

      if (expr->ts.type != BT_INTEGER || gfc_match_char ('*') != MATCH_YES)
	{
	  tail->expr = expr;
	  tail->repeat = 1;
	}
      else
	{
	  signed int tmp;
	  msg = gfc_extract_int (expr, &tmp);
	  gfc_free_expr (expr);
	  if (msg != NULL)
	    {
	      gfc_error (msg);
	      return MATCH_ERROR;
	    }
	  tail->repeat = tmp;

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
  return MATCH_ERROR;
}


/* Matches an old style initialization.  */

static match
match_old_style_init (const char *name)
{
  match m;
  gfc_symtree *st;
  gfc_data *newdata;

  /* Set up data structure to hold initializers.  */
  gfc_find_sym_tree (name, NULL, 0, &st);
	  
  newdata = gfc_get_data ();
  newdata->var = gfc_get_data_variable ();
  newdata->var->expr = gfc_get_variable_expr (st);

  /* Match initial value list. This also eats the terminal
     '/'.  */
  m = top_val_list (newdata);
  if (m != MATCH_YES)
    {
      gfc_free (newdata);
      return m;
    }

  if (gfc_pure (NULL))
    {
      gfc_error ("Initialization at %C is not allowed in a PURE procedure");
      gfc_free (newdata);
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
  gfc_data *new;
  match m;

  for (;;)
    {
      new = gfc_get_data ();
      new->where = gfc_current_locus;

      m = top_var_list (new);
      if (m != MATCH_YES)
	goto cleanup;

      m = top_val_list (new);
      if (m != MATCH_YES)
	goto cleanup;

      new->next = gfc_current_ns->data;
      gfc_current_ns->data = new;

      if (gfc_match_eos () == MATCH_YES)
	break;

      gfc_match_char (',');	/* Optional comma */
    }

  if (gfc_pure (NULL))
    {
      gfc_error ("DATA statement at %C is not allowed in a PURE procedure");
      return MATCH_ERROR;
    }

  return MATCH_YES;

cleanup:
  gfc_free_data (new);
  return MATCH_ERROR;
}


/************************ Declaration statements *********************/

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
   specification expression or a '*'.  */

static match
char_len_param_value (gfc_expr ** expr)
{

  if (gfc_match_char ('*') == MATCH_YES)
    {
      *expr = NULL;
      return MATCH_YES;
    }

  return gfc_match_expr (expr);
}


/* A character length is a '*' followed by a literal integer or a
   char_len_param_value in parenthesis.  */

static match
match_char_length (gfc_expr ** expr)
{
  int length, cnt;
  match m;

  m = gfc_match_char ('*');
  if (m != MATCH_YES)
    return m;

  /* cnt is unused, here.  */
  m = gfc_match_small_literal_int (&length, &cnt);
  if (m == MATCH_ERROR)
    return m;

  if (m == MATCH_YES)
    {
      *expr = gfc_int_expr (length);
      return m;
    }

  if (gfc_match_char ('(') == MATCH_NO)
    goto syntax;

  m = char_len_param_value (expr);
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
find_special (const char *name, gfc_symbol ** result)
{
  gfc_state_data *s;
  int i;

  i = gfc_get_symbol (name, NULL, result);
  if (i==0) 
    goto end;
  
  if (gfc_current_state () != COMP_SUBROUTINE
      && gfc_current_state () != COMP_FUNCTION)
    goto end;

  s = gfc_state_stack->previous;
  if (s == NULL)
    goto end;

  if (s->state != COMP_INTERFACE)
    goto end;
  if (s->sym == NULL)
    goto end;                  /* Nameless interface */

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
get_proc_name (const char *name, gfc_symbol ** result)
{
  gfc_symtree *st;
  gfc_symbol *sym;
  int rc;

  if (gfc_current_ns->parent == NULL)
    return gfc_get_symbol (name, NULL, result);

  rc = gfc_get_symbol (name, gfc_current_ns->parent, result);
  if (*result == NULL)
    return rc;

  /* ??? Deal with ENTRY problem */

  st = gfc_new_symtree (&gfc_current_ns->sym_root, name);

  sym = *result;
  st->n.sym = sym;
  sym->refs++;

  /* See if the procedure should be a module procedure */

  if (sym->ns->proc_name != NULL
      && sym->ns->proc_name->attr.flavor == FL_MODULE
      && sym->attr.proc != PROC_MODULE
      && gfc_add_procedure (&sym->attr, PROC_MODULE,
			    sym->name, NULL) == FAILURE)
    rc = 2;

  return rc;
}


/* Function called by variable_decl() that adds a name to the symbol
   table.  */

static try
build_sym (const char *name, gfc_charlen * cl,
	   gfc_array_spec ** as, locus * var_locus)
{
  symbol_attribute attr;
  gfc_symbol *sym;

  /* if (find_special (name, &sym)) */
  if (gfc_get_symbol (name, NULL, &sym))
    return FAILURE;

  /* Start updating the symbol table.  Add basic type attribute
     if present.  */
  if (current_ts.type != BT_UNKNOWN
      &&(sym->attr.implicit_type == 0
	 || !gfc_compare_types (&sym->ts, &current_ts))
      && gfc_add_type (sym, &current_ts, var_locus) == FAILURE)
    return FAILURE;

  if (sym->ts.type == BT_CHARACTER)
    sym->ts.cl = cl;

  /* Add dimension attribute if present.  */
  if (gfc_set_array_spec (sym, *as, var_locus) == FAILURE)
    return FAILURE;
  *as = NULL;

  /* Add attribute to symbol.  The copy is so that we can reset the
     dimension attribute.  */
  attr = current_attr;
  attr.dimension = 0;

  if (gfc_copy_attr (&sym->attr, &attr, var_locus) == FAILURE)
    return FAILURE;

  return SUCCESS;
}

/* Set character constant to the given length. The constant will be padded or
   truncated.  */

void
gfc_set_constant_character_len (int len, gfc_expr * expr)
{
  char * s;
  int slen;

  gcc_assert (expr->expr_type == EXPR_CONSTANT);
  gcc_assert (expr->ts.type == BT_CHARACTER && expr->ts.kind == 1);

  slen = expr->value.character.length;
  if (len != slen)
    {
      s = gfc_getmem (len);
      memcpy (s, expr->value.character.string, MIN (len, slen));
      if (len > slen)
	memset (&s[slen], ' ', len - slen);
      gfc_free (expr->value.character.string);
      expr->value.character.string = s;
      expr->value.character.length = len;
    }
}


/* Function to create and update the enumerator history 
   using the information passed as arguments.
   Pointer "max_enum" is also updated, to point to 
   enum history node containing largest initializer.  

   SYM points to the symbol node of enumerator.
   INIT points to its enumerator value.   */

static void 
create_enum_history(gfc_symbol *sym, gfc_expr *init)
{
  enumerator_history *new_enum_history;
  gcc_assert (sym != NULL && init != NULL);

  new_enum_history = gfc_getmem (sizeof (enumerator_history));

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
gfc_free_enum_history(void)
{
  enumerator_history *current = enum_history;  
  enumerator_history *next;  

  while (current != NULL)
    {
      next = current->next;
      gfc_free (current);
      current = next;
    }
  max_enum = NULL;
  enum_history = NULL;
}


/* Function called by variable_decl() that adds an initialization
   expression to a symbol.  */

static try
add_init_expr_to_sym (const char *name, gfc_expr ** initp,
		      locus * var_locus)
{
  symbol_attribute attr;
  gfc_symbol *sym;
  gfc_expr *init;

  init = *initp;
  if (find_special (name, &sym))
    return FAILURE;

  attr = sym->attr;

  /* If this symbol is confirming an implicit parameter type,
     then an initialization expression is not allowed.  */
  if (attr.flavor == FL_PARAMETER
      && sym->value != NULL
      && *initp != NULL)
    {
      gfc_error ("Initializer not allowed for PARAMETER '%s' at %C",
		 sym->name);
      return FAILURE;
    }

  if (attr.in_common
      && !attr.data
      && *initp != NULL)
    {
      gfc_error ("Initializer not allowed for COMMON variable '%s' at %C",
		 sym->name);
      return FAILURE;
    }

  if (init == NULL)
    {
      /* An initializer is required for PARAMETER declarations.  */
      if (attr.flavor == FL_PARAMETER)
	{
	  gfc_error ("PARAMETER at %L is missing an initializer", var_locus);
	  return FAILURE;
	}
    }
  else
    {
      /* If a variable appears in a DATA block, it cannot have an
	 initializer.  */
      if (sym->attr.data)
	{
	  gfc_error
	    ("Variable '%s' at %C with an initializer already appears "
	     "in a DATA statement", sym->name);
	  return FAILURE;
	}

      /* Check if the assignment can happen. This has to be put off
	 until later for a derived type variable.  */
      if (sym->ts.type != BT_DERIVED && init->ts.type != BT_DERIVED
	  && gfc_check_assign_symbol (sym, init) == FAILURE)
	return FAILURE;

      if (sym->ts.type == BT_CHARACTER && sym->ts.cl)
	{
	  /* Update symbol character length according initializer.  */
	  if (sym->ts.cl->length == NULL)
	    {
	      /* If there are multiple CHARACTER variables declared on
		 the same line, we don't want them to share the same
	        length.  */
	      sym->ts.cl = gfc_get_charlen ();
	      sym->ts.cl->next = gfc_current_ns->cl_list;
	      gfc_current_ns->cl_list = sym->ts.cl;

	      if (init->expr_type == EXPR_CONSTANT)
		sym->ts.cl->length =
			gfc_int_expr (init->value.character.length);
	      else if (init->expr_type == EXPR_ARRAY)
		sym->ts.cl->length = gfc_copy_expr (init->ts.cl->length);
	    }
	  /* Update initializer character length according symbol.  */
	  else if (sym->ts.cl->length->expr_type == EXPR_CONSTANT)
	    {
	      int len = mpz_get_si (sym->ts.cl->length->value.integer);
	      gfc_constructor * p;

	      if (init->expr_type == EXPR_CONSTANT)
		gfc_set_constant_character_len (len, init);
	      else if (init->expr_type == EXPR_ARRAY)
		{
		  gfc_free_expr (init->ts.cl->length);
		  init->ts.cl->length = gfc_copy_expr (sym->ts.cl->length);
		  for (p = init->value.constructor; p; p = p->next)
		    gfc_set_constant_character_len (len, p->expr);
		}
	    }
	}

      /* Add initializer.  Make sure we keep the ranks sane.  */
      if (sym->attr.dimension && init->rank == 0)
	init->rank = sym->as->rank;

      sym->value = init;
      *initp = NULL;
    }

  /* Maintain enumerator history.  */
  if (gfc_current_state () == COMP_ENUM)
    create_enum_history (sym, init);

  return SUCCESS;
}


/* Function called by variable_decl() that adds a name to a structure
   being built.  */

static try
build_struct (const char *name, gfc_charlen * cl, gfc_expr ** init,
	      gfc_array_spec ** as)
{
  gfc_component *c;

  /* If the current symbol is of the same derived type that we're
     constructing, it must have the pointer attribute.  */
  if (current_ts.type == BT_DERIVED
      && current_ts.derived == gfc_current_block ()
      && current_attr.pointer == 0)
    {
      gfc_error ("Component at %C must have the POINTER attribute");
      return FAILURE;
    }

  if (gfc_current_block ()->attr.pointer
      && (*as)->rank != 0)
    {
      if ((*as)->type != AS_DEFERRED && (*as)->type != AS_EXPLICIT)
	{
	  gfc_error ("Array component of structure at %C must have explicit "
		     "or deferred shape");
	  return FAILURE;
	}
    }

  if (gfc_add_component (gfc_current_block (), name, &c) == FAILURE)
    return FAILURE;

  c->ts = current_ts;
  c->ts.cl = cl;
  gfc_set_component_attr (c, &current_attr);

  c->initializer = *init;
  *init = NULL;

  c->as = *as;
  if (c->as != NULL)
    c->dimension = 1;
  *as = NULL;

  /* Check array components.  */
  if (!c->dimension)
    return SUCCESS;

  if (c->pointer)
    {
      if (c->as->type != AS_DEFERRED)
	{
	  gfc_error ("Pointer array component of structure at %C "
		     "must have a deferred shape");
	  return FAILURE;
	}
    }
  else
    {
      if (c->as->type != AS_EXPLICIT)
	{
	  gfc_error
	    ("Array component of structure at %C must have an explicit "
	     "shape");
	  return FAILURE;
	}
    }

  return SUCCESS;
}


/* Match a 'NULL()', and possibly take care of some side effects.  */

match
gfc_match_null (gfc_expr ** result)
{
  gfc_symbol *sym;
  gfc_expr *e;
  match m;

  m = gfc_match (" null ( )");
  if (m != MATCH_YES)
    return m;

  /* The NULL symbol now has to be/become an intrinsic function.  */
  if (gfc_get_symbol ("null", NULL, &sym))
    {
      gfc_error ("NULL() initialization at %C is ambiguous");
      return MATCH_ERROR;
    }

  gfc_intrinsic_symbol (sym);

  if (sym->attr.proc != PROC_INTRINSIC
      && (gfc_add_procedure (&sym->attr, PROC_INTRINSIC,
			     sym->name, NULL) == FAILURE
	  || gfc_add_function (&sym->attr, sym->name, NULL) == FAILURE))
    return MATCH_ERROR;

  e = gfc_get_expr ();
  e->where = gfc_current_locus;
  e->expr_type = EXPR_NULL;
  e->ts.type = BT_UNKNOWN;

  *result = e;

  return MATCH_YES;
}


/* Match a variable name with an optional initializer.  When this
   subroutine is called, a variable is expected to be parsed next.
   Depending on what is happening at the moment, updates either the
   symbol table or the current interface.  */

static match
variable_decl (int elem)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_expr *initializer, *char_len;
  gfc_array_spec *as;
  gfc_array_spec *cp_as; /* Extra copy for Cray Pointees.  */
  gfc_charlen *cl;
  locus var_locus;
  match m;
  try t;
  gfc_symbol *sym;
  locus old_locus;

  initializer = NULL;
  as = NULL;
  cp_as = NULL;
  old_locus = gfc_current_locus;

  /* When we get here, we've just matched a list of attributes and
     maybe a type and a double colon.  The next thing we expect to see
     is the name of the symbol.  */
  m = gfc_match_name (name);
  if (m != MATCH_YES)
    goto cleanup;

  var_locus = gfc_current_locus;

  /* Now we could see the optional array spec. or character length.  */
  m = gfc_match_array_spec (&as);
  if (gfc_option.flag_cray_pointer && m == MATCH_YES)
    cp_as = gfc_copy_array_spec (as);
  else if (m == MATCH_ERROR)
    goto cleanup;

  if (m == MATCH_NO)
    as = gfc_copy_array_spec (current_as);
  else if (gfc_current_state () == COMP_ENUM)
    {
      gfc_error ("Enumerator cannot be array at %C");
      gfc_free_enum_history ();
      m = MATCH_ERROR;
      goto cleanup;
    }


  char_len = NULL;
  cl = NULL;

  if (current_ts.type == BT_CHARACTER)
    {
      switch (match_char_length (&char_len))
	{
	case MATCH_YES:
	  cl = gfc_get_charlen ();
	  cl->next = gfc_current_ns->cl_list;
	  gfc_current_ns->cl_list = cl;

	  cl->length = char_len;
	  break;

	/* Non-constant lengths need to be copied after the first
	   element.  */
	case MATCH_NO:
	  if (elem > 1 && current_ts.cl->length
		&& current_ts.cl->length->expr_type != EXPR_CONSTANT)
	    {
	      cl = gfc_get_charlen ();
	      cl->next = gfc_current_ns->cl_list;
	      gfc_current_ns->cl_list = cl;
	      cl->length = gfc_copy_expr (current_ts.cl->length);
	    }
	  else
	    cl = current_ts.cl;

	  break;

	case MATCH_ERROR:
	  goto cleanup;
	}
    }

  /*  If this symbol has already shown up in a Cray Pointer declaration,
      then we want to set the type & bail out. */
  if (gfc_option.flag_cray_pointer)
    {
      gfc_find_symbol (name, gfc_current_ns, 1, &sym);
      if (sym != NULL && sym->attr.cray_pointee)
	{
	  sym->ts.type = current_ts.type;
	  sym->ts.kind = current_ts.kind;
	  sym->ts.cl = cl;
	  sym->ts.derived = current_ts.derived;
	  m = MATCH_YES;
	
	  /* Check to see if we have an array specification.  */
	  if (cp_as != NULL)
	    {
	      if (sym->as != NULL)
		{
		  gfc_error ("Duplicate array spec for Cray pointee at %C.");
		  gfc_free_array_spec (cp_as);
		  m = MATCH_ERROR;
		  goto cleanup;
		}
	      else
		{
		  if (gfc_set_array_spec (sym, cp_as, &var_locus) == FAILURE)
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
  
    
  /* OK, we've successfully matched the declaration.  Now put the
     symbol in the current namespace, because it might be used in the
     optional initialization expression for this symbol, e.g. this is
     perfectly legal:

     integer, parameter :: i = huge(i)

     This is only true for parameters or variables of a basic type.
     For components of derived types, it is not true, so we don't
     create a symbol for those yet.  If we fail to create the symbol,
     bail out.  */
  if (gfc_current_state () != COMP_DERIVED
      && build_sym (name, cl, &as, &var_locus) == FAILURE)
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  /* In functions that have a RESULT variable defined, the function
     name always refers to function calls.  Therefore, the name is
     not allowed to appear in specification statements.  */
  if (gfc_current_state () == COMP_FUNCTION
      && gfc_current_block () != NULL
      && gfc_current_block ()->result != NULL
      && gfc_current_block ()->result != gfc_current_block ()
      && strcmp (gfc_current_block ()->name, name) == 0)
    {
      gfc_error ("Function name '%s' not allowed at %C", name);
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
      if (gfc_notify_std (GFC_STD_GNU, "Extension: Old-style "
			  "initialization at %C") == FAILURE)
	return MATCH_ERROR;
     
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

	  m = gfc_match_null (&initializer);
	  if (m == MATCH_NO)
	    {
	      gfc_error ("Pointer initialization requires a NULL at %C");
	      m = MATCH_ERROR;
	    }

	  if (gfc_pure (NULL))
	    {
	      gfc_error
		("Initialization of pointer at %C is not allowed in a "
		 "PURE procedure");
	      m = MATCH_ERROR;
	    }

	  if (m != MATCH_YES)
	    goto cleanup;

	  initializer->ts = current_ts;

	}
      else if (gfc_match_char ('=') == MATCH_YES)
	{
	  if (current_attr.pointer)
	    {
	      gfc_error
		("Pointer initialization at %C requires '=>', not '='");
	      m = MATCH_ERROR;
	      goto cleanup;
	    }

	  m = gfc_match_init_expr (&initializer);
	  if (m == MATCH_NO)
	    {
	      gfc_error ("Expected an initialization expression at %C");
	      m = MATCH_ERROR;
	    }

	  if (current_attr.flavor != FL_PARAMETER && gfc_pure (NULL))
	    {
	      gfc_error
		("Initialization of variable at %C is not allowed in a "
		 "PURE procedure");
	      m = MATCH_ERROR;
	    }

	  if (m != MATCH_YES)
	    goto cleanup;
	}
    }

  /* Check if we are parsing an enumeration and if the current enumerator
     variable has an initializer or not. If it does not have an
     initializer, the initialization value of the previous enumerator 
     (stored in last_initializer) is incremented by 1 and is used to
     initialize the current enumerator.  */
  if (gfc_current_state () == COMP_ENUM)
    {
      if (initializer == NULL)
        initializer = gfc_enum_initializer (last_initializer, old_locus);
 
      if (initializer == NULL || initializer->ts.type != BT_INTEGER)
        {
          gfc_error("ENUMERATOR %L not initialized with integer expression",
		    &var_locus);
          m = MATCH_ERROR; 
          gfc_free_enum_history ();
          goto cleanup;
        }

      /* Store this current initializer, for the next enumerator
	 variable to be parsed.  */
      last_initializer = initializer;
    }

  /* Add the initializer.  Note that it is fine if initializer is
     NULL here, because we sometimes also need to check if a
     declaration *must* have an initialization expression.  */
  if (gfc_current_state () != COMP_DERIVED)
    t = add_init_expr_to_sym (name, &initializer, &var_locus);
  else
    {
      if (current_ts.type == BT_DERIVED && !current_attr.pointer && !initializer)
	initializer = gfc_default_initializer (&current_ts);
      t = build_struct (name, cl, &initializer, &as);
    }

  m = (t == SUCCESS) ? MATCH_YES : MATCH_ERROR;

cleanup:
  /* Free stuff up and return.  */
  gfc_free_expr (initializer);
  gfc_free_array_spec (as);

  return m;
}


/* Match an extended-f77 kind specification.  */

match
gfc_match_old_kind_spec (gfc_typespec * ts)
{
  match m;
  int original_kind, cnt;

  if (gfc_match_char ('*') != MATCH_YES)
    return MATCH_NO;

  /* cnt is unsed, here.  */
  m = gfc_match_small_literal_int (&ts->kind, &cnt);
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

  if (gfc_validate_kind (ts->type, ts->kind, true) < 0)
    {
      gfc_error ("Old-style type declaration %s*%d not supported at %C",
                 gfc_basic_typename (ts->type), original_kind);
      return MATCH_ERROR;
    }

  return MATCH_YES;
}


/* Match a kind specification.  Since kinds are generally optional, we
   usually return MATCH_NO if something goes wrong.  If a "kind="
   string is found, then we know we have an error.  */

match
gfc_match_kind_spec (gfc_typespec * ts)
{
  locus where;
  gfc_expr *e;
  match m, n;
  const char *msg;

  m = MATCH_NO;
  e = NULL;

  where = gfc_current_locus;

  if (gfc_match_char ('(') == MATCH_NO)
    return MATCH_NO;

  /* Also gobbles optional text.  */
  if (gfc_match (" kind = ") == MATCH_YES)
    m = MATCH_ERROR;

  n = gfc_match_init_expr (&e);
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

  msg = gfc_extract_int (e, &ts->kind);
  if (msg != NULL)
    {
      gfc_error (msg);
      m = MATCH_ERROR;
      goto no_match;
    }

  gfc_free_expr (e);
  e = NULL;

  if (gfc_validate_kind (ts->type, ts->kind, true) < 0)
    {
      gfc_error ("Kind %d not supported for type %s at %C", ts->kind,
		 gfc_basic_typename (ts->type));

      m = MATCH_ERROR;
      goto no_match;
    }

  if (gfc_match_char (')') != MATCH_YES)
    {
      gfc_error ("Missing right paren at %C");
      goto no_match;
    }

  return MATCH_YES;

no_match:
  gfc_free_expr (e);
  gfc_current_locus = where;
  return m;
}


/* Match the various kind/length specifications in a CHARACTER
   declaration.  We don't return MATCH_NO.  */

static match
match_char_spec (gfc_typespec * ts)
{
  int i, kind, seen_length;
  gfc_charlen *cl;
  gfc_expr *len;
  match m;

  kind = gfc_default_character_kind;
  len = NULL;
  seen_length = 0;

  /* Try the old-style specification first.  */
  old_char_selector = 0;

  m = match_char_length (&len);
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
      m = MATCH_YES;	/* character without length is a single char */
      goto done;
    }

  /* Try the weird case:  ( KIND = <int> [ , LEN = <len-param> ] )   */
  if (gfc_match (" kind =") == MATCH_YES)
    {
      m = gfc_match_small_int (&kind);
      if (m == MATCH_ERROR)
	goto done;
      if (m == MATCH_NO)
	goto syntax;

      if (gfc_match (" , len =") == MATCH_NO)
	goto rparen;

      m = char_len_param_value (&len);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto done;
      seen_length = 1;

      goto rparen;
    }

  /* Try to match ( LEN = <len-param> ) or ( LEN = <len-param>, KIND = <int> )  */
  if (gfc_match (" len =") == MATCH_YES)
    {
      m = char_len_param_value (&len);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	goto done;
      seen_length = 1;

      if (gfc_match_char (')') == MATCH_YES)
	goto done;

      if (gfc_match (" , kind =") != MATCH_YES)
	goto syntax;

      gfc_match_small_int (&kind);

      if (gfc_validate_kind (BT_CHARACTER, kind, true) < 0)
	{
	  gfc_error ("Kind %d is not a CHARACTER kind at %C", kind);
	  return MATCH_YES;
	}

      goto rparen;
    }

  /* Try to match   ( <len-param> ) or ( <len-param> , [ KIND = ] <int> )  */
  m = char_len_param_value (&len);
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

  gfc_match (" kind =");	/* Gobble optional text */

  m = gfc_match_small_int (&kind);
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

done:
  if (m == MATCH_YES && gfc_validate_kind (BT_CHARACTER, kind, true) < 0)
    {
      gfc_error ("Kind %d is not a CHARACTER kind at %C", kind);
      m = MATCH_ERROR;
    }

  if (m != MATCH_YES)
    {
      gfc_free_expr (len);
      return m;
    }

  /* Do some final massaging of the length values.  */
  cl = gfc_get_charlen ();
  cl->next = gfc_current_ns->cl_list;
  gfc_current_ns->cl_list = cl;

  if (seen_length == 0)
    cl->length = gfc_int_expr (1);
  else
    {
      if (len == NULL || gfc_extract_int (len, &i) != NULL || i >= 0)
	cl->length = len;
      else
	{
	  gfc_free_expr (len);
	  cl->length = gfc_int_expr (0);
	}
    }

  ts->cl = cl;
  ts->kind = kind;

  return MATCH_YES;
}


/* Matches a type specification.  If successful, sets the ts structure
   to the matched specification.  This is necessary for FUNCTION and
   IMPLICIT statements.

   If implicit_flag is nonzero, then we don't check for the optional 
   kind specification.  Not doing so is needed for matching an IMPLICIT
   statement correctly.  */

static match
match_type_spec (gfc_typespec * ts, int implicit_flag)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  match m;
  int c;

  gfc_clear_ts (ts);

  if (gfc_match (" byte") == MATCH_YES)
    {
      if (gfc_notify_std(GFC_STD_GNU, "Extension: BYTE type at %C") 
	  == FAILURE)
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

  if (gfc_match (" integer") == MATCH_YES)
    {
      ts->type = BT_INTEGER;
      ts->kind = gfc_default_integer_kind;
      goto get_kind;
    }

  if (gfc_match (" character") == MATCH_YES)
    {
      ts->type = BT_CHARACTER;
      if (implicit_flag == 0)
	return match_char_spec (ts);
      else
	return MATCH_YES;
    }

  if (gfc_match (" real") == MATCH_YES)
    {
      ts->type = BT_REAL;
      ts->kind = gfc_default_real_kind;
      goto get_kind;
    }

  if (gfc_match (" double precision") == MATCH_YES)
    {
      ts->type = BT_REAL;
      ts->kind = gfc_default_double_kind;
      return MATCH_YES;
    }

  if (gfc_match (" complex") == MATCH_YES)
    {
      ts->type = BT_COMPLEX;
      ts->kind = gfc_default_complex_kind;
      goto get_kind;
    }

  if (gfc_match (" double complex") == MATCH_YES)
    {
      ts->type = BT_COMPLEX;
      ts->kind = gfc_default_double_kind;
      return MATCH_YES;
    }

  if (gfc_match (" logical") == MATCH_YES)
    {
      ts->type = BT_LOGICAL;
      ts->kind = gfc_default_logical_kind;
      goto get_kind;
    }

  m = gfc_match (" type ( %n )", name);
  if (m != MATCH_YES)
    return m;

  /* Search for the name but allow the components to be defined later.  */
  if (gfc_get_ha_symbol (name, &sym))
    {
      gfc_error ("Type name '%s' at %C is ambiguous", name);
      return MATCH_ERROR;
    }

  if (sym->attr.flavor != FL_DERIVED
      && gfc_add_flavor (&sym->attr, FL_DERIVED, sym->name, NULL) == FAILURE)
    return MATCH_ERROR;

  ts->type = BT_DERIVED;
  ts->kind = 0;
  ts->derived = sym;

  return MATCH_YES;

get_kind:
  /* For all types except double, derived and character, look for an
     optional kind specifier.  MATCH_NO is actually OK at this point.  */
  if (implicit_flag == 1)
    return MATCH_YES;

  if (gfc_current_form == FORM_FREE)
    {
      c = gfc_peek_char();
      if (!gfc_is_whitespace(c) && c != '*' && c != '('
         && c != ':' && c != ',')
       return MATCH_NO;
    }

  m = gfc_match_kind_spec (ts);
  if (m == MATCH_NO && ts->type != BT_CHARACTER)
    m = gfc_match_old_kind_spec (ts);

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

  return (gfc_match_eos () == MATCH_YES) ? MATCH_YES : MATCH_NO;
}


/* Match the letter range(s) of an IMPLICIT statement.  */

static match
match_implicit_range (void)
{
  int c, c1, c2, inner;
  locus cur_loc;

  cur_loc = gfc_current_locus;

  gfc_gobble_whitespace ();
  c = gfc_next_char ();
  if (c != '(')
    {
      gfc_error ("Missing character range in IMPLICIT at %C");
      goto bad;
    }

  inner = 1;
  while (inner)
    {
      gfc_gobble_whitespace ();
      c1 = gfc_next_char ();
      if (!ISALPHA (c1))
	goto bad;

      gfc_gobble_whitespace ();
      c = gfc_next_char ();

      switch (c)
	{
	case ')':
	  inner = 0;		/* Fall through */

	case ',':
	  c2 = c1;
	  break;

	case '-':
	  gfc_gobble_whitespace ();
	  c2 = gfc_next_char ();
	  if (!ISALPHA (c2))
	    goto bad;

	  gfc_gobble_whitespace ();
	  c = gfc_next_char ();

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
      if (gfc_add_new_implicit_range (c1, c2) != SUCCESS)
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
  int c;
  match m;

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
      m = match_type_spec (&ts, 1);
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
	  c = gfc_next_char ();
	  if ((c == '\n') || (c == ','))
	    {
	      /* Check for CHARACTER with no length parameter.  */
	      if (ts.type == BT_CHARACTER && !ts.cl)
		{
		  ts.kind = gfc_default_character_kind;
		  ts.cl = gfc_get_charlen ();
		  ts.cl->next = gfc_current_ns->cl_list;
		  gfc_current_ns->cl_list = ts.cl;
		  ts.cl->length = gfc_int_expr (1);
		}

	      /* Record the Successful match.  */
	      if (gfc_merge_new_implicit (&ts) != SUCCESS)
		return MATCH_ERROR;
	      continue;
	    }

	  gfc_current_locus = cur_loc;
	}

      /* Discard the (incorrectly) matched range.  */
      gfc_clear_new_implicit ();

      /* Last chance -- check <TYPE> <SELECTOR> (<RANGE>).  */
      if (ts.type == BT_CHARACTER)
	m = match_char_spec (&ts);
      else
	{
	  m = gfc_match_kind_spec (&ts);
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
      c = gfc_next_char ();
      if ((c != '\n') && (c != ','))
	goto syntax;

      if (gfc_merge_new_implicit (&ts) != SUCCESS)
	return MATCH_ERROR;
    }
  while (c == ',');

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_IMPLICIT);

error:
  return MATCH_ERROR;
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
  typedef enum
  { GFC_DECL_BEGIN = 0,
    DECL_ALLOCATABLE = GFC_DECL_BEGIN, DECL_DIMENSION, DECL_EXTERNAL,
    DECL_IN, DECL_OUT, DECL_INOUT, DECL_INTRINSIC, DECL_OPTIONAL,
    DECL_PARAMETER, DECL_POINTER, DECL_PRIVATE, DECL_PUBLIC, DECL_SAVE,
    DECL_TARGET, DECL_COLON, DECL_NONE,
    GFC_DECL_END /* Sentinel */
  }
  decl_types;

/* GFC_DECL_END is the sentinel, index starts at 0.  */
#define NUM_DECL GFC_DECL_END

  static mstring decls[] = {
    minit (", allocatable", DECL_ALLOCATABLE),
    minit (", dimension", DECL_DIMENSION),
    minit (", external", DECL_EXTERNAL),
    minit (", intent ( in )", DECL_IN),
    minit (", intent ( out )", DECL_OUT),
    minit (", intent ( in out )", DECL_INOUT),
    minit (", intrinsic", DECL_INTRINSIC),
    minit (", optional", DECL_OPTIONAL),
    minit (", parameter", DECL_PARAMETER),
    minit (", pointer", DECL_POINTER),
    minit (", private", DECL_PRIVATE),
    minit (", public", DECL_PUBLIC),
    minit (", save", DECL_SAVE),
    minit (", target", DECL_TARGET),
    minit ("::", DECL_COLON),
    minit (NULL, DECL_NONE)
  };

  locus start, seen_at[NUM_DECL];
  int seen[NUM_DECL];
  decl_types d;
  const char *attr;
  match m;
  try t;

  gfc_clear_attr (&current_attr);
  start = gfc_current_locus;

  current_as = NULL;
  colon_seen = 0;

  /* See if we get all of the keywords up to the final double colon.  */
  for (d = GFC_DECL_BEGIN; d != GFC_DECL_END; d++)
    seen[d] = 0;

  for (;;)
    {
      d = (decl_types) gfc_match_strings (decls);
      if (d == DECL_NONE || d == DECL_COLON)
	break;
       
      if (gfc_current_state () == COMP_ENUM)
        {
          gfc_error ("Enumerator cannot have attributes %C");
          return MATCH_ERROR;
        }

      seen[d]++;
      seen_at[d] = gfc_current_locus;

      if (d == DECL_DIMENSION)
	{
	  m = gfc_match_array_spec (&current_as);

	  if (m == MATCH_NO)
	    {
	      gfc_error ("Missing dimension specification at %C");
	      m = MATCH_ERROR;
	    }

	  if (m == MATCH_ERROR)
	    goto cleanup;
	}
    }

  /* If we are parsing an enumeration and have ensured that no other
     attributes are present we can now set the parameter attribute.  */
  if (gfc_current_state () == COMP_ENUM)
    {
      t = gfc_add_flavor (&current_attr, FL_PARAMETER, NULL, NULL);
      if (t == FAILURE)
        {
          m = MATCH_ERROR;
          goto cleanup;
        }
    }

  /* No double colon, so assume that we've been looking at something
     else the whole time.  */
  if (d == DECL_NONE)
    {
      m = MATCH_NO;
      goto cleanup;
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
	  case DECL_PARAMETER:
	    attr = "PARAMETER";
	    break;
	  case DECL_POINTER:
	    attr = "POINTER";
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
	  case DECL_TARGET:
	    attr = "TARGET";
	    break;
	  default:
	    attr = NULL;	/* This shouldn't happen */
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

      if (gfc_current_state () == COMP_DERIVED
	  && d != DECL_DIMENSION && d != DECL_POINTER
	  && d != DECL_COLON && d != DECL_NONE)
	{

	  gfc_error ("Attribute at %L is not allowed in a TYPE definition",
		     &seen_at[d]);
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      if ((d == DECL_PRIVATE || d == DECL_PUBLIC)
	     && gfc_current_state () != COMP_MODULE)
	{
	  if (d == DECL_PRIVATE)
	    attr = "PRIVATE";
	  else
	    attr = "PUBLIC";

	  gfc_error ("%s attribute at %L is not allowed outside of a MODULE",
		     attr, &seen_at[d]);
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      switch (d)
	{
	case DECL_ALLOCATABLE:
	  t = gfc_add_allocatable (&current_attr, &seen_at[d]);
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

	case DECL_PARAMETER:
	  t = gfc_add_flavor (&current_attr, FL_PARAMETER, NULL, &seen_at[d]);
	  break;

	case DECL_POINTER:
	  t = gfc_add_pointer (&current_attr, &seen_at[d]);
	  break;

	case DECL_PRIVATE:
	  t = gfc_add_access (&current_attr, ACCESS_PRIVATE, NULL,
			      &seen_at[d]);
	  break;

	case DECL_PUBLIC:
	  t = gfc_add_access (&current_attr, ACCESS_PUBLIC, NULL,
			      &seen_at[d]);
	  break;

	case DECL_SAVE:
	  t = gfc_add_save (&current_attr, NULL, &seen_at[d]);
	  break;

	case DECL_TARGET:
	  t = gfc_add_target (&current_attr, &seen_at[d]);
	  break;

	default:
	  gfc_internal_error ("match_attr_spec(): Bad attribute");
	}

      if (t == FAILURE)
	{
	  m = MATCH_ERROR;
	  goto cleanup;
	}
    }

  colon_seen = 1;
  return MATCH_YES;

cleanup:
  gfc_current_locus = start;
  gfc_free_array_spec (current_as);
  current_as = NULL;
  return m;
}


/* Match a data declaration statement.  */

match
gfc_match_data_decl (void)
{
  gfc_symbol *sym;
  match m;
  int elem;

  m = match_type_spec (&current_ts, 0);
  if (m != MATCH_YES)
    return m;

  if (current_ts.type == BT_DERIVED && gfc_current_state () != COMP_DERIVED)
    {
      sym = gfc_use_derived (current_ts.derived);

      if (sym == NULL)
	{
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      current_ts.derived = sym;
    }

  m = match_attr_spec ();
  if (m == MATCH_ERROR)
    {
      m = MATCH_NO;
      goto cleanup;
    }

  if (current_ts.type == BT_DERIVED && current_ts.derived->components == NULL)
    {

      if (current_attr.pointer && gfc_current_state () == COMP_DERIVED)
	goto ok;

      gfc_find_symbol (current_ts.derived->name,
			 current_ts.derived->ns->parent, 1, &sym);

      /* Any symbol that we find had better be a type definition
         which has its components defined.  */
      if (sym != NULL && sym->attr.flavor == FL_DERIVED
	    && current_ts.derived->components != NULL)
	goto ok;

      /* Now we have an error, which we signal, and then fix up
	 because the knock-on is plain and simple confusing.  */
      gfc_error_now ("Derived type at %C has not been previously defined "
		 "and so cannot appear in a derived type definition.");
      current_attr.pointer = 1;
      goto ok;
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

  gfc_error ("Syntax error in data declaration at %C");
  m = MATCH_ERROR;

cleanup:
  gfc_free_array_spec (current_as);
  current_as = NULL;
  return m;
}


/* Match a prefix associated with a function or subroutine
   declaration.  If the typespec pointer is nonnull, then a typespec
   can be matched.  Note that if nothing matches, MATCH_YES is
   returned (the null string was matched).  */

static match
match_prefix (gfc_typespec * ts)
{
  int seen_type;

  gfc_clear_attr (&current_attr);
  seen_type = 0;

loop:
  if (!seen_type && ts != NULL
      && match_type_spec (ts, 0) == MATCH_YES
      && gfc_match_space () == MATCH_YES)
    {

      seen_type = 1;
      goto loop;
    }

  if (gfc_match ("elemental% ") == MATCH_YES)
    {
      if (gfc_add_elemental (&current_attr, NULL) == FAILURE)
	return MATCH_ERROR;

      goto loop;
    }

  if (gfc_match ("pure% ") == MATCH_YES)
    {
      if (gfc_add_pure (&current_attr, NULL) == FAILURE)
	return MATCH_ERROR;

      goto loop;
    }

  if (gfc_match ("recursive% ") == MATCH_YES)
    {
      if (gfc_add_recursive (&current_attr, NULL) == FAILURE)
	return MATCH_ERROR;

      goto loop;
    }

  /* At this point, the next item is not a prefix.  */
  return MATCH_YES;
}


/* Copy attributes matched by match_prefix() to attributes on a symbol.  */

static try
copy_prefix (symbol_attribute * dest, locus * where)
{

  if (current_attr.pure && gfc_add_pure (dest, where) == FAILURE)
    return FAILURE;

  if (current_attr.elemental && gfc_add_elemental (dest, where) == FAILURE)
    return FAILURE;

  if (current_attr.recursive && gfc_add_recursive (dest, where) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Match a formal argument list.  */

match
gfc_match_formal_arglist (gfc_symbol * progname, int st_flag, int null_flag)
{
  gfc_formal_arglist *head, *tail, *p, *q;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  match m;

  head = tail = NULL;

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
	sym = NULL;
      else
	{
	  m = gfc_match_name (name);
	  if (m != MATCH_YES)
	    goto cleanup;

	  if (gfc_get_symbol (name, NULL, &sym))
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
	  && (gfc_add_dummy (&sym->attr, sym->name, NULL) == FAILURE
	      || gfc_missing_attr (&sym->attr, NULL) == FAILURE))
	{
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      /* The name of a program unit can be in a different namespace,
         so check for it explicitly.  After the statement is accepted,
         the name is checked for especially in gfc_get_symbol().  */
      if (gfc_new_block != NULL && sym != NULL
	  && strcmp (sym->name, gfc_new_block->name) == 0)
	{
	  gfc_error ("Name '%s' at %C is the name of the procedure",
		     sym->name);
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      if (gfc_match_char (')') == MATCH_YES)
	goto ok;

      m = gfc_match_char (',');
      if (m != MATCH_YES)
	{
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
		gfc_error
		  ("Duplicate symbol '%s' in formal argument list at %C",
		   p->sym->name);

		m = MATCH_ERROR;
		goto cleanup;
	      }
	}
    }

  if (gfc_add_explicit_interface (progname, IFSRC_DECL, head, NULL) ==
      FAILURE)
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  return MATCH_YES;

cleanup:
  gfc_free_formal_arglist (head);
  return m;
}


/* Match a RESULT specification following a function declaration or
   ENTRY statement.  Also matches the end-of-statement.  */

static match
match_result (gfc_symbol * function, gfc_symbol ** result)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *r;
  match m;

  if (gfc_match (" result (") != MATCH_YES)
    return MATCH_NO;

  m = gfc_match_name (name);
  if (m != MATCH_YES)
    return m;

  if (gfc_match (" )%t") != MATCH_YES)
    {
      gfc_error ("Unexpected junk following RESULT variable at %C");
      return MATCH_ERROR;
    }

  if (strcmp (function->name, name) == 0)
    {
      gfc_error
	("RESULT variable at %C must be different than function name");
      return MATCH_ERROR;
    }

  if (gfc_get_symbol (name, NULL, &r))
    return MATCH_ERROR;

  if (gfc_add_flavor (&r->attr, FL_VARIABLE, r->name, NULL) == FAILURE
      || gfc_add_result (&r->attr, r->name, NULL) == FAILURE)
    return MATCH_ERROR;

  *result = r;

  return MATCH_YES;
}


/* Match a function declaration.  */

match
gfc_match_function_decl (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym, *result;
  locus old_loc;
  match m;

  if (gfc_current_state () != COMP_NONE
      && gfc_current_state () != COMP_INTERFACE
      && gfc_current_state () != COMP_CONTAINS)
    return MATCH_NO;

  gfc_clear_ts (&current_ts);

  old_loc = gfc_current_locus;

  m = match_prefix (&current_ts);
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

  if (get_proc_name (name, &sym))
    return MATCH_ERROR;
  gfc_new_block = sym;

  m = gfc_match_formal_arglist (sym, 0, 0);
  if (m == MATCH_NO)
    gfc_error ("Expected formal argument list in function definition at %C");
  else if (m == MATCH_ERROR)
    goto cleanup;

  result = NULL;

  if (gfc_match_eos () != MATCH_YES)
    {
      /* See if a result variable is present.  */
      m = match_result (sym, &result);
      if (m == MATCH_NO)
	gfc_error ("Unexpected junk after function declaration at %C");

      if (m != MATCH_YES)
	{
	  m = MATCH_ERROR;
	  goto cleanup;
	}
    }

  /* Make changes to the symbol.  */
  m = MATCH_ERROR;

  if (gfc_add_function (&sym->attr, sym->name, NULL) == FAILURE)
    goto cleanup;

  if (gfc_missing_attr (&sym->attr, NULL) == FAILURE
      || copy_prefix (&sym->attr, &sym->declared_at) == FAILURE)
    goto cleanup;

  if (current_ts.type != BT_UNKNOWN && sym->ts.type != BT_UNKNOWN)
    {
      gfc_error ("Function '%s' at %C already has a type of %s", name,
		 gfc_basic_typename (sym->ts.type));
      goto cleanup;
    }

  if (result == NULL)
    {
      sym->ts = current_ts;
      sym->result = sym;
    }
  else
    {
      result->ts = current_ts;
      sym->result = result;
    }

  return MATCH_YES;

cleanup:
  gfc_current_locus = old_loc;
  return m;
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

  m = gfc_match_name (name);
  if (m != MATCH_YES)
    return m;

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
	  case COMP_BLOCK_DATA:
	    gfc_error
	      ("ENTRY statement at %C cannot appear within a BLOCK DATA");
	    break;
	  case COMP_INTERFACE:
	    gfc_error
	      ("ENTRY statement at %C cannot appear within an INTERFACE");
	    break;
	  case COMP_DERIVED:
	    gfc_error
	      ("ENTRY statement at %C cannot appear "
	       "within a DERIVED TYPE block");
	    break;
	  case COMP_IF:
	    gfc_error
	      ("ENTRY statement at %C cannot appear within an IF-THEN block");
	    break;
	  case COMP_DO:
	    gfc_error
	      ("ENTRY statement at %C cannot appear within a DO block");
	    break;
	  case COMP_SELECT:
	    gfc_error
	      ("ENTRY statement at %C cannot appear within a SELECT block");
	    break;
	  case COMP_FORALL:
	    gfc_error
	      ("ENTRY statement at %C cannot appear within a FORALL block");
	    break;
	  case COMP_WHERE:
	    gfc_error
	      ("ENTRY statement at %C cannot appear within a WHERE block");
	    break;
	  case COMP_CONTAINS:
	    gfc_error
	      ("ENTRY statement at %C cannot appear "
	       "within a contained subprogram");
	    break;
	  default:
	    gfc_internal_error ("gfc_match_entry(): Bad state");
	}
      return MATCH_ERROR;
    }

  if (gfc_current_ns->parent != NULL
      && gfc_current_ns->parent->proc_name
      && gfc_current_ns->parent->proc_name->attr.flavor != FL_MODULE)
    {
      gfc_error("ENTRY statement at %C cannot appear in a "
		"contained procedure");
      return MATCH_ERROR;
    }

  if (get_proc_name (name, &entry))
    return MATCH_ERROR;

  proc = gfc_current_block ();

  if (state == COMP_SUBROUTINE)
    {
      /* An entry in a subroutine.  */
      m = gfc_match_formal_arglist (entry, 0, 1);
      if (m != MATCH_YES)
	return MATCH_ERROR;

      if (gfc_add_entry (&entry->attr, entry->name, NULL) == FAILURE
	  || gfc_add_subroutine (&entry->attr, entry->name, NULL) == FAILURE)
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
      old_loc = gfc_current_locus;
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
	  if (gfc_add_entry (&entry->attr, entry->name, NULL) == FAILURE
	      || gfc_add_function (&entry->attr, entry->name, NULL) == FAILURE)
	    return MATCH_ERROR;

	  entry->result = entry;
	}
      else
	{
	  m = match_result (proc, &result);
	  if (m == MATCH_NO)
	    gfc_syntax_error (ST_ENTRY);
	  if (m != MATCH_YES)
	    return MATCH_ERROR;

	  if (gfc_add_result (&result->attr, result->name, NULL) == FAILURE
	      || gfc_add_entry (&entry->attr, result->name, NULL) == FAILURE
	      || gfc_add_function (&entry->attr, result->name,
				   NULL) == FAILURE)
	    return MATCH_ERROR;

	  entry->result = result;
	}

      if (proc->attr.recursive && result == NULL)
	{
	  gfc_error ("RESULT attribute required in ENTRY statement at %C");
	  return MATCH_ERROR;
	}
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

  if (gfc_current_state () != COMP_NONE
      && gfc_current_state () != COMP_INTERFACE
      && gfc_current_state () != COMP_CONTAINS)
    return MATCH_NO;

  m = match_prefix (NULL);
  if (m != MATCH_YES)
    return m;

  m = gfc_match ("subroutine% %n", name);
  if (m != MATCH_YES)
    return m;

  if (get_proc_name (name, &sym))
    return MATCH_ERROR;
  gfc_new_block = sym;

  if (gfc_add_subroutine (&sym->attr, sym->name, NULL) == FAILURE)
    return MATCH_ERROR;

  if (gfc_match_formal_arglist (sym, 0, 1) != MATCH_YES)
    return MATCH_ERROR;

  if (gfc_match_eos () != MATCH_YES)
    {
      gfc_syntax_error (ST_SUBROUTINE);
      return MATCH_ERROR;
    }

  if (copy_prefix (&sym->attr, &sym->declared_at) == FAILURE)
    return MATCH_ERROR;

  return MATCH_YES;
}


/* Return nonzero if we're currently compiling a contained procedure.  */

static int
contained_procedure (void)
{
  gfc_state_data *s;

  for (s=gfc_state_stack; s; s=s->previous)
    if ((s->state == COMP_SUBROUTINE || s->state == COMP_FUNCTION)
       && s->previous != NULL
       && s->previous->state == COMP_CONTAINS)
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

  if (!gfc_option.fshort_enums)
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
   END to the caller.  The END INTERFACE, END IF, END DO and END
   SELECT statements cannot be replaced by a single END statement.  */

match
gfc_match_end (gfc_statement * st)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_compile_state state;
  locus old_loc;
  const char *block_name;
  const char *target;
  int eos_ok;
  match m;

  old_loc = gfc_current_locus;
  if (gfc_match ("end") != MATCH_YES)
    return MATCH_NO;

  state = gfc_current_state ();
  block_name =
    gfc_current_block () == NULL ? NULL : gfc_current_block ()->name;

  if (state == COMP_CONTAINS)
    {
      state = gfc_state_stack->previous->state;
      block_name = gfc_state_stack->previous->sym == NULL ? NULL
	: gfc_state_stack->previous->sym->name;
    }

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
      target = " subroutine";
      eos_ok = !contained_procedure ();
      break;

    case COMP_FUNCTION:
      *st = ST_END_FUNCTION;
      target = " function";
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

    case COMP_INTERFACE:
      *st = ST_END_INTERFACE;
      target = " interface";
      eos_ok = 0;
      break;

    case COMP_DERIVED:
      *st = ST_END_TYPE;
      target = " type";
      eos_ok = 0;
      break;

    case COMP_IF:
      *st = ST_ENDIF;
      target = " if";
      eos_ok = 0;
      break;

    case COMP_DO:
      *st = ST_ENDDO;
      target = " do";
      eos_ok = 0;
      break;

    case COMP_SELECT:
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

  if (gfc_match_eos () == MATCH_YES)
    {
      if (!eos_ok)
	{
	  /* We would have required END [something]  */
	  gfc_error ("%s statement expected at %L",
		     gfc_ascii_statement (*st), &old_loc);
	  goto cleanup;
	}

      return MATCH_YES;
    }

  /* Verify that we've got the sort of end-block that we're expecting.  */
  if (gfc_match (target) != MATCH_YES)
    {
      gfc_error ("Expecting %s statement at %C", gfc_ascii_statement (*st));
      goto cleanup;
    }

  /* If we're at the end, make sure a block name wasn't required.  */
  if (gfc_match_eos () == MATCH_YES)
    {

      if (*st != ST_ENDDO && *st != ST_ENDIF && *st != ST_END_SELECT)
	return MATCH_YES;

      if (gfc_current_block () == NULL)
	return MATCH_YES;

      gfc_error ("Expected block name of '%s' in %s statement at %C",
		 block_name, gfc_ascii_statement (*st));

      return MATCH_ERROR;
    }

  /* END INTERFACE has a special handler for its several possible endings.  */
  if (*st == ST_END_INTERFACE)
    return gfc_match_end_interface ();

  /* We haven't hit the end of statement, so what is left must be an end-name.  */
  m = gfc_match_space ();
  if (m == MATCH_YES)
    m = gfc_match_name (name);

  if (m == MATCH_NO)
    gfc_error ("Expected terminating name at %C");
  if (m != MATCH_YES)
    goto cleanup;

  if (block_name == NULL)
    goto syntax;

  if (strcmp (name, block_name) != 0)
    {
      gfc_error ("Expected label '%s' for %s statement at %C", block_name,
		 gfc_ascii_statement (*st));
      goto cleanup;
    }

  if (gfc_match_eos () == MATCH_YES)
    return MATCH_YES;

syntax:
  gfc_syntax_error (*st);

cleanup:
  gfc_current_locus = old_loc;
  return MATCH_ERROR;
}



/***************** Attribute declaration statements ****************/

/* Set the attribute of a single variable.  */

static match
attr_decl1 (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_array_spec *as;
  gfc_symbol *sym;
  locus var_locus;
  match m;

  as = NULL;

  m = gfc_match_name (name);
  if (m != MATCH_YES)
    goto cleanup;

  if (find_special (name, &sym))
    return MATCH_ERROR;

  var_locus = gfc_current_locus;

  /* Deal with possible array specification for certain attributes.  */
  if (current_attr.dimension
      || current_attr.allocatable
      || current_attr.pointer
      || current_attr.target)
    {
      m = gfc_match_array_spec (&as);
      if (m == MATCH_ERROR)
	goto cleanup;

      if (current_attr.dimension && m == MATCH_NO)
	{
	  gfc_error
	    ("Missing array specification at %L in DIMENSION statement",
	     &var_locus);
	  m = MATCH_ERROR;
	  goto cleanup;
	}

      if ((current_attr.allocatable || current_attr.pointer)
	  && (m == MATCH_YES) && (as->type != AS_DEFERRED))
	{
	  gfc_error ("Array specification must be deferred at %L",
		     &var_locus);
	  m = MATCH_ERROR;
	  goto cleanup;
	}
    }

  /* Update symbol table.  DIMENSION attribute is set in gfc_set_array_spec().  */
  if (current_attr.dimension == 0
      && gfc_copy_attr (&sym->attr, &current_attr, NULL) == FAILURE)
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  if (gfc_set_array_spec (sym, as, &var_locus) == FAILURE)
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

  if ((current_attr.external || current_attr.intrinsic)
      && sym->attr.flavor != FL_PROCEDURE
      && gfc_add_flavor (&sym->attr, FL_PROCEDURE, sym->name, NULL) == FAILURE)
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

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
  gfc_array_spec *as;
  gfc_symbol *cptr; /* Pointer symbol.  */
  gfc_symbol *cpte; /* Pointee symbol.  */
  locus var_locus;
  bool done = false;

  while (!done)
    {
      if (gfc_match_char ('(') != MATCH_YES)
	{
	  gfc_error ("Expected '(' at %C");
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
  
      if (gfc_add_cray_pointer (&cptr->attr, &var_locus) == FAILURE)
	return MATCH_ERROR;

      gfc_set_sym_referenced (cptr);      

      if (cptr->ts.type == BT_UNKNOWN) /* Override the type, if necessary.  */
	{
	  cptr->ts.type = BT_INTEGER;
	  cptr->ts.kind = gfc_index_integer_kind; 
	}
      else if (cptr->ts.type != BT_INTEGER)
	{
	  gfc_error ("Cray pointer at %C must be an integer.");
	  return MATCH_ERROR;
	}
      else if (cptr->ts.kind < gfc_index_integer_kind)
	gfc_warning ("Cray pointer at %C has %d bytes of precision;"
		     " memory addresses require %d bytes.",
		     cptr->ts.kind,
		     gfc_index_integer_kind);

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
      m = gfc_match_array_spec (&as);
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

      if (gfc_add_cray_pointee (&cpte->attr, &var_locus) == FAILURE)
	return MATCH_ERROR;

      gfc_set_sym_referenced (cpte);

      if (cpte->as == NULL)
	{
	  if (gfc_set_array_spec (cpte, as, &var_locus) == FAILURE)
	    gfc_internal_error ("Couldn't set Cray pointee array spec.");
	}
      else if (as != NULL)
	{
	  gfc_error ("Duplicate array spec for Cray pointee at %C.");
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
      gfc_error ("Expected \",\" or end of statement at %C");
      return MATCH_ERROR;
    }
  return MATCH_YES;
}


match
gfc_match_external (void)
{

  gfc_clear_attr (&current_attr);
  gfc_add_external (&current_attr, NULL);

  return attr_decl ();
}



match
gfc_match_intent (void)
{
  sym_intent intent;

  intent = match_intent_spec ();
  if (intent == INTENT_UNKNOWN)
    return MATCH_ERROR;

  gfc_clear_attr (&current_attr);
  gfc_add_intent (&current_attr, intent, NULL);	/* Can't fail */

  return attr_decl ();
}


match
gfc_match_intrinsic (void)
{

  gfc_clear_attr (&current_attr);
  gfc_add_intrinsic (&current_attr, NULL);

  return attr_decl ();
}


match
gfc_match_optional (void)
{

  gfc_clear_attr (&current_attr);
  gfc_add_optional (&current_attr, NULL);

  return attr_decl ();
}


match
gfc_match_pointer (void)
{
  gfc_gobble_whitespace ();
  if (gfc_peek_char () == '(')
    {
      if (!gfc_option.flag_cray_pointer)
	{
	  gfc_error ("Cray pointer declaration at %C requires -fcray-pointer"
		     " flag.");
	  return MATCH_ERROR;
	}
      return cray_pointer_decl ();
    }
  else
    {
      gfc_clear_attr (&current_attr);
      gfc_add_pointer (&current_attr, NULL);
    
      return attr_decl ();
    }
}


match
gfc_match_allocatable (void)
{

  gfc_clear_attr (&current_attr);
  gfc_add_allocatable (&current_attr, NULL);

  return attr_decl ();
}


match
gfc_match_dimension (void)
{

  gfc_clear_attr (&current_attr);
  gfc_add_dimension (&current_attr, NULL, NULL);

  return attr_decl ();
}


match
gfc_match_target (void)
{

  gfc_clear_attr (&current_attr);
  gfc_add_target (&current_attr, NULL);

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
  gfc_symbol *sym;
  gfc_intrinsic_op operator;
  match m;

  if (gfc_match (" ::") == MATCH_NO && gfc_match_space () == MATCH_NO)
    goto done;

  for (;;)
    {
      m = gfc_match_generic_spec (&type, name, &operator);
      if (m == MATCH_NO)
	goto syntax;
      if (m == MATCH_ERROR)
	return MATCH_ERROR;

      switch (type)
	{
	case INTERFACE_NAMELESS:
	  goto syntax;

	case INTERFACE_GENERIC:
	  if (gfc_get_symbol (name, NULL, &sym))
	    goto done;

	  if (gfc_add_access (&sym->attr,
			      (st ==
			       ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE,
			      sym->name, NULL) == FAILURE)
	    return MATCH_ERROR;

	  break;

	case INTERFACE_INTRINSIC_OP:
	  if (gfc_current_ns->operator_access[operator] == ACCESS_UNKNOWN)
	    {
	      gfc_current_ns->operator_access[operator] =
		(st == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE;
	    }
	  else
	    {
	      gfc_error ("Access specification of the %s operator at %C has "
			 "already been specified", gfc_op2string (operator));
	      goto done;
	    }

	  break;

	case INTERFACE_USER_OP:
	  uop = gfc_get_uop (name);

	  if (uop->access == ACCESS_UNKNOWN)
	    {
	      uop->access =
		(st == ST_PUBLIC) ? ACCESS_PUBLIC : ACCESS_PRIVATE;
	    }
	  else
	    {
	      gfc_error
		("Access specification of the .%s. operator at %C has "
		 "already been specified", sym->name);
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


/* The PRIVATE statement is a bit weird in that it can be a attribute
   declaration, but also works as a standlone statement inside of a
   type declaration or a module.  */

match
gfc_match_private (gfc_statement * st)
{

  if (gfc_match ("private") != MATCH_YES)
    return MATCH_NO;

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
gfc_match_public (gfc_statement * st)
{

  if (gfc_match ("public") != MATCH_YES)
    return MATCH_NO;

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
      && gfc_set_default_type (sym, 1, NULL) == FAILURE)
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  if (gfc_check_assign_symbol (sym, init) == FAILURE
      || gfc_add_flavor (&sym->attr, FL_PARAMETER, sym->name, NULL) == FAILURE)
    {
      m = MATCH_ERROR;
      goto cleanup;
    }

  if (sym->ts.type == BT_CHARACTER
      && sym->ts.cl != NULL
      && sym->ts.cl->length != NULL
      && sym->ts.cl->length->expr_type == EXPR_CONSTANT
      && init->expr_type == EXPR_CONSTANT
      && init->ts.type == BT_CHARACTER
      && init->ts.kind == 1)
    gfc_set_constant_character_len (
      mpz_get_si (sym->ts.cl->length->value.integer), init);

  sym->value = init;
  return MATCH_YES;

cleanup:
  gfc_free_expr (init);
  return m;
}


/* Match a parameter statement, with the weird syntax that these have.  */

match
gfc_match_parameter (void)
{
  match m;

  if (gfc_match_char ('(') == MATCH_NO)
    return MATCH_NO;

  for (;;)
    {
      m = do_parm ();
      if (m != MATCH_YES)
	break;

      if (gfc_match (" )%t") == MATCH_YES)
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
	  if (gfc_notify_std (GFC_STD_LEGACY, 
			      "Blanket SAVE statement at %C follows previous "
			      "SAVE statement")
	      == FAILURE)
	    return MATCH_ERROR;
	}

      gfc_current_ns->save_all = gfc_current_ns->seen_save = 1;
      return MATCH_YES;
    }

  if (gfc_current_ns->save_all)
    {
      if (gfc_notify_std (GFC_STD_LEGACY, 
			  "SAVE statement at %C follows blanket SAVE statement")
	  == FAILURE)
	return MATCH_ERROR;
    }

  gfc_match (" ::");

  for (;;)
    {
      m = gfc_match_symbol (&sym, 0);
      switch (m)
	{
	case MATCH_YES:
	  if (gfc_add_save (&sym->attr, sym->name,
			    &gfc_current_locus) == FAILURE)
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


/* Match a module procedure statement.  Note that we have to modify
   symbols in the parent's namespace because the current one was there
   to receive symbols that are in an interface's formal argument list.  */

match
gfc_match_modproc (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;
  match m;

  if (gfc_state_stack->state != COMP_INTERFACE
      || gfc_state_stack->previous == NULL
      || current_interface.type == INTERFACE_NAMELESS)
    {
      gfc_error
	("MODULE PROCEDURE at %C must be in a generic module interface");
      return MATCH_ERROR;
    }

  for (;;)
    {
      m = gfc_match_name (name);
      if (m == MATCH_NO)
	goto syntax;
      if (m != MATCH_YES)
	return MATCH_ERROR;

      if (gfc_get_symbol (name, gfc_current_ns->parent, &sym))
	return MATCH_ERROR;

      if (sym->attr.proc != PROC_MODULE
	  && gfc_add_procedure (&sym->attr, PROC_MODULE,
				sym->name, NULL) == FAILURE)
	return MATCH_ERROR;

      if (gfc_add_interface (sym) == FAILURE)
	return MATCH_ERROR;

      if (gfc_match_eos () == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_MODULE_PROC);
  return MATCH_ERROR;
}


/* Match the beginning of a derived type declaration.  If a type name
   was the result of a function, then it is possible to have a symbol
   already to be known as a derived type yet have no components.  */

match
gfc_match_derived_decl (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  symbol_attribute attr;
  gfc_symbol *sym;
  match m;

  if (gfc_current_state () == COMP_DERIVED)
    return MATCH_NO;

  gfc_clear_attr (&attr);

loop:
  if (gfc_match (" , private") == MATCH_YES)
    {
      if (gfc_find_state (COMP_MODULE) == FAILURE)
	{
	  gfc_error
	    ("Derived type at %C can only be PRIVATE within a MODULE");
	  return MATCH_ERROR;
	}

      if (gfc_add_access (&attr, ACCESS_PRIVATE, NULL, NULL) == FAILURE)
	return MATCH_ERROR;
      goto loop;
    }

  if (gfc_match (" , public") == MATCH_YES)
    {
      if (gfc_find_state (COMP_MODULE) == FAILURE)
	{
	  gfc_error ("Derived type at %C can only be PUBLIC within a MODULE");
	  return MATCH_ERROR;
	}

      if (gfc_add_access (&attr, ACCESS_PUBLIC, NULL, NULL) == FAILURE)
	return MATCH_ERROR;
      goto loop;
    }

  if (gfc_match (" ::") != MATCH_YES && attr.access != ACCESS_UNKNOWN)
    {
      gfc_error ("Expected :: in TYPE definition at %C");
      return MATCH_ERROR;
    }

  m = gfc_match (" %n%t", name);
  if (m != MATCH_YES)
    return m;

  /* Make sure the name isn't the name of an intrinsic type.  The
     'double precision' type doesn't get past the name matcher.  */
  if (strcmp (name, "integer") == 0
      || strcmp (name, "real") == 0
      || strcmp (name, "character") == 0
      || strcmp (name, "logical") == 0
      || strcmp (name, "complex") == 0)
    {
      gfc_error
	("Type name '%s' at %C cannot be the same as an intrinsic type",
	 name);
      return MATCH_ERROR;
    }

  if (gfc_get_symbol (name, NULL, &sym))
    return MATCH_ERROR;

  if (sym->ts.type != BT_UNKNOWN)
    {
      gfc_error ("Derived type name '%s' at %C already has a basic type "
		 "of %s", sym->name, gfc_typename (&sym->ts));
      return MATCH_ERROR;
    }

  /* The symbol may already have the derived attribute without the
     components.  The ways this can happen is via a function
     definition, an INTRINSIC statement or a subtype in another
     derived type that is a pointer.  The first part of the AND clause
     is true if a the symbol is not the return value of a function.  */
  if (sym->attr.flavor != FL_DERIVED
      && gfc_add_flavor (&sym->attr, FL_DERIVED, sym->name, NULL) == FAILURE)
    return MATCH_ERROR;

  if (sym->components != NULL)
    {
      gfc_error
	("Derived type definition of '%s' at %C has already been defined",
	 sym->name);
      return MATCH_ERROR;
    }

  if (attr.access != ACCESS_UNKNOWN
      && gfc_add_access (&sym->attr, attr.access, sym->name, NULL) == FAILURE)
    return MATCH_ERROR;

  gfc_new_block = sym;

  return MATCH_YES;
}


/* Cray Pointees can be declared as: 
      pointer (ipt, a (n,m,...,*)) 
   By default, this is treated as an AS_ASSUMED_SIZE array.  We'll
   cheat and set a constant bound of 1 for the last dimension, if this
   is the case. Since there is no bounds-checking for Cray Pointees,
   this will be okay.  */

try
gfc_mod_pointee_as (gfc_array_spec *as)
{
  as->cray_pointee = true; /* This will be useful to know later.  */
  if (as->type == AS_ASSUMED_SIZE)
    {
      as->type = AS_EXPLICIT;
      as->upper[as->rank - 1] = gfc_int_expr (1);
      as->cp_was_assumed = true;
    }
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

  if (gfc_notify_std (GFC_STD_F2003, 
		      "New in Fortran 2003: ENUM AND ENUMERATOR at %C")
      == FAILURE)
    return MATCH_ERROR;

  return MATCH_YES;
}


/* Match the enumerator definition statement. */

match
gfc_match_enumerator_def (void)
{
  match m;
  int elem; 
  
  gfc_clear_ts (&current_ts);
  
  m = gfc_match (" enumerator");
  if (m != MATCH_YES)
    return m;
  
  if (gfc_current_state () != COMP_ENUM)
    {
      gfc_error ("ENUM definition statement expected before %C");
      gfc_free_enum_history ();
      return MATCH_ERROR;
    }

  (&current_ts)->type = BT_INTEGER;
  (&current_ts)->kind = gfc_c_int_kind;
  
  m = match_attr_spec ();
  if (m == MATCH_ERROR)
    {
      m = MATCH_NO;
      goto cleanup;
    }

  elem = 1;
  for (;;)
    {
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

