/* Handle modules, which amounts to loading and saving symbols and
   their attendant structures.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, 
   Inc.
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

/* The syntax of gfortran modules resembles that of lisp lists, ie a
   sequence of atoms, which can be left or right parenthesis, names,
   integers or strings.  Parenthesis are always matched which allows
   us to skip over sections at high speed without having to know
   anything about the internal structure of the lists.  A "name" is
   usually a fortran 95 identifier, but can also start with '@' in
   order to reference a hidden symbol.

   The first line of a module is an informational message about what
   created the module, the file it came from and when it was created.
   The second line is a warning for people not to edit the module.
   The rest of the module looks like:

   ( ( <Interface info for UPLUS> )
     ( <Interface info for UMINUS> )
     ...
   )
   ( ( <name of operator interface> <module of op interface> <i/f1> ... )
     ...
   )
   ( ( <name of generic interface> <module of generic interface> <i/f1> ... )
     ...
   )
   ( ( <common name> <symbol> <saved flag>)
     ...
   )

   ( equivalence list )

   ( <Symbol Number (in no particular order)>
     <True name of symbol>
     <Module name of symbol>
     ( <symbol information> )
     ...
   )
   ( <Symtree name>
     <Ambiguous flag>
     <Symbol number>
     ...
   )

   In general, symbols refer to other symbols by their symbol number,
   which are zero based.  Symbols are written to the module in no
   particular order.  */

#include "config.h"
#include "system.h"
#include "gfortran.h"
#include "arith.h"
#include "match.h"
#include "parse.h" /* FIXME */

#define MODULE_EXTENSION ".mod"


/* Structure that describes a position within a module file.  */

typedef struct
{
  int column, line;
  fpos_t pos;
}
module_locus;


typedef enum
{
  P_UNKNOWN = 0, P_OTHER, P_NAMESPACE, P_COMPONENT, P_SYMBOL
}
pointer_t;

/* The fixup structure lists pointers to pointers that have to
   be updated when a pointer value becomes known.  */

typedef struct fixup_t
{
  void **pointer;
  struct fixup_t *next;
}
fixup_t;


/* Structure for holding extra info needed for pointers being read.  */

typedef struct pointer_info
{
  BBT_HEADER (pointer_info);
  int integer;
  pointer_t type;

  /* The first component of each member of the union is the pointer
     being stored.  */

  fixup_t *fixup;

  union
  {
    void *pointer;	/* Member for doing pointer searches.  */

    struct
    {
      gfc_symbol *sym;
      char true_name[GFC_MAX_SYMBOL_LEN + 1], module[GFC_MAX_SYMBOL_LEN + 1];
      enum
      { UNUSED, NEEDED, USED }
      state;
      int ns, referenced;
      module_locus where;
      fixup_t *stfixup;
      gfc_symtree *symtree;
    }
    rsym;

    struct
    {
      gfc_symbol *sym;
      enum
      { UNREFERENCED = 0, NEEDS_WRITE, WRITTEN }
      state;
    }
    wsym;
  }
  u;

}
pointer_info;

#define gfc_get_pointer_info() gfc_getmem(sizeof(pointer_info))


/* Lists of rename info for the USE statement.  */

typedef struct gfc_use_rename
{
  char local_name[GFC_MAX_SYMBOL_LEN + 1], use_name[GFC_MAX_SYMBOL_LEN + 1];
  struct gfc_use_rename *next;
  int found;
  gfc_intrinsic_op operator;
  locus where;
}
gfc_use_rename;

#define gfc_get_use_rename() gfc_getmem(sizeof(gfc_use_rename))

/* Local variables */

/* The FILE for the module we're reading or writing.  */
static FILE *module_fp;

/* The name of the module we're reading (USE'ing) or writing.  */
static char module_name[GFC_MAX_SYMBOL_LEN + 1];

static int module_line, module_column, only_flag;
static enum
{ IO_INPUT, IO_OUTPUT }
iomode;

static gfc_use_rename *gfc_rename_list;
static pointer_info *pi_root;
static int symbol_number;	/* Counter for assigning symbol numbers */



/*****************************************************************/

/* Pointer/integer conversion.  Pointers between structures are stored
   as integers in the module file.  The next couple of subroutines
   handle this translation for reading and writing.  */

/* Recursively free the tree of pointer structures.  */

static void
free_pi_tree (pointer_info * p)
{
  if (p == NULL)
    return;

  if (p->fixup != NULL)
    gfc_internal_error ("free_pi_tree(): Unresolved fixup");

  free_pi_tree (p->left);
  free_pi_tree (p->right);

  gfc_free (p);
}


/* Compare pointers when searching by pointer.  Used when writing a
   module.  */

static int
compare_pointers (void * _sn1, void * _sn2)
{
  pointer_info *sn1, *sn2;

  sn1 = (pointer_info *) _sn1;
  sn2 = (pointer_info *) _sn2;

  if (sn1->u.pointer < sn2->u.pointer)
    return -1;
  if (sn1->u.pointer > sn2->u.pointer)
    return 1;

  return 0;
}


/* Compare integers when searching by integer.  Used when reading a
   module.  */

static int
compare_integers (void * _sn1, void * _sn2)
{
  pointer_info *sn1, *sn2;

  sn1 = (pointer_info *) _sn1;
  sn2 = (pointer_info *) _sn2;

  if (sn1->integer < sn2->integer)
    return -1;
  if (sn1->integer > sn2->integer)
    return 1;

  return 0;
}


/* Initialize the pointer_info tree.  */

static void
init_pi_tree (void)
{
  compare_fn compare;
  pointer_info *p;

  pi_root = NULL;
  compare = (iomode == IO_INPUT) ? compare_integers : compare_pointers;

  /* Pointer 0 is the NULL pointer.  */
  p = gfc_get_pointer_info ();
  p->u.pointer = NULL;
  p->integer = 0;
  p->type = P_OTHER;

  gfc_insert_bbt (&pi_root, p, compare);

  /* Pointer 1 is the current namespace.  */
  p = gfc_get_pointer_info ();
  p->u.pointer = gfc_current_ns;
  p->integer = 1;
  p->type = P_NAMESPACE;

  gfc_insert_bbt (&pi_root, p, compare);

  symbol_number = 2;
}


/* During module writing, call here with a pointer to something,
   returning the pointer_info node.  */

static pointer_info *
find_pointer (void *gp)
{
  pointer_info *p;

  p = pi_root;
  while (p != NULL)
    {
      if (p->u.pointer == gp)
	break;
      p = (gp < p->u.pointer) ? p->left : p->right;
    }

  return p;
}


/* Given a pointer while writing, returns the pointer_info tree node,
   creating it if it doesn't exist.  */

static pointer_info *
get_pointer (void *gp)
{
  pointer_info *p;

  p = find_pointer (gp);
  if (p != NULL)
    return p;

  /* Pointer doesn't have an integer.  Give it one.  */
  p = gfc_get_pointer_info ();

  p->u.pointer = gp;
  p->integer = symbol_number++;

  gfc_insert_bbt (&pi_root, p, compare_pointers);

  return p;
}


/* Given an integer during reading, find it in the pointer_info tree,
   creating the node if not found.  */

static pointer_info *
get_integer (int integer)
{
  pointer_info *p, t;
  int c;

  t.integer = integer;

  p = pi_root;
  while (p != NULL)
    {
      c = compare_integers (&t, p);
      if (c == 0)
	break;

      p = (c < 0) ? p->left : p->right;
    }

  if (p != NULL)
    return p;

  p = gfc_get_pointer_info ();
  p->integer = integer;
  p->u.pointer = NULL;

  gfc_insert_bbt (&pi_root, p, compare_integers);

  return p;
}


/* Recursive function to find a pointer within a tree by brute force.  */

static pointer_info *
fp2 (pointer_info * p, const void *target)
{
  pointer_info *q;

  if (p == NULL)
    return NULL;

  if (p->u.pointer == target)
    return p;

  q = fp2 (p->left, target);
  if (q != NULL)
    return q;

  return fp2 (p->right, target);
}


/* During reading, find a pointer_info node from the pointer value.
   This amounts to a brute-force search.  */

static pointer_info *
find_pointer2 (void *p)
{

  return fp2 (pi_root, p);
}


/* Resolve any fixups using a known pointer.  */
static void
resolve_fixups (fixup_t *f, void * gp)
{
  fixup_t *next;

  for (; f; f = next)
    {
      next = f->next;
      *(f->pointer) = gp;
      gfc_free (f);
    }
}

/* Call here during module reading when we know what pointer to
   associate with an integer.  Any fixups that exist are resolved at
   this time.  */

static void
associate_integer_pointer (pointer_info * p, void *gp)
{
  if (p->u.pointer != NULL)
    gfc_internal_error ("associate_integer_pointer(): Already associated");

  p->u.pointer = gp;

  resolve_fixups (p->fixup, gp);

  p->fixup = NULL;
}


/* During module reading, given an integer and a pointer to a pointer,
   either store the pointer from an already-known value or create a
   fixup structure in order to store things later.  Returns zero if
   the reference has been actually stored, or nonzero if the reference
   must be fixed later (ie associate_integer_pointer must be called
   sometime later.  Returns the pointer_info structure.  */

static pointer_info *
add_fixup (int integer, void *gp)
{
  pointer_info *p;
  fixup_t *f;
  char **cp;

  p = get_integer (integer);

  if (p->integer == 0 || p->u.pointer != NULL)
    {
      cp = gp;
      *cp = p->u.pointer;
    }
  else
    {
      f = gfc_getmem (sizeof (fixup_t));

      f->next = p->fixup;
      p->fixup = f;

      f->pointer = gp;
    }

  return p;
}


/*****************************************************************/

/* Parser related subroutines */

/* Free the rename list left behind by a USE statement.  */

static void
free_rename (void)
{
  gfc_use_rename *next;

  for (; gfc_rename_list; gfc_rename_list = next)
    {
      next = gfc_rename_list->next;
      gfc_free (gfc_rename_list);
    }
}


/* Match a USE statement.  */

match
gfc_match_use (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_use_rename *tail = NULL, *new;
  interface_type type;
  gfc_intrinsic_op operator;
  match m;

  m = gfc_match_name (module_name);
  if (m != MATCH_YES)
    return m;

  free_rename ();
  only_flag = 0;

  if (gfc_match_eos () == MATCH_YES)
    return MATCH_YES;
  if (gfc_match_char (',') != MATCH_YES)
    goto syntax;

  if (gfc_match (" only :") == MATCH_YES)
    only_flag = 1;

  if (gfc_match_eos () == MATCH_YES)
    return MATCH_YES;

  for (;;)
    {
      /* Get a new rename struct and add it to the rename list.  */
      new = gfc_get_use_rename ();
      new->where = gfc_current_locus;
      new->found = 0;

      if (gfc_rename_list == NULL)
	gfc_rename_list = new;
      else
	tail->next = new;
      tail = new;

      /* See what kind of interface we're dealing with.  Assume it is
         not an operator.  */
      new->operator = INTRINSIC_NONE;
      if (gfc_match_generic_spec (&type, name, &operator) == MATCH_ERROR)
	goto cleanup;

      switch (type)
	{
	case INTERFACE_NAMELESS:
	  gfc_error ("Missing generic specification in USE statement at %C");
	  goto cleanup;

	case INTERFACE_GENERIC:
	  m = gfc_match (" =>");

	  if (only_flag)
	    {
	      if (m != MATCH_YES)
		strcpy (new->use_name, name);
	      else
		{
		  strcpy (new->local_name, name);

		  m = gfc_match_name (new->use_name);
		  if (m == MATCH_NO)
		    goto syntax;
		  if (m == MATCH_ERROR)
		    goto cleanup;
		}
	    }
	  else
	    {
	      if (m != MATCH_YES)
		goto syntax;
	      strcpy (new->local_name, name);

	      m = gfc_match_name (new->use_name);
	      if (m == MATCH_NO)
		goto syntax;
	      if (m == MATCH_ERROR)
		goto cleanup;
	    }

	  break;

	case INTERFACE_USER_OP:
	  strcpy (new->use_name, name);
	  /* Fall through */

	case INTERFACE_INTRINSIC_OP:
	  new->operator = operator;
	  break;
	}

      if (gfc_match_eos () == MATCH_YES)
	break;
      if (gfc_match_char (',') != MATCH_YES)
	goto syntax;
    }

  return MATCH_YES;

syntax:
  gfc_syntax_error (ST_USE);

cleanup:
  free_rename ();
  return MATCH_ERROR;
 }


/* Given a name and a number, inst, return the inst name
   under which to load this symbol. Returns NULL if this
   symbol shouldn't be loaded. If inst is zero, returns
   the number of instances of this name.  */

static const char *
find_use_name_n (const char *name, int *inst)
{
  gfc_use_rename *u;
  int i;

  i = 0;
  for (u = gfc_rename_list; u; u = u->next)
    {
      if (strcmp (u->use_name, name) != 0)
	continue;
      if (++i == *inst)
	break;
    }

  if (!*inst)
    {
      *inst = i;
      return NULL;
    }

  if (u == NULL)
    return only_flag ? NULL : name;

  u->found = 1;

  return (u->local_name[0] != '\0') ? u->local_name : name;
}

/* Given a name, return the name under which to load this symbol.
   Returns NULL if this symbol shouldn't be loaded.  */

static const char *
find_use_name (const char *name)
{
  int i = 1;
  return find_use_name_n (name, &i);
}

/* Given a real name, return the number of use names associated
   with it.  */

static int
number_use_names (const char *name)
{
  int i = 0;
  const char *c;
  c = find_use_name_n (name, &i);
  return i;
}


/* Try to find the operator in the current list.  */

static gfc_use_rename *
find_use_operator (gfc_intrinsic_op operator)
{
  gfc_use_rename *u;

  for (u = gfc_rename_list; u; u = u->next)
    if (u->operator == operator)
      return u;

  return NULL;
}


/*****************************************************************/

/* The next couple of subroutines maintain a tree used to avoid a
   brute-force search for a combination of true name and module name.
   While symtree names, the name that a particular symbol is known by
   can changed with USE statements, we still have to keep track of the
   true names to generate the correct reference, and also avoid
   loading the same real symbol twice in a program unit.

   When we start reading, the true name tree is built and maintained
   as symbols are read.  The tree is searched as we load new symbols
   to see if it already exists someplace in the namespace.  */

typedef struct true_name
{
  BBT_HEADER (true_name);
  gfc_symbol *sym;
}
true_name;

static true_name *true_name_root;


/* Compare two true_name structures.  */

static int
compare_true_names (void * _t1, void * _t2)
{
  true_name *t1, *t2;
  int c;

  t1 = (true_name *) _t1;
  t2 = (true_name *) _t2;

  c = ((t1->sym->module > t2->sym->module)
       - (t1->sym->module < t2->sym->module));
  if (c != 0)
    return c;

  return strcmp (t1->sym->name, t2->sym->name);
}


/* Given a true name, search the true name tree to see if it exists
   within the main namespace.  */

static gfc_symbol *
find_true_name (const char *name, const char *module)
{
  true_name t, *p;
  gfc_symbol sym;
  int c;

  sym.name = gfc_get_string (name);
  if (module != NULL)
    sym.module = gfc_get_string (module);
  else
    sym.module = NULL;
  t.sym = &sym;

  p = true_name_root;
  while (p != NULL)
    {
      c = compare_true_names ((void *)(&t), (void *) p);
      if (c == 0)
	return p->sym;

      p = (c < 0) ? p->left : p->right;
    }

  return NULL;
}


/* Given a gfc_symbol pointer that is not in the true name tree, add
   it.  */

static void
add_true_name (gfc_symbol * sym)
{
  true_name *t;

  t = gfc_getmem (sizeof (true_name));
  t->sym = sym;

  gfc_insert_bbt (&true_name_root, t, compare_true_names);
}


/* Recursive function to build the initial true name tree by
   recursively traversing the current namespace.  */

static void
build_tnt (gfc_symtree * st)
{

  if (st == NULL)
    return;

  build_tnt (st->left);
  build_tnt (st->right);

  if (find_true_name (st->n.sym->name, st->n.sym->module) != NULL)
    return;

  add_true_name (st->n.sym);
}


/* Initialize the true name tree with the current namespace.  */

static void
init_true_name_tree (void)
{
  true_name_root = NULL;

  build_tnt (gfc_current_ns->sym_root);
}


/* Recursively free a true name tree node.  */

static void
free_true_name (true_name * t)
{

  if (t == NULL)
    return;
  free_true_name (t->left);
  free_true_name (t->right);

  gfc_free (t);
}


/*****************************************************************/

/* Module reading and writing.  */

typedef enum
{
  ATOM_NAME, ATOM_LPAREN, ATOM_RPAREN, ATOM_INTEGER, ATOM_STRING
}
atom_type;

static atom_type last_atom;


/* The name buffer must be at least as long as a symbol name.  Right
   now it's not clear how we're going to store numeric constants--
   probably as a hexadecimal string, since this will allow the exact
   number to be preserved (this can't be done by a decimal
   representation).  Worry about that later.  TODO!  */

#define MAX_ATOM_SIZE 100

static int atom_int;
static char *atom_string, atom_name[MAX_ATOM_SIZE];


/* Report problems with a module.  Error reporting is not very
   elaborate, since this sorts of errors shouldn't really happen.
   This subroutine never returns.  */

static void bad_module (const char *) ATTRIBUTE_NORETURN;

static void
bad_module (const char *msgid)
{
  fclose (module_fp);

  switch (iomode)
    {
    case IO_INPUT:
      gfc_fatal_error ("Reading module %s at line %d column %d: %s",
	  	       module_name, module_line, module_column, msgid);
      break;
    case IO_OUTPUT:
      gfc_fatal_error ("Writing module %s at line %d column %d: %s",
	  	       module_name, module_line, module_column, msgid);
      break;
    default:
      gfc_fatal_error ("Module %s at line %d column %d: %s",
	  	       module_name, module_line, module_column, msgid);
      break;
    }
}


/* Set the module's input pointer.  */

static void
set_module_locus (module_locus * m)
{

  module_column = m->column;
  module_line = m->line;
  fsetpos (module_fp, &m->pos);
}


/* Get the module's input pointer so that we can restore it later.  */

static void
get_module_locus (module_locus * m)
{

  m->column = module_column;
  m->line = module_line;
  fgetpos (module_fp, &m->pos);
}


/* Get the next character in the module, updating our reckoning of
   where we are.  */

static int
module_char (void)
{
  int c;

  c = fgetc (module_fp);

  if (c == EOF)
    bad_module ("Unexpected EOF");

  if (c == '\n')
    {
      module_line++;
      module_column = 0;
    }

  module_column++;
  return c;
}


/* Parse a string constant.  The delimiter is guaranteed to be a
   single quote.  */

static void
parse_string (void)
{
  module_locus start;
  int len, c;
  char *p;

  get_module_locus (&start);

  len = 0;

  /* See how long the string is */
  for ( ; ; )
    {
      c = module_char ();
      if (c == EOF)
	bad_module ("Unexpected end of module in string constant");

      if (c != '\'')
        {
	  len++;
	  continue;
	}

      c = module_char ();
      if (c == '\'')
        {
	  len++;
	  continue;
	}

      break;
    }

  set_module_locus (&start);

  atom_string = p = gfc_getmem (len + 1);

  for (; len > 0; len--)
    {
      c = module_char ();
      if (c == '\'')
	module_char ();		/* Guaranteed to be another \' */
      *p++ = c;
    }

  module_char ();		/* Terminating \' */
  *p = '\0';			/* C-style string for debug purposes */
}


/* Parse a small integer.  */

static void
parse_integer (int c)
{
  module_locus m;

  atom_int = c - '0';

  for (;;)
    {
      get_module_locus (&m);

      c = module_char ();
      if (!ISDIGIT (c))
	break;

      atom_int = 10 * atom_int + c - '0';
      if (atom_int > 99999999)
	bad_module ("Integer overflow");
    }

  set_module_locus (&m);
}


/* Parse a name.  */

static void
parse_name (int c)
{
  module_locus m;
  char *p;
  int len;

  p = atom_name;

  *p++ = c;
  len = 1;

  get_module_locus (&m);

  for (;;)
    {
      c = module_char ();
      if (!ISALNUM (c) && c != '_' && c != '-')
	break;

      *p++ = c;
      if (++len > GFC_MAX_SYMBOL_LEN)
	bad_module ("Name too long");
    }

  *p = '\0';

  fseek (module_fp, -1, SEEK_CUR);
  module_column = m.column + len - 1;

  if (c == '\n')
    module_line--;
}


/* Read the next atom in the module's input stream.  */

static atom_type
parse_atom (void)
{
  int c;

  do
    {
      c = module_char ();
    }
  while (c == ' ' || c == '\n');

  switch (c)
    {
    case '(':
      return ATOM_LPAREN;

    case ')':
      return ATOM_RPAREN;

    case '\'':
      parse_string ();
      return ATOM_STRING;

    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      parse_integer (c);
      return ATOM_INTEGER;

    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
    case 'g':
    case 'h':
    case 'i':
    case 'j':
    case 'k':
    case 'l':
    case 'm':
    case 'n':
    case 'o':
    case 'p':
    case 'q':
    case 'r':
    case 's':
    case 't':
    case 'u':
    case 'v':
    case 'w':
    case 'x':
    case 'y':
    case 'z':
    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'G':
    case 'H':
    case 'I':
    case 'J':
    case 'K':
    case 'L':
    case 'M':
    case 'N':
    case 'O':
    case 'P':
    case 'Q':
    case 'R':
    case 'S':
    case 'T':
    case 'U':
    case 'V':
    case 'W':
    case 'X':
    case 'Y':
    case 'Z':
      parse_name (c);
      return ATOM_NAME;

    default:
      bad_module ("Bad name");
    }

  /* Not reached */
}


/* Peek at the next atom on the input.  */

static atom_type
peek_atom (void)
{
  module_locus m;
  atom_type a;

  get_module_locus (&m);

  a = parse_atom ();
  if (a == ATOM_STRING)
    gfc_free (atom_string);

  set_module_locus (&m);
  return a;
}


/* Read the next atom from the input, requiring that it be a
   particular kind.  */

static void
require_atom (atom_type type)
{
  module_locus m;
  atom_type t;
  const char *p;

  get_module_locus (&m);

  t = parse_atom ();
  if (t != type)
    {
      switch (type)
	{
	case ATOM_NAME:
	  p = _("Expected name");
	  break;
	case ATOM_LPAREN:
	  p = _("Expected left parenthesis");
	  break;
	case ATOM_RPAREN:
	  p = _("Expected right parenthesis");
	  break;
	case ATOM_INTEGER:
	  p = _("Expected integer");
	  break;
	case ATOM_STRING:
	  p = _("Expected string");
	  break;
	default:
	  gfc_internal_error ("require_atom(): bad atom type required");
	}

      set_module_locus (&m);
      bad_module (p);
    }
}


/* Given a pointer to an mstring array, require that the current input
   be one of the strings in the array.  We return the enum value.  */

static int
find_enum (const mstring * m)
{
  int i;

  i = gfc_string2code (m, atom_name);
  if (i >= 0)
    return i;

  bad_module ("find_enum(): Enum not found");

  /* Not reached */
}


/**************** Module output subroutines ***************************/

/* Output a character to a module file.  */

static void
write_char (char out)
{

  if (fputc (out, module_fp) == EOF)
    gfc_fatal_error ("Error writing modules file: %s", strerror (errno));

  if (out != '\n')
    module_column++;
  else
    {
      module_column = 1;
      module_line++;
    }
}


/* Write an atom to a module.  The line wrapping isn't perfect, but it
   should work most of the time.  This isn't that big of a deal, since
   the file really isn't meant to be read by people anyway.  */

static void
write_atom (atom_type atom, const void *v)
{
  char buffer[20];
  int i, len;
  const char *p;

  switch (atom)
    {
    case ATOM_STRING:
    case ATOM_NAME:
      p = v;
      break;

    case ATOM_LPAREN:
      p = "(";
      break;

    case ATOM_RPAREN:
      p = ")";
      break;

    case ATOM_INTEGER:
      i = *((const int *) v);
      if (i < 0)
	gfc_internal_error ("write_atom(): Writing negative integer");

      sprintf (buffer, "%d", i);
      p = buffer;
      break;

    default:
      gfc_internal_error ("write_atom(): Trying to write dab atom");

    }

  len = strlen (p);

  if (atom != ATOM_RPAREN)
    {
      if (module_column + len > 72)
	write_char ('\n');
      else
	{

	  if (last_atom != ATOM_LPAREN && module_column != 1)
	    write_char (' ');
	}
    }

  if (atom == ATOM_STRING)
    write_char ('\'');

  while (*p)
    {
      if (atom == ATOM_STRING && *p == '\'')
	write_char ('\'');
      write_char (*p++);
    }

  if (atom == ATOM_STRING)
    write_char ('\'');

  last_atom = atom;
}



/***************** Mid-level I/O subroutines *****************/

/* These subroutines let their caller read or write atoms without
   caring about which of the two is actually happening.  This lets a
   subroutine concentrate on the actual format of the data being
   written.  */

static void mio_expr (gfc_expr **);
static void mio_symbol_ref (gfc_symbol **);
static void mio_symtree_ref (gfc_symtree **);

/* Read or write an enumerated value.  On writing, we return the input
   value for the convenience of callers.  We avoid using an integer
   pointer because enums are sometimes inside bitfields.  */

static int
mio_name (int t, const mstring * m)
{

  if (iomode == IO_OUTPUT)
    write_atom (ATOM_NAME, gfc_code2string (m, t));
  else
    {
      require_atom (ATOM_NAME);
      t = find_enum (m);
    }

  return t;
}

/* Specialization of mio_name.  */

#define DECL_MIO_NAME(TYPE) \
 static inline TYPE \
 MIO_NAME(TYPE) (TYPE t, const mstring * m) \
 { \
   return (TYPE)mio_name ((int)t, m); \
 }
#define MIO_NAME(TYPE) mio_name_##TYPE

static void
mio_lparen (void)
{

  if (iomode == IO_OUTPUT)
    write_atom (ATOM_LPAREN, NULL);
  else
    require_atom (ATOM_LPAREN);
}


static void
mio_rparen (void)
{

  if (iomode == IO_OUTPUT)
    write_atom (ATOM_RPAREN, NULL);
  else
    require_atom (ATOM_RPAREN);
}


static void
mio_integer (int *ip)
{

  if (iomode == IO_OUTPUT)
    write_atom (ATOM_INTEGER, ip);
  else
    {
      require_atom (ATOM_INTEGER);
      *ip = atom_int;
    }
}


/* Read or write a character pointer that points to a string on the
   heap.  */

static const char *
mio_allocated_string (const char *s)
{
  if (iomode == IO_OUTPUT)
    {
      write_atom (ATOM_STRING, s);
      return s;
    }
  else
    {
      require_atom (ATOM_STRING);
      return atom_string;
    }
}


/* Read or write a string that is in static memory.  */

static void
mio_pool_string (const char **stringp)
{
  /* TODO: one could write the string only once, and refer to it via a
     fixup pointer.  */

  /* As a special case we have to deal with a NULL string.  This
     happens for the 'module' member of 'gfc_symbol's that are not in a
     module.  We read / write these as the empty string.  */
  if (iomode == IO_OUTPUT)
    {
      const char *p = *stringp == NULL ? "" : *stringp;
      write_atom (ATOM_STRING, p);
    }
  else
    {
      require_atom (ATOM_STRING);
      *stringp = atom_string[0] == '\0' ? NULL : gfc_get_string (atom_string);
      gfc_free (atom_string);
    }
}


/* Read or write a string that is inside of some already-allocated
   structure.  */

static void
mio_internal_string (char *string)
{

  if (iomode == IO_OUTPUT)
    write_atom (ATOM_STRING, string);
  else
    {
      require_atom (ATOM_STRING);
      strcpy (string, atom_string);
      gfc_free (atom_string);
    }
}



typedef enum
{ AB_ALLOCATABLE, AB_DIMENSION, AB_EXTERNAL, AB_INTRINSIC, AB_OPTIONAL,
  AB_POINTER, AB_SAVE, AB_TARGET, AB_DUMMY, AB_RESULT,
  AB_DATA, AB_IN_NAMELIST, AB_IN_COMMON, 
  AB_FUNCTION, AB_SUBROUTINE, AB_SEQUENCE, AB_ELEMENTAL, AB_PURE,
  AB_RECURSIVE, AB_GENERIC, AB_ALWAYS_EXPLICIT, AB_CRAY_POINTER,
  AB_CRAY_POINTEE
}
ab_attribute;

static const mstring attr_bits[] =
{
    minit ("ALLOCATABLE", AB_ALLOCATABLE),
    minit ("DIMENSION", AB_DIMENSION),
    minit ("EXTERNAL", AB_EXTERNAL),
    minit ("INTRINSIC", AB_INTRINSIC),
    minit ("OPTIONAL", AB_OPTIONAL),
    minit ("POINTER", AB_POINTER),
    minit ("SAVE", AB_SAVE),
    minit ("TARGET", AB_TARGET),
    minit ("DUMMY", AB_DUMMY),
    minit ("RESULT", AB_RESULT),
    minit ("DATA", AB_DATA),
    minit ("IN_NAMELIST", AB_IN_NAMELIST),
    minit ("IN_COMMON", AB_IN_COMMON),
    minit ("FUNCTION", AB_FUNCTION),
    minit ("SUBROUTINE", AB_SUBROUTINE),
    minit ("SEQUENCE", AB_SEQUENCE),
    minit ("ELEMENTAL", AB_ELEMENTAL),
    minit ("PURE", AB_PURE),
    minit ("RECURSIVE", AB_RECURSIVE),
    minit ("GENERIC", AB_GENERIC),
    minit ("ALWAYS_EXPLICIT", AB_ALWAYS_EXPLICIT),
    minit ("CRAY_POINTER", AB_CRAY_POINTER),
    minit ("CRAY_POINTEE", AB_CRAY_POINTEE),
    minit (NULL, -1)
};

/* Specialization of mio_name.  */
DECL_MIO_NAME(ab_attribute)
DECL_MIO_NAME(ar_type)
DECL_MIO_NAME(array_type)
DECL_MIO_NAME(bt)
DECL_MIO_NAME(expr_t)
DECL_MIO_NAME(gfc_access)
DECL_MIO_NAME(gfc_intrinsic_op)
DECL_MIO_NAME(ifsrc)
DECL_MIO_NAME(procedure_type)
DECL_MIO_NAME(ref_type)
DECL_MIO_NAME(sym_flavor)
DECL_MIO_NAME(sym_intent)
#undef DECL_MIO_NAME

/* Symbol attributes are stored in list with the first three elements
   being the enumerated fields, while the remaining elements (if any)
   indicate the individual attribute bits.  The access field is not
   saved-- it controls what symbols are exported when a module is
   written.  */

static void
mio_symbol_attribute (symbol_attribute * attr)
{
  atom_type t;

  mio_lparen ();

  attr->flavor = MIO_NAME(sym_flavor) (attr->flavor, flavors);
  attr->intent = MIO_NAME(sym_intent) (attr->intent, intents);
  attr->proc = MIO_NAME(procedure_type) (attr->proc, procedures);
  attr->if_source = MIO_NAME(ifsrc) (attr->if_source, ifsrc_types);

  if (iomode == IO_OUTPUT)
    {
      if (attr->allocatable)
	MIO_NAME(ab_attribute) (AB_ALLOCATABLE, attr_bits);
      if (attr->dimension)
	MIO_NAME(ab_attribute) (AB_DIMENSION, attr_bits);
      if (attr->external)
	MIO_NAME(ab_attribute) (AB_EXTERNAL, attr_bits);
      if (attr->intrinsic)
	MIO_NAME(ab_attribute) (AB_INTRINSIC, attr_bits);
      if (attr->optional)
	MIO_NAME(ab_attribute) (AB_OPTIONAL, attr_bits);
      if (attr->pointer)
	MIO_NAME(ab_attribute) (AB_POINTER, attr_bits);
      if (attr->save)
	MIO_NAME(ab_attribute) (AB_SAVE, attr_bits);
      if (attr->target)
	MIO_NAME(ab_attribute) (AB_TARGET, attr_bits);
      if (attr->dummy)
	MIO_NAME(ab_attribute) (AB_DUMMY, attr_bits);
      if (attr->result)
	MIO_NAME(ab_attribute) (AB_RESULT, attr_bits);
      /* We deliberately don't preserve the "entry" flag.  */

      if (attr->data)
	MIO_NAME(ab_attribute) (AB_DATA, attr_bits);
      if (attr->in_namelist)
	MIO_NAME(ab_attribute) (AB_IN_NAMELIST, attr_bits);
      if (attr->in_common)
	MIO_NAME(ab_attribute) (AB_IN_COMMON, attr_bits);

      if (attr->function)
	MIO_NAME(ab_attribute) (AB_FUNCTION, attr_bits);
      if (attr->subroutine)
	MIO_NAME(ab_attribute) (AB_SUBROUTINE, attr_bits);
      if (attr->generic)
	MIO_NAME(ab_attribute) (AB_GENERIC, attr_bits);

      if (attr->sequence)
	MIO_NAME(ab_attribute) (AB_SEQUENCE, attr_bits);
      if (attr->elemental)
	MIO_NAME(ab_attribute) (AB_ELEMENTAL, attr_bits);
      if (attr->pure)
	MIO_NAME(ab_attribute) (AB_PURE, attr_bits);
      if (attr->recursive)
	MIO_NAME(ab_attribute) (AB_RECURSIVE, attr_bits);
      if (attr->always_explicit)
        MIO_NAME(ab_attribute) (AB_ALWAYS_EXPLICIT, attr_bits);
      if (attr->cray_pointer)
	MIO_NAME(ab_attribute) (AB_CRAY_POINTER, attr_bits);
      if (attr->cray_pointee)
	MIO_NAME(ab_attribute) (AB_CRAY_POINTEE, attr_bits);

      mio_rparen ();

    }
  else
    {

      for (;;)
	{
	  t = parse_atom ();
	  if (t == ATOM_RPAREN)
	    break;
	  if (t != ATOM_NAME)
	    bad_module ("Expected attribute bit name");

	  switch ((ab_attribute) find_enum (attr_bits))
	    {
	    case AB_ALLOCATABLE:
	      attr->allocatable = 1;
	      break;
	    case AB_DIMENSION:
	      attr->dimension = 1;
	      break;
	    case AB_EXTERNAL:
	      attr->external = 1;
	      break;
	    case AB_INTRINSIC:
	      attr->intrinsic = 1;
	      break;
	    case AB_OPTIONAL:
	      attr->optional = 1;
	      break;
	    case AB_POINTER:
	      attr->pointer = 1;
	      break;
	    case AB_SAVE:
	      attr->save = 1;
	      break;
	    case AB_TARGET:
	      attr->target = 1;
	      break;
	    case AB_DUMMY:
	      attr->dummy = 1;
	      break;
	    case AB_RESULT:
	      attr->result = 1;
	      break;
	    case AB_DATA:
	      attr->data = 1;
	      break;
	    case AB_IN_NAMELIST:
	      attr->in_namelist = 1;
	      break;
	    case AB_IN_COMMON:
	      attr->in_common = 1;
	      break;
	    case AB_FUNCTION:
	      attr->function = 1;
	      break;
	    case AB_SUBROUTINE:
	      attr->subroutine = 1;
	      break;
	    case AB_GENERIC:
	      attr->generic = 1;
	      break;
	    case AB_SEQUENCE:
	      attr->sequence = 1;
	      break;
	    case AB_ELEMENTAL:
	      attr->elemental = 1;
	      break;
	    case AB_PURE:
	      attr->pure = 1;
	      break;
	    case AB_RECURSIVE:
	      attr->recursive = 1;
	      break;
            case AB_ALWAYS_EXPLICIT:
              attr->always_explicit = 1;
              break;
	    case AB_CRAY_POINTER:
	      attr->cray_pointer = 1;
	      break;
	    case AB_CRAY_POINTEE:
	      attr->cray_pointee = 1;
	      break;
	    }
	}
    }
}


static const mstring bt_types[] = {
    minit ("INTEGER", BT_INTEGER),
    minit ("REAL", BT_REAL),
    minit ("COMPLEX", BT_COMPLEX),
    minit ("LOGICAL", BT_LOGICAL),
    minit ("CHARACTER", BT_CHARACTER),
    minit ("DERIVED", BT_DERIVED),
    minit ("PROCEDURE", BT_PROCEDURE),
    minit ("UNKNOWN", BT_UNKNOWN),
    minit (NULL, -1)
};


static void
mio_charlen (gfc_charlen ** clp)
{
  gfc_charlen *cl;

  mio_lparen ();

  if (iomode == IO_OUTPUT)
    {
      cl = *clp;
      if (cl != NULL)
	mio_expr (&cl->length);
    }
  else
    {

      if (peek_atom () != ATOM_RPAREN)
	{
	  cl = gfc_get_charlen ();
	  mio_expr (&cl->length);

	  *clp = cl;

	  cl->next = gfc_current_ns->cl_list;
	  gfc_current_ns->cl_list = cl;
	}
    }

  mio_rparen ();
}


/* Return a symtree node with a name that is guaranteed to be unique
   within the namespace and corresponds to an illegal fortran name.  */

static gfc_symtree *
get_unique_symtree (gfc_namespace * ns)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  static int serial = 0;

  sprintf (name, "@%d", serial++);
  return gfc_new_symtree (&ns->sym_root, name);
}


/* See if a name is a generated name.  */

static int
check_unique_name (const char *name)
{

  return *name == '@';
}


static void
mio_typespec (gfc_typespec * ts)
{

  mio_lparen ();

  ts->type = MIO_NAME(bt) (ts->type, bt_types);

  if (ts->type != BT_DERIVED)
    mio_integer (&ts->kind);
  else
    mio_symbol_ref (&ts->derived);

  mio_charlen (&ts->cl);

  mio_rparen ();
}


static const mstring array_spec_types[] = {
    minit ("EXPLICIT", AS_EXPLICIT),
    minit ("ASSUMED_SHAPE", AS_ASSUMED_SHAPE),
    minit ("DEFERRED", AS_DEFERRED),
    minit ("ASSUMED_SIZE", AS_ASSUMED_SIZE),
    minit (NULL, -1)
};


static void
mio_array_spec (gfc_array_spec ** asp)
{
  gfc_array_spec *as;
  int i;

  mio_lparen ();

  if (iomode == IO_OUTPUT)
    {
      if (*asp == NULL)
	goto done;
      as = *asp;
    }
  else
    {
      if (peek_atom () == ATOM_RPAREN)
	{
	  *asp = NULL;
	  goto done;
	}

      *asp = as = gfc_get_array_spec ();
    }

  mio_integer (&as->rank);
  as->type = MIO_NAME(array_type) (as->type, array_spec_types);

  for (i = 0; i < as->rank; i++)
    {
      mio_expr (&as->lower[i]);
      mio_expr (&as->upper[i]);
    }

done:
  mio_rparen ();
}


/* Given a pointer to an array reference structure (which lives in a
   gfc_ref structure), find the corresponding array specification
   structure.  Storing the pointer in the ref structure doesn't quite
   work when loading from a module. Generating code for an array
   reference also needs more information than just the array spec.  */

static const mstring array_ref_types[] = {
    minit ("FULL", AR_FULL),
    minit ("ELEMENT", AR_ELEMENT),
    minit ("SECTION", AR_SECTION),
    minit (NULL, -1)
};

static void
mio_array_ref (gfc_array_ref * ar)
{
  int i;

  mio_lparen ();
  ar->type = MIO_NAME(ar_type) (ar->type, array_ref_types);
  mio_integer (&ar->dimen);

  switch (ar->type)
    {
    case AR_FULL:
      break;

    case AR_ELEMENT:
      for (i = 0; i < ar->dimen; i++)
	mio_expr (&ar->start[i]);

      break;

    case AR_SECTION:
      for (i = 0; i < ar->dimen; i++)
	{
	  mio_expr (&ar->start[i]);
	  mio_expr (&ar->end[i]);
	  mio_expr (&ar->stride[i]);
	}

      break;

    case AR_UNKNOWN:
      gfc_internal_error ("mio_array_ref(): Unknown array ref");
    }

  for (i = 0; i < ar->dimen; i++)
    mio_integer ((int *) &ar->dimen_type[i]);

  if (iomode == IO_INPUT)
    {
      ar->where = gfc_current_locus;

      for (i = 0; i < ar->dimen; i++)
	ar->c_where[i] = gfc_current_locus;
    }

  mio_rparen ();
}


/* Saves or restores a pointer.  The pointer is converted back and
   forth from an integer.  We return the pointer_info pointer so that
   the caller can take additional action based on the pointer type.  */

static pointer_info *
mio_pointer_ref (void *gp)
{
  pointer_info *p;

  if (iomode == IO_OUTPUT)
    {
      p = get_pointer (*((char **) gp));
      write_atom (ATOM_INTEGER, &p->integer);
    }
  else
    {
      require_atom (ATOM_INTEGER);
      p = add_fixup (atom_int, gp);
    }

  return p;
}


/* Save and load references to components that occur within
   expressions.  We have to describe these references by a number and
   by name.  The number is necessary for forward references during
   reading, and the name is necessary if the symbol already exists in
   the namespace and is not loaded again.  */

static void
mio_component_ref (gfc_component ** cp, gfc_symbol * sym)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_component *q;
  pointer_info *p;

  p = mio_pointer_ref (cp);
  if (p->type == P_UNKNOWN)
    p->type = P_COMPONENT;

  if (iomode == IO_OUTPUT)
    mio_pool_string (&(*cp)->name);
  else
    {
      mio_internal_string (name);

      /* It can happen that a component reference can be read before the
	 associated derived type symbol has been loaded. Return now and
	 wait for a later iteration of load_needed.  */
      if (sym == NULL)
	return;

      if (sym->components != NULL && p->u.pointer == NULL)
	{
	  /* Symbol already loaded, so search by name.  */
	  for (q = sym->components; q; q = q->next)
	    if (strcmp (q->name, name) == 0)
	      break;

	  if (q == NULL)
	    gfc_internal_error ("mio_component_ref(): Component not found");

	  associate_integer_pointer (p, q);
	}

      /* Make sure this symbol will eventually be loaded.  */
      p = find_pointer2 (sym);
      if (p->u.rsym.state == UNUSED)
	p->u.rsym.state = NEEDED;
    }
}


static void
mio_component (gfc_component * c)
{
  pointer_info *p;
  int n;

  mio_lparen ();

  if (iomode == IO_OUTPUT)
    {
      p = get_pointer (c);
      mio_integer (&p->integer);
    }
  else
    {
      mio_integer (&n);
      p = get_integer (n);
      associate_integer_pointer (p, c);
    }

  if (p->type == P_UNKNOWN)
    p->type = P_COMPONENT;

  mio_pool_string (&c->name);
  mio_typespec (&c->ts);
  mio_array_spec (&c->as);

  mio_integer (&c->dimension);
  mio_integer (&c->pointer);

  mio_expr (&c->initializer);
  mio_rparen ();
}


static void
mio_component_list (gfc_component ** cp)
{
  gfc_component *c, *tail;

  mio_lparen ();

  if (iomode == IO_OUTPUT)
    {
      for (c = *cp; c; c = c->next)
	mio_component (c);
    }
  else
    {

      *cp = NULL;
      tail = NULL;

      for (;;)
	{
	  if (peek_atom () == ATOM_RPAREN)
	    break;

	  c = gfc_get_component ();
	  mio_component (c);

	  if (tail == NULL)
	    *cp = c;
	  else
	    tail->next = c;

	  tail = c;
	}
    }

  mio_rparen ();
}


static void
mio_actual_arg (gfc_actual_arglist * a)
{

  mio_lparen ();
  mio_pool_string (&a->name);
  mio_expr (&a->expr);
  mio_rparen ();
}


static void
mio_actual_arglist (gfc_actual_arglist ** ap)
{
  gfc_actual_arglist *a, *tail;

  mio_lparen ();

  if (iomode == IO_OUTPUT)
    {
      for (a = *ap; a; a = a->next)
	mio_actual_arg (a);

    }
  else
    {
      tail = NULL;

      for (;;)
	{
	  if (peek_atom () != ATOM_LPAREN)
	    break;

	  a = gfc_get_actual_arglist ();

	  if (tail == NULL)
	    *ap = a;
	  else
	    tail->next = a;

	  tail = a;
	  mio_actual_arg (a);
	}
    }

  mio_rparen ();
}


/* Read and write formal argument lists.  */

static void
mio_formal_arglist (gfc_symbol * sym)
{
  gfc_formal_arglist *f, *tail;

  mio_lparen ();

  if (iomode == IO_OUTPUT)
    {
      for (f = sym->formal; f; f = f->next)
	mio_symbol_ref (&f->sym);

    }
  else
    {
      sym->formal = tail = NULL;

      while (peek_atom () != ATOM_RPAREN)
	{
	  f = gfc_get_formal_arglist ();
	  mio_symbol_ref (&f->sym);

	  if (sym->formal == NULL)
	    sym->formal = f;
	  else
	    tail->next = f;

	  tail = f;
	}
    }

  mio_rparen ();
}


/* Save or restore a reference to a symbol node.  */

void
mio_symbol_ref (gfc_symbol ** symp)
{
  pointer_info *p;

  p = mio_pointer_ref (symp);
  if (p->type == P_UNKNOWN)
    p->type = P_SYMBOL;

  if (iomode == IO_OUTPUT)
    {
      if (p->u.wsym.state == UNREFERENCED)
	p->u.wsym.state = NEEDS_WRITE;
    }
  else
    {
      if (p->u.rsym.state == UNUSED)
	p->u.rsym.state = NEEDED;
    }
}


/* Save or restore a reference to a symtree node.  */

static void
mio_symtree_ref (gfc_symtree ** stp)
{
  pointer_info *p;
  fixup_t *f;
  gfc_symtree * ns_st = NULL;

  if (iomode == IO_OUTPUT)
    {
      /* If this is a symtree for a symbol that came from a contained module
	 namespace, it has a unique name and we should look in the current
	 namespace to see if the required, non-contained symbol is available
	 yet. If so, the latter should be written.  */
      if ((*stp)->n.sym && check_unique_name((*stp)->name))
	ns_st = gfc_find_symtree (gfc_current_ns->sym_root, (*stp)->n.sym->name);

      mio_symbol_ref (ns_st ? &ns_st->n.sym : &(*stp)->n.sym);
    }
  else
    {
      require_atom (ATOM_INTEGER);
      p = get_integer (atom_int);
      if (p->type == P_UNKNOWN)
        p->type = P_SYMBOL;

      if (p->u.rsym.state == UNUSED)
	p->u.rsym.state = NEEDED;

      if (p->u.rsym.symtree != NULL)
        {
          *stp = p->u.rsym.symtree;
        }
      else
        {
          f = gfc_getmem (sizeof (fixup_t));

          f->next = p->u.rsym.stfixup;
          p->u.rsym.stfixup = f;

          f->pointer = (void **)stp;
        }
    }
}

static void
mio_iterator (gfc_iterator ** ip)
{
  gfc_iterator *iter;

  mio_lparen ();

  if (iomode == IO_OUTPUT)
    {
      if (*ip == NULL)
	goto done;
    }
  else
    {
      if (peek_atom () == ATOM_RPAREN)
	{
	  *ip = NULL;
	  goto done;
	}

      *ip = gfc_get_iterator ();
    }

  iter = *ip;

  mio_expr (&iter->var);
  mio_expr (&iter->start);
  mio_expr (&iter->end);
  mio_expr (&iter->step);

done:
  mio_rparen ();
}



static void
mio_constructor (gfc_constructor ** cp)
{
  gfc_constructor *c, *tail;

  mio_lparen ();

  if (iomode == IO_OUTPUT)
    {
      for (c = *cp; c; c = c->next)
	{
	  mio_lparen ();
	  mio_expr (&c->expr);
	  mio_iterator (&c->iterator);
	  mio_rparen ();
	}
    }
  else
    {

      *cp = NULL;
      tail = NULL;

      while (peek_atom () != ATOM_RPAREN)
	{
	  c = gfc_get_constructor ();

	  if (tail == NULL)
	    *cp = c;
	  else
	    tail->next = c;

	  tail = c;

	  mio_lparen ();
	  mio_expr (&c->expr);
	  mio_iterator (&c->iterator);
	  mio_rparen ();
	}
    }

  mio_rparen ();
}



static const mstring ref_types[] = {
    minit ("ARRAY", REF_ARRAY),
    minit ("COMPONENT", REF_COMPONENT),
    minit ("SUBSTRING", REF_SUBSTRING),
    minit (NULL, -1)
};


static void
mio_ref (gfc_ref ** rp)
{
  gfc_ref *r;

  mio_lparen ();

  r = *rp;
  r->type = MIO_NAME(ref_type) (r->type, ref_types);

  switch (r->type)
    {
    case REF_ARRAY:
      mio_array_ref (&r->u.ar);
      break;

    case REF_COMPONENT:
      mio_symbol_ref (&r->u.c.sym);
      mio_component_ref (&r->u.c.component, r->u.c.sym);
      break;

    case REF_SUBSTRING:
      mio_expr (&r->u.ss.start);
      mio_expr (&r->u.ss.end);
      mio_charlen (&r->u.ss.length);
      break;
    }

  mio_rparen ();
}


static void
mio_ref_list (gfc_ref ** rp)
{
  gfc_ref *ref, *head, *tail;

  mio_lparen ();

  if (iomode == IO_OUTPUT)
    {
      for (ref = *rp; ref; ref = ref->next)
	mio_ref (&ref);
    }
  else
    {
      head = tail = NULL;

      while (peek_atom () != ATOM_RPAREN)
	{
	  if (head == NULL)
	    head = tail = gfc_get_ref ();
	  else
	    {
	      tail->next = gfc_get_ref ();
	      tail = tail->next;
	    }

	  mio_ref (&tail);
	}

      *rp = head;
    }

  mio_rparen ();
}


/* Read and write an integer value.  */

static void
mio_gmp_integer (mpz_t * integer)
{
  char *p;

  if (iomode == IO_INPUT)
    {
      if (parse_atom () != ATOM_STRING)
	bad_module ("Expected integer string");

      mpz_init (*integer);
      if (mpz_set_str (*integer, atom_string, 10))
	bad_module ("Error converting integer");

      gfc_free (atom_string);

    }
  else
    {
      p = mpz_get_str (NULL, 10, *integer);
      write_atom (ATOM_STRING, p);
      gfc_free (p);
    }
}


static void
mio_gmp_real (mpfr_t * real)
{
  mp_exp_t exponent;
  char *p;

  if (iomode == IO_INPUT)
    {
      if (parse_atom () != ATOM_STRING)
	bad_module ("Expected real string");

      mpfr_init (*real);
      mpfr_set_str (*real, atom_string, 16, GFC_RND_MODE);
      gfc_free (atom_string);

    }
  else
    {
      p = mpfr_get_str (NULL, &exponent, 16, 0, *real, GFC_RND_MODE);
      atom_string = gfc_getmem (strlen (p) + 20);

      sprintf (atom_string, "0.%s@%ld", p, exponent);

      /* Fix negative numbers.  */
      if (atom_string[2] == '-')
	{
	  atom_string[0] = '-';
	  atom_string[1] = '0';
	  atom_string[2] = '.';
	}

      write_atom (ATOM_STRING, atom_string);

      gfc_free (atom_string);
      gfc_free (p);
    }
}


/* Save and restore the shape of an array constructor.  */

static void
mio_shape (mpz_t ** pshape, int rank)
{
  mpz_t *shape;
  atom_type t;
  int n;

  /* A NULL shape is represented by ().  */
  mio_lparen ();

  if (iomode == IO_OUTPUT)
    {
      shape = *pshape;
      if (!shape)
	{
	  mio_rparen ();
	  return;
	}
    }
  else
    {
      t = peek_atom ();
      if (t == ATOM_RPAREN)
	{
	  *pshape = NULL;
	  mio_rparen ();
	  return;
	}

      shape = gfc_get_shape (rank);
      *pshape = shape;
    }

  for (n = 0; n < rank; n++)
    mio_gmp_integer (&shape[n]);

  mio_rparen ();
}


static const mstring expr_types[] = {
    minit ("OP", EXPR_OP),
    minit ("FUNCTION", EXPR_FUNCTION),
    minit ("CONSTANT", EXPR_CONSTANT),
    minit ("VARIABLE", EXPR_VARIABLE),
    minit ("SUBSTRING", EXPR_SUBSTRING),
    minit ("STRUCTURE", EXPR_STRUCTURE),
    minit ("ARRAY", EXPR_ARRAY),
    minit ("NULL", EXPR_NULL),
    minit (NULL, -1)
};

/* INTRINSIC_ASSIGN is missing because it is used as an index for
   generic operators, not in expressions.  INTRINSIC_USER is also
   replaced by the correct function name by the time we see it.  */

static const mstring intrinsics[] =
{
    minit ("UPLUS", INTRINSIC_UPLUS),
    minit ("UMINUS", INTRINSIC_UMINUS),
    minit ("PLUS", INTRINSIC_PLUS),
    minit ("MINUS", INTRINSIC_MINUS),
    minit ("TIMES", INTRINSIC_TIMES),
    minit ("DIVIDE", INTRINSIC_DIVIDE),
    minit ("POWER", INTRINSIC_POWER),
    minit ("CONCAT", INTRINSIC_CONCAT),
    minit ("AND", INTRINSIC_AND),
    minit ("OR", INTRINSIC_OR),
    minit ("EQV", INTRINSIC_EQV),
    minit ("NEQV", INTRINSIC_NEQV),
    minit ("EQ", INTRINSIC_EQ),
    minit ("NE", INTRINSIC_NE),
    minit ("GT", INTRINSIC_GT),
    minit ("GE", INTRINSIC_GE),
    minit ("LT", INTRINSIC_LT),
    minit ("LE", INTRINSIC_LE),
    minit ("NOT", INTRINSIC_NOT),
    minit (NULL, -1)
};

/* Read and write expressions.  The form "()" is allowed to indicate a
   NULL expression.  */

static void
mio_expr (gfc_expr ** ep)
{
  gfc_expr *e;
  atom_type t;
  int flag;

  mio_lparen ();

  if (iomode == IO_OUTPUT)
    {
      if (*ep == NULL)
	{
	  mio_rparen ();
	  return;
	}

      e = *ep;
      MIO_NAME(expr_t) (e->expr_type, expr_types);

    }
  else
    {
      t = parse_atom ();
      if (t == ATOM_RPAREN)
	{
	  *ep = NULL;
	  return;
	}

      if (t != ATOM_NAME)
	bad_module ("Expected expression type");

      e = *ep = gfc_get_expr ();
      e->where = gfc_current_locus;
      e->expr_type = (expr_t) find_enum (expr_types);
    }

  mio_typespec (&e->ts);
  mio_integer (&e->rank);

  switch (e->expr_type)
    {
    case EXPR_OP:
      e->value.op.operator
	= MIO_NAME(gfc_intrinsic_op) (e->value.op.operator, intrinsics);

      switch (e->value.op.operator)
	{
	case INTRINSIC_UPLUS:
	case INTRINSIC_UMINUS:
	case INTRINSIC_NOT:
	  mio_expr (&e->value.op.op1);
	  break;

	case INTRINSIC_PLUS:
	case INTRINSIC_MINUS:
	case INTRINSIC_TIMES:
	case INTRINSIC_DIVIDE:
	case INTRINSIC_POWER:
	case INTRINSIC_CONCAT:
	case INTRINSIC_AND:
	case INTRINSIC_OR:
	case INTRINSIC_EQV:
	case INTRINSIC_NEQV:
	case INTRINSIC_EQ:
	case INTRINSIC_NE:
	case INTRINSIC_GT:
	case INTRINSIC_GE:
	case INTRINSIC_LT:
	case INTRINSIC_LE:
	  mio_expr (&e->value.op.op1);
	  mio_expr (&e->value.op.op2);
	  break;

	default:
	  bad_module ("Bad operator");
	}

      break;

    case EXPR_FUNCTION:
      mio_symtree_ref (&e->symtree);
      mio_actual_arglist (&e->value.function.actual);

      if (iomode == IO_OUTPUT)
	{
	  e->value.function.name
	    = mio_allocated_string (e->value.function.name);
	  flag = e->value.function.esym != NULL;
	  mio_integer (&flag);
	  if (flag)
	    mio_symbol_ref (&e->value.function.esym);
	  else
	    write_atom (ATOM_STRING, e->value.function.isym->name);

	}
      else
	{
	  require_atom (ATOM_STRING);
	  e->value.function.name = gfc_get_string (atom_string);
	  gfc_free (atom_string);

	  mio_integer (&flag);
	  if (flag)
	    mio_symbol_ref (&e->value.function.esym);
	  else
	    {
	      require_atom (ATOM_STRING);
	      e->value.function.isym = gfc_find_function (atom_string);
	      gfc_free (atom_string);
	    }
	}

      break;

    case EXPR_VARIABLE:
      mio_symtree_ref (&e->symtree);
      mio_ref_list (&e->ref);
      break;

    case EXPR_SUBSTRING:
      e->value.character.string = (char *)
	mio_allocated_string (e->value.character.string);
      mio_ref_list (&e->ref);
      break;

    case EXPR_STRUCTURE:
    case EXPR_ARRAY:
      mio_constructor (&e->value.constructor);
      mio_shape (&e->shape, e->rank);
      break;

    case EXPR_CONSTANT:
      switch (e->ts.type)
	{
	case BT_INTEGER:
	  mio_gmp_integer (&e->value.integer);
	  break;

	case BT_REAL:
          gfc_set_model_kind (e->ts.kind);
	  mio_gmp_real (&e->value.real);
	  break;

	case BT_COMPLEX:
          gfc_set_model_kind (e->ts.kind);
	  mio_gmp_real (&e->value.complex.r);
	  mio_gmp_real (&e->value.complex.i);
	  break;

	case BT_LOGICAL:
	  mio_integer (&e->value.logical);
	  break;

	case BT_CHARACTER:
	  mio_integer (&e->value.character.length);
	  e->value.character.string = (char *)
	    mio_allocated_string (e->value.character.string);
	  break;

	default:
	  bad_module ("Bad type in constant expression");
	}

      break;

    case EXPR_NULL:
      break;
    }

  mio_rparen ();
}


/* Read and write namelists */

static void
mio_namelist (gfc_symbol * sym)
{
  gfc_namelist *n, *m;
  const char *check_name;

  mio_lparen ();

  if (iomode == IO_OUTPUT)
    {
      for (n = sym->namelist; n; n = n->next)
	mio_symbol_ref (&n->sym);
    }
  else
    {
      /* This departure from the standard is flagged as an error.
	 It does, in fact, work correctly. TODO: Allow it
	 conditionally?  */
      if (sym->attr.flavor == FL_NAMELIST)
	{
	  check_name = find_use_name (sym->name);
	  if (check_name && strcmp (check_name, sym->name) != 0)
	    gfc_error("Namelist %s cannot be renamed by USE"
		      " association to %s.",
		      sym->name, check_name);
	}

      m = NULL;
      while (peek_atom () != ATOM_RPAREN)
	{
	  n = gfc_get_namelist ();
	  mio_symbol_ref (&n->sym);

	  if (sym->namelist == NULL)
	    sym->namelist = n;
	  else
	    m->next = n;

	  m = n;
	}
      sym->namelist_tail = m;
    }

  mio_rparen ();
}


/* Save/restore lists of gfc_interface stuctures.  When loading an
   interface, we are really appending to the existing list of
   interfaces.  Checking for duplicate and ambiguous interfaces has to
   be done later when all symbols have been loaded.  */

static void
mio_interface_rest (gfc_interface ** ip)
{
  gfc_interface *tail, *p;

  if (iomode == IO_OUTPUT)
    {
      if (ip != NULL)
	for (p = *ip; p; p = p->next)
	  mio_symbol_ref (&p->sym);
    }
  else
    {

      if (*ip == NULL)
	tail = NULL;
      else
	{
	  tail = *ip;
	  while (tail->next)
	    tail = tail->next;
	}

      for (;;)
	{
	  if (peek_atom () == ATOM_RPAREN)
	    break;

	  p = gfc_get_interface ();
	  p->where = gfc_current_locus;
	  mio_symbol_ref (&p->sym);

	  if (tail == NULL)
	    *ip = p;
	  else
	    tail->next = p;

	  tail = p;
	}
    }

  mio_rparen ();
}


/* Save/restore a nameless operator interface.  */

static void
mio_interface (gfc_interface ** ip)
{

  mio_lparen ();
  mio_interface_rest (ip);
}


/* Save/restore a named operator interface.  */

static void
mio_symbol_interface (const char **name, const char **module,
		      gfc_interface ** ip)
{

  mio_lparen ();

  mio_pool_string (name);
  mio_pool_string (module);

  mio_interface_rest (ip);
}


static void
mio_namespace_ref (gfc_namespace ** nsp)
{
  gfc_namespace *ns;
  pointer_info *p;

  p = mio_pointer_ref (nsp);

  if (p->type == P_UNKNOWN)
    p->type = P_NAMESPACE;

  if (iomode == IO_INPUT && p->integer != 0)
    {
      ns = (gfc_namespace *)p->u.pointer;
      if (ns == NULL)
	{
	  ns = gfc_get_namespace (NULL, 0);
	  associate_integer_pointer (p, ns);
	}
      else
	ns->refs++;
    }
}


/* Unlike most other routines, the address of the symbol node is
   already fixed on input and the name/module has already been filled
   in.  */

static void
mio_symbol (gfc_symbol * sym)
{
  gfc_formal_arglist *formal;

  mio_lparen ();

  mio_symbol_attribute (&sym->attr);
  mio_typespec (&sym->ts);

  /* Contained procedures don't have formal namespaces.  Instead we output the
     procedure namespace.  The will contain the formal arguments.  */
  if (iomode == IO_OUTPUT)
    {
      formal = sym->formal;
      while (formal && !formal->sym)
	formal = formal->next;

      if (formal)
	mio_namespace_ref (&formal->sym->ns);
      else
	mio_namespace_ref (&sym->formal_ns);
    }
  else
    {
      mio_namespace_ref (&sym->formal_ns);
      if (sym->formal_ns)
	{
	  sym->formal_ns->proc_name = sym;
	  sym->refs++;
	}
    }

  /* Save/restore common block links */
  mio_symbol_ref (&sym->common_next);

  mio_formal_arglist (sym);

  if (sym->attr.flavor == FL_PARAMETER)
    mio_expr (&sym->value);

  mio_array_spec (&sym->as);

  mio_symbol_ref (&sym->result);

  if (sym->attr.cray_pointee)
    mio_symbol_ref (&sym->cp_pointer);

  /* Note that components are always saved, even if they are supposed
     to be private.  Component access is checked during searching.  */

  mio_component_list (&sym->components);

  if (sym->components != NULL)
    sym->component_access =
      MIO_NAME(gfc_access) (sym->component_access, access_types);

  mio_namelist (sym);
  mio_rparen ();
}


/************************* Top level subroutines *************************/

/* Skip a list between balanced left and right parens.  */

static void
skip_list (void)
{
  int level;

  level = 0;
  do
    {
      switch (parse_atom ())
	{
	case ATOM_LPAREN:
	  level++;
	  break;

	case ATOM_RPAREN:
	  level--;
	  break;

	case ATOM_STRING:
	  gfc_free (atom_string);
	  break;

	case ATOM_NAME:
	case ATOM_INTEGER:
	  break;
	}
    }
  while (level > 0);
}


/* Load operator interfaces from the module.  Interfaces are unusual
   in that they attach themselves to existing symbols.  */

static void
load_operator_interfaces (void)
{
  const char *p;
  char name[GFC_MAX_SYMBOL_LEN + 1], module[GFC_MAX_SYMBOL_LEN + 1];
  gfc_user_op *uop;

  mio_lparen ();

  while (peek_atom () != ATOM_RPAREN)
    {
      mio_lparen ();

      mio_internal_string (name);
      mio_internal_string (module);

      /* Decide if we need to load this one or not.  */
      p = find_use_name (name);
      if (p == NULL)
	{
	  while (parse_atom () != ATOM_RPAREN);
	}
      else
	{
	  uop = gfc_get_uop (p);
	  mio_interface_rest (&uop->operator);
	}
    }

  mio_rparen ();
}


/* Load interfaces from the module.  Interfaces are unusual in that
   they attach themselves to existing symbols.  */

static void
load_generic_interfaces (void)
{
  const char *p;
  char name[GFC_MAX_SYMBOL_LEN + 1], module[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symbol *sym;

  mio_lparen ();

  while (peek_atom () != ATOM_RPAREN)
    {
      mio_lparen ();

      mio_internal_string (name);
      mio_internal_string (module);

      /* Decide if we need to load this one or not.  */
      p = find_use_name (name);

      if (p == NULL || gfc_find_symbol (p, NULL, 0, &sym))
	{
	  while (parse_atom () != ATOM_RPAREN);
	  continue;
	}

      if (sym == NULL)
	{
	  gfc_get_symbol (p, NULL, &sym);

	  sym->attr.flavor = FL_PROCEDURE;
	  sym->attr.generic = 1;
	  sym->attr.use_assoc = 1;
	}

      mio_interface_rest (&sym->generic);
    }

  mio_rparen ();
}


/* Load common blocks.  */

static void
load_commons(void)
{
  char name[GFC_MAX_SYMBOL_LEN+1];
  gfc_common_head *p;

  mio_lparen ();

  while (peek_atom () != ATOM_RPAREN)
    {
      mio_lparen ();
      mio_internal_string (name);

      p = gfc_get_common (name, 1);

      mio_symbol_ref (&p->head);
      mio_integer (&p->saved);
      p->use_assoc = 1;

      mio_rparen();
    }

  mio_rparen();
}

/* load_equiv()-- Load equivalences. */

static void
load_equiv(void)
{
  gfc_equiv *head, *tail, *end;

  mio_lparen();

  end = gfc_current_ns->equiv;
  while(end != NULL && end->next != NULL)
    end = end->next;

  while(peek_atom() != ATOM_RPAREN) {
    mio_lparen();
    head = tail = NULL;

    while(peek_atom() != ATOM_RPAREN)
      {
	if (head == NULL)
	  head = tail = gfc_get_equiv();
	else
	  {
	    tail->eq = gfc_get_equiv();
	    tail = tail->eq;
	  }

	mio_pool_string(&tail->module);
	mio_expr(&tail->expr);
      }

    if (end == NULL)
      gfc_current_ns->equiv = head;
    else
      end->next = head;

    end = head;
    mio_rparen();
  }

  mio_rparen();
}

/* Recursive function to traverse the pointer_info tree and load a
   needed symbol.  We return nonzero if we load a symbol and stop the
   traversal, because the act of loading can alter the tree.  */

static int
load_needed (pointer_info * p)
{
  gfc_namespace *ns;
  pointer_info *q;
  gfc_symbol *sym;

  if (p == NULL)
    return 0;
  if (load_needed (p->left))
    return 1;
  if (load_needed (p->right))
    return 1;

  if (p->type != P_SYMBOL || p->u.rsym.state != NEEDED)
    return 0;

  p->u.rsym.state = USED;

  set_module_locus (&p->u.rsym.where);

  sym = p->u.rsym.sym;
  if (sym == NULL)
    {
      q = get_integer (p->u.rsym.ns);

      ns = (gfc_namespace *) q->u.pointer;
      if (ns == NULL)
	{
	  /* Create an interface namespace if necessary.  These are
	     the namespaces that hold the formal parameters of module
	     procedures.  */

	  ns = gfc_get_namespace (NULL, 0);
	  associate_integer_pointer (q, ns);
	}

      sym = gfc_new_symbol (p->u.rsym.true_name, ns);
      sym->module = gfc_get_string (p->u.rsym.module);

      associate_integer_pointer (p, sym);
    }

  mio_symbol (sym);
  sym->attr.use_assoc = 1;

  return 1;
}


/* Recursive function for cleaning up things after a module has been
   read.  */

static void
read_cleanup (pointer_info * p)
{
  gfc_symtree *st;
  pointer_info *q;

  if (p == NULL)
    return;

  read_cleanup (p->left);
  read_cleanup (p->right);

  if (p->type == P_SYMBOL && p->u.rsym.state == USED && !p->u.rsym.referenced)
    {
      /* Add hidden symbols to the symtree.  */
      q = get_integer (p->u.rsym.ns);
      st = get_unique_symtree ((gfc_namespace *) q->u.pointer);

      st->n.sym = p->u.rsym.sym;
      st->n.sym->refs++;

      /* Fixup any symtree references.  */
      p->u.rsym.symtree = st;
      resolve_fixups (p->u.rsym.stfixup, st);
      p->u.rsym.stfixup = NULL;
    }

  /* Free unused symbols.  */
  if (p->type == P_SYMBOL && p->u.rsym.state == UNUSED)
    gfc_free_symbol (p->u.rsym.sym);
}


/* Read a module file.  */

static void
read_module (void)
{
  module_locus operator_interfaces, user_operators;
  const char *p;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_intrinsic_op i;
  int ambiguous, j, nuse, symbol;
  pointer_info *info;
  gfc_use_rename *u;
  gfc_symtree *st;
  gfc_symbol *sym;

  get_module_locus (&operator_interfaces);	/* Skip these for now */
  skip_list ();

  get_module_locus (&user_operators);
  skip_list ();
  skip_list ();

  /* Skip commons and equivalences for now.  */
  skip_list ();
  skip_list ();

  mio_lparen ();

  /* Create the fixup nodes for all the symbols.  */

  while (peek_atom () != ATOM_RPAREN)
    {
      require_atom (ATOM_INTEGER);
      info = get_integer (atom_int);

      info->type = P_SYMBOL;
      info->u.rsym.state = UNUSED;

      mio_internal_string (info->u.rsym.true_name);
      mio_internal_string (info->u.rsym.module);

      require_atom (ATOM_INTEGER);
      info->u.rsym.ns = atom_int;

      get_module_locus (&info->u.rsym.where);
      skip_list ();

      /* See if the symbol has already been loaded by a previous module.
         If so, we reference the existing symbol and prevent it from
         being loaded again.  */

      sym = find_true_name (info->u.rsym.true_name, info->u.rsym.module);

        /* See if the symbol has already been loaded by a previous module.
	 If so, we reference the existing symbol and prevent it from
	 being loaded again.  This should not happen if the symbol being
	 read is an index for an assumed shape dummy array (ns != 1).  */

      sym = find_true_name (info->u.rsym.true_name, info->u.rsym.module);

      if (sym == NULL
	   || (sym->attr.flavor == FL_VARIABLE
	       && info->u.rsym.ns !=1))
	continue;

      info->u.rsym.state = USED;
      info->u.rsym.referenced = 1;
      info->u.rsym.sym = sym;
    }

  mio_rparen ();

  /* Parse the symtree lists.  This lets us mark which symbols need to
     be loaded.  Renaming is also done at this point by replacing the
     symtree name.  */

  mio_lparen ();

  while (peek_atom () != ATOM_RPAREN)
    {
      mio_internal_string (name);
      mio_integer (&ambiguous);
      mio_integer (&symbol);

      info = get_integer (symbol);

      /* See how many use names there are.  If none, go through the start
	 of the loop at least once.  */
      nuse = number_use_names (name);
      if (nuse == 0)
	nuse = 1;

      for (j = 1; j <= nuse; j++)
	{
	  /* Get the jth local name for this symbol.  */
	  p = find_use_name_n (name, &j);

	  /* Skip symtree nodes not in an ONLY clause.  */
	  if (p == NULL)
	    continue;

	  /* Check for ambiguous symbols.  */
	  st = gfc_find_symtree (gfc_current_ns->sym_root, p);

	  if (st != NULL)
	    {
	      if (st->n.sym != info->u.rsym.sym)
		st->ambiguous = 1;
	      info->u.rsym.symtree = st;
	    }
	  else
	    {
	      /* Create a symtree node in the current namespace for this symbol.  */
	      st = check_unique_name (p) ? get_unique_symtree (gfc_current_ns) :
	      gfc_new_symtree (&gfc_current_ns->sym_root, p);

	      st->ambiguous = ambiguous;

	      sym = info->u.rsym.sym;

	      /* Create a symbol node if it doesn't already exist.  */
	      if (sym == NULL)
		{
		  sym = info->u.rsym.sym =
		      gfc_new_symbol (info->u.rsym.true_name,
				      gfc_current_ns);

		  sym->module = gfc_get_string (info->u.rsym.module);
		}

	      st->n.sym = sym;
	      st->n.sym->refs++;

	      /* Store the symtree pointing to this symbol.  */
	      info->u.rsym.symtree = st;

	      if (info->u.rsym.state == UNUSED)
	        info->u.rsym.state = NEEDED;
	      info->u.rsym.referenced = 1;
	    }
	}
    }

  mio_rparen ();

  /* Load intrinsic operator interfaces.  */
  set_module_locus (&operator_interfaces);
  mio_lparen ();

  for (i = GFC_INTRINSIC_BEGIN; i != GFC_INTRINSIC_END; i++)
    {
      if (i == INTRINSIC_USER)
	continue;

      if (only_flag)
	{
	  u = find_use_operator (i);

	  if (u == NULL)
	    {
	      skip_list ();
	      continue;
	    }

	  u->found = 1;
	}

      mio_interface (&gfc_current_ns->operator[i]);
    }

  mio_rparen ();

  /* Load generic and user operator interfaces.  These must follow the
     loading of symtree because otherwise symbols can be marked as
     ambiguous.  */

  set_module_locus (&user_operators);

  load_operator_interfaces ();
  load_generic_interfaces ();

  load_commons ();
  load_equiv();

  /* At this point, we read those symbols that are needed but haven't
     been loaded yet.  If one symbol requires another, the other gets
     marked as NEEDED if its previous state was UNUSED.  */

  while (load_needed (pi_root));

  /* Make sure all elements of the rename-list were found in the
     module.  */

  for (u = gfc_rename_list; u; u = u->next)
    {
      if (u->found)
	continue;

      if (u->operator == INTRINSIC_NONE)
	{
	  gfc_error ("Symbol '%s' referenced at %L not found in module '%s'",
		     u->use_name, &u->where, module_name);
	  continue;
	}

      if (u->operator == INTRINSIC_USER)
	{
	  gfc_error
	    ("User operator '%s' referenced at %L not found in module '%s'",
	     u->use_name, &u->where, module_name);
	  continue;
	}

      gfc_error
	("Intrinsic operator '%s' referenced at %L not found in module "
	 "'%s'", gfc_op2string (u->operator), &u->where, module_name);
    }

  gfc_check_interfaces (gfc_current_ns);

  /* Clean up symbol nodes that were never loaded, create references
     to hidden symbols.  */

  read_cleanup (pi_root);
}


/* Given an access type that is specific to an entity and the default
   access, return nonzero if the entity is publicly accessible.  */

bool
gfc_check_access (gfc_access specific_access, gfc_access default_access)
{

  if (specific_access == ACCESS_PUBLIC)
    return TRUE;
  if (specific_access == ACCESS_PRIVATE)
    return FALSE;

  if (gfc_option.flag_module_access_private)
    return default_access == ACCESS_PUBLIC;
  else
    return default_access != ACCESS_PRIVATE;

  return FALSE;
}


/* Write a common block to the module */

static void
write_common (gfc_symtree *st)
{
  gfc_common_head *p;
  const char * name;

  if (st == NULL)
    return;

  write_common(st->left);
  write_common(st->right);

  mio_lparen();

  /* Write the unmangled name.  */
  name = st->n.common->name;

  mio_pool_string(&name);

  p = st->n.common;
  mio_symbol_ref(&p->head);
  mio_integer(&p->saved);

  mio_rparen();
}

/* Write the blank common block to the module */

static void
write_blank_common (void)
{
  const char * name = BLANK_COMMON_NAME;

  if (gfc_current_ns->blank_common.head == NULL)
    return;

  mio_lparen();

  mio_pool_string(&name);

  mio_symbol_ref(&gfc_current_ns->blank_common.head);
  mio_integer(&gfc_current_ns->blank_common.saved);

  mio_rparen();
}

/* Write equivalences to the module.  */

static void
write_equiv(void)
{
  gfc_equiv *eq, *e;
  int num;

  num = 0;
  for(eq=gfc_current_ns->equiv; eq; eq=eq->next)
    {
      mio_lparen();

      for(e=eq; e; e=e->eq)
	{
	  if (e->module == NULL)
	    e->module = gfc_get_string("%s.eq.%d", module_name, num);
	  mio_allocated_string(e->module);
	  mio_expr(&e->expr);
	}

      num++;
      mio_rparen();
    }
}

/* Write a symbol to the module.  */

static void
write_symbol (int n, gfc_symbol * sym)
{

  if (sym->attr.flavor == FL_UNKNOWN || sym->attr.flavor == FL_LABEL)
    gfc_internal_error ("write_symbol(): bad module symbol '%s'", sym->name);

  mio_integer (&n);
  mio_pool_string (&sym->name);

  mio_pool_string (&sym->module);
  mio_pointer_ref (&sym->ns);

  mio_symbol (sym);
  write_char ('\n');
}


/* Recursive traversal function to write the initial set of symbols to
   the module.  We check to see if the symbol should be written
   according to the access specification.  */

static void
write_symbol0 (gfc_symtree * st)
{
  gfc_symbol *sym;
  pointer_info *p;

  if (st == NULL)
    return;

  write_symbol0 (st->left);
  write_symbol0 (st->right);

  sym = st->n.sym;
  if (sym->module == NULL)
    sym->module = gfc_get_string (module_name);

  if (sym->attr.flavor == FL_PROCEDURE && sym->attr.generic
      && !sym->attr.subroutine && !sym->attr.function)
    return;

  if (!gfc_check_access (sym->attr.access, sym->ns->default_access))
    return;

  p = get_pointer (sym);
  if (p->type == P_UNKNOWN)
    p->type = P_SYMBOL;

  if (p->u.wsym.state == WRITTEN)
    return;

  write_symbol (p->integer, sym);
  p->u.wsym.state = WRITTEN;

  return;
}


/* Recursive traversal function to write the secondary set of symbols
   to the module file.  These are symbols that were not public yet are
   needed by the public symbols or another dependent symbol.  The act
   of writing a symbol can modify the pointer_info tree, so we cease
   traversal if we find a symbol to write.  We return nonzero if a
   symbol was written and pass that information upwards.  */

static int
write_symbol1 (pointer_info * p)
{

  if (p == NULL)
    return 0;

  if (write_symbol1 (p->left))
    return 1;
  if (write_symbol1 (p->right))
    return 1;

  if (p->type != P_SYMBOL || p->u.wsym.state != NEEDS_WRITE)
    return 0;

  p->u.wsym.state = WRITTEN;
  write_symbol (p->integer, p->u.wsym.sym);

  return 1;
}


/* Write operator interfaces associated with a symbol.  */

static void
write_operator (gfc_user_op * uop)
{
  static char nullstring[] = "";
  const char *p = nullstring;

  if (uop->operator == NULL
      || !gfc_check_access (uop->access, uop->ns->default_access))
    return;

  mio_symbol_interface (&uop->name, &p, &uop->operator);
}


/* Write generic interfaces associated with a symbol.  */

static void
write_generic (gfc_symbol * sym)
{

  if (sym->generic == NULL
      || !gfc_check_access (sym->attr.access, sym->ns->default_access))
    return;

  mio_symbol_interface (&sym->name, &sym->module, &sym->generic);
}


static void
write_symtree (gfc_symtree * st)
{
  gfc_symbol *sym;
  pointer_info *p;

  sym = st->n.sym;
  if (!gfc_check_access (sym->attr.access, sym->ns->default_access)
      || (sym->attr.flavor == FL_PROCEDURE && sym->attr.generic
	  && !sym->attr.subroutine && !sym->attr.function))
    return;

  if (check_unique_name (st->name))
    return;

  p = find_pointer (sym);
  if (p == NULL)
    gfc_internal_error ("write_symtree(): Symbol not written");

  mio_pool_string (&st->name);
  mio_integer (&st->ambiguous);
  mio_integer (&p->integer);
}


static void
write_module (void)
{
  gfc_intrinsic_op i;

  /* Write the operator interfaces.  */
  mio_lparen ();

  for (i = GFC_INTRINSIC_BEGIN; i != GFC_INTRINSIC_END; i++)
    {
      if (i == INTRINSIC_USER)
	continue;

      mio_interface (gfc_check_access (gfc_current_ns->operator_access[i],
				       gfc_current_ns->default_access)
		     ? &gfc_current_ns->operator[i] : NULL);
    }

  mio_rparen ();
  write_char ('\n');
  write_char ('\n');

  mio_lparen ();
  gfc_traverse_user_op (gfc_current_ns, write_operator);
  mio_rparen ();
  write_char ('\n');
  write_char ('\n');

  mio_lparen ();
  gfc_traverse_ns (gfc_current_ns, write_generic);
  mio_rparen ();
  write_char ('\n');
  write_char ('\n');

  mio_lparen ();
  write_blank_common ();
  write_common (gfc_current_ns->common_root);
  mio_rparen ();
  write_char ('\n');
  write_char ('\n');

  mio_lparen();
  write_equiv();
  mio_rparen();
  write_char('\n');  write_char('\n');

  /* Write symbol information.  First we traverse all symbols in the
     primary namespace, writing those that need to be written.
     Sometimes writing one symbol will cause another to need to be
     written.  A list of these symbols ends up on the write stack, and
     we end by popping the bottom of the stack and writing the symbol
     until the stack is empty.  */

  mio_lparen ();

  write_symbol0 (gfc_current_ns->sym_root);
  while (write_symbol1 (pi_root));

  mio_rparen ();

  write_char ('\n');
  write_char ('\n');

  mio_lparen ();
  gfc_traverse_symtree (gfc_current_ns->sym_root, write_symtree);
  mio_rparen ();
}


/* Given module, dump it to disk.  If there was an error while
   processing the module, dump_flag will be set to zero and we delete
   the module file, even if it was already there.  */

void
gfc_dump_module (const char *name, int dump_flag)
{
  int n;
  char *filename, *p;
  time_t now;

  n = strlen (name) + strlen (MODULE_EXTENSION) + 1;
  if (gfc_option.module_dir != NULL)
    {
      filename = (char *) alloca (n + strlen (gfc_option.module_dir));
      strcpy (filename, gfc_option.module_dir);
      strcat (filename, name);
    }
  else
    {
      filename = (char *) alloca (n);
      strcpy (filename, name);
    }
  strcat (filename, MODULE_EXTENSION);

  if (!dump_flag)
    {
      unlink (filename);
      return;
    }

  module_fp = fopen (filename, "w");
  if (module_fp == NULL)
    gfc_fatal_error ("Can't open module file '%s' for writing at %C: %s",
		     filename, strerror (errno));

  now = time (NULL);
  p = ctime (&now);

  *strchr (p, '\n') = '\0';

  fprintf (module_fp, "GFORTRAN module created from %s on %s\n", 
	   gfc_source_file, p);
  fputs ("If you edit this, you'll get what you deserve.\n\n", module_fp);

  iomode = IO_OUTPUT;
  strcpy (module_name, name);

  init_pi_tree ();

  write_module ();

  free_pi_tree (pi_root);
  pi_root = NULL;

  write_char ('\n');

  if (fclose (module_fp))
    gfc_fatal_error ("Error writing module file '%s' for writing: %s",
		     filename, strerror (errno));
}


/* Process a USE directive.  */

void
gfc_use_module (void)
{
  char *filename;
  gfc_state_data *p;
  int c, line;

  filename = (char *) alloca(strlen(module_name) + strlen(MODULE_EXTENSION)
			     + 1);
  strcpy (filename, module_name);
  strcat (filename, MODULE_EXTENSION);

  module_fp = gfc_open_included_file (filename);
  if (module_fp == NULL)
    gfc_fatal_error ("Can't open module file '%s' for reading at %C: %s",
		     filename, strerror (errno));

  iomode = IO_INPUT;
  module_line = 1;
  module_column = 1;

  /* Skip the first two lines of the module.  */
  /* FIXME: Could also check for valid two lines here, instead.  */
  line = 0;
  while (line < 2)
    {
      c = module_char ();
      if (c == EOF)
	bad_module ("Unexpected end of module");
      if (c == '\n')
	line++;
    }

  /* Make sure we're not reading the same module that we may be building.  */
  for (p = gfc_state_stack; p; p = p->previous)
    if (p->state == COMP_MODULE && strcmp (p->sym->name, module_name) == 0)
      gfc_fatal_error ("Can't USE the same module we're building!");

  init_pi_tree ();
  init_true_name_tree ();

  read_module ();

  free_true_name (true_name_root);
  true_name_root = NULL;

  free_pi_tree (pi_root);
  pi_root = NULL;

  fclose (module_fp);
}


void
gfc_module_init_2 (void)
{

  last_atom = ATOM_LPAREN;
}


void
gfc_module_done_2 (void)
{

  free_rename ();
}
