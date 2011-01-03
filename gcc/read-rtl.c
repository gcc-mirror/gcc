/* RTL reader for GCC.
   Copyright (C) 1987, 1988, 1991, 1994, 1997, 1998, 1999, 2000, 2001, 2002,
   2003, 2004, 2005, 2007, 2008, 2010
   Free Software Foundation, Inc.

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

#include "bconfig.h"

/* Disable rtl checking; it conflicts with the iterator handling.  */
#undef ENABLE_RTL_CHECKING

#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "obstack.h"
#include "hashtab.h"
#include "read-md.h"
#include "gensupport.h"

/* One element in a singly-linked list of (integer, string) pairs.  */
struct map_value {
  struct map_value *next;
  int number;
  const char *string;
};

/* Maps an iterator or attribute name to a list of (integer, string) pairs.
   The integers are mode or code values; the strings are either C conditions
   or attribute values.  */
struct mapping {
  /* The name of the iterator or attribute.  */
  const char *name;

  /* The group (modes or codes) to which the iterator or attribute belongs.  */
  struct iterator_group *group;

  /* Gives a unique number to the attribute or iterator.  Numbers are
     allocated consecutively, starting at 0.  */
  int index;

  /* The list of (integer, string) pairs.  */
  struct map_value *values;
};

/* A structure for abstracting the common parts of code and mode iterators.  */
struct iterator_group {
  /* Tables of "mapping" structures, one for attributes and one for iterators.  */
  htab_t attrs, iterators;

  /* The number of "real" modes or codes (and by extension, the first
     number available for use as an iterator placeholder).  */
  int num_builtins;

  /* Treat the given string as the name of a standard mode or code and
     return its integer value.  */
  int (*find_builtin) (const char *);

  /* Return true if the given rtx uses the given mode or code.  */
  bool (*uses_iterator_p) (rtx, int);

  /* Make the given rtx use the given mode or code.  */
  void (*apply_iterator) (rtx, int);
};

/* A structure used to pass data from read_rtx to apply_iterator_traverse
   via htab_traverse.  */
struct iterator_traverse_data {
  /* Instruction queue.  */
  rtx queue;
  /* Attributes seen for modes.  */
  struct map_value *mode_maps;
  /* The last unknown attribute used as a mode.  */
  const char *unknown_mode_attr;
};

/* If CODE is the number of a code iterator, return a real rtx code that
   has the same format.  Return CODE otherwise.  */
#define BELLWETHER_CODE(CODE) \
  ((CODE) < NUM_RTX_CODE ? CODE : bellwether_codes[CODE - NUM_RTX_CODE])

static int find_mode (const char *);
static bool uses_mode_iterator_p (rtx, int);
static void apply_mode_iterator (rtx, int);
static int find_code (const char *);
static bool uses_code_iterator_p (rtx, int);
static void apply_code_iterator (rtx, int);
static const char *apply_iterator_to_string (const char *, struct mapping *, int);
static rtx apply_iterator_to_rtx (rtx, struct mapping *, int,
				  struct map_value *, const char **);
static bool uses_iterator_p (rtx, struct mapping *);
static const char *add_condition_to_string (const char *, const char *);
static void add_condition_to_rtx (rtx, const char *);
static int apply_iterator_traverse (void **, void *);
static struct mapping *add_mapping (struct iterator_group *, htab_t t,
				    const char *);
static struct map_value **add_map_value (struct map_value **,
					 int, const char *);
static void initialize_iterators (void);
static void read_conditions (void);
static void validate_const_int (const char *);
static int find_iterator (struct iterator_group *, const char *);
static struct mapping *read_mapping (struct iterator_group *, htab_t);
static void check_code_iterator (struct mapping *);
static rtx read_rtx_code (const char *, struct map_value **);
static rtx read_nested_rtx (struct map_value **);
static rtx read_rtx_variadic (struct map_value **, rtx);

/* The mode and code iterator structures.  */
static struct iterator_group modes, codes;

/* Index I is the value of BELLWETHER_CODE (I + NUM_RTX_CODE).  */
static enum rtx_code *bellwether_codes;

/* Implementations of the iterator_group callbacks for modes.  */

static int
find_mode (const char *name)
{
  int i;

  for (i = 0; i < NUM_MACHINE_MODES; i++)
    if (strcmp (GET_MODE_NAME (i), name) == 0)
      return i;

  fatal_with_file_and_line ("unknown mode `%s'", name);
}

static bool
uses_mode_iterator_p (rtx x, int mode)
{
  return (int) GET_MODE (x) == mode;
}

static void
apply_mode_iterator (rtx x, int mode)
{
  PUT_MODE (x, (enum machine_mode) mode);
}

/* Implementations of the iterator_group callbacks for codes.  */

static int
find_code (const char *name)
{
  int i;

  for (i = 0; i < NUM_RTX_CODE; i++)
    if (strcmp (GET_RTX_NAME (i), name) == 0)
      return i;

  fatal_with_file_and_line ("unknown rtx code `%s'", name);
}

static bool
uses_code_iterator_p (rtx x, int code)
{
  return (int) GET_CODE (x) == code;
}

static void
apply_code_iterator (rtx x, int code)
{
  PUT_CODE (x, (enum rtx_code) code);
}

/* Map a code or mode attribute string P to the underlying string for
   ITERATOR and VALUE.  */

static struct map_value *
map_attr_string (const char *p, struct mapping *iterator, int value)
{
  const char *attr;
  struct mapping *m;
  struct map_value *v;

  /* If there's a "iterator:" prefix, check whether the iterator name matches.
     Set ATTR to the start of the attribute name.  */
  attr = strchr (p, ':');
  if (attr == 0)
    attr = p;
  else
    {
      if (strncmp (p, iterator->name, attr - p) != 0
	  || iterator->name[attr - p] != 0)
	return 0;
      attr++;
    }

  /* Find the attribute specification.  */
  m = (struct mapping *) htab_find (iterator->group->attrs, &attr);
  if (m == 0)
    return 0;

  /* Find the attribute value for VALUE.  */
  for (v = m->values; v != 0; v = v->next)
    if (v->number == value)
      break;

  return v;
}

/* Given an attribute string used as a machine mode, return an index
   to store in the machine mode to be translated by
   apply_iterator_to_rtx.  */

static unsigned int
mode_attr_index (struct map_value **mode_maps, const char *string)
{
  char *p;
  struct map_value *mv;

  /* Copy the attribute string into permanent storage, without the
     angle brackets around it.  */
  obstack_grow0 (&string_obstack, string + 1, strlen (string) - 2);
  p = XOBFINISH (&string_obstack, char *);

  mv = XNEW (struct map_value);
  mv->number = *mode_maps == 0 ? 0 : (*mode_maps)->number + 1;
  mv->string = p;
  mv->next = *mode_maps;
  *mode_maps = mv;

  /* We return a code which we can map back into this string: the
     number of machine modes + the number of mode iterators + the index
     we just used.  */
  return MAX_MACHINE_MODE + htab_elements (modes.iterators) + mv->number;
}

/* Apply MODE_MAPS to the top level of X, expanding cases where an
   attribute is used for a mode.  ITERATOR is the current iterator we are
   expanding, and VALUE is the value to which we are expanding it.
   This sets *UNKNOWN to true if we find a mode attribute which has not
   yet been defined, and does not change it otherwise.  */

static void
apply_mode_maps (rtx x, struct map_value *mode_maps, struct mapping *iterator,
		 int value, const char **unknown)
{
  unsigned int offset;
  int indx;
  struct map_value *pm;

  offset = MAX_MACHINE_MODE + htab_elements (modes.iterators);
  if (GET_MODE (x) < offset)
    return;

  indx = GET_MODE (x) - offset;
  for (pm = mode_maps; pm; pm = pm->next)
    {
      if (pm->number == indx)
	{
	  struct map_value *v;

	  v = map_attr_string (pm->string, iterator, value);
	  if (v)
	    PUT_MODE (x, (enum machine_mode) find_mode (v->string));
	  else
	    *unknown = pm->string;
	  return;
	}
    }
}

/* Given that ITERATOR is being expanded as VALUE, apply the appropriate
   string substitutions to STRING.  Return the new string if any changes
   were needed, otherwise return STRING itself.  */

static const char *
apply_iterator_to_string (const char *string, struct mapping *iterator, int value)
{
  char *base, *copy, *p, *start, *end;
  struct map_value *v;

  if (string == 0)
    return string;

  base = p = copy = ASTRDUP (string);
  while ((start = strchr (p, '<')) && (end = strchr (start, '>')))
    {
      p = start + 1;

      *end = 0;
      v = map_attr_string (p, iterator, value);
      *end = '>';
      if (v == 0)
	continue;

      /* Add everything between the last copied byte and the '<',
	 then add in the attribute value.  */
      obstack_grow (&string_obstack, base, start - base);
      obstack_grow (&string_obstack, v->string, strlen (v->string));
      base = end + 1;
    }
  if (base != copy)
    {
      obstack_grow (&string_obstack, base, strlen (base) + 1);
      copy = XOBFINISH (&string_obstack, char *);
      copy_md_ptr_loc (copy, string);
      return copy;
    }
  return string;
}

/* Return a copy of ORIGINAL in which all uses of ITERATOR have been
   replaced by VALUE.  MODE_MAPS holds information about attribute
   strings used for modes.  This sets *UNKNOWN_MODE_ATTR to the value of
   an unknown mode attribute, and does not change it otherwise.  */

static rtx
apply_iterator_to_rtx (rtx original, struct mapping *iterator, int value,
		       struct map_value *mode_maps,
		       const char **unknown_mode_attr)
{
  struct iterator_group *group;
  const char *format_ptr;
  int i, j;
  rtx x;
  enum rtx_code bellwether_code;

  if (original == 0)
    return original;

  /* Create a shallow copy of ORIGINAL.  */
  bellwether_code = BELLWETHER_CODE (GET_CODE (original));
  x = rtx_alloc (bellwether_code);
  memcpy (x, original, RTX_CODE_SIZE (bellwether_code));

  /* Change the mode or code itself.  */
  group = iterator->group;
  if (group->uses_iterator_p (x, iterator->index + group->num_builtins))
    group->apply_iterator (x, value);

  if (mode_maps)
    apply_mode_maps (x, mode_maps, iterator, value, unknown_mode_attr);

  /* Change each string and recursively change each rtx.  */
  format_ptr = GET_RTX_FORMAT (bellwether_code);
  for (i = 0; format_ptr[i] != 0; i++)
    switch (format_ptr[i])
      {
      case 'T':
	XTMPL (x, i) = apply_iterator_to_string (XTMPL (x, i), iterator, value);
	break;

      case 'S':
      case 's':
	XSTR (x, i) = apply_iterator_to_string (XSTR (x, i), iterator, value);
	break;

      case 'e':
	XEXP (x, i) = apply_iterator_to_rtx (XEXP (x, i), iterator, value,
					     mode_maps, unknown_mode_attr);
	break;

      case 'V':
      case 'E':
	if (XVEC (original, i))
	  {
	    XVEC (x, i) = rtvec_alloc (XVECLEN (original, i));
	    for (j = 0; j < XVECLEN (x, i); j++)
	      XVECEXP (x, i, j) = apply_iterator_to_rtx (XVECEXP (original, i, j),
							 iterator, value, mode_maps,
							 unknown_mode_attr);
	  }
	break;

      default:
	break;
      }
  return x;
}

/* Return true if X (or some subexpression of X) uses iterator ITERATOR.  */

static bool
uses_iterator_p (rtx x, struct mapping *iterator)
{
  struct iterator_group *group;
  const char *format_ptr;
  int i, j;

  if (x == 0)
    return false;

  group = iterator->group;
  if (group->uses_iterator_p (x, iterator->index + group->num_builtins))
    return true;

  format_ptr = GET_RTX_FORMAT (BELLWETHER_CODE (GET_CODE (x)));
  for (i = 0; format_ptr[i] != 0; i++)
    switch (format_ptr[i])
      {
      case 'e':
	if (uses_iterator_p (XEXP (x, i), iterator))
	  return true;
	break;

      case 'V':
      case 'E':
	if (XVEC (x, i))
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (uses_iterator_p (XVECEXP (x, i, j), iterator))
	      return true;
	break;

      default:
	break;
      }
  return false;
}

/* Return a condition that must satisfy both ORIGINAL and EXTRA.  If ORIGINAL
   has the form "&& ..." (as used in define_insn_and_splits), assume that
   EXTRA is already satisfied.  Empty strings are treated like "true".  */

static const char *
add_condition_to_string (const char *original, const char *extra)
{
  if (original != 0 && original[0] == '&' && original[1] == '&')
    return original;
  return join_c_conditions (original, extra);
}

/* Like add_condition, but applied to all conditions in rtx X.  */

static void
add_condition_to_rtx (rtx x, const char *extra)
{
  switch (GET_CODE (x))
    {
    case DEFINE_INSN:
    case DEFINE_EXPAND:
      XSTR (x, 2) = add_condition_to_string (XSTR (x, 2), extra);
      break;

    case DEFINE_SPLIT:
    case DEFINE_PEEPHOLE:
    case DEFINE_PEEPHOLE2:
    case DEFINE_COND_EXEC:
      XSTR (x, 1) = add_condition_to_string (XSTR (x, 1), extra);
      break;

    case DEFINE_INSN_AND_SPLIT:
      XSTR (x, 2) = add_condition_to_string (XSTR (x, 2), extra);
      XSTR (x, 4) = add_condition_to_string (XSTR (x, 4), extra);
      break;

    default:
      break;
    }
}

/* A htab_traverse callback.  Search the EXPR_LIST given by DATA
   for rtxes that use the iterator in *SLOT.  Replace each such rtx
   with a list of expansions.  */

static int
apply_iterator_traverse (void **slot, void *data)
{
  struct iterator_traverse_data *mtd = (struct iterator_traverse_data *) data;
  struct mapping *iterator;
  struct map_value *v;
  rtx elem, new_elem, original, x;

  iterator = (struct mapping *) *slot;
  for (elem = mtd->queue; elem != 0; elem = XEXP (elem, 1))
    if (uses_iterator_p (XEXP (elem, 0), iterator))
      {
	/* For each iterator we expand, we set UNKNOWN_MODE_ATTR to NULL.
	   If apply_iterator_rtx finds an unknown attribute for a mode,
	   it will set it to the attribute.  We want to know whether
	   the attribute is unknown after we have expanded all
	   possible iterators, so setting it to NULL here gives us the
	   right result when the hash table traversal is complete.  */
	mtd->unknown_mode_attr = NULL;

	original = XEXP (elem, 0);
	for (v = iterator->values; v != 0; v = v->next)
	  {
	    x = apply_iterator_to_rtx (original, iterator, v->number,
				       mtd->mode_maps,
				       &mtd->unknown_mode_attr);
	    add_condition_to_rtx (x, v->string);
	    if (v != iterator->values)
	      {
		/* Insert a new EXPR_LIST node after ELEM and put the
		   new expansion there.  */
		new_elem = rtx_alloc (EXPR_LIST);
		XEXP (new_elem, 1) = XEXP (elem, 1);
		XEXP (elem, 1) = new_elem;
		elem = new_elem;
	      }
	    XEXP (elem, 0) = x;
	  }
    }
  return 1;
}

/* Add a new "mapping" structure to hashtable TABLE.  NAME is the name
   of the mapping and GROUP is the group to which it belongs.  */

static struct mapping *
add_mapping (struct iterator_group *group, htab_t table, const char *name)
{
  struct mapping *m;
  void **slot;

  m = XNEW (struct mapping);
  m->name = xstrdup (name);
  m->group = group;
  m->index = htab_elements (table);
  m->values = 0;

  slot = htab_find_slot (table, m, INSERT);
  if (*slot != 0)
    fatal_with_file_and_line ("`%s' already defined", name);

  *slot = m;
  return m;
}

/* Add the pair (NUMBER, STRING) to a list of map_value structures.
   END_PTR points to the current null terminator for the list; return
   a pointer the new null terminator.  */

static struct map_value **
add_map_value (struct map_value **end_ptr, int number, const char *string)
{
  struct map_value *value;

  value = XNEW (struct map_value);
  value->next = 0;
  value->number = number;
  value->string = string;

  *end_ptr = value;
  return &value->next;
}

/* Do one-time initialization of the mode and code attributes.  */

static void
initialize_iterators (void)
{
  struct mapping *lower, *upper;
  struct map_value **lower_ptr, **upper_ptr;
  char *copy, *p;
  int i;

  modes.attrs = htab_create (13, leading_string_hash, leading_string_eq_p, 0);
  modes.iterators = htab_create (13, leading_string_hash,
				 leading_string_eq_p, 0);
  modes.num_builtins = MAX_MACHINE_MODE;
  modes.find_builtin = find_mode;
  modes.uses_iterator_p = uses_mode_iterator_p;
  modes.apply_iterator = apply_mode_iterator;

  codes.attrs = htab_create (13, leading_string_hash, leading_string_eq_p, 0);
  codes.iterators = htab_create (13, leading_string_hash,
				 leading_string_eq_p, 0);
  codes.num_builtins = NUM_RTX_CODE;
  codes.find_builtin = find_code;
  codes.uses_iterator_p = uses_code_iterator_p;
  codes.apply_iterator = apply_code_iterator;

  lower = add_mapping (&modes, modes.attrs, "mode");
  upper = add_mapping (&modes, modes.attrs, "MODE");
  lower_ptr = &lower->values;
  upper_ptr = &upper->values;
  for (i = 0; i < MAX_MACHINE_MODE; i++)
    {
      copy = xstrdup (GET_MODE_NAME (i));
      for (p = copy; *p != 0; p++)
	*p = TOLOWER (*p);

      upper_ptr = add_map_value (upper_ptr, i, GET_MODE_NAME (i));
      lower_ptr = add_map_value (lower_ptr, i, copy);
    }

  lower = add_mapping (&codes, codes.attrs, "code");
  upper = add_mapping (&codes, codes.attrs, "CODE");
  lower_ptr = &lower->values;
  upper_ptr = &upper->values;
  for (i = 0; i < NUM_RTX_CODE; i++)
    {
      copy = xstrdup (GET_RTX_NAME (i));
      for (p = copy; *p != 0; p++)
	*p = TOUPPER (*p);

      lower_ptr = add_map_value (lower_ptr, i, GET_RTX_NAME (i));
      upper_ptr = add_map_value (upper_ptr, i, copy);
    }
}

/* Provide a version of a function to read a long long if the system does
   not provide one.  */
#if HOST_BITS_PER_WIDE_INT > HOST_BITS_PER_LONG && !defined(HAVE_ATOLL) && !defined(HAVE_ATOQ)
HOST_WIDE_INT atoll (const char *);

HOST_WIDE_INT
atoll (const char *p)
{
  int neg = 0;
  HOST_WIDE_INT tmp_wide;

  while (ISSPACE (*p))
    p++;
  if (*p == '-')
    neg = 1, p++;
  else if (*p == '+')
    p++;

  tmp_wide = 0;
  while (ISDIGIT (*p))
    {
      HOST_WIDE_INT new_wide = tmp_wide*10 + (*p - '0');
      if (new_wide < tmp_wide)
	{
	  /* Return INT_MAX equiv on overflow.  */
	  tmp_wide = (~(unsigned HOST_WIDE_INT) 0) >> 1;
	  break;
	}
      tmp_wide = new_wide;
      p++;
    }

  if (neg)
    tmp_wide = -tmp_wide;
  return tmp_wide;
}
#endif

/* Process a define_conditions directive, starting with the optional
   space after the "define_conditions".  The directive looks like this:

     (define_conditions [
        (number "string")
        (number "string")
        ...
     ])

   It's not intended to appear in machine descriptions.  It is
   generated by (the program generated by) genconditions.c, and
   slipped in at the beginning of the sequence of MD files read by
   most of the other generators.  */
static void
read_conditions (void)
{
  int c;

  c = read_skip_spaces ();
  if (c != '[')
    fatal_expected_char ('[', c);

  while ( (c = read_skip_spaces ()) != ']')
    {
      struct md_name name;
      char *expr;
      int value;

      if (c != '(')
	fatal_expected_char ('(', c);

      read_name (&name);
      validate_const_int (name.string);
      value = atoi (name.string);

      c = read_skip_spaces ();
      if (c != '"')
	fatal_expected_char ('"', c);
      expr = read_quoted_string ();

      c = read_skip_spaces ();
      if (c != ')')
	fatal_expected_char (')', c);

      add_c_test (expr, value);
    }
}

static void
validate_const_int (const char *string)
{
  const char *cp;
  int valid = 1;

  cp = string;
  while (*cp && ISSPACE (*cp))
    cp++;
  if (*cp == '-' || *cp == '+')
    cp++;
  if (*cp == 0)
    valid = 0;
  for (; *cp; cp++)
    if (! ISDIGIT (*cp))
      valid = 0;
  if (!valid)
    fatal_with_file_and_line ("invalid decimal constant \"%s\"\n", string);
}

/* Search GROUP for a mode or code called NAME and return its numerical
   identifier.  */

static int
find_iterator (struct iterator_group *group, const char *name)
{
  struct mapping *m;

  m = (struct mapping *) htab_find (group->iterators, &name);
  if (m != 0)
    return m->index + group->num_builtins;
  return group->find_builtin (name);
}

/* Finish reading a declaration of the form:

       (define... <name> [<value1> ... <valuen>])

   from the MD file, where each <valuei> is either a bare symbol name or a
   "(<name> <string>)" pair.  The "(define..." part has already been read.

   Represent the declaration as a "mapping" structure; add it to TABLE
   (which belongs to GROUP) and return it.  */

static struct mapping *
read_mapping (struct iterator_group *group, htab_t table)
{
  struct md_name name;
  struct mapping *m;
  struct map_value **end_ptr;
  const char *string;
  int number, c;

  /* Read the mapping name and create a structure for it.  */
  read_name (&name);
  m = add_mapping (group, table, name.string);

  c = read_skip_spaces ();
  if (c != '[')
    fatal_expected_char ('[', c);

  /* Read each value.  */
  end_ptr = &m->values;
  c = read_skip_spaces ();
  do
    {
      if (c != '(')
	{
	  /* A bare symbol name that is implicitly paired to an
	     empty string.  */
	  unread_char (c);
	  read_name (&name);
	  string = "";
	}
      else
	{
	  /* A "(name string)" pair.  */
	  read_name (&name);
	  string = read_string (false);
	  c = read_skip_spaces ();
	  if (c != ')')
	    fatal_expected_char (')', c);
	}
      number = group->find_builtin (name.string);
      end_ptr = add_map_value (end_ptr, number, string);
      c = read_skip_spaces ();
    }
  while (c != ']');

  return m;
}

/* Check newly-created code iterator ITERATOR to see whether every code has the
   same format.  Initialize the iterator's entry in bellwether_codes.  */

static void
check_code_iterator (struct mapping *iterator)
{
  struct map_value *v;
  enum rtx_code bellwether;

  bellwether = (enum rtx_code) iterator->values->number;
  for (v = iterator->values->next; v != 0; v = v->next)
    if (strcmp (GET_RTX_FORMAT (bellwether), GET_RTX_FORMAT (v->number)) != 0)
      fatal_with_file_and_line ("code iterator `%s' combines "
				"different rtx formats", iterator->name);

  bellwether_codes = XRESIZEVEC (enum rtx_code, bellwether_codes,
				 iterator->index + 1);
  bellwether_codes[iterator->index] = bellwether;
}

/* Read an rtx-related declaration from the MD file, given that it
   starts with directive name RTX_NAME.  Return true if it expands to
   one or more rtxes (as defined by rtx.def).  When returning true,
   store the list of rtxes as an EXPR_LIST in *X.  */

bool
read_rtx (const char *rtx_name, rtx *x)
{
  static rtx queue_head;
  struct map_value *mode_maps;
  struct iterator_traverse_data mtd;

  /* Do one-time initialization.  */
  if (queue_head == 0)
    {
      initialize_iterators ();
      queue_head = rtx_alloc (EXPR_LIST);
    }

  /* Handle various rtx-related declarations that aren't themselves
     encoded as rtxes.  */
  if (strcmp (rtx_name, "define_conditions") == 0)
    {
      read_conditions ();
      return false;
    }
  if (strcmp (rtx_name, "define_mode_attr") == 0)
    {
      read_mapping (&modes, modes.attrs);
      return false;
    }
  if (strcmp (rtx_name, "define_mode_iterator") == 0)
    {
      read_mapping (&modes, modes.iterators);
      return false;
    }
  if (strcmp (rtx_name, "define_code_attr") == 0)
    {
      read_mapping (&codes, codes.attrs);
      return false;
    }
  if (strcmp (rtx_name, "define_code_iterator") == 0)
    {
      check_code_iterator (read_mapping (&codes, codes.iterators));
      return false;
    }

  mode_maps = 0;
  XEXP (queue_head, 0) = read_rtx_code (rtx_name, &mode_maps);
  XEXP (queue_head, 1) = 0;

  mtd.queue = queue_head;
  mtd.mode_maps = mode_maps;
  mtd.unknown_mode_attr = mode_maps ? mode_maps->string : NULL;
  htab_traverse (modes.iterators, apply_iterator_traverse, &mtd);
  htab_traverse (codes.iterators, apply_iterator_traverse, &mtd);
  if (mtd.unknown_mode_attr)
    fatal_with_file_and_line ("undefined attribute '%s' used for mode",
			      mtd.unknown_mode_attr);

  *x = queue_head;
  return true;
}

/* Subroutine of read_rtx and read_nested_rtx.  CODE_NAME is the name of
   either an rtx code or a code iterator.  Parse the rest of the rtx and
   return it.  MODE_MAPS is as for iterator_traverse_data.  */

static rtx
read_rtx_code (const char *code_name, struct map_value **mode_maps)
{
  int i;
  RTX_CODE real_code, bellwether_code;
  const char *format_ptr;
  struct md_name name;
  rtx return_rtx;
  int c;
  int tmp_int;
  HOST_WIDE_INT tmp_wide;

  /* Linked list structure for making RTXs: */
  struct rtx_list
    {
      struct rtx_list *next;
      rtx value;		/* Value of this node.  */
    };

  real_code = (enum rtx_code) find_iterator (&codes, code_name);
  bellwether_code = BELLWETHER_CODE (real_code);

  /* If we end up with an insn expression then we free this space below.  */
  return_rtx = rtx_alloc (bellwether_code);
  format_ptr = GET_RTX_FORMAT (bellwether_code);
  PUT_CODE (return_rtx, real_code);

  /* If what follows is `: mode ', read it and
     store the mode in the rtx.  */

  i = read_skip_spaces ();
  if (i == ':')
    {
      unsigned int mode;

      read_name (&name);
      if (name.string[0] != '<' || name.string[strlen (name.string) - 1] != '>')
	mode = find_iterator (&modes, name.string);
      else
	mode = mode_attr_index (mode_maps, name.string);
      PUT_MODE (return_rtx, (enum machine_mode) mode);
      if (GET_MODE (return_rtx) != mode)
	fatal_with_file_and_line ("mode too large");
    }
  else
    unread_char (i);

  for (i = 0; format_ptr[i] != 0; i++)
    switch (format_ptr[i])
      {
	/* 0 means a field for internal use only.
	   Don't expect it to be present in the input.  */
      case '0':
	break;

      case 'e':
      case 'u':
	XEXP (return_rtx, i) = read_nested_rtx (mode_maps);
	break;

      case 'V':
	/* 'V' is an optional vector: if a closeparen follows,
	   just store NULL for this element.  */
	c = read_skip_spaces ();
	unread_char (c);
	if (c == ')')
	  {
	    XVEC (return_rtx, i) = 0;
	    break;
	  }
	/* Now process the vector.  */

      case 'E':
	{
	  /* Obstack to store scratch vector in.  */
	  struct obstack vector_stack;
	  int list_counter = 0;
	  rtvec return_vec = NULL_RTVEC;

	  c = read_skip_spaces ();
	  if (c != '[')
	    fatal_expected_char ('[', c);

	  /* Add expressions to a list, while keeping a count.  */
	  obstack_init (&vector_stack);
	  while ((c = read_skip_spaces ()) && c != ']')
	    {
	      if (c == EOF)
		fatal_expected_char (']', c);
	      unread_char (c);
	      list_counter++;
	      obstack_ptr_grow (&vector_stack, read_nested_rtx (mode_maps));
	    }
	  if (list_counter > 0)
	    {
	      return_vec = rtvec_alloc (list_counter);
	      memcpy (&return_vec->elem[0], obstack_finish (&vector_stack),
		      list_counter * sizeof (rtx));
	    }
	  else if (format_ptr[i] == 'E')
	    fatal_with_file_and_line ("vector must have at least one element");
	  XVEC (return_rtx, i) = return_vec;
	  obstack_free (&vector_stack, NULL);
	  /* close bracket gotten */
	}
	break;

      case 'S':
      case 'T':
      case 's':
	{
	  char *stringbuf;
	  int star_if_braced;

	  c = read_skip_spaces ();
	  unread_char (c);
	  if (c == ')')
	    {
	      /* 'S' fields are optional and should be NULL if no string
		 was given.  Also allow normal 's' and 'T' strings to be
		 omitted, treating them in the same way as empty strings.  */
	      XSTR (return_rtx, i) = (format_ptr[i] == 'S' ? NULL : "");
	      break;
	    }

	  /* The output template slot of a DEFINE_INSN,
	     DEFINE_INSN_AND_SPLIT, or DEFINE_PEEPHOLE automatically
	     gets a star inserted as its first character, if it is
	     written with a brace block instead of a string constant.  */
	  star_if_braced = (format_ptr[i] == 'T');

	  stringbuf = read_string (star_if_braced);

	  /* For insn patterns, we want to provide a default name
	     based on the file and line, like "*foo.md:12", if the
	     given name is blank.  These are only for define_insn and
	     define_insn_and_split, to aid debugging.  */
	  if (*stringbuf == '\0'
	      && i == 0
	      && (GET_CODE (return_rtx) == DEFINE_INSN
		  || GET_CODE (return_rtx) == DEFINE_INSN_AND_SPLIT))
	    {
	      char line_name[20];
	      const char *fn = (read_md_filename ? read_md_filename : "rtx");
	      const char *slash;
	      for (slash = fn; *slash; slash ++)
		if (*slash == '/' || *slash == '\\' || *slash == ':')
		  fn = slash + 1;
	      obstack_1grow (&string_obstack, '*');
	      obstack_grow (&string_obstack, fn, strlen (fn));
	      sprintf (line_name, ":%d", read_md_lineno);
	      obstack_grow (&string_obstack, line_name, strlen (line_name)+1);
	      stringbuf = XOBFINISH (&string_obstack, char *);
	    }

	  if (star_if_braced)
	    XTMPL (return_rtx, i) = stringbuf;
	  else
	    XSTR (return_rtx, i) = stringbuf;
	}
	break;

      case 'w':
	read_name (&name);
	validate_const_int (name.string);
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
	tmp_wide = atoi (name.string);
#else
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
	tmp_wide = atol (name.string);
#else
	/* Prefer atoll over atoq, since the former is in the ISO C99 standard.
	   But prefer not to use our hand-rolled function above either.  */
#if defined(HAVE_ATOLL) || !defined(HAVE_ATOQ)
	tmp_wide = atoll (name.string);
#else
	tmp_wide = atoq (name.string);
#endif
#endif
#endif
	XWINT (return_rtx, i) = tmp_wide;
	break;

      case 'i':
      case 'n':
	read_name (&name);
	validate_const_int (name.string);
	tmp_int = atoi (name.string);
	XINT (return_rtx, i) = tmp_int;
	break;

      default:
	gcc_unreachable ();
      }

  c = read_skip_spaces ();
  /* Syntactic sugar for AND and IOR, allowing Lisp-like
     arbitrary number of arguments for them.  */
  if (c == '('
      && (GET_CODE (return_rtx) == AND
	  || GET_CODE (return_rtx) == IOR))
    return read_rtx_variadic (mode_maps, return_rtx);

  unread_char (c);
  return return_rtx;
}

/* Read a nested rtx construct from the MD file and return it.
   MODE_MAPS is as for iterator_traverse_data.  */

static rtx
read_nested_rtx (struct map_value **mode_maps)
{
  struct md_name name;
  int c;
  rtx return_rtx;

  c = read_skip_spaces ();
  if (c != '(')
    fatal_expected_char ('(', c);

  read_name (&name);
  if (strcmp (name.string, "nil") == 0)
    return_rtx = NULL;
  else
    return_rtx = read_rtx_code (name.string, mode_maps);

  c = read_skip_spaces ();
  if (c != ')')
    fatal_expected_char (')', c);

  return return_rtx;
}

/* Mutually recursive subroutine of read_rtx which reads
   (thing x1 x2 x3 ...) and produces RTL as if
   (thing x1 (thing x2 (thing x3 ...)))  had been written.
   When called, FORM is (thing x1 x2), and the file position
   is just past the leading parenthesis of x3.  Only works
   for THINGs which are dyadic expressions, e.g. AND, IOR.  */
static rtx
read_rtx_variadic (struct map_value **mode_maps, rtx form)
{
  char c = '(';
  rtx p = form, q;

  do
    {
      unread_char (c);

      q = rtx_alloc (GET_CODE (p));
      PUT_MODE (q, GET_MODE (p));

      XEXP (q, 0) = XEXP (p, 1);
      XEXP (q, 1) = read_nested_rtx (mode_maps);

      XEXP (p, 1) = q;
      p = q;
      c = read_skip_spaces ();
    }
  while (c == '(');
  unread_char (c);
  return form;
}
