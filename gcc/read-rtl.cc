/* RTL reader for GCC.
   Copyright (C) 1987-2023 Free Software Foundation, Inc.

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

/* This file is compiled twice: once for the generator programs
   once for the compiler.  */
#ifdef GENERATOR_FILE
#include "bconfig.h"
#else
#include "config.h"
#endif

/* Disable rtl checking; it conflicts with the iterator handling.  */
#undef ENABLE_RTL_CHECKING

#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "obstack.h"
#include "read-md.h"
#include "gensupport.h"

/* One element in a singly-linked list of (integer, string) pairs.  */
struct map_value {
  struct map_value *next;
  int number;
  const char *string;
};

/* Maps an iterator or attribute name to a list of (integer, string) pairs.
   The integers are iterator values; the strings are either C conditions
   or attribute values.  */
struct mapping {
  /* The name of the iterator or attribute.  */
  const char *name;

  /* The group (modes or codes) to which the iterator or attribute belongs.  */
  struct iterator_group *group;

  /* The list of (integer, string) pairs.  */
  struct map_value *values;

  /* For iterators, records the current value of the iterator.  */
  struct map_value *current_value;
};

/* A structure for abstracting the common parts of iterators.  */
struct iterator_group {
  /* Tables of "mapping" structures, one for attributes and one for
     iterators.  */
  htab_t attrs, iterators;

  /* The C++ type of the iterator, such as "machine_mode" for modes.  */
  const char *type;

  /* Treat the given string as the name of a standard mode, etc., and
     return its integer value.  */
  HOST_WIDE_INT (*find_builtin) (const char *);

  /* Make the given rtx use the iterator value given by the third argument.
     If the iterator applies to operands, the second argument gives the
     operand index, otherwise it is ignored.  */
  void (*apply_iterator) (rtx, unsigned int, HOST_WIDE_INT);

  /* Return the C token for the given standard mode, code, etc.  */
  const char *(*get_c_token) (int);
};

/* Records one use of an iterator.  */
struct iterator_use {
  /* The iterator itself.  */
  struct mapping *iterator;

  /* The location of the use, as passed to the apply_iterator callback.
     The index is the number of the operand that used the iterator
     if applicable, otherwise it is ignored.  */
  rtx x;
  unsigned int index;
};

/* Records one use of an attribute (the "<[iterator:]attribute>" syntax)
   in a non-string rtx field.  */
struct attribute_use {
  /* The group that describes the use site.  */
  struct iterator_group *group;

  /* The location at which the use occurs.  */
  file_location loc;

  /* The name of the attribute, possibly with an "iterator:" prefix.  */
  const char *value;

  /* The location of the use, as passed to GROUP's apply_iterator callback.
     The index is the number of the operand that used the iterator
     if applicable, otherwise it is ignored.  */
  rtx x;
  unsigned int index;
};

/* This struct is used to link subst_attr named ATTR_NAME with
   corresponding define_subst named ITER_NAME.  */
struct subst_attr_to_iter_mapping
{
    char *attr_name;
    char *iter_name;
};

/* Hash-table to store links between subst-attributes and
   define_substs.  */
htab_t subst_attr_to_iter_map = NULL;
/* This global stores name of subst-iterator which is currently being
   processed.  */
const char *current_iterator_name;

static void validate_const_int (const char *);
static void one_time_initialization (void);

/* Global singleton.  */
rtx_reader *rtx_reader_ptr = NULL;

/* The mode and code iterator structures.  */
static struct iterator_group modes, codes, ints, substs;

/* All iterators used in the current rtx.  */
static vec<mapping *> current_iterators;

/* The list of all iterator uses in the current rtx.  */
static vec<iterator_use> iterator_uses;

/* The list of all attribute uses in the current rtx.  */
static vec<attribute_use> attribute_uses;

/* Provide a version of a function to read a long long if the system does
   not provide one.  */
#if (HOST_BITS_PER_WIDE_INT > HOST_BITS_PER_LONG			\
     && !HAVE_DECL_ATOLL						\
     && !defined (HAVE_ATOQ))
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
	  tmp_wide = HOST_WIDE_INT_M1U >> 1;
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

/* Implementations of the iterator_group callbacks for modes.  */

static HOST_WIDE_INT
find_mode (const char *name)
{
  int i;

  for (i = 0; i < NUM_MACHINE_MODES; i++)
    if (strcmp (GET_MODE_NAME (i), name) == 0)
      return i;

  fatal_with_file_and_line ("unknown mode `%s'", name);
}

static void
apply_mode_iterator (rtx x, unsigned int, HOST_WIDE_INT mode)
{
  PUT_MODE (x, (machine_mode) mode);
}

static const char *
get_mode_token (int mode)
{
  return concat ("E_", GET_MODE_NAME (mode), "mode", NULL);
}

/* In compact dumps, the code of insns is prefixed with "c", giving "cinsn",
   "cnote" etc, and CODE_LABEL is special-cased as "clabel".  */

struct compact_insn_name {
  RTX_CODE code;
  const char *name;
};

static const compact_insn_name compact_insn_names[] = {
  { DEBUG_INSN, "cdebug_insn" },
  { INSN, "cinsn" },
  { JUMP_INSN, "cjump_insn" },
  { CALL_INSN, "ccall_insn" },
  { JUMP_TABLE_DATA, "cjump_table_data" },
  { BARRIER, "cbarrier" },
  { CODE_LABEL, "clabel" },
  { NOTE, "cnote" }
};

/* Return the rtx code for NAME, or UNKNOWN if NAME isn't a valid rtx code.  */

static rtx_code
maybe_find_code (const char *name)
{
  for (int i = 0; i < NUM_RTX_CODE; i++)
    if (strcmp (GET_RTX_NAME (i), name) == 0)
      return (rtx_code) i;

  for (int i = 0; i < (signed)ARRAY_SIZE (compact_insn_names); i++)
    if (strcmp (compact_insn_names[i].name, name) == 0)
      return compact_insn_names[i].code;

  return UNKNOWN;
}

/* Implementations of the iterator_group callbacks for codes.  */

static HOST_WIDE_INT
find_code (const char *name)
{
  rtx_code code = maybe_find_code (name);
  if (code == UNKNOWN)
    fatal_with_file_and_line ("unknown rtx code `%s'", name);
  return code;
}

static void
apply_code_iterator (rtx x, unsigned int, HOST_WIDE_INT code)
{
  PUT_CODE (x, (enum rtx_code) code);
}

static const char *
get_code_token (int code)
{
  char *name = xstrdup (GET_RTX_NAME (code));
  for (int i = 0; name[i]; ++i)
    name[i] = TOUPPER (name[i]);
  return name;
}

/* Implementations of the iterator_group callbacks for ints.  */

/* Since GCC does not construct a table of valid constants,
   we have to accept any int as valid.  No cross-checking can
   be done.  */

static HOST_WIDE_INT
find_int (const char *name)
{
  HOST_WIDE_INT tmp;

  struct md_constant tmp_def;
  tmp_def.name = const_cast<char *> (name);
  auto htab = rtx_reader_ptr->get_md_constants ();
  if (auto def = (struct md_constant *) htab_find (htab, &tmp_def))
    name = def->value;

  validate_const_int (name);
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
  tmp = atoi (name);
#else
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
  tmp = atol (name);
#else
  /* Prefer atoll over atoq, since the former is in the ISO C99 standard.
     But prefer not to use our hand-rolled function above either.  */
#if HAVE_DECL_ATOLL || !defined(HAVE_ATOQ)
  tmp = atoll (name);
#else
  tmp = atoq (name);
#endif
#endif
#endif
  return tmp;
}

static void
apply_int_iterator (rtx x, unsigned int index, HOST_WIDE_INT value)
{
  RTX_CODE code = GET_CODE (x);
  const char *format_ptr = GET_RTX_FORMAT (code);

  switch (format_ptr[index])
    {
    case 'i':
    case 'n':
      XINT (x, index) = value;
      break;
    case 'w':
      XWINT (x, index) = value;
      break;
    case 'p':
      gcc_assert (code == SUBREG);
      SUBREG_BYTE (x) = value;
      break;
    default:
      gcc_unreachable ();
    }
}

static const char *
get_int_token (int value)
{
  char buffer[HOST_BITS_PER_INT + 1];
  sprintf (buffer, "%d", value);
  return xstrdup (buffer);
}

#ifdef GENERATOR_FILE

/* This routine adds attribute or does nothing depending on VALUE.  When
   VALUE is 1, it does nothing - the first duplicate of original
   template is kept untouched when it's subjected to a define_subst.
   When VALUE isn't 1, the routine modifies RTL-template RT, adding
   attribute, named exactly as define_subst, which later will be
   applied.  If such attribute has already been added, then no the
   routine has no effect.  */
static void
apply_subst_iterator (rtx rt, unsigned int, HOST_WIDE_INT value)
{
  rtx new_attr;
  rtvec attrs_vec, new_attrs_vec;
  int i;
  /* define_split has no attributes.  */
  if (value == 1 || GET_CODE (rt) == DEFINE_SPLIT)
    return;
  gcc_assert (GET_CODE (rt) == DEFINE_INSN
	      || GET_CODE (rt) == DEFINE_INSN_AND_SPLIT
	      || GET_CODE (rt) == DEFINE_INSN_AND_REWRITE
	      || GET_CODE (rt) == DEFINE_EXPAND);

  int attrs = (GET_CODE (rt) == DEFINE_INSN_AND_SPLIT ? 7
	       : GET_CODE (rt) == DEFINE_INSN_AND_REWRITE ? 6 : 4);
  attrs_vec = XVEC (rt, attrs);

  /* If we've already added attribute 'current_iterator_name', then we
     have nothing to do now.  */
  if (attrs_vec)
    {
      for (i = 0; i < GET_NUM_ELEM (attrs_vec); i++)
	{
	  if (strcmp (XSTR (attrs_vec->elem[i], 0), current_iterator_name) == 0)
	    return;
	}
    }

  /* Add attribute with subst name - it serves as a mark for
     define_subst which later would be applied to this pattern.  */
  new_attr = rtx_alloc (SET_ATTR);
  PUT_CODE (new_attr, SET_ATTR);
  XSTR (new_attr, 0) = xstrdup (current_iterator_name);
  XSTR (new_attr, 1) = xstrdup ("yes");

  if (!attrs_vec)
    {
      new_attrs_vec = rtvec_alloc (1);
      new_attrs_vec->elem[0] = new_attr;
    }
  else
    {
      new_attrs_vec = rtvec_alloc (GET_NUM_ELEM (attrs_vec) + 1);
      memcpy (&new_attrs_vec->elem[0], &attrs_vec->elem[0],
	      GET_NUM_ELEM (attrs_vec) * sizeof (rtx));
      new_attrs_vec->elem[GET_NUM_ELEM (attrs_vec)] = new_attr;
    }
  XVEC (rt, attrs) = new_attrs_vec;
}

/* Map subst-attribute ATTR to subst iterator ITER.  */

static void
bind_subst_iter_and_attr (const char *iter, const char *attr)
{
  struct subst_attr_to_iter_mapping *value;
  void **slot;
  if (!subst_attr_to_iter_map)
    subst_attr_to_iter_map =
      htab_create (1, leading_string_hash, leading_string_eq_p, 0);
  value = XNEW (struct subst_attr_to_iter_mapping);
  value->attr_name = xstrdup (attr);
  value->iter_name = xstrdup (iter);
  slot = htab_find_slot (subst_attr_to_iter_map, value, INSERT);
  *slot = value;
}

#endif /* #ifdef GENERATOR_FILE */

/* Return name of a subst-iterator, corresponding to subst-attribute ATTR.  */

static char*
find_subst_iter_by_attr (const char *attr)
{
  char *iter_name = NULL;
  struct subst_attr_to_iter_mapping *value;
  value = (struct subst_attr_to_iter_mapping*)
    htab_find (subst_attr_to_iter_map, &attr);
  if (value)
    iter_name = value->iter_name;
  return iter_name;
}

/* Map attribute string P to its current value.  Return null if the attribute
   isn't known.  If ITERATOR_OUT is nonnull, store the associated iterator
   there.  Report any errors against location LOC.  */

static struct map_value *
map_attr_string (file_location loc, const char *p, mapping **iterator_out = 0)
{
  const char *attr;
  struct mapping *iterator;
  unsigned int i;
  struct mapping *m;
  struct map_value *v;
  int iterator_name_len;
  struct map_value *res = NULL;
  struct mapping *prev = NULL;

  /* Peel off any "iterator:" prefix.  Set ATTR to the start of the
     attribute name.  */
  attr = strchr (p, ':');
  if (attr == 0)
    {
      iterator_name_len = -1;
      attr = p;
    }
  else
    {
      iterator_name_len = attr - p;
      attr++;
    }

  FOR_EACH_VEC_ELT (current_iterators, i, iterator)
    {
      /* If an iterator name was specified, check that it matches.  */
      if (iterator_name_len >= 0
	  && (strncmp (p, iterator->name, iterator_name_len) != 0
	      || iterator->name[iterator_name_len] != 0))
	continue;

      /* Find the attribute specification.  */
      m = (struct mapping *) htab_find (iterator->group->attrs, &attr);
      if (m)
	{
	  /* In contrast to code/mode/int iterators, attributes of subst
	     iterators are linked to one specific subst-iterator.  So, if
	     we are dealing with subst-iterator, we should check if it's
	     the one which linked with the given attribute.  */
	  if (iterator->group == &substs)
	    {
	      char *iter_name = find_subst_iter_by_attr (attr);
	      if (strcmp (iter_name, iterator->name) != 0)
		continue;
	    }
	  /* Find the attribute value associated with the current
	     iterator value.  */
	  for (v = m->values; v; v = v->next)
	    if (v->number == iterator->current_value->number)
	      {
		if (res && strcmp (v->string, res->string) != 0)
		  {
		    error_at (loc, "ambiguous attribute '%s'; could be"
			      " '%s' (via '%s:%s') or '%s' (via '%s:%s')",
			      attr, res->string, prev->name, attr,
			      v->string, iterator->name, attr);
		    return v;
		  }
		if (iterator_out)
		  *iterator_out = iterator;
		prev = iterator;
		res = v;
	      }
	}
    }
  return res;
}

/* Apply the current iterator values to STRING.  Return the new string
   if any changes were needed, otherwise return STRING itself.  */

const char *
md_reader::apply_iterator_to_string (const char *string)
{
  char *base, *copy, *p, *start, *end;
  struct map_value *v;

  if (string == 0 || string[0] == 0)
    return string;

  file_location loc = get_md_ptr_loc (string)->loc;
  base = p = copy = ASTRDUP (string);
  while ((start = strchr (p, '<')) && (end = strchr (start, '>')))
    {
      p = start + 1;

      *end = 0;
      v = map_attr_string (loc, p);
      *end = '>';
      if (v == 0)
	continue;

      /* Add everything between the last copied byte and the '<',
	 then add in the attribute value.  */
      obstack_grow (&m_string_obstack, base, start - base);
      obstack_grow (&m_string_obstack, v->string, strlen (v->string));
      base = end + 1;
    }
  if (base != copy)
    {
      obstack_grow (&m_string_obstack, base, strlen (base) + 1);
      copy = XOBFINISH (&m_string_obstack, char *);
      copy_md_ptr_loc (copy, string);
      return copy;
    }
  return string;
}

/* Return a deep copy of X, substituting the current iterator
   values into any strings.  */

rtx
md_reader::copy_rtx_for_iterators (rtx original)
{
  const char *format_ptr, *p;
  int i, j;
  rtx x;

  if (original == 0)
    return original;

  /* Create a shallow copy of ORIGINAL.  */
  x = rtx_alloc (GET_CODE (original));
  memcpy (x, original, RTX_CODE_SIZE (GET_CODE (original)));

  /* Change each string and recursively change each rtx.  */
  format_ptr = GET_RTX_FORMAT (GET_CODE (original));
  for (i = 0; format_ptr[i] != 0; i++)
    switch (format_ptr[i])
      {
      case 'T':
	while (XTMPL (x, i) != (p = apply_iterator_to_string (XTMPL (x, i))))
	  XTMPL (x, i) = p;
	break;

      case 'S':
      case 's':
	while (XSTR (x, i) != (p = apply_iterator_to_string (XSTR (x, i))))
	  XSTR (x, i) = p;
	break;

      case 'e':
	XEXP (x, i) = copy_rtx_for_iterators (XEXP (x, i));
	break;

      case 'V':
      case 'E':
	if (XVEC (original, i))
	  {
	    XVEC (x, i) = rtvec_alloc (XVECLEN (original, i));
	    for (j = 0; j < XVECLEN (x, i); j++)
	      XVECEXP (x, i, j)
		= copy_rtx_for_iterators (XVECEXP (original, i, j));
	  }
	break;

      default:
	break;
      }
  return x;
}

#ifdef GENERATOR_FILE

/* Return a condition that must satisfy both ORIGINAL and EXTRA.  If ORIGINAL
   has the form "&& ..." (as used in define_insn_and_splits), assume that
   EXTRA is already satisfied.  Empty strings are treated like "true".  */

static const char *
add_condition_to_string (const char *original, const char *extra)
{
  if (original != 0 && original[0] == '&' && original[1] == '&')
    return original;
  return rtx_reader_ptr->join_c_conditions (original, extra);
}

/* Like add_condition, but applied to all conditions in rtx X.  */

static void
add_condition_to_rtx (rtx x, const char *extra)
{
  switch (GET_CODE (x))
    {
    case DEFINE_INSN:
    case DEFINE_EXPAND:
    case DEFINE_SUBST:
      XSTR (x, 2) = add_condition_to_string (XSTR (x, 2), extra);
      break;

    case DEFINE_SPLIT:
    case DEFINE_PEEPHOLE:
    case DEFINE_PEEPHOLE2:
    case DEFINE_COND_EXEC:
      XSTR (x, 1) = add_condition_to_string (XSTR (x, 1), extra);
      break;

    case DEFINE_INSN_AND_SPLIT:
    case DEFINE_INSN_AND_REWRITE:
      XSTR (x, 2) = add_condition_to_string (XSTR (x, 2), extra);
      XSTR (x, 4) = add_condition_to_string (XSTR (x, 4), extra);
      break;

    default:
      break;
    }
}

/* Apply the current iterator values to all attribute_uses.  */

static void
apply_attribute_uses (void)
{
  struct map_value *v;
  attribute_use *ause;
  unsigned int i;

  FOR_EACH_VEC_ELT (attribute_uses, i, ause)
    {
      v = map_attr_string (ause->loc, ause->value);
      if (!v)
	fatal_with_file_and_line ("unknown iterator value `%s'", ause->value);
      ause->group->apply_iterator (ause->x, ause->index,
				   ause->group->find_builtin (v->string));
    }
}

/* A htab_traverse callback for iterators.  Add all used iterators
   to current_iterators.  */

static int
add_current_iterators (void **slot, void *data ATTRIBUTE_UNUSED)
{
  struct mapping *iterator;

  iterator = (struct mapping *) *slot;
  if (iterator->current_value)
    current_iterators.safe_push (iterator);
  return 1;
}

/* Return a hash value for overloaded_name UNCAST_ONAME.  There shouldn't
   be many instances of two overloaded_names having the same name but
   different arguments, so hashing on the name should be good enough in
   practice.  */

static hashval_t
overloaded_name_hash (const void *uncast_oname)
{
  const overloaded_name *oname = (const overloaded_name *) uncast_oname;
  return htab_hash_string (oname->name);
}

/* Return true if two overloaded_names are similar enough to share
   the same generated functions.  */

static int
overloaded_name_eq_p (const void *uncast_oname1, const void *uncast_oname2)
{
  const overloaded_name *oname1 = (const overloaded_name *) uncast_oname1;
  const overloaded_name *oname2 = (const overloaded_name *) uncast_oname2;
  if (strcmp (oname1->name, oname2->name) != 0
      || oname1->arg_types.length () != oname2->arg_types.length ())
    return 0;

  for (unsigned int i = 0; i < oname1->arg_types.length (); ++i)
    if (strcmp (oname1->arg_types[i], oname2->arg_types[i]) != 0)
      return 0;

  return 1;
}

/* Return true if X has an instruction name in XSTR (X, 0).  */

static bool
named_rtx_p (rtx x)
{
  switch (GET_CODE (x))
    {
    case DEFINE_EXPAND:
    case DEFINE_INSN:
    case DEFINE_INSN_AND_SPLIT:
    case DEFINE_INSN_AND_REWRITE:
      return true;

    default:
      return false;
    }
}

/* Check whether ORIGINAL is a named pattern whose name starts with '@'.
   If so, return the associated overloaded_name and add the iterator for
   each argument to ITERATORS.  Return null otherwise.  */

overloaded_name *
md_reader::handle_overloaded_name (rtx original, vec<mapping *> *iterators)
{
  /* Check for the leading '@'.  */
  if (!named_rtx_p (original) || XSTR (original, 0)[0] != '@')
    return NULL;

  /* Remove the '@', so that no other code needs to worry about it.  */
  const char *name = XSTR (original, 0);
  file_location loc = get_md_ptr_loc (name)->loc;
  copy_md_ptr_loc (name + 1, name);
  name += 1;
  XSTR (original, 0) = name;

  /* Build a copy of the name without the '<...>' attribute strings.
     Add the iterator associated with each such attribute string to ITERATORS
     and add an associated argument to TMP_ONAME.  */
  char *copy = ASTRDUP (name);
  char *base = copy, *start, *end;
  overloaded_name tmp_oname;
  tmp_oname.arg_types.create (current_iterators.length ());
  bool pending_underscore_p = false;
  while ((start = strchr (base, '<')) && (end = strchr (start, '>')))
    {
      *end = 0;
      mapping *iterator;
      if (!map_attr_string (loc, start + 1, &iterator))
	fatal_with_file_and_line ("unknown iterator `%s'", start + 1);
      *end = '>';

      /* Remove a trailing underscore, so that we don't end a name
	 with "_" or turn "_<...>_" into "__".  */
      if (start != base && start[-1] == '_')
	{
	  start -= 1;
	  pending_underscore_p = true;
	}

      /* Add the text between either the last '>' or the start of
	 the string and this '<'.  */
      obstack_grow (&m_string_obstack, base, start - base);
      base = end + 1;

      /* If there's a character we need to keep after the '>', check
	 whether we should prefix it with a previously-dropped '_'.  */
      if (base[0] != 0 && base[0] != '<')
	{
	  if (pending_underscore_p && base[0] != '_')
	    obstack_1grow (&m_string_obstack, '_');
	  pending_underscore_p = false;
	}

      /* Record an argument for ITERATOR.  */
      iterators->safe_push (iterator);
      tmp_oname.arg_types.safe_push (iterator->group->type);
    }
  if (base == copy)
    fatal_with_file_and_line ("no iterator attributes in name `%s'", name);

  size_t length = obstack_object_size (&m_string_obstack);
  if (length == 0)
    fatal_with_file_and_line ("`%s' only contains iterator attributes", name);

  /* Get the completed name.  */
  obstack_grow (&m_string_obstack, base, strlen (base) + 1);
  char *new_name = XOBFINISH (&m_string_obstack, char *);
  tmp_oname.name = new_name;

  if (!m_overloads_htab)
    m_overloads_htab = htab_create (31, overloaded_name_hash,
				    overloaded_name_eq_p, NULL);

  /* See whether another pattern had the same overload name and list
     of argument types.  Create a new permanent one if not.  */
  void **slot = htab_find_slot (m_overloads_htab, &tmp_oname, INSERT);
  overloaded_name *oname = (overloaded_name *) *slot;
  if (!oname)
    {
      *slot = oname = new overloaded_name;
      oname->name = tmp_oname.name;
      oname->arg_types = tmp_oname.arg_types;
      oname->next = NULL;
      oname->first_instance = NULL;
      oname->next_instance_ptr = &oname->first_instance;

      *m_next_overload_ptr = oname;
      m_next_overload_ptr = &oname->next;
    }
  else
    {
      obstack_free (&m_string_obstack, new_name);
      tmp_oname.arg_types.release ();
    }

  return oname;
}

/* Add an instance of ONAME for instruction pattern X.  ITERATORS[I]
   gives the iterator associated with argument I of ONAME.  */

static void
add_overload_instance (overloaded_name *oname, const vec<mapping *> &iterators, rtx x)
{
  /* Create the instance.  */
  overloaded_instance *instance = new overloaded_instance;
  instance->next = NULL;
  instance->arg_values.create (oname->arg_types.length ());
  for (unsigned int i = 0; i < iterators.length (); ++i)
    {
      int value = iterators[i]->current_value->number;
      const char *name = iterators[i]->group->get_c_token (value);
      instance->arg_values.quick_push (name);
    }
  instance->name = XSTR (x, 0);
  instance->insn = x;

  /* Chain it onto the end of ONAME's list.  */
  *oname->next_instance_ptr = instance;
  oname->next_instance_ptr = &instance->next;
}

/* Expand all iterators in the current rtx, which is given as ORIGINAL.
   Build a list of expanded rtxes in the EXPR_LIST pointed to by QUEUE.  */

static void
apply_iterators (rtx original, vec<rtx> *queue)
{
  unsigned int i;
  const char *condition;
  iterator_use *iuse;
  struct mapping *iterator;
  struct map_value *v;
  rtx x;

  if (iterator_uses.is_empty ())
    {
      /* Raise an error if any attributes were used.  */
      apply_attribute_uses ();

      if (named_rtx_p (original) && XSTR (original, 0)[0] == '@')
	fatal_with_file_and_line ("'@' used without iterators");

      queue->safe_push (original);
      return;
    }

  /* Clear out the iterators from the previous run.  */
  FOR_EACH_VEC_ELT (current_iterators, i, iterator)
    iterator->current_value = NULL;
  current_iterators.truncate (0);

  /* Mark the iterators that we need this time.  */
  FOR_EACH_VEC_ELT (iterator_uses, i, iuse)
    iuse->iterator->current_value = iuse->iterator->values;

  /* Get the list of iterators that are in use, preserving the
     definition order within each group.  */
  htab_traverse (modes.iterators, add_current_iterators, NULL);
  htab_traverse (codes.iterators, add_current_iterators, NULL);
  htab_traverse (ints.iterators, add_current_iterators, NULL);
  htab_traverse (substs.iterators, add_current_iterators, NULL);
  gcc_assert (!current_iterators.is_empty ());

  /* Check whether this is a '@' overloaded pattern.  */
  auto_vec<mapping *, 16> iterators;
  overloaded_name *oname
    = rtx_reader_ptr->handle_overloaded_name (original, &iterators);

  for (;;)
    {
      /* Apply the current iterator values.  Accumulate a condition to
	 say when the resulting rtx can be used.  */
      condition = "";
      FOR_EACH_VEC_ELT (iterator_uses, i, iuse)
	{
	  if (iuse->iterator->group == &substs)
	    continue;
	  v = iuse->iterator->current_value;
	  iuse->iterator->group->apply_iterator (iuse->x, iuse->index,
						 v->number);
	  condition = rtx_reader_ptr->join_c_conditions (condition, v->string);
	}
      apply_attribute_uses ();
      x = rtx_reader_ptr->copy_rtx_for_iterators (original);
      add_condition_to_rtx (x, condition);

      /* We apply subst iterator after RTL-template is copied, as during
	 subst-iterator processing, we could add an attribute to the
	 RTL-template, and we don't want to do it in the original one.  */
      FOR_EACH_VEC_ELT (iterator_uses, i, iuse)
	{
	  v = iuse->iterator->current_value;
	  if (iuse->iterator->group == &substs)
	    {
	      iuse->x = x;
	      iuse->index = 0;
	      current_iterator_name = iuse->iterator->name;
	      iuse->iterator->group->apply_iterator (iuse->x, iuse->index,
						     v->number);
	    }
	}

      if (oname)
	add_overload_instance (oname, iterators, x);

      /* Add the new rtx to the end of the queue.  */
      queue->safe_push (x);

      /* Lexicographically increment the iterator value sequence.
	 That is, cycle through iterator values, starting from the right,
	 and stopping when one of them doesn't wrap around.  */
      i = current_iterators.length ();
      for (;;)
	{
	  if (i == 0)
	    return;
	  i--;
	  iterator = current_iterators[i];
	  iterator->current_value = iterator->current_value->next;
	  if (iterator->current_value)
	    break;
	  iterator->current_value = iterator->values;
	}
    }
}
#endif /* #ifdef GENERATOR_FILE */

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
  m->values = 0;
  m->current_value = NULL;

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
  modes.type = "machine_mode";
  modes.find_builtin = find_mode;
  modes.apply_iterator = apply_mode_iterator;
  modes.get_c_token = get_mode_token;

  codes.attrs = htab_create (13, leading_string_hash, leading_string_eq_p, 0);
  codes.iterators = htab_create (13, leading_string_hash,
				 leading_string_eq_p, 0);
  codes.type = "rtx_code";
  codes.find_builtin = find_code;
  codes.apply_iterator = apply_code_iterator;
  codes.get_c_token = get_code_token;

  ints.attrs = htab_create (13, leading_string_hash, leading_string_eq_p, 0);
  ints.iterators = htab_create (13, leading_string_hash,
				 leading_string_eq_p, 0);
  ints.type = "int";
  ints.find_builtin = find_int;
  ints.apply_iterator = apply_int_iterator;
  ints.get_c_token = get_int_token;

  substs.attrs = htab_create (13, leading_string_hash, leading_string_eq_p, 0);
  substs.iterators = htab_create (13, leading_string_hash,
				 leading_string_eq_p, 0);
  substs.type = "int";
  substs.find_builtin = find_int; /* We don't use it, anyway.  */
#ifdef GENERATOR_FILE
  substs.apply_iterator = apply_subst_iterator;
#endif
  substs.get_c_token = get_int_token;

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


#ifdef GENERATOR_FILE
/* Process a define_conditions directive, starting with the optional
   space after the "define_conditions".  The directive looks like this:

     (define_conditions [
        (number "string")
        (number "string")
        ...
     ])

   It's not intended to appear in machine descriptions.  It is
   generated by (the program generated by) genconditions.cc, and
   slipped in at the beginning of the sequence of MD files read by
   most of the other generators.  */
void
md_reader::read_conditions ()
{
  int c;

  require_char_ws ('[');

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

      require_char_ws ('"');
      expr = read_quoted_string ();

      require_char_ws (')');

      add_c_test (expr, value);
    }
}
#endif /* #ifdef GENERATOR_FILE */

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
      {
        valid = 0;
	break;
      }
  if (!valid)
    fatal_with_file_and_line ("invalid decimal constant \"%s\"\n", string);
}

static void
validate_const_wide_int (const char *string)
{
  const char *cp;
  int valid = 1;

  cp = string;
  while (*cp && ISSPACE (*cp))
    cp++;
  /* Skip the leading 0x.  */
  if (cp[0] == '0' || cp[1] == 'x')
    cp += 2;
  else
    valid = 0;
  if (*cp == 0)
    valid = 0;
  for (; *cp; cp++)
    if (! ISXDIGIT (*cp))
      valid = 0;
  if (!valid)
    fatal_with_file_and_line ("invalid hex constant \"%s\"\n", string);
}

/* Record that X uses iterator ITERATOR.  If the use is in an operand
   of X, INDEX is the index of that operand, otherwise it is ignored.  */

static void
record_iterator_use (struct mapping *iterator, rtx x, unsigned int index)
{
  struct iterator_use iuse = {iterator, x, index};
  iterator_uses.safe_push (iuse);
}

/* Record that X uses attribute VALUE at location LOC, where VALUE must
   match a built-in value from group GROUP.  If the use is in an operand
   of X, INDEX is the index of that operand, otherwise it is ignored.  */

static void
record_attribute_use (struct iterator_group *group, file_location loc, rtx x,
		      unsigned int index, const char *value)
{
  struct attribute_use ause = {group, loc, value, x, index};
  attribute_uses.safe_push (ause);
}

/* Interpret NAME as either a built-in value, iterator or attribute
   for group GROUP.  X and INDEX are the values to pass to GROUP's
   apply_iterator callback.  LOC is the location of the use.  */

void
md_reader::record_potential_iterator_use (struct iterator_group *group,
					  file_location loc,
					  rtx x, unsigned int index,
					  const char *name)
{
  struct mapping *m;
  size_t len;

  len = strlen (name);
  if (name[0] == '<' && name[len - 1] == '>')
    {
      /* Copy the attribute string into permanent storage, without the
	 angle brackets around it.  */
      obstack_grow0 (&m_string_obstack, name + 1, len - 2);
      record_attribute_use (group, loc, x, index,
			    XOBFINISH (&m_string_obstack, char *));
    }
  else
    {
      m = (struct mapping *) htab_find (group->iterators, &name);
      if (m != 0)
	record_iterator_use (m, x, index);
      else
	group->apply_iterator (x, index, group->find_builtin (name));
    }
}

#ifdef GENERATOR_FILE

/* Finish reading a declaration of the form:

       (define... <name> [<value1> ... <valuen>])

   from the MD file, where each <valuei> is either a bare symbol name or a
   "(<name> <string>)" pair.  The "(define..." part has already been read.

   Represent the declaration as a "mapping" structure; add it to TABLE
   (which belongs to GROUP) and return it.  */

struct mapping *
md_reader::read_mapping (struct iterator_group *group, htab_t table)
{
  struct md_name name;
  struct mapping *m;
  struct map_value **end_ptr;
  const char *string;
  int number, c;

  /* Read the mapping name and create a structure for it.  */
  read_name (&name);
  m = add_mapping (group, table, name.string);

  require_char_ws ('[');

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
	  require_char_ws (')');
	}
      number = group->find_builtin (name.string);
      end_ptr = add_map_value (end_ptr, number, string);
      c = read_skip_spaces ();
    }
  while (c != ']');

  return m;
}

/* For iterator with name ATTR_NAME generate define_attr with values
   'yes' and 'no'.  This attribute is used to mark templates to which
   define_subst ATTR_NAME should be applied.  This attribute is set and
   defined implicitly and automatically.  */
static void
add_define_attr_for_define_subst (const char *attr_name, vec<rtx> *queue)
{
  rtx const_str, return_rtx;

  return_rtx = rtx_alloc (DEFINE_ATTR);
  PUT_CODE (return_rtx, DEFINE_ATTR);

  const_str = rtx_alloc (CONST_STRING);
  PUT_CODE (const_str, CONST_STRING);
  XSTR (const_str, 0) = xstrdup ("no");

  XSTR (return_rtx, 0) = xstrdup (attr_name);
  XSTR (return_rtx, 1) = xstrdup ("no,yes");
  XEXP (return_rtx, 2) = const_str;

  queue->safe_push (return_rtx);
}

/* This routine generates DEFINE_SUBST_ATTR expression with operands
   ATTR_OPERANDS and places it to QUEUE.  */
static void
add_define_subst_attr (const char **attr_operands, vec<rtx> *queue)
{
  rtx return_rtx;
  int i;

  return_rtx = rtx_alloc (DEFINE_SUBST_ATTR);
  PUT_CODE (return_rtx, DEFINE_SUBST_ATTR);

  for (i = 0; i < 4; i++)
    XSTR (return_rtx, i) = xstrdup (attr_operands[i]);

  queue->safe_push (return_rtx);
}

/* Read define_subst_attribute construction.  It has next form:
	(define_subst_attribute <attribute_name> <iterator_name> <value1> <value2>)
   Attribute is substituted with value1 when no subst is applied and with
   value2 in the opposite case.
   Attributes are added to SUBST_ATTRS_TABLE.
   In case the iterator is encountered for the first time, it's added to
   SUBST_ITERS_TABLE.  Also, implicit define_attr is generated.  */

static void
read_subst_mapping (htab_t subst_iters_table, htab_t subst_attrs_table,
		    vec<rtx> *queue)
{
  struct mapping *m;
  struct map_value **end_ptr;
  const char *attr_operands[4];
  int i;

  for (i = 0; i < 4; i++)
    attr_operands[i] = rtx_reader_ptr->read_string (false);

  add_define_subst_attr (attr_operands, queue);

  bind_subst_iter_and_attr (attr_operands[1], attr_operands[0]);

  m = (struct mapping *) htab_find (substs.iterators, &attr_operands[1]);
  if (!m)
    {
      m = add_mapping (&substs, subst_iters_table, attr_operands[1]);
      end_ptr = &m->values;
      end_ptr = add_map_value (end_ptr, 1, "");
      add_map_value (end_ptr, 2, "");

      add_define_attr_for_define_subst (attr_operands[1], queue);
    }

  m = add_mapping (&substs, subst_attrs_table, attr_operands[0]);
  end_ptr = &m->values;
  end_ptr = add_map_value (end_ptr, 1, attr_operands[2]);
  add_map_value (end_ptr, 2, attr_operands[3]);
}

/* Check newly-created code iterator ITERATOR to see whether every code has the
   same format.  */

static void
check_code_iterator (struct mapping *iterator)
{
  struct map_value *v;
  enum rtx_code bellwether;

  bellwether = (enum rtx_code) iterator->values->number;
  for (v = iterator->values->next; v != 0; v = v->next)
    if (strcmp (GET_RTX_FORMAT (bellwether), GET_RTX_FORMAT (v->number)) != 0)
      fatal_with_file_and_line ("code iterator `%s' combines "
				"`%s' and `%s', which have different "
				"rtx formats", iterator->name,
				GET_RTX_NAME (bellwether),
				GET_RTX_NAME (v->number));
}

/* Check that all values of attribute ATTR are rtx codes that have a
   consistent format.  Return a representative code.  */

static rtx_code
check_code_attribute (mapping *attr)
{
  rtx_code bellwether = UNKNOWN;
  for (map_value *v = attr->values; v != 0; v = v->next)
    {
      rtx_code code = maybe_find_code (v->string);
      if (code == UNKNOWN)
	fatal_with_file_and_line ("code attribute `%s' contains "
				  "unrecognized rtx code `%s'",
				  attr->name, v->string);
      if (bellwether == UNKNOWN)
	bellwether = code;
      else if (strcmp (GET_RTX_FORMAT (bellwether),
		       GET_RTX_FORMAT (code)) != 0)
	fatal_with_file_and_line ("code attribute `%s' combines "
				  "`%s' and `%s', which have different "
				  "rtx formats", attr->name,
				  GET_RTX_NAME (bellwether),
				  GET_RTX_NAME (code));
    }
  return bellwether;
}

/* Read an rtx-related declaration from the MD file, given that it
   starts with directive name RTX_NAME.  Return true if it expands to
   one or more rtxes (as defined by rtx.def).  When returning true,
   store the list of rtxes as an EXPR_LIST in *X.  */

bool
rtx_reader::read_rtx (const char *rtx_name, vec<rtx> *rtxen)
{
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
  if (strcmp (rtx_name, "define_int_attr") == 0)
    {
      read_mapping (&ints, ints.attrs);
      return false;
    }
  if (strcmp (rtx_name, "define_int_iterator") == 0)
    {
      read_mapping (&ints, ints.iterators);
      return false;
    }
  if (strcmp (rtx_name, "define_subst_attr") == 0)
    {
      read_subst_mapping (substs.iterators, substs.attrs, rtxen);

      /* READ_SUBST_MAPPING could generate a new DEFINE_ATTR.  Return
	 TRUE to process it.  */
      return true;
    }

  apply_iterators (rtx_reader_ptr->read_rtx_code (rtx_name), rtxen);
  iterator_uses.truncate (0);
  attribute_uses.truncate (0);

  return true;
}

#endif /* #ifdef GENERATOR_FILE */

/* Do one-time initialization.  */

static void
one_time_initialization (void)
{
  static bool initialized = false;

  if (!initialized)
    {
      initialize_iterators ();
      initialized = true;
    }
}

/* Consume characters until encountering a character in TERMINATOR_CHARS,
   consuming the terminator character if CONSUME_TERMINATOR is true.
   Return all characters before the terminator as an allocated buffer.  */

char *
rtx_reader::read_until (const char *terminator_chars, bool consume_terminator)
{
  int ch = read_skip_spaces ();
  unread_char (ch);
  auto_vec<char> buf;
  while (1)
    {
      ch = read_char ();
      if (strchr (terminator_chars, ch))
	{
	  if (!consume_terminator)
	    unread_char (ch);
	  break;
	}
      buf.safe_push (ch);
    }
  buf.safe_push ('\0');
  return xstrdup (buf.address ());
}

/* Subroutine of read_rtx_code, for parsing zero or more flags.  */

static void
read_flags (rtx return_rtx)
{
  while (1)
    {
      int ch = read_char ();
      if (ch != '/')
	{
	  unread_char (ch);
	  break;
	}

      int flag_char = read_char ();
      switch (flag_char)
	{
	  case 's':
	    RTX_FLAG (return_rtx, in_struct) = 1;
	    break;
	  case 'v':
	    RTX_FLAG (return_rtx, volatil) = 1;
	    break;
	  case 'u':
	    RTX_FLAG (return_rtx, unchanging) = 1;
	    break;
	  case 'f':
	    RTX_FLAG (return_rtx, frame_related) = 1;
	    break;
	  case 'j':
	    RTX_FLAG (return_rtx, jump) = 1;
	    break;
	  case 'c':
	    RTX_FLAG (return_rtx, call) = 1;
	    break;
	  case 'i':
	    RTX_FLAG (return_rtx, return_val) = 1;
	    break;
	  default:
	    fatal_with_file_and_line ("unrecognized flag: `%c'", flag_char);
	}
    }
}

/* Return the numeric value n for GET_REG_NOTE_NAME (n) for STRING,
   or fail if STRING isn't recognized.  */

static int
parse_reg_note_name (const char *string)
{
  for (int i = 0; i < REG_NOTE_MAX; i++)
    if (strcmp (string, GET_REG_NOTE_NAME (i)) == 0)
      return i;
  fatal_with_file_and_line ("unrecognized REG_NOTE name: `%s'", string);
}

/* Allocate an rtx for code NAME.  If NAME is a code iterator or code
   attribute, record its use for later and use one of its possible
   values as an interim rtx code.  */

rtx
rtx_reader::rtx_alloc_for_name (const char *name)
{
#ifdef GENERATOR_FILE
  size_t len = strlen (name);
  if (name[0] == '<' && name[len - 1] == '>')
    {
      /* Copy the attribute string into permanent storage, without the
	 angle brackets around it.  */
      obstack *strings = get_string_obstack ();
      obstack_grow0 (strings, name + 1, len - 2);
      char *deferred_name = XOBFINISH (strings, char *);

      /* Find the name of the attribute.  */
      const char *attr = strchr (deferred_name, ':');
      if (!attr)
	attr = deferred_name;

      /* Find the attribute itself.  */
      mapping *m = (mapping *) htab_find (codes.attrs, &attr);
      if (!m)
	fatal_with_file_and_line ("unknown code attribute `%s'", attr);

      /* Pick the first possible code for now, and record the attribute
	 use for later.  */
      rtx x = rtx_alloc (check_code_attribute (m));
      record_attribute_use (&codes, get_current_location (),
			    x, 0, deferred_name);
      return x;
    }

  mapping *iterator = (mapping *) htab_find (codes.iterators, &name);
  if (iterator != 0)
    {
      /* Pick the first possible code for now, and record the iterator
	 use for later.  */
      rtx x = rtx_alloc (rtx_code (iterator->values->number));
      record_iterator_use (iterator, x, 0);
      return x;
    }
#endif

  return rtx_alloc (rtx_code (codes.find_builtin (name)));
}

/* Subroutine of read_rtx and read_nested_rtx.  CODE_NAME is the name of
   either an rtx code or a code iterator.  Parse the rest of the rtx and
   return it.  */

rtx
rtx_reader::read_rtx_code (const char *code_name)
{
  RTX_CODE code;
  const char *format_ptr;
  struct md_name name;
  rtx return_rtx;
  int c;
  long reuse_id = -1;

  /* Linked list structure for making RTXs: */
  struct rtx_list
    {
      struct rtx_list *next;
      rtx value;		/* Value of this node.  */
    };

  /* Handle reuse_rtx ids e.g. "(0|scratch:DI)".  */
  if (ISDIGIT (code_name[0]))
    {
      reuse_id = atoi (code_name);
      while (char ch = *code_name++)
	if (ch == '|')
	  break;
    }

  /* Handle "reuse_rtx".  */
  if (strcmp (code_name, "reuse_rtx") == 0)
    {
      read_name (&name);
      unsigned idx = atoi (name.string);
      /* Look it up by ID.  */
      gcc_assert (idx < m_reuse_rtx_by_id.length ());
      return_rtx = m_reuse_rtx_by_id[idx];
      return return_rtx;
    }

  /* Handle "const_double_zero".  */
  if (strcmp (code_name, "const_double_zero") == 0)
    {
      code = CONST_DOUBLE;
      return_rtx = rtx_alloc (code);
      memset (return_rtx, 0, RTX_CODE_SIZE (code));
      PUT_CODE (return_rtx, code);
      c = read_skip_spaces ();
      if (c == ':')
	{
	  file_location loc = read_name (&name);
	  record_potential_iterator_use (&modes, loc, return_rtx, 0,
					 name.string);
	}
      else
	unread_char (c);
      return return_rtx;
    }

  /* If we end up with an insn expression then we free this space below.  */
  return_rtx = rtx_alloc_for_name (code_name);
  code = GET_CODE (return_rtx);
  format_ptr = GET_RTX_FORMAT (code);
  memset (return_rtx, 0, RTX_CODE_SIZE (code));
  PUT_CODE (return_rtx, code);

  if (reuse_id != -1)
    {
      /* Store away for later reuse.  */
      m_reuse_rtx_by_id.safe_grow_cleared (reuse_id + 1, true);
      m_reuse_rtx_by_id[reuse_id] = return_rtx;
    }

  /* Check for flags. */
  read_flags (return_rtx);

  /* Read REG_NOTE names for EXPR_LIST and INSN_LIST.  */
  if ((GET_CODE (return_rtx) == EXPR_LIST
       || GET_CODE (return_rtx) == INSN_LIST
       || GET_CODE (return_rtx) == INT_LIST)
      && !m_in_call_function_usage)
    {
      char ch = read_char ();
      if (ch == ':')
	{
	  read_name (&name);
	  PUT_MODE_RAW (return_rtx,
			(machine_mode)parse_reg_note_name (name.string));
	}
      else
	unread_char (ch);
    }

  /* If what follows is `: mode ', read it and
     store the mode in the rtx.  */

  c = read_skip_spaces ();
  if (c == ':')
    {
      file_location loc = read_name (&name);
      record_potential_iterator_use (&modes, loc, return_rtx, 0, name.string);
    }
  else
    unread_char (c);

  if (INSN_CHAIN_CODE_P (code))
    {
      read_name (&name);
      INSN_UID (return_rtx) = atoi (name.string);
    }

  /* Use the format_ptr to parse the various operands of this rtx.  */
  for (int idx = 0; format_ptr[idx] != 0; idx++)
    return_rtx = read_rtx_operand (return_rtx, idx);

  /* Handle any additional information that after the regular fields
     (e.g. when parsing function dumps).  */
  handle_any_trailing_information (return_rtx);

  if (CONST_WIDE_INT_P (return_rtx))
    {
      read_name (&name);
      validate_const_wide_int (name.string);
      {
	const char *s = name.string;
	int len;
	int index = 0;
	int gs = HOST_BITS_PER_WIDE_INT/4;
	int pos;
	char * buf = XALLOCAVEC (char, gs + 1);
	unsigned HOST_WIDE_INT wi;
	int wlen;

	/* Skip the leading spaces.  */
	while (*s && ISSPACE (*s))
	  s++;

	/* Skip the leading 0x.  */
	gcc_assert (s[0] == '0');
	gcc_assert (s[1] == 'x');
	s += 2;

	len = strlen (s);
	pos = len - gs;
	wlen = (len + gs - 1) / gs;	/* Number of words needed */

	return_rtx = const_wide_int_alloc (wlen);

	while (pos > 0)
	  {
#if HOST_BITS_PER_WIDE_INT == 64
	    sscanf (s + pos, "%16" HOST_WIDE_INT_PRINT "x", &wi);
#else
	    sscanf (s + pos, "%8" HOST_WIDE_INT_PRINT "x", &wi);
#endif
	    CWI_ELT (return_rtx, index++) = wi;
	    pos -= gs;
	  }
	strncpy (buf, s, gs - pos);
	buf [gs - pos] = 0;
	sscanf (buf, "%" HOST_WIDE_INT_PRINT "x", &wi);
	CWI_ELT (return_rtx, index++) = wi;
	/* TODO: After reading, do we want to canonicalize with:
	   value = lookup_const_wide_int (value); ? */
      }
    }

  c = read_skip_spaces ();
  /* Syntactic sugar for AND and IOR, allowing Lisp-like
     arbitrary number of arguments for them.  */
  if (c == '('
      && (GET_CODE (return_rtx) == AND
	  || GET_CODE (return_rtx) == IOR))
    return read_rtx_variadic (return_rtx);

  unread_char (c);
  return return_rtx;
}

/* Subroutine of read_rtx_code.  Parse operand IDX within RETURN_RTX,
   based on the corresponding format character within GET_RTX_FORMAT
   for the GET_CODE (RETURN_RTX), and return RETURN_RTX.
   This is a virtual function, so that function_reader can override
   some parsing, and potentially return a different rtx.  */

rtx
rtx_reader::read_rtx_operand (rtx return_rtx, int idx)
{
  RTX_CODE code = GET_CODE (return_rtx);
  const char *format_ptr = GET_RTX_FORMAT (code);
  int c;
  struct md_name name;

  switch (format_ptr[idx])
    {
      /* 0 means a field for internal use only.
	 Don't expect it to be present in the input.  */
    case '0':
      if (code == REG)
	ORIGINAL_REGNO (return_rtx) = REGNO (return_rtx);
      break;

    case 'e':
      XEXP (return_rtx, idx) = read_nested_rtx ();
      break;

    case 'u':
      XEXP (return_rtx, idx) = read_nested_rtx ();
      break;

    case 'V':
      /* 'V' is an optional vector: if a closeparen follows,
	 just store NULL for this element.  */
      c = read_skip_spaces ();
      unread_char (c);
      if (c == ')')
	{
	  XVEC (return_rtx, idx) = 0;
	  break;
	}
      /* Now process the vector.  */
      /* FALLTHRU */

    case 'E':
      {
	/* Obstack to store scratch vector in.  */
	struct obstack vector_stack;
	int list_counter = 0;
	rtvec return_vec = NULL_RTVEC;
	rtx saved_rtx = NULL_RTX;

	require_char_ws ('[');

	/* Add expressions to a list, while keeping a count.  */
	obstack_init (&vector_stack);
	while ((c = read_skip_spaces ()) && c != ']')
	  {
	    if (c == EOF)
	      fatal_expected_char (']', c);
	    unread_char (c);

	    rtx value;
	    int repeat_count = 1;
	    if (c == 'r')
	      {
		/* Process "repeated xN" directive.  */
		read_name (&name);
		if (strcmp (name.string, "repeated"))
		  fatal_with_file_and_line ("invalid directive \"%s\"\n",
					    name.string);
		read_name (&name);
		if (!sscanf (name.string, "x%d", &repeat_count))
		  fatal_with_file_and_line ("invalid repeat count \"%s\"\n",
					    name.string);

		/* We already saw one of the instances.  */
		repeat_count--;
		value = saved_rtx;
	      }
	    else
	      value = read_nested_rtx ();

	    for (; repeat_count > 0; repeat_count--)
	      {
		list_counter++;
		obstack_ptr_grow (&vector_stack, value);
	      }
	    saved_rtx = value;
	  }
	if (list_counter > 0)
	  {
	    return_vec = rtvec_alloc (list_counter);
	    memcpy (&return_vec->elem[0], obstack_finish (&vector_stack),
		    list_counter * sizeof (rtx));
	  }
	else if (format_ptr[idx] == 'E')
	  fatal_with_file_and_line ("vector must have at least one element");
	XVEC (return_rtx, idx) = return_vec;
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
	    XSTR (return_rtx, idx) = (format_ptr[idx] == 'S' ? NULL : "");
	    break;
	  }

	/* The output template slot of a DEFINE_INSN, DEFINE_INSN_AND_SPLIT,
	   DEFINE_INSN_AND_REWRITE or DEFINE_PEEPHOLE automatically
	   gets a star inserted as its first character, if it is
	   written with a brace block instead of a string constant.  */
	star_if_braced = (format_ptr[idx] == 'T');

	stringbuf = read_string (star_if_braced);
	if (!stringbuf)
	  break;

#ifdef GENERATOR_FILE
	/* For insn patterns, we want to provide a default name
	   based on the file and line, like "*foo.md:12", if the
	   given name is blank.  These are only for define_insn and
	   define_insn_and_split, to aid debugging.  */
	if (*stringbuf == '\0'
	    && idx == 0
	    && (GET_CODE (return_rtx) == DEFINE_INSN
		|| GET_CODE (return_rtx) == DEFINE_INSN_AND_SPLIT
		|| GET_CODE (return_rtx) == DEFINE_INSN_AND_REWRITE))
	  {
	    const char *old_stringbuf = stringbuf;
	    struct obstack *string_obstack = get_string_obstack ();
	    char line_name[20];
	    const char *read_md_filename = get_filename ();
	    const char *fn = (read_md_filename ? read_md_filename : "rtx");
	    const char *slash;
	    for (slash = fn; *slash; slash ++)
	      if (*slash == '/' || *slash == '\\' || *slash == ':')
		fn = slash + 1;
	    obstack_1grow (string_obstack, '*');
	    obstack_grow (string_obstack, fn, strlen (fn));
	    sprintf (line_name, ":%d", get_lineno ());
	    obstack_grow (string_obstack, line_name, strlen (line_name)+1);
	    stringbuf = XOBFINISH (string_obstack, char *);
	    copy_md_ptr_loc (stringbuf, old_stringbuf);
	  }

	/* Find attr-names in the string.  */
	char *str;
	char *start, *end, *ptr;
	char tmpstr[256];
	ptr = &tmpstr[0];
	end = stringbuf;
	while ((start = strchr (end, '<')) && (end  = strchr (start, '>')))
	  {
	    if ((end - start - 1 > 0)
		&& (end - start - 1 < (int)sizeof (tmpstr)))
	      {
		strncpy (tmpstr, start+1, end-start-1);
		tmpstr[end-start-1] = 0;
		end++;
	      }
	    else
	      break;
	    struct mapping *m
	      = (struct mapping *) htab_find (substs.attrs, &ptr);
	    if (m != 0)
	      {
		/* Here we should find linked subst-iter.  */
		str = find_subst_iter_by_attr (ptr);
		if (str)
		  m = (struct mapping *) htab_find (substs.iterators, &str);
		else
		  m = 0;
	      }
	    if (m != 0)
	      record_iterator_use (m, return_rtx, 0);
	  }
#endif /* #ifdef GENERATOR_FILE */

	const char *string_ptr = finalize_string (stringbuf);

	if (star_if_braced)
	  XTMPL (return_rtx, idx) = string_ptr;
	else
	  XSTR (return_rtx, idx) = string_ptr;
      }
      break;

    case 'i':
    case 'n':
    case 'w':
    case 'p':
      {
	/* Can be an iterator or an integer constant.  */
	file_location loc = read_name (&name);
	record_potential_iterator_use (&ints, loc, return_rtx, idx,
				       name.string);
	break;
      }

    case 'r':
      read_name (&name);
      validate_const_int (name.string);
      set_regno_raw (return_rtx, atoi (name.string), 1);
      REG_ATTRS (return_rtx) = NULL;
      break;

    default:
      gcc_unreachable ();
    }

  return return_rtx;
}

/* Read a nested rtx construct from the MD file and return it.  */

rtx
rtx_reader::read_nested_rtx ()
{
  struct md_name name;
  rtx return_rtx;

  /* In compact dumps, trailing "(nil)" values can be omitted.
     Handle such dumps.  */
  if (peek_char () == ')')
    return NULL_RTX;

  require_char_ws ('(');

  read_name (&name);
  if (strcmp (name.string, "nil") == 0)
    return_rtx = NULL;
  else
    return_rtx = read_rtx_code (name.string);

  require_char_ws (')');

  return_rtx = postprocess (return_rtx);

  return return_rtx;
}

/* Mutually recursive subroutine of read_rtx which reads
   (thing x1 x2 x3 ...) and produces RTL as if
   (thing x1 (thing x2 (thing x3 ...)))  had been written.
   When called, FORM is (thing x1 x2), and the file position
   is just past the leading parenthesis of x3.  Only works
   for THINGs which are dyadic expressions, e.g. AND, IOR.  */
rtx
rtx_reader::read_rtx_variadic (rtx form)
{
  char c = '(';
  rtx p = form, q;

  do
    {
      unread_char (c);

      q = rtx_alloc (GET_CODE (p));
      PUT_MODE (q, GET_MODE (p));

      XEXP (q, 0) = XEXP (p, 1);
      XEXP (q, 1) = read_nested_rtx ();

      XEXP (p, 1) = q;
      p = q;
      c = read_skip_spaces ();
    }
  while (c == '(');
  unread_char (c);
  return form;
}

/* Constructor for class rtx_reader.  */

rtx_reader::rtx_reader (bool compact)
: md_reader (compact),
  m_in_call_function_usage (false)
{
  /* Set the global singleton pointer.  */
  rtx_reader_ptr = this;

  one_time_initialization ();
}

/* Destructor for class rtx_reader.  */

rtx_reader::~rtx_reader ()
{
  /* Clear the global singleton pointer.  */
  rtx_reader_ptr = NULL;
}
