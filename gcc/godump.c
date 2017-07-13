/* Output Go language descriptions of types.
   Copyright (C) 2008-2017 Free Software Foundation, Inc.
   Written by Ian Lance Taylor <iant@google.com>.

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

/* This file is used during the build process to emit Go language
   descriptions of declarations from C header files.  It uses the
   debug info hooks to emit the descriptions.  The Go language
   descriptions then become part of the Go runtime support
   library.

   All global names are output with a leading underscore, so that they
   are all hidden in Go.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "diagnostic-core.h"
#include "debug.h"
#include "stor-layout.h"

/* We dump this information from the debug hooks.  This gives us a
   stable and maintainable API to hook into.  In order to work
   correctly when -g is used, we build our own hooks structure which
   wraps the hooks we need to change.  */

/* Our debug hooks.  This is initialized by dump_go_spec_init.  */

static struct gcc_debug_hooks go_debug_hooks;

/* The real debug hooks.  */

static const struct gcc_debug_hooks *real_debug_hooks;

/* The file where we should write information.  */

static FILE *go_dump_file;

/* A queue of decls to output.  */

static GTY(()) vec<tree, va_gc> *queue;

/* A hash table of macros we have seen.  */

static htab_t macro_hash;

/* The type of a value in macro_hash.  */

struct macro_hash_value
{
  /* The name stored in the hash table.  */
  char *name;
  /* The value of the macro.  */
  char *value;
};

/* Returns the number of units necessary to represent an integer with the given
   PRECISION (in bits).  */

static inline unsigned int
precision_to_units (unsigned int precision)
{
  return (precision + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
}

/* Calculate the hash value for an entry in the macro hash table.  */

static hashval_t
macro_hash_hashval (const void *val)
{
  const struct macro_hash_value *mhval = (const struct macro_hash_value *) val;
  return htab_hash_string (mhval->name);
}

/* Compare values in the macro hash table for equality.  */

static int
macro_hash_eq (const void *v1, const void *v2)
{
  const struct macro_hash_value *mhv1 = (const struct macro_hash_value *) v1;
  const struct macro_hash_value *mhv2 = (const struct macro_hash_value *) v2;
  return strcmp (mhv1->name, mhv2->name) == 0;
}

/* Free values deleted from the macro hash table.  */

static void
macro_hash_del (void *v)
{
  struct macro_hash_value *mhv = (struct macro_hash_value *) v;
  XDELETEVEC (mhv->name);
  XDELETEVEC (mhv->value);
  XDELETE (mhv);
}

/* For the string hash tables.  */

static int
string_hash_eq (const void *y1, const void *y2)
{
  return strcmp ((const char *) y1, (const char *) y2) == 0;
}

/* A macro definition.  */

static void
go_define (unsigned int lineno, const char *buffer)
{
  const char *p;
  const char *name_end;
  size_t out_len;
  char *out_buffer;
  char *q;
  bool saw_operand;
  bool need_operand;
  struct macro_hash_value *mhval;
  char *copy;
  hashval_t hashval;
  void **slot;

  real_debug_hooks->define (lineno, buffer);

  /* Skip macro functions.  */
  for (p = buffer; *p != '\0' && *p != ' '; ++p)
    if (*p == '(')
      return;

  if (*p == '\0')
    return;

  name_end = p;

  ++p;
  if (*p == '\0')
    return;

  copy = XNEWVEC (char, name_end - buffer + 1);
  memcpy (copy, buffer, name_end - buffer);
  copy[name_end - buffer] = '\0';

  mhval = XNEW (struct macro_hash_value);
  mhval->name = copy;
  mhval->value = NULL;

  hashval = htab_hash_string (copy);
  slot = htab_find_slot_with_hash (macro_hash, mhval, hashval, NO_INSERT);

  /* For simplicity, we force all names to be hidden by adding an
     initial underscore, and let the user undo this as needed.  */
  out_len = strlen (p) * 2 + 1;
  out_buffer = XNEWVEC (char, out_len);
  q = out_buffer;
  saw_operand = false;
  need_operand = false;
  while (*p != '\0')
    {
      switch (*p)
	{
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
	case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
	case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
	case 'Y': case 'Z':
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
	case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
	case 's': case 't': case 'u': case 'v': case 'w': case 'x':
	case 'y': case 'z':
	case '_':
	  {
	    /* The start of an identifier.  Technically we should also
	       worry about UTF-8 identifiers, but they are not a
	       problem for practical uses of -fdump-go-spec so we
	       don't worry about them.  */
	    const char *start;
	    char *n;
	    struct macro_hash_value idval;

	    if (saw_operand)
	      goto unknown;

	    start = p;
	    while (ISALNUM (*p) || *p == '_')
	      ++p;
	    n = XALLOCAVEC (char, p - start + 1);
	    memcpy (n, start, p - start);
	    n[p - start] = '\0';
	    idval.name = n;
	    idval.value = NULL;
	    if (htab_find (macro_hash, &idval) == NULL)
	      {
		/* This is a reference to a name which was not defined
		   as a macro.  */
		goto unknown;
	      }

	    *q++ = '_';
	    memcpy (q, start, p - start);
	    q += p - start;

	    saw_operand = true;
	    need_operand = false;
	  }
	  break;

	case '.':
	  if (!ISDIGIT (p[1]))
	    goto unknown;
	  /* Fall through.  */
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	  {
	    const char *start;
	    bool is_hex;

	    start = p;
	    is_hex = false;
	    if (*p == '0' && (p[1] == 'x' || p[1] == 'X'))
	      {
		p += 2;
		is_hex = true;
	      }
	    while (ISDIGIT (*p) || *p == '.' || *p == 'e' || *p == 'E'
		   || (is_hex
		       && ((*p >= 'a' && *p <= 'f')
			   || (*p >= 'A' && *p <= 'F'))))
	      ++p;
	    memcpy (q, start, p - start);
	    q += p - start;
	    while (*p == 'u' || *p == 'U' || *p == 'l' || *p == 'L'
		   || *p == 'f' || *p == 'F'
		   || *p == 'd' || *p == 'D')
	      {
		/* Go doesn't use any of these trailing type
		   modifiers.  */
		++p;
	      }

	    /* We'll pick up the exponent, if any, as an
	       expression.  */

	    saw_operand = true;
	    need_operand = false;
	  }
	  break;

	case ' ': case '\t':
	  *q++ = *p++;
	  break;

	case '(':
	  /* Always OK, not part of an operand, presumed to start an
	     operand.  */
	  *q++ = *p++;
	  saw_operand = false;
	  need_operand = false;
	  break;

	case ')':
	  /* OK if we don't need an operand, and presumed to indicate
	     an operand.  */
	  if (need_operand)
	    goto unknown;
	  *q++ = *p++;
	  saw_operand = true;
	  break;

	case '+': case '-':
	  /* Always OK, but not part of an operand.  */
	  *q++ = *p++;
	  saw_operand = false;
	  break;

	case '*': case '/': case '%': case '|': case '&': case '^':
	  /* Must be a binary operator.  */
	  if (!saw_operand)
	    goto unknown;
	  *q++ = *p++;
	  saw_operand = false;
	  need_operand = true;
	  break;

	case '=':
	  *q++ = *p++;
	  if (*p != '=')
	    goto unknown;
	  /* Must be a binary operator.  */
	  if (!saw_operand)
	    goto unknown;
	  *q++ = *p++;
	  saw_operand = false;
	  need_operand = true;
	  break;

	case '!':
	  *q++ = *p++;
	  if (*p == '=')
	    {
	      /* Must be a binary operator.  */
	      if (!saw_operand)
		goto unknown;
	      *q++ = *p++;
	      saw_operand = false;
	      need_operand = true;
	    }
	  else
	    {
	      /* Must be a unary operator.  */
	      if (saw_operand)
		goto unknown;
	      need_operand = true;
	    }
	  break;

	case '<': case '>':
	  /* Must be a binary operand, may be << or >> or <= or >=.  */
	  if (!saw_operand)
	    goto unknown;
	  *q++ = *p++;
	  if (*p == *(p - 1) || *p == '=')
	    *q++ = *p++;
	  saw_operand = false;
	  need_operand = true;
	  break;

	case '~':
	  /* Must be a unary operand, must be translated for Go.  */
	  if (saw_operand)
	    goto unknown;
	  *q++ = '^';
	  p++;
	  need_operand = true;
	  break;

	case '"':
	case '\'':
	  {
	    char quote;
	    int count;

	    if (saw_operand)
	      goto unknown;
	    quote = *p;
	    *q++ = *p++;
	    count = 0;
	    while (*p != quote)
	      {
		int c;

		if (*p == '\0')
		  goto unknown;

		++count;

		if (*p != '\\')
		  {
		    *q++ = *p++;
		    continue;
		  }

		*q++ = *p++;
		switch (*p)
		  {
		  case '0': case '1': case '2': case '3':
		  case '4': case '5': case '6': case '7':
		    c = 0;
		    while (*p >= '0' && *p <= '7')
		      {
			*q++ = *p++;
			++c;
		      }
		    /* Go octal characters are always 3
		       digits.  */
		    if (c != 3)
		      goto unknown;
		    break;

		  case 'x':
		    *q++ = *p++;
		    c = 0;
		    while (ISXDIGIT (*p))
		      {
			*q++ = *p++;
			++c;
		      }
		    /* Go hex characters are always 2 digits.  */
		    if (c != 2)
		      goto unknown;
		    break;

		  case 'a': case 'b': case 'f': case 'n': case 'r':
		  case 't': case 'v': case '\\': case '\'': case '"':
		    *q++ = *p++;
		    break;

		  default:
		    goto unknown;
		  }
	      }

	    *q++ = *p++;

	    if (quote == '\'' && count != 1)
	      goto unknown;

	    saw_operand = true;
	    need_operand = false;

	    break;
	  }

	default:
	  goto unknown;
	}
    }

  if (need_operand)
    goto unknown;

  gcc_assert ((size_t) (q - out_buffer) < out_len);
  *q = '\0';

  mhval->value = out_buffer;

  if (slot == NULL)
    {
      slot = htab_find_slot_with_hash (macro_hash, mhval, hashval, INSERT);
      gcc_assert (slot != NULL && *slot == NULL);
    }
  else
    {
      if (*slot != NULL)
	macro_hash_del (*slot);
    }

  *slot = mhval;

  return;

 unknown:
  fprintf (go_dump_file, "// unknowndefine %s\n", buffer);
  if (slot != NULL)
    htab_clear_slot (macro_hash, slot);
  XDELETEVEC (out_buffer);
  XDELETEVEC (copy);
}

/* A macro undef.  */

static void
go_undef (unsigned int lineno, const char *buffer)
{
  struct macro_hash_value mhval;
  void **slot;

  real_debug_hooks->undef (lineno, buffer);

  mhval.name = CONST_CAST (char *, buffer);
  mhval.value = NULL;
  slot = htab_find_slot (macro_hash, &mhval, NO_INSERT);
  if (slot != NULL)
    htab_clear_slot (macro_hash, slot);
}

/* A function or variable decl.  */

static void
go_decl (tree decl)
{
  if (!TREE_PUBLIC (decl)
      || DECL_IS_BUILTIN (decl)
      || DECL_NAME (decl) == NULL_TREE)
    return;
  vec_safe_push (queue, decl);
}

/* A function decl.  */

static void
go_function_decl (tree decl)
{
  real_debug_hooks->function_decl (decl);
  go_decl (decl);
}

static void
go_early_global_decl (tree decl)
{
  go_decl (decl);
  if (TREE_CODE (decl) != FUNCTION_DECL || DECL_STRUCT_FUNCTION (decl) != NULL)
    real_debug_hooks->early_global_decl (decl);
}

/* A global variable decl.  */

static void
go_late_global_decl (tree decl)
{
  real_debug_hooks->late_global_decl (decl);
}

/* A type declaration.  */

static void
go_type_decl (tree decl, int local)
{
  real_debug_hooks->type_decl (decl, local);

  if (local || DECL_IS_BUILTIN (decl))
    return;
  if (DECL_NAME (decl) == NULL_TREE
      && (TYPE_NAME (TREE_TYPE (decl)) == NULL_TREE
	  || TREE_CODE (TYPE_NAME (TREE_TYPE (decl))) != IDENTIFIER_NODE)
      && TREE_CODE (TREE_TYPE (decl)) != ENUMERAL_TYPE)
    return;
  vec_safe_push (queue, decl);
}

/* A container for the data we pass around when generating information
   at the end of the compilation.  */

struct godump_container
{
  /* DECLs that we have already seen.  */
  hash_set<tree> decls_seen;

  /* Types which may potentially have to be defined as dummy
     types.  */
  hash_set<const char *> pot_dummy_types;

  /* Go keywords.  */
  htab_t keyword_hash;

  /* Global type definitions.  */
  htab_t type_hash;

  /* Invalid types.  */
  htab_t invalid_hash;

  /* Obstack used to write out a type definition.  */
  struct obstack type_obstack;
};

/* Append an IDENTIFIER_NODE to OB.  */

static void
go_append_string (struct obstack *ob, tree id)
{
  obstack_grow (ob, IDENTIFIER_POINTER (id), IDENTIFIER_LENGTH (id));
}

/* Given an integer PRECISION in bits, returns a constant string that is the
   matching go int or uint type (depending on the IS_UNSIGNED flag).  Returns a
   NULL pointer if there is no matching go type.  */

static const char *
go_get_uinttype_for_precision (unsigned int precision, bool is_unsigned)
{
  switch (precision)
    {
    case 8:
      return is_unsigned ? "uint8" : "int8";
    case 16:
      return is_unsigned ? "uint16" : "int16";
    case 32:
      return is_unsigned ? "uint32" : "int32";
    case 64:
      return is_unsigned ? "uint64" : "int64";
    default:
      return NULL;
    }
}

/* Append an artificial variable name with the suffix _INDEX to OB.  Returns
   INDEX + 1.  */

static unsigned int
go_append_artificial_name (struct obstack *ob, unsigned int index)
{
  char buf[100];

  /* FIXME: identifier may not be unique.  */
  obstack_grow (ob, "Godump_", 7);
  snprintf (buf, sizeof buf, "%u", index);
  obstack_grow (ob, buf, strlen (buf));

  return index + 1;
}

/* Append the variable name from DECL to OB.  If the name is in the
   KEYWORD_HASH, prepend an '_'.  */

static void
go_append_decl_name (struct obstack *ob, tree decl, htab_t keyword_hash)
{
  const char *var_name;
  void **slot;

  /* Start variable name with an underscore if a keyword.  */
  var_name = IDENTIFIER_POINTER (DECL_NAME (decl));
  slot = htab_find_slot (keyword_hash, var_name, NO_INSERT);
  if (slot != NULL)
    obstack_1grow (ob, '_');
  go_append_string (ob, DECL_NAME (decl));
}

/* Appends a byte array with the necessary number of elements and the name
   "Godump_INDEX_pad" to pad from FROM_OFFSET to TO_OFFSET to OB assuming that
   the next field is automatically aligned to ALIGN_UNITS.  Returns INDEX + 1,
   or INDEX if no padding had to be appended.  The resulting offset where the
   next field is allocated is returned through RET_OFFSET.  */

static unsigned int
go_append_padding (struct obstack *ob, unsigned int from_offset,
		   unsigned int to_offset, unsigned int align_units,
		   unsigned int index, unsigned int *ret_offset)
{
  if (from_offset % align_units > 0)
    from_offset += align_units - (from_offset % align_units);
  gcc_assert (to_offset >= from_offset);
  if (to_offset > from_offset)
    {
      char buf[100];

      index = go_append_artificial_name (ob, index);
      snprintf (buf, sizeof buf, "_pad [%u]byte; ", to_offset - from_offset);
      obstack_grow (ob, buf, strlen (buf));
    }
  *ret_offset = to_offset;

  return index;
}

/* Appends an array of type TYPE_STRING with zero elements and the name
   "Godump_INDEX_align" to OB.  If TYPE_STRING is a null pointer, ERROR_STRING
   is appended instead of the type.  Returns INDEX + 1.  */

static unsigned int
go_force_record_alignment (struct obstack *ob, const char *type_string,
			   unsigned int index, const char *error_string)
{
  index = go_append_artificial_name (ob, index);
  obstack_grow (ob, "_align ", 7);
  if (type_string == NULL)
    obstack_grow (ob, error_string, strlen (error_string));
  else
    {
      obstack_grow (ob, "[0]", 3);
      obstack_grow (ob, type_string, strlen (type_string));
    }
  obstack_grow (ob, "; ", 2);

  return index;
}

/* Write the Go version of TYPE to CONTAINER->TYPE_OBSTACK.
   USE_TYPE_NAME is true if we can simply use a type name here without
   needing to define it.  IS_FUNC_OK is true if we can output a func
   type here; the "func" keyword will already have been added.
   Return true if the type can be represented in Go, false otherwise.
   P_ART_I is used for indexing artificial elements in nested structures and
   should always be a NULL pointer when called, except by certain recursive
   calls from go_format_type() itself.  */

static bool
go_format_type (struct godump_container *container, tree type,
		bool use_type_name, bool is_func_ok, unsigned int *p_art_i,
		bool is_anon_record_or_union)
{
  bool ret;
  struct obstack *ob;
  unsigned int art_i_dummy;
  bool is_union = false;

  if (p_art_i == NULL)
    {
      art_i_dummy = 0;
      p_art_i = &art_i_dummy;
    }
  ret = true;
  ob = &container->type_obstack;

  if (TYPE_NAME (type) != NULL_TREE
      && (container->decls_seen.contains (type)
	  || container->decls_seen.contains (TYPE_NAME (type)))
      && (AGGREGATE_TYPE_P (type)
	  || POINTER_TYPE_P (type)
	  || TREE_CODE (type) == FUNCTION_TYPE))
    {
      tree name;
      void **slot;

      name = TYPE_IDENTIFIER (type);

      slot = htab_find_slot (container->invalid_hash, IDENTIFIER_POINTER (name),
			     NO_INSERT);
      if (slot != NULL)
	ret = false;

      obstack_1grow (ob, '_');
      go_append_string (ob, name);
      return ret;
    }

  container->decls_seen.add (type);

  switch (TREE_CODE (type))
    {
    case TYPE_DECL:
      {
	void **slot;

	slot = htab_find_slot (container->invalid_hash,
			       IDENTIFIER_POINTER (DECL_NAME (type)),
			       NO_INSERT);
	if (slot != NULL)
	  ret = false;

	obstack_1grow (ob, '_');
	go_append_string (ob, DECL_NAME (type));
      }
      break;

    case ENUMERAL_TYPE:
    case INTEGER_TYPE:
      {
	const char *s;
	char buf[100];

	s = go_get_uinttype_for_precision (TYPE_PRECISION (type),
					   TYPE_UNSIGNED (type));
	if (s == NULL)
	  {
	    snprintf (buf, sizeof buf, "INVALID-int-%u%s",
		      TYPE_PRECISION (type),
		      TYPE_UNSIGNED (type) ? "u" : "");
	    s = buf;
	    ret = false;
	  }
	obstack_grow (ob, s, strlen (s));
      }
      break;

    case REAL_TYPE:
      {
	const char *s;
	char buf[100];

	switch (TYPE_PRECISION (type))
	  {
	  case 32:
	    s = "float32";
	    break;
	  case 64:
	    s = "float64";
	    break;
	  default:
	    snprintf (buf, sizeof buf, "INVALID-float-%u",
		      TYPE_PRECISION (type));
	    s = buf;
	    ret = false;
	    break;
	  }
	obstack_grow (ob, s, strlen (s));
      }
      break;

    case COMPLEX_TYPE:
      {
	const char *s;
	char buf[100];
	tree real_type;

	real_type = TREE_TYPE (type);
	if (TREE_CODE (real_type) == REAL_TYPE)
	  {
	    switch (TYPE_PRECISION (real_type))
	      {
	      case 32:
		s = "complex64";
		break;
	      case 64:
		s = "complex128";
		break;
	      default:
		snprintf (buf, sizeof buf, "INVALID-complex-%u",
			  2 * TYPE_PRECISION (real_type));
		s = buf;
		ret = false;
		break;
	      }
	  }
	else
	  {
	    s = "INVALID-complex-non-real";
	    ret = false;
	  }
	obstack_grow (ob, s, strlen (s));
      }
      break;

    case BOOLEAN_TYPE:
      obstack_grow (ob, "bool", 4);
      break;

    case POINTER_TYPE:
      if (use_type_name
          && TYPE_NAME (TREE_TYPE (type)) != NULL_TREE
          && (RECORD_OR_UNION_TYPE_P (TREE_TYPE (type))
	      || (POINTER_TYPE_P (TREE_TYPE (type))
                  && (TREE_CODE (TREE_TYPE (TREE_TYPE (type)))
		      == FUNCTION_TYPE))))
        {
	  tree name;
	  void **slot;

	  name = TYPE_IDENTIFIER (TREE_TYPE (type));

	  slot = htab_find_slot (container->invalid_hash,
				 IDENTIFIER_POINTER (name), NO_INSERT);
	  if (slot != NULL)
	    ret = false;

	  obstack_grow (ob, "*_", 2);
	  go_append_string (ob, name);

	  /* The pointer here can be used without the struct or union
	     definition.  So this struct or union is a potential dummy
	     type.  */
	  if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (type)))
	    container->pot_dummy_types.add (IDENTIFIER_POINTER (name));

	  return ret;
        }
      if (TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
	obstack_grow (ob, "func", 4);
      else
	obstack_1grow (ob, '*');
      if (VOID_TYPE_P (TREE_TYPE (type)))
	obstack_grow (ob, "byte", 4);
      else
	{
	  if (!go_format_type (container, TREE_TYPE (type), use_type_name,
			       true, NULL, false))
	    ret = false;
	}
      break;

    case ARRAY_TYPE:
      obstack_1grow (ob, '[');
      if (TYPE_DOMAIN (type) != NULL_TREE
	  && TREE_CODE (TYPE_DOMAIN (type)) == INTEGER_TYPE
	  && TYPE_MIN_VALUE (TYPE_DOMAIN (type)) != NULL_TREE
	  && TREE_CODE (TYPE_MIN_VALUE (TYPE_DOMAIN (type))) == INTEGER_CST
	  && tree_int_cst_sgn (TYPE_MIN_VALUE (TYPE_DOMAIN (type))) == 0
	  && TYPE_MAX_VALUE (TYPE_DOMAIN (type)) != NULL_TREE
	  && TREE_CODE (TYPE_MAX_VALUE (TYPE_DOMAIN (type))) == INTEGER_CST
	  && tree_fits_shwi_p (TYPE_MAX_VALUE (TYPE_DOMAIN (type))))
	{
	  char buf[100];

	  snprintf (buf, sizeof buf, HOST_WIDE_INT_PRINT_DEC "+1",
		    tree_to_shwi (TYPE_MAX_VALUE (TYPE_DOMAIN (type))));
	  obstack_grow (ob, buf, strlen (buf));
	}
      else
	obstack_1grow (ob, '0');
      obstack_1grow (ob, ']');
      if (!go_format_type (container, TREE_TYPE (type), use_type_name, false,
			   NULL, false))
	ret = false;
      break;

    case UNION_TYPE:
      is_union = true;
      /* Fall through to RECORD_TYPE case.  */
      gcc_fallthrough ();
    case RECORD_TYPE:
      {
	unsigned int prev_field_end;
	unsigned int known_alignment;
	tree field;
	bool emitted_a_field;

	/* FIXME: Why is this necessary?  Without it we can get a core
	   dump on the s390x headers, or from a file containing simply
	   "typedef struct S T;".  */
	layout_type (type);

	prev_field_end = 0;
	known_alignment = 1;
	/* Anonymous records and unions are flattened, i.e. they are not put
	   into "struct { ... }".  */
	if (!is_anon_record_or_union)
	  obstack_grow (ob, "struct { ", 9);
	for (field = TYPE_FIELDS (type), emitted_a_field = false;
	     field != NULL_TREE;
	     field = TREE_CHAIN (field))
	  {
	    if (TREE_CODE (field) != FIELD_DECL)
	      continue;
	    if (DECL_BIT_FIELD (field))
	      /* Bit fields are replaced by padding.  */
	      continue;
	    /* Only the first non-bitfield field is emitted for unions.  */
	    if (!is_union || !emitted_a_field)
	      {
		/* Emit the field.  */
		bool field_ok;
		bool is_anon_substructure;
		unsigned int decl_align_unit;
		unsigned int decl_offset;

		field_ok = true;
		emitted_a_field = true;
		is_anon_substructure =
		  (DECL_NAME (field) == NULL
		   && (TREE_CODE (TREE_TYPE (field)) == RECORD_TYPE
		       || TREE_CODE (TREE_TYPE (field)) == UNION_TYPE));
		/* Keep track of the alignment of named substructures, either
		   of the whole record, or the alignment of the emitted field
		   (for unions).  */
		decl_align_unit = DECL_ALIGN_UNIT (field);
		if (!is_anon_substructure && decl_align_unit > known_alignment)
		  known_alignment = decl_align_unit;
		/* Pad to start of field.  */
		decl_offset =
		  TREE_INT_CST_LOW (DECL_FIELD_OFFSET (field))
		  + precision_to_units
		  (TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (field)));
		{
		  unsigned int align_unit;

		  /* For anonymous records and unions there is no automatic
		     structure alignment, so use 1 as the alignment.  */
		  align_unit = (is_anon_substructure) ? 1 : decl_align_unit;
		  *p_art_i = go_append_padding
		    (ob, prev_field_end, decl_offset, align_unit, *p_art_i,
		     &prev_field_end);
		}
		if (DECL_SIZE_UNIT (field))
		  prev_field_end +=
		    TREE_INT_CST_LOW (DECL_SIZE_UNIT (field));
		/* Emit the field name, but not for anonymous records and
		   unions.  */
		if (!is_anon_substructure)
		  {
		    if ((DECL_NAME (field) == NULL))
		      *p_art_i = go_append_artificial_name (ob, *p_art_i);
		    else
		      go_append_decl_name
			(ob, field, container->keyword_hash);
		    obstack_1grow (ob, ' ');
		  }
		/* Do not expand type if a record or union type or a function
		   pointer.  */
		if (TYPE_NAME (TREE_TYPE (field)) != NULL_TREE
		    && (RECORD_OR_UNION_TYPE_P (TREE_TYPE (field))
			|| (POINTER_TYPE_P (TREE_TYPE (field))
			    && (TREE_CODE (TREE_TYPE (TREE_TYPE (field)))
				== FUNCTION_TYPE))))
		  {
		    tree name;
		    void **slot;

		    name = TYPE_IDENTIFIER (TREE_TYPE (field));

		    slot = htab_find_slot (container->invalid_hash,
					   IDENTIFIER_POINTER (name),
					   NO_INSERT);
		    if (slot != NULL)
		      field_ok = false;

		    obstack_1grow (ob, '_');
		    go_append_string (ob, name);
		  }
		else
		  {
		    if (!go_format_type (container, TREE_TYPE (field), true,
					 false, p_art_i, is_anon_substructure))
		      field_ok = false;
		  }
		if (!is_anon_substructure)
		  obstack_grow (ob, "; ", 2);
		if (!field_ok)
		  ret = false;
	      }
	  }
	/* Padding.  */
	*p_art_i = go_append_padding (ob, prev_field_end,
				      TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type)),
				      1, *p_art_i, &prev_field_end);
	/* Alignment.  */
	if (!is_anon_record_or_union
	    && known_alignment < TYPE_ALIGN_UNIT (type))
	  {
	    const char *s;
	    char buf[100];

	    /* Enforce proper record alignment.  */
	    s = go_get_uinttype_for_precision
	      (TYPE_ALIGN (type), TYPE_UNSIGNED (type));
	    if (s == NULL)
	      {
		snprintf (buf, sizeof buf, "INVALID-int-%u%s",
			  TYPE_ALIGN (type), TYPE_UNSIGNED (type) ? "u" : "");
		s = buf;
		ret = false;
	      }
	    *p_art_i = go_force_record_alignment (ob, s, *p_art_i, buf);
	  }
	if (!is_anon_record_or_union)
	  obstack_1grow (ob, '}');
      }
    break;

    case FUNCTION_TYPE:
      {
	tree arg_type;
	bool is_varargs;
	tree result;
	function_args_iterator iter;
	bool seen_arg;

	/* Go has no way to write a type which is a function but not a
	   pointer to a function.  */
	if (!is_func_ok)
	  {
	    obstack_grow (ob, "func*", 5);
	    ret = false;
	  }

	obstack_1grow (ob, '(');
	is_varargs = stdarg_p (type);
	seen_arg = false;
	FOREACH_FUNCTION_ARGS (type, arg_type, iter)
	  {
	    if (VOID_TYPE_P (arg_type))
	      break;
	    if (seen_arg)
	      obstack_grow (ob, ", ", 2);
	    if (!go_format_type (container, arg_type, true, false, NULL, false))
	      ret = false;
	    seen_arg = true;
	  }
	if (is_varargs)
	  {
	    if (prototype_p (type))
	      obstack_grow (ob, ", ", 2);
	    obstack_grow (ob, "...interface{}", 14);
	  }
	obstack_1grow (ob, ')');

	result = TREE_TYPE (type);
	if (!VOID_TYPE_P (result))
	  {
	    obstack_1grow (ob, ' ');
	    if (!go_format_type (container, result, use_type_name, false, NULL,
				 false))
	      ret = false;
	  }
      }
      break;

    default:
      obstack_grow (ob, "INVALID-type", 12);
      ret = false;
      break;
    }

  return ret;
}

/* Output the type which was built on the type obstack, and then free
   it.  */

static void
go_output_type (struct godump_container *container)
{
  struct obstack *ob;

  ob = &container->type_obstack;
  obstack_1grow (ob, '\0');
  fputs ((char *) obstack_base (ob), go_dump_file);
  obstack_free (ob, obstack_base (ob));
}

/* Output a function declaration.  */

static void
go_output_fndecl (struct godump_container *container, tree decl)
{
  if (!go_format_type (container, TREE_TYPE (decl), false, true, NULL, false))
    fprintf (go_dump_file, "// ");
  fprintf (go_dump_file, "func _%s ",
	   IDENTIFIER_POINTER (DECL_NAME (decl)));
  go_output_type (container);
  fprintf (go_dump_file, " __asm__(\"%s\")\n",
	   IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));
}

/* Output a typedef or something like a struct definition.  */

static void
go_output_typedef (struct godump_container *container, tree decl)
{
  /* If we have an enum type, output the enum constants
     separately.  */
  if (TREE_CODE (TREE_TYPE (decl)) == ENUMERAL_TYPE
      && TYPE_SIZE (TREE_TYPE (decl)) != 0
      && !container->decls_seen.contains (TREE_TYPE (decl))
      && (TYPE_CANONICAL (TREE_TYPE (decl)) == NULL_TREE
	  || !container->decls_seen.contains
				    (TYPE_CANONICAL (TREE_TYPE (decl)))))
    {
      tree element;

      for (element = TYPE_VALUES (TREE_TYPE (decl));
	   element != NULL_TREE;
	   element = TREE_CHAIN (element))
	{
	  const char *name;
	  struct macro_hash_value *mhval;
	  void **slot;
	  char buf[WIDE_INT_PRINT_BUFFER_SIZE];

	  name = IDENTIFIER_POINTER (TREE_PURPOSE (element));

	  /* Sometimes a name will be defined as both an enum constant
	     and a macro.  Avoid duplicate definition errors by
	     treating enum constants as macros.  */
	  mhval = XNEW (struct macro_hash_value);
	  mhval->name = xstrdup (name);
	  mhval->value = NULL;
	  slot = htab_find_slot (macro_hash, mhval, INSERT);
	  if (*slot != NULL)
	    macro_hash_del (*slot);

	  if (tree_fits_shwi_p (TREE_VALUE (element)))
	    snprintf (buf, sizeof buf, HOST_WIDE_INT_PRINT_DEC,
		     tree_to_shwi (TREE_VALUE (element)));
	  else if (tree_fits_uhwi_p (TREE_VALUE (element)))
	    snprintf (buf, sizeof buf, HOST_WIDE_INT_PRINT_UNSIGNED,
		      tree_to_uhwi (TREE_VALUE (element)));
	  else
	    print_hex (element, buf);

	  mhval->value = xstrdup (buf);
	  *slot = mhval;
	}
      container->decls_seen.add (TREE_TYPE (decl));
      if (TYPE_CANONICAL (TREE_TYPE (decl)) != NULL_TREE)
	container->decls_seen.add (TYPE_CANONICAL (TREE_TYPE (decl)));
    }

  if (DECL_NAME (decl) != NULL_TREE)
    {
      void **slot;
      const char *type;

      type = IDENTIFIER_POINTER (DECL_NAME (decl));
      /* If type defined already, skip.  */
      slot = htab_find_slot (container->type_hash, type, INSERT);
      if (*slot != NULL)
	return;
      *slot = CONST_CAST (void *, (const void *) type);

      if (!go_format_type (container, TREE_TYPE (decl), false, false, NULL,
			   false))
	{
	  fprintf (go_dump_file, "// ");
	  slot = htab_find_slot (container->invalid_hash, type, INSERT);
	  *slot = CONST_CAST (void *, (const void *) type);
	}
      fprintf (go_dump_file, "type _%s ",
	       IDENTIFIER_POINTER (DECL_NAME (decl)));
      go_output_type (container);

      if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (decl)))
	{
	  HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (decl));

	  if (size > 0)
	    fprintf (go_dump_file,
		     "\nconst _sizeof_%s = " HOST_WIDE_INT_PRINT_DEC,
		     IDENTIFIER_POINTER (DECL_NAME (decl)),
		     size);
	}

      container->decls_seen.add (decl);
    }
  else if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (decl)))
    {
       void **slot;
       const char *type;
       HOST_WIDE_INT size;

       type = IDENTIFIER_POINTER (TYPE_NAME (TREE_TYPE ((decl))));
       /* If type defined already, skip.  */
       slot = htab_find_slot (container->type_hash, type, INSERT);
       if (*slot != NULL)
         return;
       *slot = CONST_CAST (void *, (const void *) type);

       if (!go_format_type (container, TREE_TYPE (decl), false, false, NULL,
			    false))
	 {
	   fprintf (go_dump_file, "// ");
	   slot = htab_find_slot (container->invalid_hash, type, INSERT);
	   *slot = CONST_CAST (void *, (const void *) type);
	 }
       fprintf (go_dump_file, "type _%s ",
	       IDENTIFIER_POINTER (TYPE_NAME (TREE_TYPE (decl))));
       go_output_type (container);

       size = int_size_in_bytes (TREE_TYPE (decl));
       if (size > 0)
	 fprintf (go_dump_file,
		  "\nconst _sizeof_%s = " HOST_WIDE_INT_PRINT_DEC,
		  IDENTIFIER_POINTER (TYPE_NAME (TREE_TYPE (decl))),
		  size);
    }
  else
    return;

  fprintf (go_dump_file, "\n");
}

/* Output a variable.  */

static void
go_output_var (struct godump_container *container, tree decl)
{
  bool is_valid;
  tree type_name;
  tree id;

  if (container->decls_seen.contains (decl)
      || container->decls_seen.contains (DECL_NAME (decl)))
    return;
  container->decls_seen.add (decl);
  container->decls_seen.add (DECL_NAME (decl));

  type_name = TYPE_NAME (TREE_TYPE (decl));
  id = NULL_TREE;
  if (type_name != NULL_TREE && TREE_CODE (type_name) == IDENTIFIER_NODE)
    id = type_name;
  else if (type_name != NULL_TREE && TREE_CODE (type_name) == TYPE_DECL
	   && DECL_SOURCE_LOCATION (type_name) != BUILTINS_LOCATION
	   && DECL_NAME (type_name))
    id = DECL_NAME (type_name);
  if (id != NULL_TREE
      && (!htab_find_slot (container->type_hash, IDENTIFIER_POINTER (id),
			   NO_INSERT)
	  || htab_find_slot (container->invalid_hash, IDENTIFIER_POINTER (id),
			     NO_INSERT)))
    id = NULL_TREE;
  if (id != NULL_TREE)
    {
      struct obstack *ob;

      ob = &container->type_obstack;
      obstack_1grow (ob, '_');
      go_append_string (ob, id);
      is_valid = htab_find_slot (container->type_hash, IDENTIFIER_POINTER (id),
				 NO_INSERT) != NULL;
    }
  else
    is_valid = go_format_type (container, TREE_TYPE (decl), true, false, NULL,
			       false);
  if (is_valid
      && htab_find_slot (container->type_hash,
			 IDENTIFIER_POINTER (DECL_NAME (decl)),
			 NO_INSERT) != NULL)
    {
      /* There is already a type with this name, probably from a
	 struct tag.  Prefer the type to the variable.  */
      is_valid = false;
    }
  if (!is_valid)
    fprintf (go_dump_file, "// ");

  fprintf (go_dump_file, "var _%s ",
	   IDENTIFIER_POINTER (DECL_NAME (decl)));
  go_output_type (container);
  fprintf (go_dump_file, "\n");

  /* Sometimes an extern variable is declared with an unknown struct
     type.  */
  if (type_name != NULL_TREE && RECORD_OR_UNION_TYPE_P (TREE_TYPE (decl)))
    {
      if (TREE_CODE (type_name) == IDENTIFIER_NODE)
	container->pot_dummy_types.add (IDENTIFIER_POINTER (type_name));
      else if (TREE_CODE (type_name) == TYPE_DECL)
	container->pot_dummy_types.add
			    (IDENTIFIER_POINTER (DECL_NAME (type_name)));
    }
}

/* Output the final value of a preprocessor macro or enum constant.
   This is called via htab_traverse_noresize.  */

static int
go_print_macro (void **slot, void *arg ATTRIBUTE_UNUSED)
{
  struct macro_hash_value *mhval = (struct macro_hash_value *) *slot;
  fprintf (go_dump_file, "const _%s = %s\n", mhval->name, mhval->value);
  return 1;
}

/* Build a hash table with the Go keywords.  */

static const char * const keywords[] = {
  "__asm__", "break", "case", "chan", "const", "continue", "default",
  "defer", "else", "fallthrough", "for", "func", "go", "goto", "if",
  "import", "interface", "map", "package", "range", "return", "select",
  "struct", "switch", "type", "var"
};

static void
keyword_hash_init (struct godump_container *container)
{
  size_t i;
  size_t count = sizeof (keywords) / sizeof (keywords[0]);
  void **slot;

  for (i = 0; i < count; i++)
    {
      slot = htab_find_slot (container->keyword_hash, keywords[i], INSERT);
      *slot = CONST_CAST (void *, (const void *) keywords[i]);
    }
}

/* Traversing the pot_dummy_types and seeing which types are present
   in the global types hash table and creating dummy definitions if
   not found.  This function is invoked by hash_set::traverse.  */

bool
find_dummy_types (const char *const &ptr, godump_container *adata)
{
  struct godump_container *data = (struct godump_container *) adata;
  const char *type = (const char *) ptr;
  void **slot;
  void **islot;

  slot = htab_find_slot (data->type_hash, type, NO_INSERT);
  islot = htab_find_slot (data->invalid_hash, type, NO_INSERT);
  if (slot == NULL || islot != NULL)
    fprintf (go_dump_file, "type _%s struct {}\n", type);
  return true;
}

/* Output symbols.  */

static void
go_finish (const char *filename)
{
  struct godump_container container;
  unsigned int ix;
  tree decl;

  real_debug_hooks->finish (filename);

  container.type_hash = htab_create (100, htab_hash_string,
                                     string_hash_eq, NULL);
  container.invalid_hash = htab_create (10, htab_hash_string,
					string_hash_eq, NULL);
  container.keyword_hash = htab_create (50, htab_hash_string,
                                        string_hash_eq, NULL);
  obstack_init (&container.type_obstack);

  keyword_hash_init (&container);

  FOR_EACH_VEC_SAFE_ELT (queue, ix, decl)
    {
      switch (TREE_CODE (decl))
	{
	case FUNCTION_DECL:
	  go_output_fndecl (&container, decl);
	  break;

	case TYPE_DECL:
	  go_output_typedef (&container, decl);
	  break;

	case VAR_DECL:
	  go_output_var (&container, decl);
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  htab_traverse_noresize (macro_hash, go_print_macro, NULL);

  /* To emit dummy definitions.  */
  container.pot_dummy_types.traverse<godump_container *, find_dummy_types>
                        (&container);

  htab_delete (container.type_hash);
  htab_delete (container.invalid_hash);
  htab_delete (container.keyword_hash);
  obstack_free (&container.type_obstack, NULL);

  vec_free (queue);

  if (fclose (go_dump_file) != 0)
    error ("could not close Go dump file: %m");
  go_dump_file = NULL;
}

/* Set up our hooks.  */

const struct gcc_debug_hooks *
dump_go_spec_init (const char *filename, const struct gcc_debug_hooks *hooks)
{
  go_dump_file = fopen (filename, "w");
  if (go_dump_file == NULL)
    {
      error ("could not open Go dump file %qs: %m", filename);
      return hooks;
    }

  go_debug_hooks = *hooks;
  real_debug_hooks = hooks;

  go_debug_hooks.finish = go_finish;
  go_debug_hooks.define = go_define;
  go_debug_hooks.undef = go_undef;
  go_debug_hooks.function_decl = go_function_decl;
  go_debug_hooks.early_global_decl = go_early_global_decl;
  go_debug_hooks.late_global_decl = go_late_global_decl;
  go_debug_hooks.type_decl = go_type_decl;

  macro_hash = htab_create (100, macro_hash_hashval, macro_hash_eq,
			    macro_hash_del);

  return &go_debug_hooks;
}

#include "gt-godump.h"
