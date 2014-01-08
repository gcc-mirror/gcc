/* Output Go language descriptions of types.
   Copyright (C) 2008-2014 Free Software Foundation, Inc.
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
#include "diagnostic-core.h"
#include "tree.h"
#include "ggc.h"
#include "pointer-set.h"
#include "obstack.h"
#include "debug.h"

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

/* A global variable decl.  */

static void
go_global_decl (tree decl)
{
  real_debug_hooks->global_decl (decl);
  go_decl (decl);
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
  struct pointer_set_t *decls_seen;

  /* Types which may potentially have to be defined as dummy
     types.  */
  struct pointer_set_t *pot_dummy_types;

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

/* Write the Go version of TYPE to CONTAINER->TYPE_OBSTACK.
   USE_TYPE_NAME is true if we can simply use a type name here without
   needing to define it.  IS_FUNC_OK is true if we can output a func
   type here; the "func" keyword will already have been added.  Return
   true if the type can be represented in Go, false otherwise.  */

static bool
go_format_type (struct godump_container *container, tree type,
		bool use_type_name, bool is_func_ok)
{
  bool ret;
  struct obstack *ob;

  ret = true;
  ob = &container->type_obstack;

  if (TYPE_NAME (type) != NULL_TREE
      && (pointer_set_contains (container->decls_seen, type)
	  || pointer_set_contains (container->decls_seen, TYPE_NAME (type)))
      && (AGGREGATE_TYPE_P (type)
	  || POINTER_TYPE_P (type)
	  || TREE_CODE (type) == FUNCTION_TYPE))
    {
      tree name;
      void **slot;

      name = TYPE_NAME (type);
      if (TREE_CODE (name) == TYPE_DECL)
	name = DECL_NAME (name);

      slot = htab_find_slot (container->invalid_hash, IDENTIFIER_POINTER (name),
			     NO_INSERT);
      if (slot != NULL)
	ret = false;

      obstack_1grow (ob, '_');
      go_append_string (ob, name);
      return ret;
    }

  pointer_set_insert (container->decls_seen, type);

  switch (TREE_CODE (type))
    {
    case ENUMERAL_TYPE:
      obstack_grow (ob, "int", 3);
      break;

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

    case INTEGER_TYPE:
      {
	const char *s;
	char buf[100];

	switch (TYPE_PRECISION (type))
	  {
	  case 8:
	    s = TYPE_UNSIGNED (type) ? "uint8" : "int8";
	    break;
	  case 16:
	    s = TYPE_UNSIGNED (type) ? "uint16" : "int16";
	    break;
	  case 32:
	    s = TYPE_UNSIGNED (type) ? "uint32" : "int32";
	    break;
	  case 64:
	    s = TYPE_UNSIGNED (type) ? "uint64" : "int64";
	    break;
	  default:
	    snprintf (buf, sizeof buf, "INVALID-int-%u%s",
		      TYPE_PRECISION (type),
		      TYPE_UNSIGNED (type) ? "u" : "");
	    s = buf;
	    ret = false;
	    break;
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

	  name = TYPE_NAME (TREE_TYPE (type));
	  if (TREE_CODE (name) == TYPE_DECL)
	    name = DECL_NAME (name);

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
	    pointer_set_insert (container->pot_dummy_types,
				IDENTIFIER_POINTER (name));

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
			       true))
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
      obstack_1grow (ob, ']');
      if (!go_format_type (container, TREE_TYPE (type), use_type_name, false))
	ret = false;
      break;

    case UNION_TYPE:
    case RECORD_TYPE:
      {
	tree field;
	int i;

	obstack_grow (ob, "struct { ", 9);
	i = 0;
	for (field = TYPE_FIELDS (type);
	     field != NULL_TREE;
	     field = TREE_CHAIN (field))
	  {
	    struct obstack hold_type_obstack;
	    bool field_ok;

	    if (TREE_CODE (type) == UNION_TYPE)
	      {
		hold_type_obstack = container->type_obstack;
		obstack_init (&container->type_obstack);
	      }

	    field_ok = true;

	    if (DECL_NAME (field) == NULL)
	      {
		char buf[100];

		obstack_grow (ob, "Godump_", 7);
		snprintf (buf, sizeof buf, "%d", i);
		obstack_grow (ob, buf, strlen (buf));
		i++;
	      }
	    else
              {
		const char *var_name;
		void **slot;

		/* Start variable name with an underscore if a keyword.  */
		var_name = IDENTIFIER_POINTER (DECL_NAME (field));
		slot = htab_find_slot (container->keyword_hash, var_name,
				       NO_INSERT);
		if (slot != NULL)
		  obstack_1grow (ob, '_');
		go_append_string (ob, DECL_NAME (field));
	      }
	    obstack_1grow (ob, ' ');
	    if (DECL_BIT_FIELD (field))
	      {
		obstack_grow (ob, "INVALID-bit-field", 17);
		field_ok = false;
	      }
	    else
              {
		/* Do not expand type if a record or union type or a
		   function pointer.  */
		if (TYPE_NAME (TREE_TYPE (field)) != NULL_TREE
		    && (RECORD_OR_UNION_TYPE_P (TREE_TYPE (field))
			|| (POINTER_TYPE_P (TREE_TYPE (field))
			    && (TREE_CODE (TREE_TYPE (TREE_TYPE (field)))
                                == FUNCTION_TYPE))))
		  {
		    tree name;
		    void **slot;

		    name = TYPE_NAME (TREE_TYPE (field));
		    if (TREE_CODE (name) == TYPE_DECL)
		      name = DECL_NAME (name);

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
					 false))
		      field_ok = false;
		  }
              }
	    obstack_grow (ob, "; ", 2);

	    /* Only output the first successful field of a union, and
	       hope for the best.  */
	    if (TREE_CODE (type) == UNION_TYPE)
	      {
		if (!field_ok && TREE_CHAIN (field) == NULL_TREE)
		  {
		    field_ok = true;
		    ret = false;
		  }
		if (field_ok)
		  {
		    unsigned int sz;

		    sz = obstack_object_size (&container->type_obstack);
		    obstack_grow (&hold_type_obstack,
				  obstack_base (&container->type_obstack),
				  sz);
		  }
		obstack_free (&container->type_obstack, NULL);
		container->type_obstack = hold_type_obstack;
		if (field_ok)
		  break;
	      }
	    else
	      {
		if (!field_ok)
		  ret = false;
	      }
	  }
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
	    if (!go_format_type (container, arg_type, true, false))
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
	    if (!go_format_type (container, result, use_type_name, false))
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
  fputs (obstack_base (ob), go_dump_file);
  obstack_free (ob, obstack_base (ob));
}

/* Output a function declaration.  */

static void
go_output_fndecl (struct godump_container *container, tree decl)
{
  if (!go_format_type (container, TREE_TYPE (decl), false, true))
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
      && !pointer_set_contains (container->decls_seen, TREE_TYPE (decl))
      && (TYPE_CANONICAL (TREE_TYPE (decl)) == NULL_TREE
	  || !pointer_set_contains (container->decls_seen,
				    TYPE_CANONICAL (TREE_TYPE (decl)))))
    {
      tree element;

      for (element = TYPE_VALUES (TREE_TYPE (decl));
	   element != NULL_TREE;
	   element = TREE_CHAIN (element))
	{
	  const char *name;
	  struct macro_hash_value *mhval;
	  void **slot;
	  char buf[100];

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
	    snprintf (buf, sizeof buf, HOST_WIDE_INT_PRINT_DOUBLE_HEX,
		     ((unsigned HOST_WIDE_INT)
		      TREE_INT_CST_HIGH (TREE_VALUE (element))),
		     TREE_INT_CST_LOW (TREE_VALUE (element)));

	  mhval->value = xstrdup (buf);
	  *slot = mhval;
	}
      pointer_set_insert (container->decls_seen, TREE_TYPE (decl));
      if (TYPE_CANONICAL (TREE_TYPE (decl)) != NULL_TREE)
	pointer_set_insert (container->decls_seen,
			    TYPE_CANONICAL (TREE_TYPE (decl)));
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

      if (!go_format_type (container, TREE_TYPE (decl), false, false))
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

      pointer_set_insert (container->decls_seen, decl);
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

       if (!go_format_type (container, TREE_TYPE (decl), false, false))
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

  if (pointer_set_contains (container->decls_seen, decl)
      || pointer_set_contains (container->decls_seen, DECL_NAME (decl)))
    return;
  pointer_set_insert (container->decls_seen, decl);
  pointer_set_insert (container->decls_seen, DECL_NAME (decl));

  is_valid = go_format_type (container, TREE_TYPE (decl), true, false);
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
  if (TYPE_NAME (TREE_TYPE (decl)) != NULL_TREE
      && RECORD_OR_UNION_TYPE_P (TREE_TYPE (decl)))
    {
      tree type_name = TYPE_NAME (TREE_TYPE (decl));
      if (TREE_CODE (type_name) == IDENTIFIER_NODE)
	pointer_set_insert (container->pot_dummy_types,
			    IDENTIFIER_POINTER (type_name));
      else if (TREE_CODE (type_name) == TYPE_DECL)
	pointer_set_insert (container->pot_dummy_types,
			    IDENTIFIER_POINTER (DECL_NAME (type_name)));
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
   not found.  This function is invoked by pointer_set_traverse.  */

static bool
find_dummy_types (const void *ptr, void *adata)
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

  container.decls_seen = pointer_set_create ();
  container.pot_dummy_types = pointer_set_create ();
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
  pointer_set_traverse (container.pot_dummy_types, find_dummy_types,
                        (void *) &container);

  pointer_set_destroy (container.decls_seen);
  pointer_set_destroy (container.pot_dummy_types);
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
  go_debug_hooks.global_decl = go_global_decl;
  go_debug_hooks.type_decl = go_type_decl;

  macro_hash = htab_create (100, macro_hash_hashval, macro_hash_eq,
			    macro_hash_del);

  return &go_debug_hooks;
}

#include "gt-godump.h"
