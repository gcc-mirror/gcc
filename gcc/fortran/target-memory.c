/* Simulate storage of variables into target memory.
   Copyright (C) 2007-2019 Free Software Foundation, Inc.
   Contributed by Paul Thomas and Brooks Moses

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
#include "tree.h"
#include "gfortran.h"
#include "trans.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "arith.h"
#include "constructor.h"
#include "trans-const.h"
#include "trans-types.h"
#include "target-memory.h"

/* --------------------------------------------------------------- */
/* Calculate the size of an expression.  */


static size_t
size_integer (int kind)
{
  return GET_MODE_SIZE (SCALAR_INT_TYPE_MODE (gfc_get_int_type (kind)));
}


static size_t
size_float (int kind)
{
  return GET_MODE_SIZE (SCALAR_FLOAT_TYPE_MODE (gfc_get_real_type (kind)));
}


static size_t
size_complex (int kind)
{
  return 2 * size_float (kind);
}


static size_t
size_logical (int kind)
{
  return GET_MODE_SIZE (SCALAR_INT_TYPE_MODE (gfc_get_logical_type (kind)));
}


static size_t
size_character (gfc_charlen_t length, int kind)
{
  int i = gfc_validate_kind (BT_CHARACTER, kind, false);
  return length * gfc_character_kinds[i].bit_size / 8;
}


/* Return the size of a single element of the given expression.
   Equivalent to gfc_target_expr_size for scalars.  */

bool
gfc_element_size (gfc_expr *e, size_t *siz)
{
  tree type;

  switch (e->ts.type)
    {
    case BT_INTEGER:
      *siz = size_integer (e->ts.kind);
      return true;
    case BT_REAL:
      *siz = size_float (e->ts.kind);
      return true;
    case BT_COMPLEX:
      *siz = size_complex (e->ts.kind);
      return true;
    case BT_LOGICAL:
      *siz = size_logical (e->ts.kind);
      return true;
    case BT_CHARACTER:
      if (e->expr_type == EXPR_CONSTANT)
	*siz = size_character (e->value.character.length, e->ts.kind);
      else if (e->ts.u.cl != NULL && e->ts.u.cl->length != NULL
	       && e->ts.u.cl->length->expr_type == EXPR_CONSTANT
	       && e->ts.u.cl->length->ts.type == BT_INTEGER)
	{
	  HOST_WIDE_INT length;

	  gfc_extract_hwi (e->ts.u.cl->length, &length);
	  *siz = size_character (length, e->ts.kind);
	}
      else
	{
	  *siz = 0;
	  return false;
	}
      return true;

    case BT_HOLLERITH:
      *siz = e->representation.length;
      return true;
    case BT_DERIVED:
    case BT_CLASS:
    case BT_VOID:
    case BT_ASSUMED:
    case BT_PROCEDURE:
      {
	/* Determine type size without clobbering the typespec for ISO C
	   binding types.  */
	gfc_typespec ts;
	HOST_WIDE_INT size;
	ts = e->ts;
	type = gfc_typenode_for_spec (&ts);
	size = int_size_in_bytes (type);
	gcc_assert (size >= 0);
	*siz = size;
      }
      return true;
    default:
      gfc_internal_error ("Invalid expression in gfc_element_size.");
      *siz = 0;
      return false;
    }
  return true;
}


/* Return the size of an expression in its target representation.  */

bool
gfc_target_expr_size (gfc_expr *e, size_t *size)
{
  mpz_t tmp;
  size_t asz, el_size;

  gcc_assert (e != NULL);

  *size = 0;
  if (e->rank)
    {
      if (gfc_array_size (e, &tmp))
	asz = mpz_get_ui (tmp);
      else
	return false;
    }
  else
    asz = 1;

  if (!gfc_element_size (e, &el_size))
    return false;
  *size = asz * el_size;
  return true;
}


/* The encode_* functions export a value into a buffer, and
   return the number of bytes of the buffer that have been
   used.  */

static unsigned HOST_WIDE_INT
encode_array (gfc_expr *expr, unsigned char *buffer, size_t buffer_size)
{
  mpz_t array_size;
  int i;
  int ptr = 0;

  gfc_constructor_base ctor = expr->value.constructor;

  gfc_array_size (expr, &array_size);
  for (i = 0; i < (int)mpz_get_ui (array_size); i++)
    {
      ptr += gfc_target_encode_expr (gfc_constructor_lookup_expr (ctor, i),
				     &buffer[ptr], buffer_size - ptr);
    }

  mpz_clear (array_size);
  return ptr;
}


static int
encode_integer (int kind, mpz_t integer, unsigned char *buffer,
		size_t buffer_size)
{
  return native_encode_expr (gfc_conv_mpz_to_tree (integer, kind),
			     buffer, buffer_size);
}


static int
encode_float (int kind, mpfr_t real, unsigned char *buffer, size_t buffer_size)
{
  return native_encode_expr (gfc_conv_mpfr_to_tree (real, kind, 0), buffer,
			     buffer_size);
}


static int
encode_complex (int kind, mpc_t cmplx,
		unsigned char *buffer, size_t buffer_size)
{
  int size;
  size = encode_float (kind, mpc_realref (cmplx), &buffer[0], buffer_size);
  size += encode_float (kind, mpc_imagref (cmplx),
			&buffer[size], buffer_size - size);
  return size;
}


static int
encode_logical (int kind, int logical, unsigned char *buffer, size_t buffer_size)
{
  return native_encode_expr (build_int_cst (gfc_get_logical_type (kind),
					    logical),
			     buffer, buffer_size);
}


size_t
gfc_encode_character (int kind, size_t length, const gfc_char_t *string,
		      unsigned char *buffer, size_t buffer_size)
{
  size_t elsize = size_character (1, kind);
  tree type = gfc_get_char_type (kind);

  gcc_assert (buffer_size >= size_character (length, kind));

  for (size_t i = 0; i < length; i++)
    native_encode_expr (build_int_cst (type, string[i]), &buffer[i*elsize],
			elsize);

  return length;
}


static unsigned HOST_WIDE_INT
encode_derived (gfc_expr *source, unsigned char *buffer, size_t buffer_size)
{
  gfc_constructor *c;
  gfc_component *cmp;
  int ptr;
  tree type;
  HOST_WIDE_INT size;

  type = gfc_typenode_for_spec (&source->ts);

  for (c = gfc_constructor_first (source->value.constructor),
       cmp = source->ts.u.derived->components;
       c;
       c = gfc_constructor_next (c), cmp = cmp->next)
    {
      gcc_assert (cmp);
      if (!c->expr)
	continue;
      ptr = TREE_INT_CST_LOW(DECL_FIELD_OFFSET(cmp->backend_decl))
	    + TREE_INT_CST_LOW(DECL_FIELD_BIT_OFFSET(cmp->backend_decl))/8;

      if (c->expr->expr_type == EXPR_NULL)
	{
	  size = int_size_in_bytes (TREE_TYPE (cmp->backend_decl));
	  gcc_assert (size >= 0);
	  memset (&buffer[ptr], 0, size);
	}
      else
	gfc_target_encode_expr (c->expr, &buffer[ptr],
				buffer_size - ptr);
    }

  size = int_size_in_bytes (type);
  gcc_assert (size >= 0);
  return size;
}


/* Write a constant expression in binary form to a buffer.  */
unsigned HOST_WIDE_INT
gfc_target_encode_expr (gfc_expr *source, unsigned char *buffer,
			size_t buffer_size)
{
  if (source == NULL)
    return 0;

  if (source->expr_type == EXPR_ARRAY)
    return encode_array (source, buffer, buffer_size);

  gcc_assert (source->expr_type == EXPR_CONSTANT
	      || source->expr_type == EXPR_STRUCTURE
	      || source->expr_type == EXPR_SUBSTRING);

  /* If we already have a target-memory representation, we use that rather
     than recreating one.  */
  if (source->representation.string)
    {
      memcpy (buffer, source->representation.string,
	      source->representation.length);
      return source->representation.length;
    }

  switch (source->ts.type)
    {
    case BT_INTEGER:
      return encode_integer (source->ts.kind, source->value.integer, buffer,
			     buffer_size);
    case BT_REAL:
      return encode_float (source->ts.kind, source->value.real, buffer,
			   buffer_size);
    case BT_COMPLEX:
      return encode_complex (source->ts.kind, source->value.complex,
			     buffer, buffer_size);
    case BT_LOGICAL:
      return encode_logical (source->ts.kind, source->value.logical, buffer,
			     buffer_size);
    case BT_CHARACTER:
      if (source->expr_type == EXPR_CONSTANT || source->ref == NULL)
	return gfc_encode_character (source->ts.kind,
				     source->value.character.length,
				     source->value.character.string,
				     buffer, buffer_size);
      else
	{
	  HOST_WIDE_INT start, end;

	  gcc_assert (source->expr_type == EXPR_SUBSTRING);
	  gfc_extract_hwi (source->ref->u.ss.start, &start);
	  gfc_extract_hwi (source->ref->u.ss.end, &end);
	  return gfc_encode_character (source->ts.kind, MAX(end - start + 1, 0),
				       &source->value.character.string[start-1],
				       buffer, buffer_size);
	}

    case BT_DERIVED:
      if (source->ts.u.derived->ts.f90_type == BT_VOID)
	{
	  gfc_constructor *c;
	  gcc_assert (source->expr_type == EXPR_STRUCTURE);
	  c = gfc_constructor_first (source->value.constructor);
	  gcc_assert (c->expr->expr_type == EXPR_CONSTANT
		      && c->expr->ts.type == BT_INTEGER);
	  return encode_integer (gfc_index_integer_kind, c->expr->value.integer,
				 buffer, buffer_size);
	}

      return encode_derived (source, buffer, buffer_size);
    default:
      gfc_internal_error ("Invalid expression in gfc_target_encode_expr.");
      return 0;
    }
}


static size_t
interpret_array (unsigned char *buffer, size_t buffer_size, gfc_expr *result)
{
  gfc_constructor_base base = NULL;
  size_t array_size = 1;
  size_t ptr = 0;

  /* Calculate array size from its shape and rank.  */
  gcc_assert (result->rank > 0 && result->shape);

  for (int i = 0; i < result->rank; i++)
    array_size *= mpz_get_ui (result->shape[i]);

  /* Iterate over array elements, producing constructors.  */
  for (size_t i = 0; i < array_size; i++)
    {
      gfc_expr *e = gfc_get_constant_expr (result->ts.type, result->ts.kind,
					   &result->where);
      e->ts = result->ts;

      if (e->ts.type == BT_CHARACTER)
	e->value.character.length = result->value.character.length;

      gfc_constructor_append_expr (&base, e, &result->where);

      ptr += gfc_target_interpret_expr (&buffer[ptr], buffer_size - ptr, e,
					true);
    }

  result->value.constructor = base;
  return ptr;
}


int
gfc_interpret_integer (int kind, unsigned char *buffer, size_t buffer_size,
		   mpz_t integer)
{
  mpz_init (integer);
  gfc_conv_tree_to_mpz (integer,
			native_interpret_expr (gfc_get_int_type (kind),
					       buffer, buffer_size));
  return size_integer (kind);
}


int
gfc_interpret_float (int kind, unsigned char *buffer, size_t buffer_size,
		     mpfr_t real)
{
  gfc_set_model_kind (kind);
  mpfr_init (real);
  gfc_conv_tree_to_mpfr (real,
			 native_interpret_expr (gfc_get_real_type (kind),
						buffer, buffer_size));

  return size_float (kind);
}


int
gfc_interpret_complex (int kind, unsigned char *buffer, size_t buffer_size,
		       mpc_t complex)
{
  int size;
  size = gfc_interpret_float (kind, &buffer[0], buffer_size,
			      mpc_realref (complex));
  size += gfc_interpret_float (kind, &buffer[size], buffer_size - size,
			       mpc_imagref (complex));
  return size;
}


int
gfc_interpret_logical (int kind, unsigned char *buffer, size_t buffer_size,
		   int *logical)
{
  tree t = native_interpret_expr (gfc_get_logical_type (kind), buffer,
				  buffer_size);
  *logical = wi::to_wide (t) == 0 ? 0 : 1;
  return size_logical (kind);
}


size_t
gfc_interpret_character (unsigned char *buffer, size_t buffer_size,
			 gfc_expr *result)
{
  if (result->ts.u.cl && result->ts.u.cl->length)
    result->value.character.length =
      gfc_mpz_get_hwi (result->ts.u.cl->length->value.integer);

  gcc_assert (buffer_size >= size_character (result->value.character.length,
					     result->ts.kind));
  result->value.character.string =
    gfc_get_wide_string (result->value.character.length + 1);

  if (result->ts.kind == gfc_default_character_kind)
    for (size_t i = 0; i < (size_t) result->value.character.length; i++)
      result->value.character.string[i] = (gfc_char_t) buffer[i];
  else
    {
      mpz_t integer;
      size_t bytes = size_character (1, result->ts.kind);
      mpz_init (integer);
      gcc_assert (bytes <= sizeof (unsigned long));

      for (size_t i = 0; i < (size_t) result->value.character.length; i++)
	{
	  gfc_conv_tree_to_mpz (integer,
	    native_interpret_expr (gfc_get_char_type (result->ts.kind),
				   &buffer[bytes*i], buffer_size-bytes*i));
	  result->value.character.string[i]
	    = (gfc_char_t) mpz_get_ui (integer);
	}

      mpz_clear (integer);
    }

  result->value.character.string[result->value.character.length] = '\0';

  return result->value.character.length;
}


int
gfc_interpret_derived (unsigned char *buffer, size_t buffer_size, gfc_expr *result)
{
  gfc_component *cmp;
  int ptr;
  tree type;

  /* The attributes of the derived type need to be bolted to the floor.  */
  result->expr_type = EXPR_STRUCTURE;

  cmp = result->ts.u.derived->components;

  if (result->ts.u.derived->from_intmod == INTMOD_ISO_C_BINDING
      && (result->ts.u.derived->intmod_sym_id == ISOCBINDING_PTR
	  || result->ts.u.derived->intmod_sym_id == ISOCBINDING_FUNPTR))
    {
      gfc_constructor *c;
      gfc_expr *e;
      /* Needed as gfc_typenode_for_spec as gfc_typenode_for_spec
	 sets this to BT_INTEGER.  */
      result->ts.type = BT_DERIVED;
      e = gfc_get_constant_expr (cmp->ts.type, cmp->ts.kind, &result->where);
      c = gfc_constructor_append_expr (&result->value.constructor, e, NULL);
      c->n.component = cmp;
      gfc_target_interpret_expr (buffer, buffer_size, e, true);
      e->ts.is_iso_c = 1;
      return int_size_in_bytes (ptr_type_node);
    }

  type = gfc_typenode_for_spec (&result->ts);

  /* Run through the derived type components.  */
  for (;cmp; cmp = cmp->next)
    {
      gfc_constructor *c;
      gfc_expr *e = gfc_get_constant_expr (cmp->ts.type, cmp->ts.kind,
					   &result->where);
      e->ts = cmp->ts;

      /* Copy shape, if needed.  */
      if (cmp->as && cmp->as->rank)
	{
	  int n;

	  e->expr_type = EXPR_ARRAY;
	  e->rank = cmp->as->rank;

	  e->shape = gfc_get_shape (e->rank);
	  for (n = 0; n < e->rank; n++)
	     {
	       mpz_init_set_ui (e->shape[n], 1);
	       mpz_add (e->shape[n], e->shape[n],
			cmp->as->upper[n]->value.integer);
	       mpz_sub (e->shape[n], e->shape[n],
			cmp->as->lower[n]->value.integer);
	     }
	}

      c = gfc_constructor_append_expr (&result->value.constructor, e, NULL);

      /* The constructor points to the component.  */
      c->n.component = cmp;

      /* Calculate the offset, which consists of the FIELD_OFFSET in
	 bytes, which appears in multiples of DECL_OFFSET_ALIGN-bit-sized,
	 and additional bits of FIELD_BIT_OFFSET. The code assumes that all
	 sizes of the components are multiples of BITS_PER_UNIT,
	 i.e. there are, e.g., no bit fields.  */

      gcc_assert (cmp->backend_decl);
      ptr = TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (cmp->backend_decl));
      gcc_assert (ptr % 8 == 0);
      ptr = ptr/8 + TREE_INT_CST_LOW (DECL_FIELD_OFFSET (cmp->backend_decl));

      gcc_assert (e->ts.type != BT_VOID || cmp->attr.caf_token);
      gfc_target_interpret_expr (&buffer[ptr], buffer_size - ptr, e, true);
    }

  return int_size_in_bytes (type);
}


/* Read a binary buffer to a constant expression.  */
size_t
gfc_target_interpret_expr (unsigned char *buffer, size_t buffer_size,
			   gfc_expr *result, bool convert_widechar)
{
  if (result->expr_type == EXPR_ARRAY)
    return interpret_array (buffer, buffer_size, result);

  switch (result->ts.type)
    {
    case BT_INTEGER:
      result->representation.length =
        gfc_interpret_integer (result->ts.kind, buffer, buffer_size,
			       result->value.integer);
      break;

    case BT_REAL:
      result->representation.length =
        gfc_interpret_float (result->ts.kind, buffer, buffer_size,
    			     result->value.real);
      break;

    case BT_COMPLEX:
      result->representation.length =
        gfc_interpret_complex (result->ts.kind, buffer, buffer_size,
			       result->value.complex);
      break;

    case BT_LOGICAL:
      result->representation.length =
        gfc_interpret_logical (result->ts.kind, buffer, buffer_size,
			       &result->value.logical);
      break;

    case BT_CHARACTER:
      result->representation.length =
        gfc_interpret_character (buffer, buffer_size, result);
      break;

    case BT_CLASS:
      result->ts = CLASS_DATA (result)->ts;
      /* Fall through.  */
    case BT_DERIVED:
      result->representation.length =
        gfc_interpret_derived (buffer, buffer_size, result);
      gcc_assert (result->representation.length >= 0);
      break;

    case BT_VOID:
      /* This deals with caf_tokens.  */
      result->representation.length =
        gfc_interpret_integer (result->ts.kind, buffer, buffer_size,
			       result->value.integer);
      break;

    default:
      gfc_internal_error ("Invalid expression in gfc_target_interpret_expr.");
      break;
    }

  if (result->ts.type == BT_CHARACTER && convert_widechar)
    result->representation.string
      = gfc_widechar_to_char (result->value.character.string,
			      result->value.character.length);
  else
    {
      result->representation.string =
        XCNEWVEC (char, result->representation.length + 1);
      memcpy (result->representation.string, buffer,
	      result->representation.length);
      result->representation.string[result->representation.length] = '\0';
    }

  return result->representation.length;
}


/* --------------------------------------------------------------- */
/* Two functions used by trans-common.c to write overlapping
   equivalence initializers to a buffer.  This is added to the union
   and the original initializers freed.  */


/* Writes the values of a constant expression to a char buffer. If another
   unequal initializer has already been written to the buffer, this is an
   error.  */

static size_t
expr_to_char (gfc_expr *e, locus *loc,
	      unsigned char *data, unsigned char *chk, size_t len)
{
  int i;
  int ptr;
  gfc_constructor *c;
  gfc_component *cmp;
  unsigned char *buffer;

  if (e == NULL)
    return 0;

  /* Take a derived type, one component at a time, using the offsets from the backend
     declaration.  */
  if (e->ts.type == BT_DERIVED)
    {
      for (c = gfc_constructor_first (e->value.constructor),
	   cmp = e->ts.u.derived->components;
	   c; c = gfc_constructor_next (c), cmp = cmp->next)
	{
	  gcc_assert (cmp && cmp->backend_decl);
	  if (!c->expr)
	    continue;
	  ptr = TREE_INT_CST_LOW(DECL_FIELD_OFFSET(cmp->backend_decl))
	    + TREE_INT_CST_LOW(DECL_FIELD_BIT_OFFSET(cmp->backend_decl))/8;
	  expr_to_char (c->expr, loc, &data[ptr], &chk[ptr], len);
	}
      return len;
    }

  /* Otherwise, use the target-memory machinery to write a bitwise image, appropriate
     to the target, in a buffer and check off the initialized part of the buffer.  */
  gfc_target_expr_size (e, &len);
  buffer = (unsigned char*)alloca (len);
  len = gfc_target_encode_expr (e, buffer, len);

  for (i = 0; i < (int)len; i++)
    {
      if (chk[i] && (buffer[i] != data[i]))
	{
	  if (loc)
	    gfc_error ("Overlapping unequal initializers in EQUIVALENCE "
			"at %L", loc);
	  else
	    gfc_error ("Overlapping unequal initializers in EQUIVALENCE "
			"at %C");
	  return 0;
	}
      chk[i] = 0xFF;
    }

  memcpy (data, buffer, len);
  return len;
}


/* Writes the values from the equivalence initializers to a char* array
   that will be written to the constructor to make the initializer for
   the union declaration.  */

size_t
gfc_merge_initializers (gfc_typespec ts, gfc_expr *e, locus *loc,
			unsigned char *data,
			unsigned char *chk, size_t length)
{
  size_t len = 0;
  gfc_constructor * c;

  switch (e->expr_type)
    {
    case EXPR_CONSTANT:
    case EXPR_STRUCTURE:
      len = expr_to_char (e, loc, &data[0], &chk[0], length);
      break;

    case EXPR_ARRAY:
      for (c = gfc_constructor_first (e->value.constructor);
	   c; c = gfc_constructor_next (c))
	{
	  size_t elt_size;

	  gfc_target_expr_size (c->expr, &elt_size);

	  if (mpz_cmp_si (c->offset, 0) != 0)
	    len = elt_size * (size_t)mpz_get_si (c->offset);

	  len = len + gfc_merge_initializers (ts, c->expr, loc, &data[len],
					      &chk[len], length - len);
	}
      break;

    default:
      return 0;
    }

  return len;
}


/* Transfer the bitpattern of a (integer) BOZ to real or complex variables.
   When successful, no BOZ or nothing to do, true is returned.  */

bool
gfc_convert_boz (gfc_expr *expr, gfc_typespec *ts)
{
  size_t buffer_size, boz_bit_size, ts_bit_size;
  int index;
  unsigned char *buffer;

  if (expr->ts.type != BT_INTEGER)
    return true;

  /* Don't convert BOZ to logical, character, derived etc.  */
  gcc_assert (ts->type == BT_REAL);

  buffer_size = size_float (ts->kind);
  ts_bit_size = buffer_size * 8;

  /* Convert BOZ to the smallest possible integer kind.  */
  boz_bit_size = mpz_sizeinbase (expr->value.integer, 2);

  gcc_assert (boz_bit_size <= ts_bit_size);

  for (index = 0; gfc_integer_kinds[index].kind != 0; ++index)
    if ((unsigned) gfc_integer_kinds[index].bit_size >= ts_bit_size)
      break;

  expr->ts.kind = gfc_integer_kinds[index].kind;
  buffer_size = MAX (buffer_size, size_integer (expr->ts.kind));

  buffer = (unsigned char*)alloca (buffer_size);
  encode_integer (expr->ts.kind, expr->value.integer, buffer, buffer_size);
  mpz_clear (expr->value.integer);

  mpfr_init (expr->value.real);
  gfc_interpret_float (ts->kind, buffer, buffer_size, expr->value.real);

  expr->ts.type = ts->type;
  expr->ts.kind = ts->kind;

  return true;
}
