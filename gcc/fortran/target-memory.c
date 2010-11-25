/* Simulate storage of variables into target memory.
   Copyright (C) 2007, 2008, 2009
   Free Software Foundation, Inc.
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
#include "flags.h"
#include "machmode.h"
#include "tree.h"
#include "gfortran.h"
#include "arith.h"
#include "trans.h"
#include "trans-const.h"
#include "trans-types.h"
#include "target-memory.h"

/* --------------------------------------------------------------- */ 
/* Calculate the size of an expression.  */

static size_t
size_array (gfc_expr *e)
{
  mpz_t array_size;
  size_t elt_size = gfc_target_expr_size (e->value.constructor->expr);

  gfc_array_size (e, &array_size);
  return (size_t)mpz_get_ui (array_size) * elt_size;
}

static size_t
size_integer (int kind)
{
  return GET_MODE_SIZE (TYPE_MODE (gfc_get_int_type (kind)));;
}


static size_t
size_float (int kind)
{
  return GET_MODE_SIZE (TYPE_MODE (gfc_get_real_type (kind)));;
}


static size_t
size_complex (int kind)
{
  return 2 * size_float (kind);
}


static size_t
size_logical (int kind)
{
  return GET_MODE_SIZE (TYPE_MODE (gfc_get_logical_type (kind)));;
}


static size_t
size_character (int length, int kind)
{
  int i = gfc_validate_kind (BT_CHARACTER, kind, false);
  return length * gfc_character_kinds[i].bit_size / 8;
}


size_t
gfc_target_expr_size (gfc_expr *e)
{
  tree type;

  gcc_assert (e != NULL);

  if (e->expr_type == EXPR_ARRAY)
    return size_array (e);

  switch (e->ts.type)
    {
    case BT_INTEGER:
      return size_integer (e->ts.kind);
    case BT_REAL:
      return size_float (e->ts.kind);
    case BT_COMPLEX:
      return size_complex (e->ts.kind);
    case BT_LOGICAL:
      return size_logical (e->ts.kind);
    case BT_CHARACTER:
      if (e->expr_type == EXPR_SUBSTRING && e->ref)
        {
          int start, end;

          gfc_extract_int (e->ref->u.ss.start, &start);
          gfc_extract_int (e->ref->u.ss.end, &end);
          return size_character (MAX(end - start + 1, 0), e->ts.kind);
        }
      else
        return size_character (e->value.character.length, e->ts.kind);
    case BT_HOLLERITH:
      return e->representation.length;
    case BT_DERIVED:
      type = gfc_typenode_for_spec (&e->ts);
      return int_size_in_bytes (type);
    default:
      gfc_internal_error ("Invalid expression in gfc_target_expr_size.");
      return 0;
    }
}


/* The encode_* functions export a value into a buffer, and 
   return the number of bytes of the buffer that have been
   used.  */

static int
encode_array (gfc_expr *expr, unsigned char *buffer, size_t buffer_size)
{
  mpz_t array_size;
  int i;
  int ptr = 0;

  gfc_array_size (expr, &array_size);
  for (i = 0; i < (int)mpz_get_ui (array_size); i++)
    {
      ptr += gfc_target_encode_expr (gfc_get_array_element (expr, i),
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
  return native_encode_expr (gfc_conv_mpfr_to_tree (real, kind), buffer,
			     buffer_size);
}


static int
encode_complex (int kind, mpfr_t real, mpfr_t imaginary, unsigned char *buffer,
		size_t buffer_size)
{
  int size;
  size = encode_float (kind, real, &buffer[0], buffer_size);
  size += encode_float (kind, imaginary, &buffer[size], buffer_size - size);
  return size;
}


static int
encode_logical (int kind, int logical, unsigned char *buffer, size_t buffer_size)
{
  return native_encode_expr (build_int_cst (gfc_get_logical_type (kind),
					    logical),
			     buffer, buffer_size);
}


int
gfc_encode_character (int kind, int length, const gfc_char_t *string,
		      unsigned char *buffer, size_t buffer_size)
{
  size_t elsize = size_character (1, kind);
  tree type = gfc_get_char_type (kind);
  int i;

  gcc_assert (buffer_size >= size_character (length, kind));

  for (i = 0; i < length; i++)
    native_encode_expr (build_int_cst (type, string[i]), &buffer[i*elsize],
			elsize);

  return length;
}


static int
encode_derived (gfc_expr *source, unsigned char *buffer, size_t buffer_size)
{
  gfc_constructor *ctr;
  gfc_component *cmp;
  int ptr;
  tree type;

  type = gfc_typenode_for_spec (&source->ts);

  ctr = source->value.constructor;
  cmp = source->ts.derived->components;
  for (;ctr; ctr = ctr->next, cmp = cmp->next)
    {
      gcc_assert (cmp);
      if (!ctr->expr)
	continue;
      ptr = TREE_INT_CST_LOW(DECL_FIELD_OFFSET(cmp->backend_decl))
	    + TREE_INT_CST_LOW(DECL_FIELD_BIT_OFFSET(cmp->backend_decl))/8;

      if (ctr->expr->expr_type == EXPR_NULL)
 	memset (&buffer[ptr], 0,
		int_size_in_bytes (TREE_TYPE (cmp->backend_decl)));
      else
	gfc_target_encode_expr (ctr->expr, &buffer[ptr],
				buffer_size - ptr);
    }

  return int_size_in_bytes (type);
}


/* Write a constant expression in binary form to a buffer.  */
int
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
      return encode_complex (source->ts.kind, source->value.complex.r,
			     source->value.complex.i, buffer, buffer_size);
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
	  int start, end;

	  gcc_assert (source->expr_type == EXPR_SUBSTRING);
	  gfc_extract_int (source->ref->u.ss.start, &start);
	  gfc_extract_int (source->ref->u.ss.end, &end);
	  return gfc_encode_character (source->ts.kind, MAX(end - start + 1, 0),
				       &source->value.character.string[start-1],
				       buffer, buffer_size);
	}

    case BT_DERIVED:
      return encode_derived (source, buffer, buffer_size);
    default:
      gfc_internal_error ("Invalid expression in gfc_target_encode_expr.");
      return 0;
    }
}


static int
interpret_array (unsigned char *buffer, size_t buffer_size, gfc_expr *result)
{
  int array_size = 1;
  int i;
  int ptr = 0;
  gfc_constructor *head = NULL, *tail = NULL;

  /* Calculate array size from its shape and rank.  */
  gcc_assert (result->rank > 0 && result->shape);

  for (i = 0; i < result->rank; i++)
    array_size *= (int)mpz_get_ui (result->shape[i]);

  /* Iterate over array elements, producing constructors.  */
  for (i = 0; i < array_size; i++)
    {
      if (head == NULL)
	head = tail = gfc_get_constructor ();
      else
	{
	  tail->next = gfc_get_constructor ();
	  tail = tail->next;
	}

      tail->where = result->where;
      tail->expr = gfc_constant_result (result->ts.type,
					  result->ts.kind, &result->where);
      tail->expr->ts = result->ts;

      if (tail->expr->ts.type == BT_CHARACTER)
	tail->expr->value.character.length = result->value.character.length;

      ptr += gfc_target_interpret_expr (&buffer[ptr], buffer_size - ptr,
					tail->expr);
    }
  result->value.constructor = head;

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
		   mpfr_t real, mpfr_t imaginary)
{
  int size;
  size = gfc_interpret_float (kind, &buffer[0], buffer_size, real);
  size += gfc_interpret_float (kind, &buffer[size], buffer_size - size,
			       imaginary);
  return size;
}


int
gfc_interpret_logical (int kind, unsigned char *buffer, size_t buffer_size,
		   int *logical)
{
  tree t = native_interpret_expr (gfc_get_logical_type (kind), buffer,
				  buffer_size);
  *logical = double_int_zero_p (tree_to_double_int (t))
	     ? 0 : 1;
  return size_logical (kind);
}


int
gfc_interpret_character (unsigned char *buffer, size_t buffer_size,
			 gfc_expr *result)
{
  int i;

  if (result->ts.cl && result->ts.cl->length)
    result->value.character.length =
      (int) mpz_get_ui (result->ts.cl->length->value.integer);

  gcc_assert (buffer_size >= size_character (result->value.character.length,
					     result->ts.kind));
  result->value.character.string =
    gfc_get_wide_string (result->value.character.length + 1);

  if (result->ts.kind == gfc_default_character_kind)
    for (i = 0; i < result->value.character.length; i++)
      result->value.character.string[i] = (gfc_char_t) buffer[i];
  else
    {
      mpz_t integer;
      unsigned bytes = size_character (1, result->ts.kind);
      mpz_init (integer);
      gcc_assert (bytes <= sizeof (unsigned long));

      for (i = 0; i < result->value.character.length; i++)
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
  gfc_constructor *head = NULL, *tail = NULL;
  int ptr;
  tree type;

  /* The attributes of the derived type need to be bolted to the floor.  */
  result->expr_type = EXPR_STRUCTURE;

  type = gfc_typenode_for_spec (&result->ts);
  cmp = result->ts.derived->components;

  /* Run through the derived type components.  */
  for (;cmp; cmp = cmp->next)
    {
      if (head == NULL)
	head = tail = gfc_get_constructor ();
      else
	{
	  tail->next = gfc_get_constructor ();
	  tail = tail->next;
	}

      /* The constructor points to the component.  */
      tail->n.component = cmp;

      tail->expr = gfc_constant_result (cmp->ts.type, cmp->ts.kind,
					&result->where);
      tail->expr->ts = cmp->ts;

      /* Copy shape, if needed.  */
      if (cmp->as && cmp->as->rank)
	{
	  int n;

	  tail->expr->expr_type = EXPR_ARRAY;
	  tail->expr->rank = cmp->as->rank;

	  tail->expr->shape = gfc_get_shape (tail->expr->rank);
	  for (n = 0; n < tail->expr->rank; n++)
	     {
	       mpz_init_set_ui (tail->expr->shape[n], 1);
	       mpz_add (tail->expr->shape[n], tail->expr->shape[n],
			cmp->as->upper[n]->value.integer);
	       mpz_sub (tail->expr->shape[n], tail->expr->shape[n],
			cmp->as->lower[n]->value.integer);
	     }
	}

      /* Calculate the offset, which consists of the the FIELD_OFFSET in
	 bytes, which appears in multiples of DECL_OFFSET_ALIGN-bit-sized,
	 and additional bits of FIELD_BIT_OFFSET. The code assumes that all
	 sizes of the components are multiples of BITS_PER_UNIT,
	 i.e. there are, e.g., no bit fields.  */

      ptr = TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (cmp->backend_decl));
      gcc_assert (ptr % 8 == 0);
      ptr = ptr/8 + TREE_INT_CST_LOW (DECL_FIELD_OFFSET (cmp->backend_decl));

      gfc_target_interpret_expr (&buffer[ptr], buffer_size - ptr,
				 tail->expr);

      result->value.constructor = head;
    }
    
  return int_size_in_bytes (type);
}


/* Read a binary buffer to a constant expression.  */
int
gfc_target_interpret_expr (unsigned char *buffer, size_t buffer_size,
			   gfc_expr *result)
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
			       result->value.complex.r,
			       result->value.complex.i);
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

    case BT_DERIVED:
      result->representation.length = 
        gfc_interpret_derived (buffer, buffer_size, result);
      break;

    default:
      gfc_internal_error ("Invalid expression in gfc_target_interpret_expr.");
      break;
    }

  if (result->ts.type == BT_CHARACTER)
    result->representation.string
      = gfc_widechar_to_char (result->value.character.string,
			      result->value.character.length);
  else
    {
      result->representation.string =
        (char *) gfc_getmem (result->representation.length + 1);
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
expr_to_char (gfc_expr *e, unsigned char *data, unsigned char *chk, size_t len)
{
  int i;
  int ptr;
  gfc_constructor *ctr;
  gfc_component *cmp;
  unsigned char *buffer;

  if (e == NULL)
    return 0;

  /* Take a derived type, one component at a time, using the offsets from the backend
     declaration.  */
  if (e->ts.type == BT_DERIVED)
    {
      ctr = e->value.constructor;
      cmp = e->ts.derived->components;
      for (;ctr; ctr = ctr->next, cmp = cmp->next)
	{
	  gcc_assert (cmp && cmp->backend_decl);
	  if (!ctr->expr)
	    continue;
	    ptr = TREE_INT_CST_LOW(DECL_FIELD_OFFSET(cmp->backend_decl))
			+ TREE_INT_CST_LOW(DECL_FIELD_BIT_OFFSET(cmp->backend_decl))/8;
	  expr_to_char (ctr->expr, &data[ptr], &chk[ptr], len);
	}
      return len;
    }

  /* Otherwise, use the target-memory machinery to write a bitwise image, appropriate
     to the target, in a buffer and check off the initialized part of the buffer.  */
  len = gfc_target_expr_size (e);
  buffer = (unsigned char*)alloca (len);
  len = gfc_target_encode_expr (e, buffer, len);

    for (i = 0; i < (int)len; i++)
    {
      if (chk[i] && (buffer[i] != data[i]))
	{
	  gfc_error ("Overlapping unequal initializers in EQUIVALENCE "
		     "at %L", &e->where);
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
gfc_merge_initializers (gfc_typespec ts, gfc_expr *e, unsigned char *data,
			unsigned char *chk, size_t length)
{
  size_t len = 0;
  gfc_constructor * c;

  switch (e->expr_type)
    {
    case EXPR_CONSTANT:
    case EXPR_STRUCTURE:
      len = expr_to_char (e, &data[0], &chk[0], length);

      break;

    case EXPR_ARRAY:
      for (c = e->value.constructor; c; c = c->next)
	{
	  size_t elt_size = gfc_target_expr_size (c->expr);

	  if (c->n.offset)
	    len = elt_size * (size_t)mpz_get_si (c->n.offset);

	  len = len + gfc_merge_initializers (ts, c->expr, &data[len],
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

  if (!expr->is_boz)
    return true;

  gcc_assert (expr->expr_type == EXPR_CONSTANT
	      && expr->ts.type == BT_INTEGER);

  /* Don't convert BOZ to logical, character, derived etc.  */
  if (ts->type == BT_REAL)
    {
      buffer_size = size_float (ts->kind);
      ts_bit_size = buffer_size * 8;
    }
  else if (ts->type == BT_COMPLEX)
    {
      buffer_size = size_complex (ts->kind);
      ts_bit_size = buffer_size * 8 / 2;
    }
  else
    return true;

  /* Convert BOZ to the smallest possible integer kind.  */
  boz_bit_size = mpz_sizeinbase (expr->value.integer, 2);

  if (boz_bit_size > ts_bit_size)
    {
      gfc_error_now ("BOZ constant at %L is too large (%ld vs %ld bits)",
		     &expr->where, (long) boz_bit_size, (long) ts_bit_size);
      return false;
    }

  for (index = 0; gfc_integer_kinds[index].kind != 0; ++index)
    if ((unsigned) gfc_integer_kinds[index].bit_size >= ts_bit_size)
      break;

  expr->ts.kind = gfc_integer_kinds[index].kind;
  buffer_size = MAX (buffer_size, size_integer (expr->ts.kind));

  buffer = (unsigned char*)alloca (buffer_size);
  encode_integer (expr->ts.kind, expr->value.integer, buffer, buffer_size);
  mpz_clear (expr->value.integer);

  if (ts->type == BT_REAL)
    {
      mpfr_init (expr->value.real);
      gfc_interpret_float (ts->kind, buffer, buffer_size, expr->value.real);
    }
  else
    {
      mpfr_init (expr->value.complex.r);
      mpfr_init (expr->value.complex.i);
      gfc_interpret_complex (ts->kind, buffer, buffer_size,
			     expr->value.complex.r, expr->value.complex.i);
    }
  expr->is_boz = 0;  
  expr->ts.type = ts->type;
  expr->ts.kind = ts->kind;

  return true;
}
