/* This file contains some useful routines for debugging problems with C
   descriptors.  Compiling it also acts as a test that the implementation of
   ISO_Fortran_binding.h provides all the types and constants specified in
   TS29113.  */

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include "dump-descriptors.h"

void
dump_CFI_cdesc_t (CFI_cdesc_t *d)
{
  fprintf (stderr, "<CFI_cdesc_t base_addr=%p elem_len=%ld version=%d",
	   d->base_addr, (long)(d->elem_len), d->version);
  fprintf (stderr, "\n  rank=");
  dump_CFI_rank_t (d->rank);
  fprintf (stderr, " type=");
  dump_CFI_type_t (d->type);
  fprintf (stderr, " attribute=");
  dump_CFI_attribute_t (d->attribute);
  
  /* Dimension info may not be initialized if it's an allocatable
     or pointer descriptor with a null base_addr.  */
  if (d->rank > 0 && d->base_addr)
    {
      CFI_rank_t i;
      for (i = 0; i < d->rank; i++)
	{
	  if (i == 0)
	    fprintf (stderr, "\n  dim=[");
	  else
	    fprintf (stderr, ",\n       ");
	  dump_CFI_dim_t (d->dim + i);
	}
      fprintf (stderr, "]");
    }
  fprintf (stderr, ">\n");
}

void
dump_CFI_dim_t (CFI_dim_t *d)
{
  fprintf (stderr, "<CFI_dim_t lower_bound=");
  dump_CFI_index_t (d->lower_bound);
  fprintf (stderr, " extent=");
  dump_CFI_index_t (d->extent);
  fprintf (stderr, " sm=");
  dump_CFI_index_t (d->sm);
  fprintf (stderr, ">");
}

void
dump_CFI_attribute_t (CFI_attribute_t a)
{
  switch (a)
    {
    case CFI_attribute_pointer:
      fprintf (stderr, "CFI_attribute_pointer");
      break;
    case CFI_attribute_allocatable:
      fprintf (stderr, "CFI_attribute_allocatable");
      break;
    case CFI_attribute_other:
      fprintf (stderr, "CFI_attribute_other");
      break;
    default:
      fprintf (stderr, "unknown(%d)", (int)a);
      break;
    }
}

void
dump_CFI_index_t (CFI_index_t i)
{
  fprintf (stderr, "%ld", (long)i);
}

void
dump_CFI_rank_t (CFI_rank_t r)
{
  fprintf (stderr, "%d", (int)r);
}

/* We can't use a switch statement to dispatch CFI_type_t because
   the type name macros may not be unique.  Iterate over a table
   instead.  */

struct type_name_map {
  CFI_type_t t;
  const char *n;
};

struct type_name_map type_names[] =
{
  { CFI_type_signed_char, "CFI_type_signed_char" },
  { CFI_type_short, "CFI_type_short" },
  { CFI_type_int, "CFI_type_int" },
  { CFI_type_long, "CFI_type_long" },
  { CFI_type_long_long, "CFI_type_long_long" },
  { CFI_type_size_t, "CFI_type_size_t" },
  { CFI_type_int8_t, "CFI_type_int8_t" },
  { CFI_type_int16_t, "CFI_type_int16_t" },
  { CFI_type_int32_t, "CFI_type_int32_t" },
  { CFI_type_int64_t, "CFI_type_int64_t" },
  { CFI_type_int_least8_t, "CFI_type_int_least8_t" },
  { CFI_type_int_least16_t, "CFI_type_int_least16_t" },
  { CFI_type_int_least32_t, "CFI_type_int_least32_t" },
  { CFI_type_int_least64_t, "CFI_type_int_least64_t" },
  { CFI_type_int_fast8_t, "CFI_type_int_fast8_t" },
  { CFI_type_int_fast16_t, "CFI_type_int_fast16_t" },
  { CFI_type_int_fast32_t, "CFI_type_int_fast32_t" },
  { CFI_type_int_fast64_t, "CFI_type_int_fast64_t" },
  { CFI_type_intmax_t, "CFI_type_intmax_t" },
  { CFI_type_intptr_t, "CFI_type_intptr_t" },
  { CFI_type_ptrdiff_t, "CFI_type_ptrdiff_t" },
  { CFI_type_float, "CFI_type_float" },
  { CFI_type_double, "CFI_type_double" },
  { CFI_type_long_double, "CFI_type_long_double" },
  { CFI_type_float_Complex, "CFI_type_float_Complex" },
  { CFI_type_double_Complex, "CFI_type_double_Complex" },
  { CFI_type_long_double_Complex, "CFI_type_long_double_Complex" },
  { CFI_type_Bool, "CFI_type_Bool" },
  { CFI_type_char, "CFI_type_char" },
  { CFI_type_cptr, "CFI_type_cptr" },
  { CFI_type_struct, "CFI_type_struct" },
  { CFI_type_other, "CFI_type_other" },
  /* Extension types */
  { CFI_type_int128_t, "CFI_type_int128_t" },
  { CFI_type_int_least128_t, "CFI_type_int_least128_t" },
  { CFI_type_int_fast128_t, "CFI_type_int_fast128_t" },
  { CFI_type_ucs4_char, "CFI_type_ucs4_char" },
  { CFI_type_float128, "CFI_type_float128" },
  { CFI_type_float128_Complex, "CFI_type_float128_Complex" },
  { CFI_type_cfunptr, "CFI_type_cfunptr" }
};
  
void
dump_CFI_type_t (CFI_type_t t)
{
  int i;
  for (i = 0; i < sizeof (type_names) / sizeof (struct type_name_map); i++)
    if (type_names[i].t == t)
      {
	fprintf (stderr, "%s", type_names[i].n);
	return;
      }
  fprintf (stderr, "unknown(%d)", (int)t);
}

void
check_CFI_status (const char *fn, int code)
{
  const char *msg;
  switch (code)
    {
    case CFI_SUCCESS:
      return;
    case CFI_ERROR_BASE_ADDR_NULL:
      msg = "CFI_ERROR_BASE_ADDR_NULL";
      break;
    case CFI_ERROR_BASE_ADDR_NOT_NULL:
      msg = "CFI_ERROR_BASE_ADDR_NOT_NULL";
      break;
    case CFI_INVALID_ELEM_LEN:
      msg = "CFI_INVALID_ELEM_LEN";
      break;
    case CFI_INVALID_RANK:
      msg = "CFI_INVALID_RANK";
      break;
    case CFI_INVALID_TYPE:
      msg = "CFI_INVALID_TYPE";
      break;
    case CFI_INVALID_ATTRIBUTE:
      msg = "CFI_INVALID_ATTRIBUTE";
      break;
    case CFI_INVALID_EXTENT:
      msg = "CFI_INVALID_EXTENT";
      break;
    case CFI_INVALID_DESCRIPTOR:
      msg = "CFI_INVALID_DESCRIPTOR";
      break;
    case CFI_ERROR_MEM_ALLOCATION:
      msg = "CFI_ERROR_MEM_ALLOCATION";
      break;
    case CFI_ERROR_OUT_OF_BOUNDS:
      msg = "CFI_ERROR_OUT_OF_BOUNDS";
      break;
    default:
      msg = "unknown error";
      break;
    }
  fprintf (stderr, "%s returned %s\n", fn, msg);
  abort ();
}
