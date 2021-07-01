#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

/* For simplicity, point descriptors at a static buffer.  BUFSIZE should
   be large enough for any of the standard types and we'll use DIM0 and DIM1
   for array dimensions.  */
#define BUFSIZE 64
#define DIM0 3
#define DIM1 10
#define ARRAYBUFSIZE BUFSIZE * DIM0 * DIM1
static char *buf[ARRAYBUFSIZE] __attribute__ ((aligned (8)));
static CFI_index_t extents[] = {DIM0, DIM1};

/* Magic number to use for elem_len field.  */
#define MAGIC_ELEM_LEN 20

struct tc_info
{
  CFI_type_t typecode;
  char *name;
  size_t size;
};

static struct tc_info tc_table[] =
{
  { CFI_type_signed_char, "CFI_type_signed_char", sizeof (signed char) },
  { CFI_type_short, "CFI_type_short", sizeof (short) },
  { CFI_type_int, "CFI_type_int", sizeof (int) },
  { CFI_type_long, "CFI_type_long", sizeof (long) },
  { CFI_type_long_long, "CFI_type_long_long", sizeof (long long) },
  { CFI_type_size_t, "CFI_type_size_t", sizeof (size_t) },
  { CFI_type_int8_t, "CFI_type_int8_t", sizeof (int8_t) },
  { CFI_type_int16_t, "CFI_type_int16_t", sizeof (int16_t) },
  { CFI_type_int32_t, "CFI_type_int32_t", sizeof (int32_t) },
  { CFI_type_int64_t, "CFI_type_int64_t", sizeof (int64_t) },
  { CFI_type_int_least8_t, "CFI_type_int_least8_t", sizeof (int_least8_t) },
  { CFI_type_int_least16_t, "CFI_type_int_least16_t", sizeof (int_least16_t) },
  { CFI_type_int_least32_t, "CFI_type_int_least32_t", sizeof (int_least32_t) },
  { CFI_type_int_least64_t, "CFI_type_int_least64_t", sizeof (int_least64_t) },
  { CFI_type_int_fast8_t, "CFI_type_int_fast8_t", sizeof (int_fast8_t) },
  { CFI_type_int_fast16_t, "CFI_type_int_fast16_t", sizeof (int_fast16_t) },
  { CFI_type_int_fast32_t, "CFI_type_int_fast32_t", sizeof (int_fast32_t) },
  { CFI_type_int_fast64_t, "CFI_type_int_fast64_t", sizeof (int_fast64_t) },
  { CFI_type_intmax_t, "CFI_type_intmax_t", sizeof (intmax_t) },
  { CFI_type_intptr_t, "CFI_type_intptr_t", sizeof (intptr_t) },
  { CFI_type_ptrdiff_t, "CFI_type_ptrdiff_t", sizeof (ptrdiff_t) },
  { CFI_type_float, "CFI_type_float", sizeof (float) },
  { CFI_type_double, "CFI_type_double", sizeof (double) },
  { CFI_type_long_double, "CFI_type_long_double", sizeof (long double) },
  { CFI_type_float_Complex, "CFI_type_float_Complex",
    sizeof (float _Complex) },
  { CFI_type_double_Complex, "CFI_type_double_Complex",
    sizeof (double _Complex) },
  { CFI_type_long_double_Complex, "CFI_type_long_double_Complex",
    sizeof (long double _Complex) },
  { CFI_type_Bool, "CFI_type_Bool", sizeof (_Bool) },
  { CFI_type_char, "CFI_type_char", sizeof (char) },
  { CFI_type_cptr, "CFI_type_cptr", sizeof (void *) },
  { CFI_type_struct, "CFI_type_struct", 0 },
  { CFI_type_other, "CFI_type_other", -1 }
};

int
test_array (struct tc_info *tc, void *ptr, CFI_attribute_t attr)
{
  CFI_CDESC_T(2) desc;
  CFI_cdesc_t *a = (CFI_cdesc_t *) &desc;
  int bad = 0;
  size_t elem_len;

  /* Initialize the descriptor to garbage values so we can confirm it's
     properly initialized with good ones later.  */
  memset (a, -1, sizeof(desc));
  
  check_CFI_status ("CFI_establish",
		    CFI_establish (a, ptr, attr, tc->typecode,
				   MAGIC_ELEM_LEN, 2, extents));

  /* elem_len is ignored unless type is CFI type struct, CFI type other, 
     or a character type.  */
  if (tc->typecode == CFI_type_char
      || tc->typecode == CFI_type_struct
      || tc->typecode == CFI_type_other)
    elem_len = MAGIC_ELEM_LEN;
  else
    elem_len = tc->size;

  if (a->elem_len != elem_len
      || a->base_addr != ptr
      || a->type != tc->typecode
      || a->version != CFI_VERSION
      || a->attribute != attr
      || a->rank != 2
      || (ptr &&
	  /* extents parameter is ignored if ptr is null */
	  (a->dim[0].lower_bound != 0
	   || a->dim[0].extent != DIM0
	   || a->dim[0].sm != elem_len
	   || a->dim[1].lower_bound != 0
	   || a->dim[1].extent != DIM1
	   || a->dim[1].sm != elem_len*DIM0)))
    {
      fprintf (stderr, "Bad array descriptor for %s:\n", tc->name);
      dump_CFI_cdesc_t (a);
      return 1;
    }
  return 0;
}

/* External entry point.  */
extern void ctest_establish (void);

void
ctest_establish (void)
{
  int ncodes = sizeof (tc_table) / sizeof (struct tc_info);
  int i;
  int bad = 0;

  for (i = 0; i < ncodes; i++)
    {
      bad += test_array (&tc_table[i], (void *)buf, CFI_attribute_other);
      bad += test_array (&tc_table[i], NULL, CFI_attribute_allocatable);
      bad += test_array (&tc_table[i], (void *)buf, CFI_attribute_pointer);
    }
  if (bad)
    abort ();
}

