#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *arg_long_double,
		   CFI_cdesc_t *arg_long_double_complex);

/* Sanity check the type info in the descriptor a.  */

static void
check (CFI_cdesc_t *a, size_t size, int typecode)
{
  dump_CFI_cdesc_t (a);
  if (a->attribute != CFI_attribute_other)
    abort ();
  if (a->base_addr == NULL)
    abort ();
  if (a->rank != 1)
    abort ();
  if (size && a->elem_len != size)
    abort ();
  if (a->type != typecode)
    abort ();
}

void
ctest (CFI_cdesc_t *arg_long_double,
       CFI_cdesc_t *arg_long_double_complex)
{
  check (arg_long_double, sizeof (long double), CFI_type_long_double);
  check (arg_long_double_complex, sizeof (long double _Complex),
	 CFI_type_long_double_Complex);
}
