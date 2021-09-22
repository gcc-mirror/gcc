#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *arg_float128,
		   CFI_cdesc_t *arg_complex128);

/* Sanity check the type info in the descriptor a.  */

static void
check (CFI_cdesc_t *a, size_t size, int typecode)
{
  dump_CFI_cdesc_t (a);
  if (a->attribute != CFI_attribute_pointer)
    abort ();
  if (a->base_addr != NULL)
    abort ();
  if (a->rank != 0)
    abort ();
  if (size && a->elem_len != size)
    abort ();
  if (a->type != typecode)
    abort ();
}

void
ctest (CFI_cdesc_t *arg_float128,
       CFI_cdesc_t *arg_complex128)
{
  check (arg_float128, sizeof (_Float128), CFI_type_float128);
  check (arg_complex128, sizeof (_Float128) * 2,
	 CFI_type_float128_Complex);
}

