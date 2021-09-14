#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *arg_int128,
		   CFI_cdesc_t *arg_least128,
		   CFI_cdesc_t *arg_fast128);

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
ctest (CFI_cdesc_t *arg_int128,
       CFI_cdesc_t *arg_least128,
       CFI_cdesc_t *arg_fast128)
{
  check (arg_int128, sizeof (__int128), CFI_type_int128_t);
  check (arg_least128, sizeof (__int128), CFI_type_int_least128_t);
  check (arg_fast128, sizeof (__int128), CFI_type_int_fast128_t);
}

