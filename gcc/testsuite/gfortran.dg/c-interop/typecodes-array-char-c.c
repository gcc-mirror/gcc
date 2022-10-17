#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest_1 (CFI_cdesc_t *arg_char, CFI_cdesc_t *arg_ucs4);

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
ctest_1 (CFI_cdesc_t *arg_char, CFI_cdesc_t *arg_ucs4)
{
  check (arg_char, 1, CFI_type_char);
  check (arg_ucs4, 4, CFI_type_ucs4_char);
}

void
ctest_5 (CFI_cdesc_t *arg_char, CFI_cdesc_t *arg_ucs4)
{
  check (arg_char, 5*1, CFI_type_char);
  check (arg_ucs4, 5*4, CFI_type_ucs4_char);
}
