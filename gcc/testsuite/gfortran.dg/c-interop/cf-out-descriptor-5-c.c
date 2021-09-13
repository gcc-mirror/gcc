#include <stdlib.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *a, int n);
extern void ftest (CFI_cdesc_t *a, int n);

void
ctest (CFI_cdesc_t *a, int n)
{
  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);

  /* The actual argument object on the Fortran side has length n and
     was passed as character(len=*).
     Make sure that matches what's in the descriptor.  */
  if (!a->base_addr)
    abort ();
  if (a->elem_len != n)
    abort ();
  if (a->rank != 0)
    abort ();
  if (a->type != CFI_type_char)
    abort ();
  if (a->attribute != CFI_attribute_other)
    abort ();
  ftest (a, n);
}
