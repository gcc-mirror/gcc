#include <stdlib.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *a);

void
ctest (CFI_cdesc_t *a)
{
  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);

  /* The character object passed as the argument was declared on the
     Fortran side as character(len=26) :: aa
     Make sure that matches what's in the descriptor.  */
  if (!a->base_addr)
    abort ();
  if (a->elem_len != 26)
    abort ();
  if (a->rank != 0)
    abort ();
  if (a->type != CFI_type_char)
    abort ();
  if (a->attribute != CFI_attribute_other)
    abort ();
  strncpy ((char *)a->base_addr, "0123456789", 10);
}
