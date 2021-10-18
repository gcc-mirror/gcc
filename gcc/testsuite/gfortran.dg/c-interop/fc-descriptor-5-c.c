#include <stdlib.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *a);

void
ctest (CFI_cdesc_t *a)
{
  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);

  /* The actual argument on the Fortran side was declared as 
     character(len=20) :: aa
     Make sure that matches what's in the descriptor.  */
  if (!a->base_addr)
    abort ();
  if (a->elem_len != 20)
    abort ();
  if (a->rank != 0)
    abort ();
  if (a->type != CFI_type_char)
    abort ();
  if (a->attribute != CFI_attribute_other)
    abort ();
}
