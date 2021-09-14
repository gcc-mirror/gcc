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

  /* We expect to get an array of shape (5,10) that may not be
     contiguous.  */
  if (!a->base_addr)
    abort ();
  if (a->elem_len != sizeof(int))
    abort ();
  if (a->rank != 2)
    abort ();
  if (a->type != CFI_type_int)
    abort ();
  if (a->attribute != CFI_attribute_other)
    abort ();
  if (a->dim[0].lower_bound != 0)
    abort ();
  if (a->dim[0].extent != 5)
    abort ();
  if (a->dim[1].lower_bound != 0)
    abort ();
  if (a->dim[1].extent != 10)
    abort ();

  /* There shall be an ordering of the dimensions such that the absolute
     value of the sm member of the first dimension is not less than the 
     elem_len member of the C descriptor and the absolute value of the sm 
     member of each subsequent dimension is not less than the absolute 
     value of the sm member of the previous dimension multiplied
     by the extent of the previous dimension.  */
  if (abs (a->dim[0].sm) < a->elem_len)
    abort ();
  if (abs (a->dim[1].sm) < abs (a->dim[0].sm) * a->dim[0].extent)
    abort ();
}
