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
       integer(C_INT) :: aa(10,-1:3)
     Make sure that matches what's in the descriptor.  Note that per
     section 18.5.3 in the 2018 standard, for a nonallocatable nonpointer
     array, the array dimensions in the descriptor reflect the shape of
     the array rather than the actual bounds; the lower_bound is required
     to be zero.  */
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
  if (a->dim[0].extent != 10)
    abort ();
  if (a->dim[0].sm != sizeof(int))
    abort ();
  if (a->dim[1].lower_bound != 0)
    abort ();
  if (a->dim[1].extent != 5)
    abort ();
  if (a->dim[1].sm != a->dim[0].extent * sizeof(int))
    abort ();
  if (!CFI_is_contiguous (a))
    abort ();
}
