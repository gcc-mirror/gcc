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
       integer(C_INT) :: aa(10,5:8)
     but was passed via other functions that variously describe it as
     having size (10,*), (10,1:*), or (10,5:*).  But, the spec says:

       For a C descriptor of a nonallocatable nonpointer object, the
       value of the lower_bound member of each element of the dim member
       of the descriptor is zero.

       In a C descriptor of an assumed-size array, the extent member of
       the last element of the dim member has the value −1.  */

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
  if (a->dim[1].extent != -1)
    abort ();
  if (a->dim[1].sm != a->dim[0].extent * sizeof(int))
    abort ();
}


