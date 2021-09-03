#include <stdlib.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *a, int n);

void
ctest (CFI_cdesc_t *a, int n)
{
  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);

  if (!a->base_addr)
    abort ();
  if (a->elem_len != sizeof(float))
    abort ();
  if (a->type != CFI_type_float)
    abort ();
  if (a->attribute != CFI_attribute_other)
    abort ();

  if (n == 1)
    {
      /* The actual argument on the Fortran side was declared as 
	 real(C_FLOAT):: aa(100)  */
      if (a->rank != 1)
	abort ();
      if (a->dim[0].lower_bound != 0)
	abort ();
      if (a->dim[0].extent != 100)
	abort ();
      if (a->dim[0].sm != sizeof(float))
	abort ();
      if (!CFI_is_contiguous (a))
	abort ();
    }
  else if (n == 3)
    {
      /* The actual argument on the Fortran side was declared as 
	 real(C_FLOAT) :: bb(3,4,5)  */
      if (a->rank != 3)
	abort ();
      if (a->dim[0].lower_bound != 0)
	abort ();
      if (a->dim[0].extent != 3)
	abort ();
      if (a->dim[0].sm != sizeof(float))
	abort ();
      if (a->dim[1].lower_bound != 0)
	abort ();
      if (a->dim[1].extent != 4)
	abort ();
      if (a->dim[1].sm != a->dim[0].sm * a->dim[0].extent)
	abort ();
      if (a->dim[2].lower_bound != 0)
	abort ();
      if (a->dim[2].extent != 5)
	abort ();
      if (a->dim[2].sm != a->dim[1].sm * a->dim[1].extent)
	abort ();
      if (!CFI_is_contiguous (a))
	abort ();
    }
  else
    abort ();
}
