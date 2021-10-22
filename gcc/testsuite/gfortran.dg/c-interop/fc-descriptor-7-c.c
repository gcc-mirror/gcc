#include <stdlib.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *, _Bool);

void
ctest (CFI_cdesc_t *a, _Bool is_cont)
{
  CFI_index_t subscripts[2];
  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */

#if DEBUG
  dump_CFI_cdesc_t (a);
#endif

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

  if (is_cont != CFI_is_contiguous (a))
    abort ();

  if (abs (a->dim[0].sm) < a->elem_len)
    abort ();

  for (int j = 0; j < 5; ++j)
    for (int i = 0; i < 10; ++i)
      {
	subscripts[0] = j; subscripts[1] = i;
	if (*(int *) CFI_address (a, subscripts) != (i+1) + 100*(j+1))
	  abort ();
      }
}
