#include <stdlib.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *a, CFI_cdesc_t *b, int initp);

void
ctest (CFI_cdesc_t *a, CFI_cdesc_t *b, int initp)
{
  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);
  dump_CFI_cdesc_t (b);

  /* Make sure the descriptors match what we are expecting.  a is an
     allocatable derived type object, b is a pointer which points at a
     if initp is true.  */
  if (initp && !a->base_addr)
    abort ();
  else if (!initp && a->base_addr)
    abort ();
  if (a->base_addr != b->base_addr)
    abort ();

  if (a->type != CFI_type_struct)
    abort ();
  if (b->type != CFI_type_struct)
    abort ();
  if (a->elem_len != 3 * 3 * sizeof(double))
    abort ();
  if (b->elem_len != 3 * 3 * sizeof(double))
    abort ();
  if (a->attribute != CFI_attribute_allocatable)
    abort ();
  if (b->attribute != CFI_attribute_pointer)
    abort ();

  if (initp)
    /* The actual array is allocated with
         allocate (aa(3:7))
       Per 8.3.3 of TS29113, the lower_bound must reflect that.  */
    {
      if (a->rank != 1)
	abort ();
      if (b->rank != 1)
	abort ();
      if (a->dim[0].lower_bound != 3)
	abort ();
      if (b->dim[0].lower_bound != 3)
	abort ();
      if (a->dim[0].extent != 5)
	abort ();
      if (b->dim[0].extent != 5)
	abort ();
    }
}
