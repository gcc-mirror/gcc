#include <stdlib.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest1 (CFI_cdesc_t *a);
extern void ctest2 (CFI_cdesc_t *a);

static void
ctest (CFI_cdesc_t *a)
{
  int i;
  int *p;

  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);

  /* Make sure we got a valid descriptor.  */
  if (!a->base_addr)
    abort ();
  if (a->elem_len != sizeof(int))
    abort ();
  if (a->rank != 1)
    abort ();
  if (a->type != CFI_type_int)
    abort ();
  if (a->attribute != CFI_attribute_other)
    abort ();
  if (a->dim[0].sm != sizeof(int))
    abort ();
  if (!CFI_is_contiguous (a))
    abort ();

  /* Negate the elements of the array.  */
  p = (int *)a->base_addr;
  for (i = 0; i < a->dim[0].extent; i++)
    p[i] = -p[i];
}


/* The two entry points are declared differently on the C side, but both
   should do the same thing.  */

void
ctest1 (CFI_cdesc_t *a)
{
  ctest (a);
}

void
ctest2 (CFI_cdesc_t *a)
{
  ctest (a);
}

