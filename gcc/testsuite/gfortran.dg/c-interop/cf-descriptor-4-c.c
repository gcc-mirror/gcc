#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (int imagic, int jmagic);
extern void ftest (CFI_cdesc_t *a, CFI_cdesc_t *b, int initp);

struct m {
  int i;
  int j;
};

void
ctest (int imax, int jmax)
{
  CFI_CDESC_T(2) adesc;
  CFI_CDESC_T(2) bdesc;
  CFI_cdesc_t *a = (CFI_cdesc_t *) &adesc;
  CFI_cdesc_t *b = (CFI_cdesc_t *) &bdesc;
  struct m* mp;
  CFI_index_t lower[2], upper[2], subscripts[2];
  CFI_index_t i, j;

  /* Create the descriptor for a, then sanity-check it.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (a, NULL, CFI_attribute_allocatable,
				   CFI_type_struct,
				   sizeof (struct m), 2, NULL));
  dump_CFI_cdesc_t (a);
  if (a->version != CFI_VERSION)
    abort ();
  if (a->rank != 2)
    abort ();
  if (a->attribute != CFI_attribute_allocatable)
    abort ();
  if (a->base_addr)
    abort ();
  if (a->elem_len != sizeof (struct m))
    abort ();

  /* Likewise for b.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (b, NULL, CFI_attribute_pointer,
				   CFI_type_struct,
				   sizeof (struct m), 2, NULL));
  dump_CFI_cdesc_t (b);
  if (b->version != CFI_VERSION)
    abort ();
  if (b->rank != 2)
    abort ();
  if (b->attribute != CFI_attribute_pointer)
    abort ();
  if (b->base_addr)
    abort ();
  if (b->elem_len != sizeof (struct m))
    abort ();

  /* Call back into Fortran, passing the unallocated descriptors.  */
  ftest (a, b, 0);

  /* Allocate and initialize both variables, and try again.  */
  lower[0] = 1;
  lower[1] = 1;
  upper[0] = imax;
  upper[1] = jmax;

  check_CFI_status ("CFI_allocate",
		    CFI_allocate (a, lower, upper, 0));
  dump_CFI_cdesc_t (a);
  if (!a->base_addr)
    abort ();
  if (a->elem_len != sizeof (struct m))
    abort ();

  upper[0] = jmax;
  upper[1] = imax;
  check_CFI_status ("CFI_allocate",
		    CFI_allocate (b, lower, upper, 0));
  dump_CFI_cdesc_t (b);
  if (!b->base_addr)
    abort ();
  if (b->elem_len != sizeof (struct m))
    abort ();

  for (i = 1; i <= imax; i++)
    for (j = 1; j <= jmax; j++)
      {
	subscripts[0] = i;
	subscripts[1] = j;
	mp = (struct m *) CFI_address (a, subscripts);
	mp->i = i;
	mp->j = j;
	subscripts[0] = j;
	subscripts[1] = i;
	mp = (struct m *) CFI_address (b, subscripts);
	mp->i = i;
	mp->j = j;
      }

  ftest (a, b, 1);

  /* Deallocate both objects and try again.  */
  check_CFI_status ("CFI_deallocate", CFI_deallocate (a));
  if (a->base_addr)
    abort ();
  check_CFI_status ("CFI_deallocate", CFI_deallocate (b));
  if (b->base_addr)
    abort ();
  ftest (a, b, 0);
}
