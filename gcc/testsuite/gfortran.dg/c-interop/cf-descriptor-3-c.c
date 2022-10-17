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
ctest (int imagic, int jmagic)
{
  CFI_CDESC_T(0) adesc;
  CFI_CDESC_T(0) bdesc;
  CFI_cdesc_t *a = (CFI_cdesc_t *) &adesc;
  CFI_cdesc_t *b = (CFI_cdesc_t *) &bdesc;
  struct m* mp;

  /* Create the descriptor for a, then sanity-check it.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (a, NULL, CFI_attribute_allocatable,
				   CFI_type_struct,
				   sizeof (struct m), 0, NULL));
  dump_CFI_cdesc_t (a);
  if (a->version != CFI_VERSION)
    abort ();
  if (a->rank != 0)
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
				   sizeof (struct m), 0, NULL));
  dump_CFI_cdesc_t (b);
  if (b->version != CFI_VERSION)
    abort ();
  if (b->rank != 0)
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
  check_CFI_status ("CFI_allocate",
		    CFI_allocate (a, NULL, NULL, 0));
  dump_CFI_cdesc_t (a);
  if (!a->base_addr)
    abort ();
  if (a->elem_len != sizeof (struct m))
    abort ();
  ((struct m *)a->base_addr)->i = imagic;
  ((struct m *)a->base_addr)->j = jmagic;

  check_CFI_status ("CFI_allocate",
		    CFI_allocate (b, NULL, NULL, 0));
  dump_CFI_cdesc_t (b);
  if (!b->base_addr)
    abort ();
  if (b->elem_len != sizeof (struct m))
    abort ();
  ((struct m *)b->base_addr)->i = imagic + 1;
  ((struct m *)b->base_addr)->j = jmagic + 1;

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
