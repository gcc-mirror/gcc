#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (int imagic, int jmagic);
extern void frob (CFI_cdesc_t *a, CFI_cdesc_t *aa, CFI_cdesc_t *p);

struct m {
  int i;
  int j;
};

void
ctest (int imagic, int jmagic)
{
  CFI_CDESC_T(0) adesc;
  CFI_CDESC_T(0) aadesc;
  CFI_CDESC_T(0) bdesc;
  CFI_cdesc_t *a = (CFI_cdesc_t *) &adesc;
  CFI_cdesc_t *aa = (CFI_cdesc_t *) &aadesc;
  CFI_cdesc_t *b = (CFI_cdesc_t *) &bdesc;

  /* Create and sanity-check descriptors.  */
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

  check_CFI_status ("CFI_establish",
		    CFI_establish (aa, NULL, CFI_attribute_allocatable,
				   CFI_type_struct,
				   sizeof (struct m), 0, NULL));
  dump_CFI_cdesc_t (aa);
  if (aa->version != CFI_VERSION)
    abort ();
  if (aa->rank != 0)
    abort ();
  if (aa->attribute != CFI_attribute_allocatable)
    abort ();
  if (aa->base_addr)
    abort ();
  if (aa->elem_len != sizeof (struct m))
    abort ();
  check_CFI_status ("CFI_allocate",
		    CFI_allocate (aa, NULL, NULL, 0));
  ((struct m *)aa->base_addr)->i = 0;
  ((struct m *)aa->base_addr)->j = 0;
  
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

  /* Call back into Fortran, which will allocate and initialize the
     objects.  */
  frob (a, aa, b);

  if (!a->base_addr)
    abort ();
  if (a->elem_len != sizeof (struct m))
    abort ();
  if (((struct m *)a->base_addr)->i != imagic)
    abort ();
  if (((struct m *)a->base_addr)->j != jmagic)
    abort ();

  if (!aa->base_addr)
    abort ();
  if (aa->elem_len != sizeof (struct m))
    abort ();
  if (((struct m *)aa->base_addr)->i != imagic)
    abort ();
  if (((struct m *)aa->base_addr)->j != jmagic)
    abort ();

  if (!b->base_addr)
    abort ();
  if (b->elem_len != sizeof (struct m))
    abort ();
  if (((struct m *)b->base_addr)->i != imagic)
    abort ();
  if (((struct m *)b->base_addr)->j != jmagic)
    abort ();
}
