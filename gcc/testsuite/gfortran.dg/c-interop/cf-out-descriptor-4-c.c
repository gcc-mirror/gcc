#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (int imax, int jmax);
extern void frob (CFI_cdesc_t *a, CFI_cdesc_t *aa, CFI_cdesc_t *p);

struct m {
  int i;
  int j;
};

void
ctest (int imax, int jmax)
{
  CFI_CDESC_T(2) adesc;
  CFI_CDESC_T(2) aadesc;
  CFI_CDESC_T(2) bdesc;
  CFI_cdesc_t *a = (CFI_cdesc_t *) &adesc;
  CFI_cdesc_t *aa = (CFI_cdesc_t *) &aadesc;
  CFI_cdesc_t *b = (CFI_cdesc_t *) &bdesc;
  CFI_index_t i, j;
  CFI_index_t s[2];
  CFI_index_t lb[2], ub[2];
  struct m* mp;

  /* Create and sanity-check a. */
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

  check_CFI_status ("CFI_establish",
		    CFI_establish (aa, NULL, CFI_attribute_allocatable,
				   CFI_type_struct,
				   sizeof (struct m), 2, NULL));
  dump_CFI_cdesc_t (aa);
  if (aa->version != CFI_VERSION)
    abort ();
  if (aa->rank != 2)
    abort ();
  if (aa->attribute != CFI_attribute_allocatable)
    abort ();
  if (aa->base_addr)
    abort ();
  if (aa->elem_len != sizeof (struct m))
    abort ();

  /* aa is allocated/initialized so that we can confirm that it's
     magically deallocated when passed as intent(out).  */
  lb[0] = 0;
  lb[1] = 0;
  ub[0] = jmax;
  ub[1] = jmax;
  check_CFI_status ("CFI_allocate",
		    CFI_allocate (aa, lb, ub, 0));
  for (j = 1; j <= jmax; j++)
    for (i = 1; i <= imax; i++)
      {
	s[0] = j;
	s[1] = i;
	mp = (struct m *)CFI_address (aa, s);
	mp->i = 0;
	mp->j = 0;
      }

  /* Likewise create and sanity-check b. */
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

  /* Call back into Fortran, which will allocate and initialize the
     objects.  */
  frob (a, aa, b);

  dump_CFI_cdesc_t (a);
  if (!a->base_addr)
    abort ();
  if (a->elem_len != sizeof (struct m))
    abort ();
  if (a->dim[0].lower_bound != 1)
    abort ();
  if (a->dim[0].extent != imax)
    abort ();
  if (a->dim[1].lower_bound != 1)
    abort ();
  if (a->dim[1].extent != jmax)
    abort ();
  for (j = 1; j <= jmax; j++)
    for (i = 1; i <= imax; i++)
      {
	s[0] = i;
	s[1] = j;
	mp = (struct m *)CFI_address (a, s);
	if (mp->i != i)
	  abort ();
	if (mp->j != j)
	  abort ();
      }

  dump_CFI_cdesc_t (aa);
  if (!aa->base_addr)
    abort ();
  if (aa->elem_len != sizeof (struct m))
    abort ();
  if (aa->dim[0].lower_bound != 1)
    abort ();
  if (aa->dim[0].extent != imax)
    abort ();
  if (aa->dim[1].lower_bound != 1)
    abort ();
  if (aa->dim[1].extent != jmax)
    abort ();
  for (j = 1; j <= jmax; j++)
    for (i = 1; i <= imax; i++)
      {
	s[0] = i;
	s[1] = j;
	mp = (struct m *)CFI_address (aa, s);
	if (mp->i != i)
	  abort ();
	if (mp->j != j)
	  abort ();
      }

  dump_CFI_cdesc_t (b);
  if (!b->base_addr)
    abort ();
  if (b->elem_len != sizeof (struct m))
    abort ();
  if (b->dim[0].lower_bound != 1)
    abort ();
  if (b->dim[0].extent != jmax)
    abort ();
  if (b->dim[1].lower_bound != 1)
    abort ();
  if (b->dim[1].extent != imax)
    abort ();
  for (j = 1; j <= jmax; j++)
    for (i = 1; i <= imax; i++)
      {
	s[0] = j;
	s[1] = i;
	mp = (struct m *)CFI_address (b, s);
	if (mp->i != i)
	  abort ();
	if (mp->j != j)
	  abort ();
      }
}
