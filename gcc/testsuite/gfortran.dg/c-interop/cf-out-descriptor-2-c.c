#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *a, CFI_cdesc_t *b);
extern void ftest2 (CFI_cdesc_t *a, CFI_cdesc_t *b);

struct m {
  int i;
  int j;
};

#define imax 10
#define jmax 5

void
ctest (CFI_cdesc_t *a, CFI_cdesc_t *b)
{
  CFI_index_t i, j;
  CFI_index_t s[2];
  struct m *mpa, *mpb;
  
  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);
  if (a->rank != 2)
    abort ();
  if (a->attribute != CFI_attribute_other)
    abort ();
  if (a->dim[0].lower_bound != 0)
    abort ();
  if (a->dim[0].extent != imax)
    abort ();
  if (a->dim[1].lower_bound != 0)
    abort ();
  if (a->dim[1].extent != jmax)
    abort ();

  dump_CFI_cdesc_t (b);
  if (b->rank != 2)
    abort ();
  if (b->attribute != CFI_attribute_other)
    abort ();
  if (b->dim[0].lower_bound != 0)
    abort ();
  if (b->dim[0].extent != jmax)
    abort ();
  if (b->dim[1].lower_bound != 0)
    abort ();
  if (b->dim[1].extent != imax)
    abort ();

  /* Call back into Fortran, passing both the a and b arrays.  */
  ftest2 (a, b);

  /* Check that we got a valid b array back.  */
  dump_CFI_cdesc_t (b);
  if (b->rank != 2)
    abort ();
  if (b->attribute != CFI_attribute_other)
    abort ();
  if (b->dim[0].lower_bound != 0)
    abort ();
  if (b->dim[0].extent != jmax)
    abort ();
  if (b->dim[1].lower_bound != 0)
    abort ();
  if (b->dim[1].extent != imax)
    abort ();

  for (j = 0; j < jmax; j++)
    for (i = 0; i < imax; i++)
      {
	s[0] = i;
	s[1] = j;
	mpa = (struct m *) CFI_address (a, s);
	s[0] = j;
	s[1] = i;
	mpb = (struct m *) CFI_address (b, s);
	if (mpa->i != mpb->i)
	  abort ();
	if (mpa->j != mpb->j)
	  abort ();
      }
}
