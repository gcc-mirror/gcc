#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (int imax, int jmax, CFI_cdesc_t *a);

struct m {
  int i;
  int j;
};

void
ctest (int imax, int jmax, CFI_cdesc_t *a)
{
  
  int i, j;
  CFI_index_t subscripts[2];
  struct m* mp;

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

  /* Fill in the contents of a.  a is zero-based but we want the ->i and ->j
     members of each element to be numbered starting from 1.  */
  for (j = 0; j < jmax; j++)
    {
      subscripts[1] = j;
      for (i = 0; i < imax; i++)
	{
	  subscripts[0] = i;
	  mp = (struct m *) CFI_address (a, subscripts);
	  mp->i = i + 1;
	  mp->j = j + 1;
	}
    }
}
