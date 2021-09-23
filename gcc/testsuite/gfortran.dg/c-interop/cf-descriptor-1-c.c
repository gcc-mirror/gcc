#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *a);
extern void ftest (CFI_cdesc_t *a, CFI_cdesc_t *b);

struct m {
  int i;
  int j;
};

#define imax 10
#define jmax 5

void
ctest (CFI_cdesc_t *a)
{
  
  struct m bdata[imax][jmax];
  CFI_CDESC_T(2) bdesc;
  CFI_cdesc_t *b = (CFI_cdesc_t *) &bdesc;
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

  /* Transpose a's contents into bdata.  */
  for (j = 0; j < jmax; j++)
    {
      subscripts[1] = j;
      for (i = 0; i < imax; i++)
	{
	  subscripts[0] = i;
	  mp = (struct m *) CFI_address (a, subscripts);
	  if (mp->i != i + 1)
	    abort ();
	  if (mp->j != j + 1)
	    abort ();
	  bdata[i][j].i = mp->i;
	  bdata[i][j].j = mp->j;
	}
    }

  /* Fill in bdesc.  */
  subscripts[0] = jmax;
  subscripts[1] = imax;
  check_CFI_status ("CFI_establish",
		    CFI_establish (b, bdata, CFI_attribute_other,
				   CFI_type_struct,
				   sizeof (struct m), 2, subscripts));
  
  /* Sanity checking to make sure the descriptor has been initialized
     properly.  */
  dump_CFI_cdesc_t (b);
  if (b->version != CFI_VERSION)
    abort ();
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
  ftest (a, b);
}
