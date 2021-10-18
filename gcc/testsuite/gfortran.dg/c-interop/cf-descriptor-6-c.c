#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *a, int lb1, int lb2, int ub1, int ub2, int step1, int step2);
extern void ftest (CFI_cdesc_t *b);

struct m {
  int i;
  int j;
};

void
ctest (CFI_cdesc_t *a, int lb1, int lb2, int ub1, int ub2,
       int step1, int step2)
{
  CFI_CDESC_T(2) bdesc;
  CFI_cdesc_t *b = (CFI_cdesc_t *) &bdesc;
  CFI_index_t lb[2], ub[2], step[2];
  int i, j;

  fprintf (stderr, "got new bound info (%d:%d:%d, %d:%d:%d)\n",
	   lb1, ub1, step1, lb2, ub2, step2);
  lb[0] = lb1 - 1;
  lb[1] = lb2 - 1;
  ub[0] = ub1 - 1;
  ub[1] = ub2 - 1;
  step[0] = step1;
  step[1] = step2;

  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);

  if (a->rank != 2)
    abort ();

  /* Fill in bdesc.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (b, NULL, CFI_attribute_pointer,
				   CFI_type_struct,
				   sizeof (struct m), 2, NULL));
  check_CFI_status ("CFI_section",
		    CFI_section (b, a, lb, ub, step));
  
  /* Sanity checking to make sure the descriptor has been initialized
     properly.  */
  dump_CFI_cdesc_t (b);
  if (b->version != CFI_VERSION)
    abort ();
  if (b->rank != 2)
    abort ();
  if (b->attribute != CFI_attribute_pointer)
    abort ();
  if (!b->base_addr)
    abort ();
  if (CFI_is_contiguous (b))
    abort ();

  for (j = b->dim[1].lower_bound;
       j < b->dim[1].lower_bound + b->dim[1].extent;
       j++)
    {
      for (i = b->dim[0].lower_bound;
	   i < b->dim[0].lower_bound + b->dim[0].extent;
	   i++)
	{
	  CFI_index_t subscripts[2];
	  struct m *mp;
	  subscripts[0] = i;
	  subscripts[1] = j;
	  mp = (struct m *) CFI_address (b, subscripts);
	  fprintf (stderr, "b(%d,%d) = (%d,%d)\n", i, j, mp->i, mp->j);
	}
    }

  /* Call back into Fortran.  */
  ftest (b);
}
