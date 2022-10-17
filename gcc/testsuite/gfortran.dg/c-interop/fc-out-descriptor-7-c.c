#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

struct m {
  int i;
  int j;
};

extern void ctest (CFI_cdesc_t *a, int lb1, int ub1, int s1,
		   int lb2, int ub2, int s2, CFI_cdesc_t *b);

/* Check array b against the section of array a defined by the given
   bounds.  */
static void
check_array (CFI_cdesc_t *a, CFI_cdesc_t *b,
             int lb1, int ub1, int s1, int lb2, int ub2, int s2)
{
  int bad = 0;
  int i, ii, j, jj;
  CFI_index_t sub[2];
  struct m *ap, *bp;

  for (j = lb2, jj = b->dim[1].lower_bound; j <= ub2; jj++, j += s2)
    for (i = lb1, ii = b->dim[0].lower_bound; i <= ub1; ii++, i += s1)
      {
	sub[0] = i;
	sub[1] = j;
	ap = (struct m *) CFI_address (a, sub);
	sub[0] = ii;
	sub[1] = jj;
	bp = (struct m *) CFI_address (b, sub);
#if 0
	fprintf (stderr, "b(%d,%d) = (%d,%d) expecting (%d,%d)\n",
		 ii, jj, bp->i, bp->j, ap->i, ap->j);
#endif
	if (ap->i != bp->i || ap->j != bp->j)
	  bad = 1;
      }
  if (bad)
    abort ();
}

void
ctest (CFI_cdesc_t *a, int lb1, int ub1, int s1,
       int lb2, int ub2, int s2, CFI_cdesc_t *b)
{
  CFI_index_t lb[2], ub[2], s[2];
  CFI_index_t i, j;

  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  fprintf (stderr, "input arrays\n");
  dump_CFI_cdesc_t (a);
  dump_CFI_cdesc_t (b);

  /* We expect to get a zero-based input array of shape (10,5).  */
  if (a->rank != 2)
    abort ();
  if (a->attribute != CFI_attribute_other)
    abort ();
  if (a->type != CFI_type_struct)
    abort ();
  if (a->dim[0].lower_bound != 0)
    abort ();
  if (a->dim[0].extent != 10)
    abort ();
  if (a->dim[1].lower_bound != 0)
    abort ();
  if (a->dim[1].extent != 5)
    abort ();

  /* The output descriptor has to agree with the input descriptor.  */
  if (b->rank != 2)
    abort ();
  if (b->attribute != CFI_attribute_pointer)
    abort ();
  if (b->type != CFI_type_struct)
    abort ();
  if (b->elem_len != a->elem_len)
    abort ();

  /* Point b at a, keeping the 0-based bounds.  */
  check_CFI_status ("CFI_setpointer",
		    CFI_setpointer (b, a, NULL));
  fprintf (stderr, "After initializing b\n");
  dump_CFI_cdesc_t (b);
  if (b->dim[0].lower_bound != 0)
    abort ();
  if (b->dim[1].lower_bound != 0)
    abort ();
  check_array (a, b,
               a->dim[0].lower_bound,
               a->dim[0].lower_bound + a->dim[0].extent - 1,
               1,
               a->dim[1].lower_bound,
               a->dim[1].lower_bound + a->dim[1].extent - 1,
               1);

  /* Take a section of the array.  The bounds passed in to this function
     assume the array is 1-based in both dimensions, so subtract 1.  */
  lb[0] = b->dim[0].lower_bound + lb1 - 1;
  lb[1] = b->dim[1].lower_bound + lb2 - 1;
  ub[0] = b->dim[0].lower_bound + ub1 - 1;
  ub[1] = b->dim[1].lower_bound + ub2 - 1;
  s[0] = s1;
  s[1] = s2;
  check_CFI_status ("CFI_section",
		    CFI_section (b, b, lb, ub, s));
  fprintf (stderr, "After CFI_section\n");
  dump_CFI_cdesc_t (b);
  check_array (a, b,
               a->dim[0].lower_bound + lb1 - 1,
               a->dim[0].lower_bound + ub1 - 1,
               s1,
               a->dim[1].lower_bound + lb2 - 1,
               a->dim[1].lower_bound + ub2 - 1,
               s2);

  /* Adjust b to be 1-based.  */
  lb[0] = 1;
  lb[1] = 1;
  fprintf (stderr, "After rebasing b again\n");
  check_CFI_status ("CFI_setpointer",
		    CFI_setpointer (b, b, lb));
  dump_CFI_cdesc_t (b);
  check_array (a, b,
               a->dim[0].lower_bound + lb1 - 1,
               a->dim[0].lower_bound + ub1 - 1,
               s1,
               a->dim[1].lower_bound + lb2 - 1,
               a->dim[1].lower_bound + ub2 - 1,
               s2);
}
