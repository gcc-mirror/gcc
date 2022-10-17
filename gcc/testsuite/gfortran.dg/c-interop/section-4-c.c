#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

struct m {
  int i, j, k, l;
};

extern void ctest (void);

#define IMAX 6
#define JMAX 8
#define KMAX 10
#define LMAX 12

static struct m buffer[LMAX][KMAX][JMAX][IMAX];

static void
check_element (struct m *mp, int i, int j, int k, int l)
{
#if 0
  fprintf (stderr, "expected (%d, %d, %d, %d), got (%d, %d, %d, %d)\n",
	   i, j, k, l, mp->i, mp->j, mp->k, mp->l);
#endif  
  if (mp->i != i || mp->j != j || mp->k != k || mp->l != l)
    abort ();
}

void
ctest (void)
{
  CFI_CDESC_T(4) sdesc;
  CFI_cdesc_t *source = (CFI_cdesc_t *) &sdesc;
  CFI_CDESC_T(4) rdesc;
  CFI_cdesc_t *result = (CFI_cdesc_t *) &rdesc;
  CFI_index_t extents[4] = { IMAX, JMAX, KMAX, LMAX };
  CFI_index_t lb[4], ub[4], s[4];
  int i, j, k, l;
  int ii, jj, kk, ll;

  /* Initialize the buffer to uniquely label each element.  */
  for (i = 0; i < IMAX; i++)
    for (j = 0; j < JMAX; j++)
      for (k = 0; k < KMAX; k++)
	for (l = 0; l < LMAX; l++)
	  {
	    buffer[l][k][j][i].i = i;
	    buffer[l][k][j][i].j = j;
	    buffer[l][k][j][i].k = k;
	    buffer[l][k][j][i].l = l;
	  }

  /* Establish the source array.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (source, (void *)buffer,
				   CFI_attribute_pointer, CFI_type_struct,
				   sizeof (struct m), 4, extents));

  /* Try taking a degenerate section (single element).  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (result, NULL,
				   CFI_attribute_pointer, CFI_type_struct,
				   sizeof (struct m), 0, NULL));
  lb[0] = 3; lb[1] = 4; lb[2] = 5; lb[3] = 6;
  ub[0] = 3; ub[1] = 4; ub[2] = 5; ub[3] = 6;
  s[0] = 0; s[1] = 0; s[2] = 0; s[3] = 0;
  check_CFI_status ("CFI_section",
		    CFI_section (result, source, lb, ub, s));
  dump_CFI_cdesc_t (result);
  check_element ((struct m *)result->base_addr, 3, 4, 5, 6);

  /* Try taking a 2d chunk out of the 4d array.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (result, NULL,
				   CFI_attribute_pointer, CFI_type_struct,
				   sizeof (struct m), 2, NULL));
  lb[0] = 1; lb[1] = 2; lb[2] = 3; lb[3] = 4;
  ub[0] = 1; ub[1] = JMAX - 2; ub[2] = 3; ub[3] = LMAX - 2;
  s[0] = 0; s[1] = 2; s[2] = 0; s[3] = 3;
  check_CFI_status ("CFI_section",
		    CFI_section (result, source, lb, ub, s));
  dump_CFI_cdesc_t (result);

  i = lb[0];
  k = lb[2];
  for (j = lb[1], jj = result->dim[0].lower_bound;
       j <= ub[1];
       j += s[1], jj++)
    for (l = lb[3], ll = result->dim[1].lower_bound;
	 l <= ub[3];
	 l += s[3], ll++)
      {
	CFI_index_t subscripts[2];
	subscripts[0] = jj;
	subscripts[1] = ll;
	check_element ((struct m *) CFI_address (result, subscripts),
		       i, j, k, l);
      }
}
