#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *a);
extern void ftest1 (CFI_cdesc_t *a, int lb1, int lb2);
extern void ftest2 (CFI_cdesc_t *a);

struct m {
  int i;
  int j;
};

#define imax 10
#define jmax 5

void
ctest (CFI_cdesc_t *a)
{
  
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

  /* Fill in bdesc.  */
  subscripts[0] = a->dim[0].extent;
  subscripts[1] = a->dim[1].extent;
  check_CFI_status ("CFI_establish",
		    CFI_establish (b, NULL, CFI_attribute_pointer,
				   CFI_type_struct,
				   sizeof (struct m), 2, subscripts));

  /* Pass the unassociated pointer descriptor b back to Fortran for
     checking.  */
  dump_CFI_cdesc_t (b);
  ftest2 (b);

  /* Point the descriptor b at the input argument array, and check that
     on the Fortran side.  */
  subscripts[0] = a->dim[0].lower_bound;
  subscripts[1] = a->dim[1].lower_bound;
  check_CFI_status ("CFI_setpointer",
		    CFI_setpointer (b, a, subscripts));
  dump_CFI_cdesc_t (b);
  ftest1 (b, (int)subscripts[0], (int)subscripts[1]);

  /* Diddle the lower bounds and try again.  */
  subscripts[0] = 42;
  subscripts[1] = -69;
  check_CFI_status ("CFI_setpointer",
		    CFI_setpointer (b, b, subscripts));
  dump_CFI_cdesc_t (b);
  ftest1 (b, 42, -69);

  /* Disassociate the pointer and check that.  */
  check_CFI_status ("CFI_setpointer",
		    CFI_setpointer (b, NULL, NULL));
  dump_CFI_cdesc_t (b);
  ftest2 (b);
}
