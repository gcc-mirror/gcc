#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *a);
extern void ftest (CFI_cdesc_t *iarray, CFI_cdesc_t *jarray);

struct m {
  int i;
  int j;
};

void
ctest (CFI_cdesc_t *a)
{
  CFI_CDESC_T(2) idesc;
  CFI_cdesc_t *iarray = (CFI_cdesc_t *) &idesc;
  CFI_CDESC_T(2) jdesc;
  CFI_cdesc_t *jarray = (CFI_cdesc_t *) &jdesc;
  int i, j;

  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);

  if (a->rank != 2)
    abort ();

  /* Fill in the new descriptors.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (iarray, NULL, CFI_attribute_pointer,
				   CFI_type_int,
				   sizeof (int), 2, NULL));
  check_CFI_status ("CFI_select_part",
		    CFI_select_part (iarray, a, offsetof (struct m, i),
				     sizeof (int)));

  check_CFI_status ("CFI_establish",
		    CFI_establish (jarray, NULL, CFI_attribute_pointer,
				   CFI_type_int,
				   sizeof (int), 2, NULL));
  check_CFI_status ("CFI_select_part",
		    CFI_select_part (jarray, a, offsetof (struct m, j),
				     sizeof (int)));
  
  /* Sanity checking to make sure the descriptor has been initialized
     properly.  */
  dump_CFI_cdesc_t (iarray);
  if (iarray->version != CFI_VERSION)
    abort ();
  if (iarray->rank != 2)
    abort ();
  if (iarray->attribute != CFI_attribute_pointer)
    abort ();
  if (!iarray->base_addr)
    abort ();
  if (iarray->dim[0].extent != a->dim[0].extent)
    abort ();
  if (iarray->dim[1].extent != a->dim[1].extent)
    abort ();

  dump_CFI_cdesc_t (jarray);
  if (jarray->version != CFI_VERSION)
    abort ();
  if (jarray->rank != 2)
    abort ();
  if (jarray->attribute != CFI_attribute_pointer)
    abort ();
  if (!jarray->base_addr)
    abort ();
  if (jarray->dim[0].extent != a->dim[0].extent)
    abort ();
  if (jarray->dim[1].extent != a->dim[1].extent)
    abort ();

  /* Call back into Fortran.  */
  ftest (iarray, jarray);
}
