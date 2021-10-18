#include <stdlib.h>
#include <stdio.h>
#include <alloca.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (int n);
extern void ftest (CFI_cdesc_t *a, int n);

void
ctest (int n)
{
  CFI_CDESC_T(0) adesc;
  CFI_cdesc_t *a = (CFI_cdesc_t *) &adesc;
  char *adata = (char *) alloca (n);

  /* Fill in adesc.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (a, adata, CFI_attribute_other,
				   CFI_type_char, n, 0, NULL));

  /* Sanity checking to make sure the descriptor has been initialized
     properly.  */
  dump_CFI_cdesc_t (a);
  if (a->version != CFI_VERSION)
    abort ();
  if (a->rank != 0)
    abort ();
  if (a->attribute != CFI_attribute_other)
    abort ();
  if (a->elem_len != n)
    abort ();
  
  /* Call back into Fortran.  */
  ftest (a, n);
}
