/* TS29113 8.3.1: ISO_Fortran_binding.h may be included more than once.  */

#include <stdlib.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"
#include <ISO_Fortran_binding.h>

extern void ctest (CFI_cdesc_t *a);

void
ctest (CFI_cdesc_t *a)
{
  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);

  if (a->version != CFI_VERSION)
    abort ();
}
