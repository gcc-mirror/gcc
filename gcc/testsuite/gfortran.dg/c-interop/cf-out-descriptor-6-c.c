#include <stdlib.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *a, int n);
extern void ftest (CFI_cdesc_t *a, int n);

void
ctest (CFI_cdesc_t *a, int n)
{
  int i;
  CFI_index_t s[1];

  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  dump_CFI_cdesc_t (a);

  if (!a->base_addr)
    abort ();
  if (a->elem_len != sizeof(int))
    abort ();
  if (a->rank != 1)
    abort ();
  if (a->type != CFI_type_int)
    abort ();
  if (a->attribute != CFI_attribute_other)
    abort ();
  if (a->dim[0].lower_bound != 0)
    abort ();
  if (a->dim[0].extent != -1)
    abort ();

  ftest (a, n);

  for (i = 0; i < n; i++)
    {
      s[0] = i;
      if (*((int *)CFI_address (a, s)) != i + 1)
	abort ();
    }
}
