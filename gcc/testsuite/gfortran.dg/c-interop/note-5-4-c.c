#include <ISO_Fortran_binding.h>

extern int test_rank (CFI_cdesc_t *a);

int test_rank (CFI_cdesc_t *a)
{
  if (!a)
    return -1;  /* Should not happen.  */
  return a->rank;
}
