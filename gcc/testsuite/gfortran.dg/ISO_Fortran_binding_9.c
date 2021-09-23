/* Test fix of a problem with CFI_is_contiguous.  */

/* Contributed by Gilles Gouaillardet  <gilles@rist.or.jp> */

#include <ISO_Fortran_binding.h>
#include <stdlib.h>

int cdesc_c(CFI_cdesc_t* x, long *expected)
{
  int res;
  res = CFI_is_contiguous (x);
  if (x->base_addr != (void *)*expected) res = 0;
  return res;
}