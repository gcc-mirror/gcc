/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2 -ffast-math -ftree-vectorize" } */

#include <stddef.h>

typedef _Complex float GFC_COMPLEX_4;

void
test (GFC_COMPLEX_4 *a, GFC_COMPLEX_4 *b, GFC_COMPLEX_4 c, ptrdiff_t i, ptrdiff_t j)
{
  ptrdiff_t l;
  for (l = 0; l <= i; ++l)
    c += b[l] * a[j];
  b[j] = c;
}
