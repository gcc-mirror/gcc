/* { dg-do run { target { powerpc64*-*-* } } } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-mvsx" } */

#include <altivec.h>

void abort (void);

vector double vec = (vector double) {99.0, 99.0};

int main() {

  int actual = vec_all_nge(vec, vec);
  if ( actual != 0)
    abort();

  actual = vec_all_nle(vec, vec);
  if ( actual != 0)
    abort();

  actual = vec_any_nge(vec, vec);
  if ( actual != 0)
    abort();

  actual = vec_any_nle(vec, vec);
  if ( actual != 0)
    abort();

  return 0;
}
