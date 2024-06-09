/* { dg-options "-mvsx -O2 -fprefetch-loop-arrays" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Verify there is no ICE or hanging.  */

#include <altivec.h>

void b(float *c, vector float a, vector float, vector float)
{
  vector float d;
  vector char ahbc;
  vec_xst(vec_perm(a, d, ahbc), 0, c);
}

vector float e(vector unsigned);

void f() {
  float *dst;
  int g = 0;
  for (;; g += 16) {
    vector unsigned m, i;
    vector unsigned n, j;
    vector unsigned k, l;
    b(dst + g * 3, e(m), e(n), e(k));
    b(dst + (g + 4) * 3, e(i), e(j), e(l));
  }
}
