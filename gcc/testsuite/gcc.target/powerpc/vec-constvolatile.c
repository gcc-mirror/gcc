/* Test that const and volatile qualifiers can mix on vec_mul operands.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -mvsx" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

void P() {
  const volatile vector float cvva = vec_splats(0.00187682f);
  volatile vector float vva = vec_splats(0.00187682f);
  const vector float cva = vec_splats(0.00187682f);
  vector float va = vec_splats(0.00187682f);
  vector float dx = {1.0f, 2.0f, 3.0f, 4.0f};

  vector float X1m0 = vec_mul(va, va);
  vector float X2m0 = vec_mul(va, dx);
  vector float X3m0 = vec_mul(dx, va);

  vector float X1m1 = vec_mul(cva, cva);
  vector float X2m1 = vec_mul(cva, dx);
  vector float X3m1 = vec_mul(dx, cva);

  vector float Y1m2 = vec_mul(vva, vva);
  vector float Y2m2 = vec_mul(vva, dx);
  vector float Y3m2 = vec_mul(dx, vva);

  vector float X1m3 = vec_mul(cvva, cvva);
  vector float X2m3 = vec_mul(cvva, dx);
  vector float X3m3 = vec_mul(dx, cvva);
}
