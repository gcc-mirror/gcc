/* PR target/83399 */
/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-options "-O1 -mabi=elfv2 -mlittle -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

typedef __attribute__((altivec(vector__))) int v4si_t;
int
foo (void)
{
  v4si_t a, u, v, y;
  u = __builtin_altivec_lvx (32, ((void *) &a) - 32);
  v = __builtin_altivec_lvx (64, ((void *) &a) - 32);
  y = u + v;
  return y[0];
}
