/* { dg-do compile { target powerpc-*-* } } */
/* { dg-options "-maltivec -O0 -Wall" } */

#define vector __attribute__((vector_size(16)))

int __attribute__((mode(V4SI))) x, y;

vector int i,j,k;
vector short s,t,u;
vector signed char c,d,e;
vector float f,g,h;

void
b()
{
  __builtin_altivec_vadduwm (x, y);

  /* Make sure the predicates accept correct argument types.  */
  
  k = __builtin_altivec_vcmpbfp_p (f, g);
  k = __builtin_altivec_vcmpeqfp_p (f, g);
  k = __builtin_altivec_vcmpequb_p (c, d);
  k = __builtin_altivec_vcmpequh_p (s, t);
  k = __builtin_altivec_vcmpequw_p (i, j);
  k = __builtin_altivec_vcmpgefp_p (f, g);
  k = __builtin_altivec_vcmpgtfp_p (f, g);
  k = __builtin_altivec_vcmpgtsb_p (c, d);
  k = __builtin_altivec_vcmpgtsh_p (s, t);
  k = __builtin_altivec_vcmpgtsw_p (i, j);
  k = __builtin_altivec_vcmpgtub_p (c, d);
  k = __builtin_altivec_vcmpgtuh_p (s, t);
  k = __builtin_altivec_vcmpgtuw_p (i, j);
}
