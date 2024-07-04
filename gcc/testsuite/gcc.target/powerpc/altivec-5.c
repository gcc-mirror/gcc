/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#define vector __attribute__((vector_size(16)))

void foo (const unsigned long x,
	  vector signed int a, vector signed int b)
{
  unsigned char d[64];

  __builtin_altivec_stvewx (b, 0, d);
}
