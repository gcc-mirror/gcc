/* { dg-do compile { target powerpc-*-* } } */
/* { dg-options "-maltivec -O2" } */

#define vector __attribute__((vector_size(16)))

void foo (const unsigned long x,
	  vector signed int a, vector signed int b)
{
  unsigned char d[64];

  __builtin_altivec_stvewx (b, 0, d);
}
