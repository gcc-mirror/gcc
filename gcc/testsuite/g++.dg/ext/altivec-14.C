/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec" } */

void f (__attribute__((altivec (vector__))) signed int * a,
	__attribute__((altivec (vector__))) signed int * const b);

void
foo (void)
{
  __attribute__((altivec (vector__))) signed int a[1], b[1];
  f (a, b);
}
