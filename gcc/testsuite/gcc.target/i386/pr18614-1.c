/* PR rtl-optimization/18614 */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse2" } */

typedef double v2df __attribute__ ((vector_size (16)));

v2df foo (void)
{
  v2df yd = { 1.0, 4.0 };
  v2df xd;

  xd = __builtin_ia32_cvtps2pd (__builtin_ia32_rsqrtps
				(__builtin_ia32_cvtpd2ps (yd)));
  return xd;
}
