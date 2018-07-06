/* PR rtl-optimization/83682 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef float V __attribute__((__vector_size__(16)));
typedef double W __attribute__((__vector_size__(16)));
V b;
W c;

void
foo (void *p)
{
  V e = __builtin_ia32_cvtsd2ss (b, c);
  V g = e;
  float f = g[0];
  __builtin_memcpy (p, &f, sizeof (f));
}
