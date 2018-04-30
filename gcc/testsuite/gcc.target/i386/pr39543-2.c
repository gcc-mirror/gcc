/* PR inline-asm/39543 */
/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-skip-if "" { ia32 && { ! nonpic } } } */

float __attribute__ ((aligned (16))) s0[128];
const float s1 = 0.707;
float s2[8] __attribute__ ((aligned (16)));
float s3[8] __attribute__ ((aligned (16)));
float s4[16] __attribute__ ((aligned (16)));
float s5[16] __attribute__ ((aligned (16)));

void
foo (int k, float *x, float *y, const float *d, const float *z)
{
  float *a, *b, *c, *e;

  a = x + 2 * k;
  b = a + 2 * k;
  c = b + 2 * k;
  e = y + 2 * k;
  __asm__ volatile (""
		    : "=m" (x[0]), "=m" (b[0]), "=m" (a[0]), "=m" (c[0])
		    : "m" (y[0]), "m" (y[k * 2]), "m" (x[0]), "m" (a[0])
		    : "memory");
  for (;;)
    {
      __asm__ volatile (""
			:
			: "m" (y[2]), "m" (d[2]), "m" (e[2]), "m" (z[2])
			: "memory");
      if (!--k)
	break;
    }
  __asm__ volatile (""
		    : "=m" (x[2]), "=m" (x[10]), "=m" (x[6]), "=m" (x[14])
		    : "m" (y[2]), "m" (y[6]), "m" (x[2]), "m" (x[6]), "m" (s1)
		    : "memory");
}

void
bar (float *a)
{
  foo (4, a, a + 16, s2, s3);
  foo (8, a, a + 32, s4, s5);
}

void
baz (void)
{
  bar (s0);
}
