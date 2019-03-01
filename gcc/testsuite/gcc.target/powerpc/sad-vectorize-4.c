/* { dg-do run { target { powerpc*-*-linux* && { lp64 && p9vector_hw } } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-O3 -mdejagnu-cpu=power9" } */

/* Verify that we get correct code when we vectorize this SAD loop using
   vabsduh. */

extern void abort ();
extern int abs (int __x) __attribute__ ((__nothrow__, __leaf__)) __attribute__ ((__const__));

static int
foo (unsigned short *w, int i, unsigned short *x, int j)
{
  int tot = 0;
  for (int a = 0; a < 16; a++)
    {
      for (int b = 0; b < 8; b++)
	tot += abs (w[b] - x[b]);
      w += i;
      x += j;
    }
  return tot;
}

void
bar (unsigned short *w, unsigned short *x, int i, int *result)
{
  *result = foo (w, 8, x, i);
}

int
main ()
{
  unsigned short m[128];
  unsigned short n[128];
  int sum, i;

  for (i = 0; i < 128; ++i)
    if (i % 2 == 0)
      {
	m[i] = (i % 8) * 2 + 1;
	n[i] = i % 8;
      }
    else
      {
	m[i] = (i % 8) * 4 - 3;
	n[i] = (i % 8) >> 1;
      }
  
  bar (m, n, 8, &sum);

  if (sum != 992)
    abort ();

  return 0;
}
