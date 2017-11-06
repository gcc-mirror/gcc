/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-O3 -mcpu=power9" } */

/* Verify that we vectorize this SAD loop using vabsduh. */

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

/* { dg-final { scan-assembler-times "vabsduh" 16 } } */
/* { dg-final { scan-assembler-times "vsum4shs" 16 } } */
/* { dg-final { scan-assembler-times "vadduwm" 17 } } */

/* Note: One of the 16 adds is optimized out (add with zero),
   leaving 15.  The extra two adds are for the final reduction.  */
