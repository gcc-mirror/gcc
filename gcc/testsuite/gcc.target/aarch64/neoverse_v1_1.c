/* { dg-options "-O2 -mcpu=neoverse-v1" } */

void
foo (short *restrict x, short y[restrict][128])
{
  for (int i = 0; i < 128; ++i)
    {
      y[0][i] = x[i * 3 + 0];
      y[1][i] = x[i * 3 + 1];
      y[2][i] = x[i * 3 + 2];
    }
}

/* This shouldn't be a post-increment.  */
/* { dg-final { scan-assembler {ld3\t{[^{}]*}, \[x[0-9]+\]\n} } } */
