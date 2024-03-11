/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d" } */

void
f (int *restrict y, int *restrict x, int *restrict indices, int n)
{
  for (int i = 0; i < n; ++i)
    y[i] = x[indices[i]] + 1;
}

/* { dg-final { scan-assembler {vluxei64\.v} } } */
/* { dg-final { scan-assembler {vsll\.vi} } } */
/* { dg-final { scan-assembler {vsext\.vf2} } } */
/* { dg-final { scan-assembler-not {vluxei32\.v} } } */
