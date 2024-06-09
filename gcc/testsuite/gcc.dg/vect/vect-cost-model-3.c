/* { dg-do compile } */
/* { dg-additional-options "-O2 -ftree-vectorize -fvect-cost-model=cheap" } */

void
f (int *restrict x, int *restrict y)
{
  for (unsigned int i = 0; i < 1024; ++i)
    x[i] += y[i];
}

/* { dg-final { scan-tree-dump {LOOP VECTORIZED} vect { target { vect_int && vect_hw_misalign } } } } */
