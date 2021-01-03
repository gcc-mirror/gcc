/* { dg-do compile } */
/* { dg-additional-options "-O2 -ftree-vectorize -fvect-cost-model=very-cheap" } */

void
f (int *x, int *y)
{
  for (unsigned int i = 0; i < 1024; ++i)
    x[i] += y[i];
}

/* { dg-final { scan-tree-dump-not {LOOP VECTORIZED} vect { target vect_int } } } */
