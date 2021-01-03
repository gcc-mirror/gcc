/* { dg-do compile } */
/* { dg-additional-options "-O2 -ftree-vectorize -fvect-cost-model=very-cheap" } */

void
f (int *restrict x, int *restrict y)
{
  for (unsigned int i = 0; i < 1023; ++i)
    x[i] += y[i];
}

/* { dg-final { scan-tree-dump {LOOP VECTORIZED} vect { target { vect_int && vect_partial_vectors_usage_2 } } } } */
/* { dg-final { scan-tree-dump-not {LOOP VECTORIZED} vect { target { vect_int && { ! vect_partial_vectors_usage_2 } } } } } */
