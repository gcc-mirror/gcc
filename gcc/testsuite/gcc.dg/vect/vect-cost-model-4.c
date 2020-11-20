/* { dg-do compile } */
/* { dg-additional-options "-O2 -ftree-vectorize -fvect-cost-model=very-cheap" } */

int x[1024], y[1024];

void
f (void)
{
  for (unsigned int i = 0; i < 1024; ++i)
    x[i] += y[i];
}

/* { dg-final { scan-tree-dump {LOOP VECTORIZED} vect { target vect_int } } } */
