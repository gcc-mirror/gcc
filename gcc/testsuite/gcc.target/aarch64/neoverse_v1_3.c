/* { dg-options "-O2 -mcpu=neoverse-v1 --param aarch64-autovec-preference=2 -fdump-tree-vect-details" } */

void
f (float x[restrict][100], float y[restrict][100])
{
  for (int i = 0; i < 100; ++i)
    {
      x[0][i] = y[0][i] * y[1][i] - y[3][i] * y[4][i];
      x[1][i] = y[1][i] * y[2][i] - y[3][i] * y[4][i];
    }
}

/* { dg-final { scan-tree-dump {_[0-9]+ - _[0-9]+ 1 times vector_stmt costs 0 } "vect" } } */
/* { dg-final { scan-tree-dump {_[0-9]+ - _[0-9]+ 1 times scalar_stmt costs 0 } "vect" } } */
