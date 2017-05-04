/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fdump-tree-vect-details-blocks" } */

/* At least one of these should correspond to a full vector.  */

void
f1 (int *x)
{
  for (int j = 0; j < 2; ++j)
    x[j] += 1;
}

void
f2 (int *x)
{
  for (int j = 0; j < 4; ++j)
    x[j] += 1;
}

void
f3 (int *x)
{
  for (int j = 0; j < 8; ++j)
    x[j] += 1;
}

void
f4 (int *x)
{
  for (int j = 0; j < 16; ++j)
    x[j] += 1;
}

/* { dg-final { scan-tree-dump {goto <bb [0-9]+>; \[0+.0*%\]} vect } } */
