/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

float a[4];

void foo()
{
  int i, j;

  for (i = 0; i < 4; ++i)
    for (j = 0; j < 17; ++j)
      a[i] = 0;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 1 "vect"  } } */
/* { dg-final { cleanup-tree-dump "vect" } } */


