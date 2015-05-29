/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

float dvec[256];

void test1 (float x)
{
  long i, j;
  for (i = 0; i < 256; ++i)
    for (j = 0; j < 131072; ++j)
      dvec[i] *= x;
}

void test2 (float x)
{
  long i, j;
  for (i = 0; i < 256; ++i)
    for (j = 0; j < 131072; ++j)
      dvec[i] *= 1.001f;
}

/* { dg-final { scan-tree-dump-times "OUTER LOOP VECTORIZED" 2 "vect" } } */
