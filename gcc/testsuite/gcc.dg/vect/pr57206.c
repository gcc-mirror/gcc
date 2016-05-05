/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

void bad0(float * d, unsigned int n)
{
  unsigned int i;
  for (i=n; i>0; --i) 
    d[n-i] = 0.0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
