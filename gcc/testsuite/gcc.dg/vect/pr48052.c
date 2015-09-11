/* { dg-do compile } */
/* { dg-require-effective-target vect_int_mult } */

int foo(int* A, int* B,  unsigned start, unsigned BS)
{
  int s = 0;
  for (unsigned k = start;  k < start + BS; k++)
    {
      s += A[k] * B[k];
    }

  return s;
}

int bar(int* A, int* B, unsigned BS)
{
  int s = 0;
  for (unsigned k = 0;  k < BS; k++)
    {
      s += A[k] * B[k];
    }

  return s;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
