// { dg-do compile }
/* { dg-require-effective-target alloca } */

void foo(int n, int i)
{
  int A[n];

  #pragma omp parallel shared(A)
    {
      A[i] = sizeof(A);
    }
}
