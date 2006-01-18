// { dg-do compile }

void foo(int n, int i)
{
  int A[n];

  #pragma omp parallel shared(A)
    {
      A[i] = sizeof(A);
    }
}
