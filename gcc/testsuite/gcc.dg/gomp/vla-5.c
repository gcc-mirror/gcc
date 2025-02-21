// { dg-do compile }

void foo(int n, int i)
{
  int A[n];

  #pragma omp parallel sections lastprivate(A)
    {
      A[i] = 1;
    }
}
