// { dg-do compile }

void foo(int n, int i)
{
  int A[n];

  #pragma omp parallel firstprivate(A)
    {
      A[i] = 1;
    }
}
