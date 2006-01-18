// { dg-do compile }

void foo(int n, int i)
{
  int A[n];

  #pragma omp parallel private(A)
    {
      A[i] = 0;
    }
}
