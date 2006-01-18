// PR 24612
// { dg-do compile }
// { dg-options "-O -Wuninitialized -fopenmp" }

void foo()
{
    int i;
#pragma omp parallel shared(i)
    {
      i = 0;
      ++i;
    }
}
