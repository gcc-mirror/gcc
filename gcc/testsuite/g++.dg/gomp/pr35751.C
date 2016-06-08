// PR c/35751
// { dg-do compile }
// { dg-options "-fopenmp" }

void
foo (int i)
{
  extern int a[i];	// { dg-error "14:storage size of" }
  static int b[i];	// { dg-error "14:storage size of" }

#pragma omp parallel
  {
    a[0] = 0;
    b[0] = 0;
  }

#pragma omp parallel shared (a, b)
  {
    a[0] = 0;
    b[0] = 0;
  }

#pragma omp parallel private (a, b)
  {
    a[0] = 0;
    b[0] = 0;
  }

#pragma omp parallel firstprivate (a, b)
  {
    a[0] = 0;
    b[0] = 0;
  }
}
