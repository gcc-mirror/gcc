/* { dg-do compile { target c++ } } */

struct Q {
  int *arr1;
  int *arr2;
  int *arr3;
};

int foo (void)
{
  int x = 5;
  #pragma omp declare mapper (struct Q myq) map(myq.arr2[0:x])
  return x;
}

struct R {
  int *arr1;
  int *arr2;
  int *arr3;
};

int bar (void)
{
  #pragma omp declare mapper (struct R myr) map(myr.arr3[0:y])
  /* { dg-error "'y' undeclared" "" { target c } .-1 } */
  /* { dg-error "'y' was not declared in this scope" "" { target c++ } .-2 } */
  int y = 7;
  return y;
}
