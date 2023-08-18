/* { dg-do compile { target c++11 } } */

int x = 5;

struct Q {
  int *arr1;
  int *arr2;
  int *arr3;
};

[[omp::directive (declare mapper (struct Q myq) map(myq.arr2[0:x]))]];

struct R {
  int *arr1;
  int *arr2;
  int *arr3;
};

[[omp::directive (declare mapper (struct R myr) map(myr.arr3[0:y]))]];
/* { dg-error "'y' was not declared in this scope" "" { target c++ } .-1 } */

int y = 7;
