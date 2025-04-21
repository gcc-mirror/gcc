/* { dg-do compile } */

int x = 5;

struct Q {
  int *arr1;
  int *arr2;
  int *arr3;
};

int y = 5;

#pragma omp declare mapper (struct Q myq) map(myq.arr2[0:x])
/* { dg-error "previous '#pragma omp declare mapper'" "" { target c } .-1 } */
/* { dg-note "'#pragma omp declare mapper \\(Q\\)' previously defined here" "" { target c++ } .-2 } */

#pragma omp declare mapper (struct Q myq) map(myq.arr2[0:y])
/* { dg-error "redeclaration of '<default>' '#pragma omp declare mapper' for type 'struct Q'" "" { target c } .-1 } */
/* { dg-error "redefinition of '#pragma omp declare mapper \\(Q\\)'" "" { target c++ } .-2 } */

struct R {
  int *arr1;
};

void foo (void)
{
#pragma omp declare mapper (struct R myr) map(myr.arr1[0:x])
/* { dg-error "previous '#pragma omp declare mapper'" "" { target c } .-1 } */
/* { dg-note "'#pragma omp declare mapper \\(R\\)' previously declared here" "" { target c++ } .-2 } */

#pragma omp declare mapper (struct R myr) map(myr.arr1[0:y])
/* { dg-error "redeclaration of '<default>' '#pragma omp declare mapper' for type 'struct R'" "" { target c } .-1 } */
/* { dg-error "redeclaration of '#pragma omp declare mapper \\(R\\)'" "" { target c++ } .-2 } */
}
