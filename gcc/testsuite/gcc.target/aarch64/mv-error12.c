/* { dg-do compile } */
/* { dg-options "-fopenmp" } */
/* { dg-require-ifunc "" } */

#pragma omp declare simd
int fn [[gnu::target_version("sve")]] () { return 1; } /* { dg-error ".#pragma omp declare simd. cannot be used with function multi-versioning" } */

#pragma omp declare simd
int fn2 () { return 1; }

int fn2 [[gnu::target_version("sve")]] (); /* { dg-warning "ignoring attribute .target_version. because it conflicts with attribute .omp declare simd." } */

int fn3 [[gnu::target_version("sve")]] [[gnu::simd]] () { return 1; } /* { dg-warning "ignoring attribute .simd. because it conflicts with attribute .target_version." } */
