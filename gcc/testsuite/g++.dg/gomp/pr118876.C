// PR c++/118876
// { dg-do compile }
// { dg-additional-options "-Wno-deprecated-openmp" }
#pragma omp declare target
struct A { ~A () {} } a[2];
#pragma omp end declare target
