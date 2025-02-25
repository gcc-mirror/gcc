// PR c++/118876
// { dg-do compile }

#pragma omp declare target
struct A { ~A () {} } a[2];
#pragma omp end declare target
