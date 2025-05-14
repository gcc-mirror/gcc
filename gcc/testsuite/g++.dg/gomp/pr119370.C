// PR c++/119370
// { dg-do compile }

#pragma omp declare target
struct S {
  int s;
  S () : s (0) {}
};
S a[2];
#pragma omp end declare target
