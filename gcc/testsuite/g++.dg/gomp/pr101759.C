// PR c++/101759
// { dg-do compile { target c++11 } }

#pragma omp declare simd
int foo (int x = []() { extern int bar (int); return 1; }());
int corge (int = 1);
#pragma omp declare variant (corge) match (user={condition(true)})
int baz (int x = []() { extern int qux (int); return 1; }());
