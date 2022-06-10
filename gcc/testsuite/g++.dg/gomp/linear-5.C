/* { dg-do compile { target c++11 } } */
/* { dg-options "-fopenmp" } */

constexpr int step (int x) { return x; }
constexpr int val = 1;
constexpr int ref = 2;
constexpr int uval = 3;
#pragma omp declare simd linear (val (x) : step (1)) linear (ref (y) : step (2)) linear (uval (z) : step (3))
int foo (int x, int &y, int &z);
#pragma omp declare simd linear (val (x) : val) linear (ref (y) : ref) linear (uval (z) : uval)
int bar (int x, int &y, int &z);
#pragma omp declare simd linear (val (x) : ref) linear (ref (y) : uval) linear (uval (z) : val)
int baz (int x, int &y, int &z);
#pragma omp declare simd linear (val (x) : uval) linear (ref (y) : val) linear (uval (z) : ref)
int qux (int x, int &y, int &z);
