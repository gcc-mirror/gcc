/* { dg-do compile { target c++11 } } */
/* { dg-options "-O2 -msse2 -fdump-tree-optimized" } */

template <class T>
using V [[gnu::vector_size(16)]] = T;

template <class T, unsigned M = sizeof(V<T>)>
V<T> load(const void *p) {
  V<T> r = {};
  __builtin_memcpy(&r, p, M);
  return r;
}

// movq or movsd
template V<char> load<char, 8>(const void *);     // bad
template V<short> load<short, 8>(const void *);   // bad
template V<int> load<int, 8>(const void *);       // bad
template V<long> load<long, 8>(const void *);     // good
// the following is disabled because V2SF isn't a supported mode
// template V<float> load<float, 8>(const void *);   // bad
template V<double> load<double, 8>(const void *); // good (movsd?)

// movd or movss
template V<char> load<char, 4>(const void *);   // bad
template V<short> load<short, 4>(const void *); // bad
template V<int> load<int, 4>(const void *);     // good
template V<float> load<float, 4>(const void *); // good

/* We should end up with one load and one insert for each function.  */
/* { dg-final { scan-tree-dump-times "BIT_INSERT_EXPR" 9 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MEM" 9 "optimized" } } */
