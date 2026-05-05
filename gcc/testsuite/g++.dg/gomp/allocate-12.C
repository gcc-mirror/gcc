/* { dg-do compile { target c++14 } } */
/* { dg-additional-options "-fdump-tree-gimple" } */

/* Ensure allocate directive does not break attributes on a decl (and vice-versa).  */

/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\(64," 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\(32," 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\(16," 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\(8," 2 "gimple" } } */

/* { dg-final { scan-tree-dump-times "__builtin_GOMP_free" 8 "gimple" } } */

template<unsigned Align>
int test0 ()
{
  int a [[deprecated]];
  #pragma omp allocate(a) align(Align)
  return a; /* { dg-warning  "'a' is deprecated" } */
}

template int test0<64u>();
template int test0<32u>();
template int test0<16u>();
template int test0<8u>();


template<unsigned Align>
int test1 ()
{
  int a [[deprecated]];
  #pragma omp allocate(a) align(Align)
  return a; /* { dg-warning  "'a' is deprecated" } */
}

template int test1<8u>();
template int test1<16u>();
template int test1<32u>();
template int test1<64u>();
