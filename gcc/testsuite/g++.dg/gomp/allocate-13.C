/* { dg-do compile { target c++11 } } */
/* { dg-additional-options "-fdump-tree-gimple" } */

/* Misc valid constexpr expressions in a clause.
   TODO: Really needs more cases.  */

/* Not constexpr, invalid value, not called.  */
template<typename T>
int get_align(T) { return 42; }

template<typename T>
int dependent_call_adl()
{
  int a = 42;
  #pragma omp allocate(a) align(get_align(T{}))
  return a;
}

namespace foo
{
  struct S {};

  constexpr int get_align(S) { return 32; }
}

template int dependent_call_adl<foo::S>();
/* { dg-final { scan-tree-dump "__builtin_GOMP_alloc \\(32," "gimple" } } */
/* { dg-final { scan-tree-dump "__builtin_GOMP_free" "gimple" } } */
