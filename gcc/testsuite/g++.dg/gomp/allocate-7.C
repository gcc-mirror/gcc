/* { dg-do compile { target c++20 } } */

#include "allocate-allocator-handle.h"

/* C++20 tests */

struct S0 {
  int _v;
  S0(int v) : _v(v) {}
  operator int() const { return 42; }
};

struct S1 {
  int _v[32];
  S1(int v) : _v{v, v, v, v, v, v, v, v,
		 v, v, v, v, v, v, v, v,
		 v, v, v, v, v, v, v, v,
		 v, v, v, v, v, v, v, v} {}
  operator int() const { return 42; }
};

/*****************************
 * template parameter (auto) *
 *****************************/

int auto_parm(auto)
{
  int a = 42;
  #pragma omp allocate(a)
  static_assert(alignof(a) == alignof(int));
  return a;
}

int auto_parm_align(auto)
{
  int a = 42;
  #pragma omp allocate(a) align(32)
  static_assert(alignof(a) == alignof(int));
  return a;
}

int auto_parm_alloc_0(auto)
{
  int a = 42;
  #pragma omp allocate(a) allocator(omp_default_mem_alloc)
  static_assert(alignof(a) == alignof(int));
  return a;
}

int auto_parm_alloc_1(auto)
{
  int a = 42;
  #pragma omp allocate(a) allocator(omp_large_cap_mem_alloc)
  static_assert(alignof(a) == alignof(int));
  return a;
}

int auto_parm_align_alloc_0(auto)
{
  int a = 42;
  #pragma omp allocate(a) align(32) allocator(omp_default_mem_alloc)
  static_assert(alignof(a) == alignof(int));
  return a;
}

int auto_parm_align_alloc_1(auto)
{
  int a = 42;
  #pragma omp allocate(a) align(32) allocator(omp_large_cap_mem_alloc)
  static_assert(alignof(a) == alignof(int));
  return a;
}

#define INSTANTIATE_ALL_WITH_T(type)		\
  do {						\
    type a = 42;				\
    int v0 = auto_parm(a);			\
    int v1 = auto_parm_align(a);		\
    int v2 = auto_parm_alloc_0(a);		\
    int v3 = auto_parm_alloc_1(a);		\
    int v4 = auto_parm_align_alloc_0(a);	\
    int v5 = auto_parm_align_alloc_1(a);	\
    static_cast<void>(v0);			\
    static_cast<void>(v1);			\
    static_cast<void>(v2);			\
    static_cast<void>(v3);			\
    static_cast<void>(v4);			\
    static_cast<void>(v5);			\
  } while (false)

void instantiate_auto_parm_tests()
{
  INSTANTIATE_ALL_WITH_T(int);
  INSTANTIATE_ALL_WITH_T(float);
  INSTANTIATE_ALL_WITH_T(S0);
  INSTANTIATE_ALL_WITH_T(S1);
}

#undef INSTANTIATE_ALL_WITH_T
