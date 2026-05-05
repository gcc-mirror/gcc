/* { dg-do compile { target c++20 } } */
/* { dg-skip-if "" { ilp32 } } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#include "allocate-allocator-handle.h"


/* Valid C++20 cases.

   Note: cases that take omp_default_mem_alloc and have no align clause elide
   the GOMP_alloc/GOMP_free transformations.  */

/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc" 40 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_free" 40 "gimple" } } */

/* FIXME: This almost certainly needs tweaks for systems where alignof(int)
   is not 4.  */

/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(32," 24 "gimple" } } */

/* The align argument is at least alignof(int) (4) in all cases, thus we can subtract
   the cases where align >= 8 from the total cases to determine how many cases
   there are where align == 4.
   40 - 24 = 16.
   Please update this math if changes are made to this test so everything
   is (somewhat) coherent.  */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(4," 16 "gimple" } } */

/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(\\d+, \\d+, 0B\\\)" 16 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(\\d+, \\d+, 1\\\)" 8 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(\\d+, \\d+, 2\\\)" 16 "gimple" } } */

struct S0 {
  int _v;
  S0(int v) : _v(v) {}
  operator int() const { return 42; }
};

struct S1 {
  int _v[2];
  S1(int v) : _v{v, v} {}
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

/* 5 cases per expansion, auto_parm_alloc_0 uses omp_default_mem_alloc
   without an align clause.
   2 allocator==0B,
   1 allocator==1 (because 1 case elided),
   2 allocator==2 cases per expansion.

   3 align==32 cases per expansion.  */
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

/* 4 * 5 cases, 20.
   4 * 2 allocator==0B cases, 8.
   4 * 1 allocator==1 cases, 4.
   4 * 2 allocator==2 cases, 8.
   4 * 3 align==32 cases, 12.  */
void instantiate_auto_parm_tests()
{
  INSTANTIATE_ALL_WITH_T(int);
  INSTANTIATE_ALL_WITH_T(float);
  INSTANTIATE_ALL_WITH_T(S0);
  INSTANTIATE_ALL_WITH_T(S1);
}

#undef INSTANTIATE_ALL_WITH_T


auto dep_local_auto_parm(auto p)
{
  auto a = p;
  #pragma omp allocate(a)
  static_assert(alignof(a) == alignof(decltype(p)));
  return a;
}

auto dep_local_auto_parm_align(auto p)
{
  auto a = p;
  #pragma omp allocate(a) align(32)
  static_assert(alignof(a) == alignof(decltype(p)));
  return a;
}

auto dep_local_auto_parm_alloc_0(auto p)
{
  auto a = p;
  #pragma omp allocate(a) allocator(omp_default_mem_alloc)
  static_assert(alignof(a) == alignof(decltype(p)));
  return a;
}

auto dep_local_auto_parm_alloc_1(auto p)
{
  auto a = p;
  #pragma omp allocate(a) allocator(omp_large_cap_mem_alloc)
  static_assert(alignof(a) == alignof(decltype(p)));
  return a;
}

auto dep_local_auto_parm_align_alloc_0(auto p)
{
  auto a = p;
  #pragma omp allocate(a) align(32) allocator(omp_default_mem_alloc)
  static_assert(alignof(a) == alignof(decltype(p)));
  return a;
}

auto dep_local_auto_parm_align_alloc_1(auto p)
{
  auto a = p;
  #pragma omp allocate(a) align(32) allocator(omp_large_cap_mem_alloc)
  static_assert(alignof(a) == alignof(decltype(p)));
  return a;
}

/* 5 cases per expansion, dep_local_auto_parm_alloc_0 uses omp_default_mem_alloc
   without an align clause.
   2 allocator==0B,
   1 allocator==1 (because 1 case elided),
   2 allocator==2 cases per expansion.

   3 align==32 cases per expansion.  */
#define INSTANTIATE_ALL_WITH_T(type)			\
  do {							\
    type a = 42;					\
    type v0 = dep_local_auto_parm(a);			\
    type v1 = dep_local_auto_parm_align(a);		\
    type v2 = dep_local_auto_parm_alloc_0(a);		\
    type v3 = dep_local_auto_parm_alloc_1(a);		\
    type v4 = dep_local_auto_parm_align_alloc_0(a);	\
    type v5 = dep_local_auto_parm_align_alloc_1(a);	\
    static_cast<void>(v0);				\
    static_cast<void>(v1);				\
    static_cast<void>(v2);				\
    static_cast<void>(v3);				\
    static_cast<void>(v4);				\
    static_cast<void>(v5);				\
  } while (false)

/* 4 * 5 cases, 20.
   4 * 2 allocator==0B cases, 8.
   4 * 1 allocator==1 cases, 4.
   4 * 2 allocator==2 cases, 8.
   4 * 3 align==32 cases, 12.  */
void instantiate_dep_local_auto_parm_tests()
{
  INSTANTIATE_ALL_WITH_T(int);
  INSTANTIATE_ALL_WITH_T(float);
  INSTANTIATE_ALL_WITH_T(S0);
  INSTANTIATE_ALL_WITH_T(S1);
}

#undef INSTANTIATE_ALL_WITH_T
