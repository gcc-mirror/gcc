/* { dg-additional-options "-fdump-tree-gimple" } */
/* { dg-skip-if "" { ilp32 } } */

#include "allocate-allocator-handle.h"

/* All cases are valid and should work.  */

/* Note: the OpenMP allocate directive is not supposed to change the alignof
   of expr a, it was decided to have too many edge cases.  The static asserts
   in these cases correctly test that it remains untouched.
   
   Note: cases that take omp_default_mem_alloc and have no align clause elide
   the GOMP_alloc/GOMP_free transformations.  */

/* 86 total cases.  */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc" 86 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_free" 86 "gimple" } } */

/* FIXME: This almost certainly needs tweaks for systems where alignof(int)
   is not 4.  */

/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(32," 31 "gimple" } } */

/* 9 * 3 = 27 cases scanned here.  */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(8," 3 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(16," 3 "gimple" } } */
/* Align==32 scanned above.  */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(64," 3 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(128," 3 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(256," 3 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(512," 3 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(1024," 3 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(2048," 3 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(4096," 3 "gimple" } } */

/* The align argument is at least alignof(int) (4) in all cases, thus we can subtract
   the cases where align >= 8 from the total cases to determine how many cases
   there are where align == 4.
   86 - (27 + 31) = 28.
   Please update this math if changes are made to this test so everything
   is (somewhat) coherent.  */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(4," 28 "gimple" } } */


/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(\\d+, \\d+, 0B\\\)" 29 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(\\d+, \\d+, 1\\\)" 22 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(\\d+, \\d+, 2\\\)" 31 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(\\d+, \\d+, 3\\\)" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc \\\(\\d+, \\d+, 4\\\)" 2 "gimple" } } */

struct S0 {
  int _v;
  S0(int v) : _v(v) {}
  operator int() const { return 42; }
};

struct S1 {
  int _v[2];
  S1(int v) : _v() {
    int *end = _v + sizeof(_v) / sizeof(*_v);
    for (int *it = _v; it != end; ++it)
      *it = v;
  }
  operator int() const { return 42; }
};

/**********************
 * dependent variable *
 **********************/

template<typename T>
T dep_local()
{
  T a = 42;
  #pragma omp allocate(a)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(T));
  #endif
  return a;
}

template<typename T>
T dep_local_align()
{
  T a = 42;
  #pragma omp allocate(a) align(32)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(T));
  #endif
  return a;
}

template<typename T>
T dep_local_alloc_0()
{
  T a = 42;
  #pragma omp allocate(a) allocator(omp_default_mem_alloc)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(T));
  #endif
  return a;
}

template<typename T>
T dep_local_alloc_1()
{
  T a = 42;
  #pragma omp allocate(a) allocator(omp_large_cap_mem_alloc)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(T));
  #endif
  return a;
}

template<typename T>
T dep_local_align_alloc_0()
{
  T a = 42;
  #pragma omp allocate(a) align(32) allocator(omp_default_mem_alloc)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(T));
  #endif
  return a;
}

template<typename T>
T dep_local_align_alloc_1()
{
  T a = 42;
  #pragma omp allocate(a) align(32) allocator(omp_large_cap_mem_alloc)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(T));
  #endif
  return a;
}

/* 5 cases per expansion, dep_local_alloc_0 uses omp_default_mem_alloc
   without an align clause.
   2 allocator==0B,
   1 allocator==1 (because 1 case elided),
   2 allocator==2 cases per expansion.

   3 align==32 cases per expansion.  */
#define INSTANTIATE_ALL_WITH_T(type)		\
  do {						\
    type v0 = dep_local<type>();		\
    type v1 = dep_local_align<type>();		\
    type v2 = dep_local_alloc_0<type>();	\
    type v3 = dep_local_alloc_1<type>();	\
    type v4 = dep_local_align_alloc_0<type>();	\
    type v5 = dep_local_align_alloc_1<type>();	\
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
void instantiate_dep_tests()
{
  INSTANTIATE_ALL_WITH_T(int);
  INSTANTIATE_ALL_WITH_T(float);
  INSTANTIATE_ALL_WITH_T(S0);
  INSTANTIATE_ALL_WITH_T(S1);
}

#undef INSTANTIATE_ALL_WITH_T

/**********************
 * template parameter *
 **********************/

template<typename T>
int template_parm(T)
{
  int a = 42;
  #pragma omp allocate(a)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(int));
  #endif
  return a;
}

template<typename T>
int template_parm_align(T)
{
  int a = 42;
  #pragma omp allocate(a) align(32)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(int));
  #endif
  return a;
}

template<typename T>
int template_parm_alloc_0(T)
{
  int a = 42;
  #pragma omp allocate(a) allocator(omp_default_mem_alloc)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(int));
  #endif
  return a;
}

template<typename T>
int template_parm_alloc_1(T)
{
  int a = 42;
  #pragma omp allocate(a) allocator(omp_large_cap_mem_alloc)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(int));
  #endif
  return a;
}

template<typename T>
int template_parm_align_alloc_0(T)
{
  int a = 42;
  #pragma omp allocate(a) align(32) allocator(omp_default_mem_alloc)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(int));
  #endif
  return a;
}

template<typename T>
int template_parm_align_alloc_1(T)
{
  int a = 42;
  #pragma omp allocate(a) align(32) allocator(omp_large_cap_mem_alloc)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(int));
  #endif
  return a;
}

/* 5 cases per expansion, template_parm_alloc_0 uses omp_default_mem_alloc
   without an align clause.
   2 allocator==0B,
   1 allocator==1 (because 1 case elided),
   2 allocator==2 cases per expansion.

   3 align==32 cases per expansion.  */
#define INSTANTIATE_ALL_WITH_T(type)		\
  do {						\
    type a = 42;				\
    int v0 = template_parm(a);			\
    int v1 = template_parm_align(a);		\
    int v2 = template_parm_alloc_0(a);		\
    int v3 = template_parm_alloc_1(a);		\
    int v4 = template_parm_align_alloc_0(a);	\
    int v5 = template_parm_align_alloc_1(a);	\
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
void instantiate_template_parm_tests()
{
  INSTANTIATE_ALL_WITH_T(int);
  INSTANTIATE_ALL_WITH_T(float);
  INSTANTIATE_ALL_WITH_T(S0);
  INSTANTIATE_ALL_WITH_T(S1);
}

#undef INSTANTIATE_ALL_WITH_T

/*************************************
 * non-type template parameter align *
 *************************************/

template<int Align>
int nttp_align()
{
  int a = 42;
  #pragma omp allocate(a) align(Align)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(int));
  #endif
  return a;
}

template<int Align>
int nttp_align_alloc_0()
{
  int a = 42;
  #pragma omp allocate(a) align(Align) allocator(omp_default_mem_alloc)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(int));
  #endif
  return a;
}

template<int Align>
int nttp_align_alloc_1()
{
  int a = 42;
  #pragma omp allocate(a) align(Align) allocator(omp_large_cap_mem_alloc)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(int));
  #endif
  return a;
}

/* 3 cases per expansion.
   1 allocator==0B, 1 allocator==1, 1 allocator==2 cases per expansion.
   3 align==value cases per expansion.  */
#define INSTANTIATE_ALL_WITH_V(value)		\
  do {						\
    int v0 = nttp_align<value>();		\
    int v1 = nttp_align_alloc_0<value>();	\
    int v2 = nttp_align_alloc_1<value>();	\
    static_cast<void>(v0);			\
    static_cast<void>(v1);			\
    static_cast<void>(v2);			\
  } while (false)

/* 13 * 3 cases, 39.
   13 * allocator==0B cases.
   13 * allocator==1 cases.
   13 * allocator==2 cases.  */
void instantiate_nttp_align_tests()
{
  /* Minimum align for int is 4, 3 * 3 alloc=4, 9 alloc=4.  */
  INSTANTIATE_ALL_WITH_V(1);
  INSTANTIATE_ALL_WITH_V(2);
  INSTANTIATE_ALL_WITH_V(4);
  /* 3 alloc=N for the rest.  */
  INSTANTIATE_ALL_WITH_V(8);
  INSTANTIATE_ALL_WITH_V(16);
  INSTANTIATE_ALL_WITH_V(32);
  INSTANTIATE_ALL_WITH_V(64);
  INSTANTIATE_ALL_WITH_V(128);
  INSTANTIATE_ALL_WITH_V(256);
  INSTANTIATE_ALL_WITH_V(512);
  INSTANTIATE_ALL_WITH_V(1024);
  INSTANTIATE_ALL_WITH_V(2048);
  INSTANTIATE_ALL_WITH_V(4096);
}

#undef INSTANTIATE_ALL_WITH_V


template<omp_allocator_handle_t Alloc>
int nttp_alloc()
{
  int a = 42;
  #pragma omp allocate(a) allocator(Alloc)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(int));
  #endif
  return a;
}

template<omp_allocator_handle_t Alloc>
int nttp_alloc_align()
{
  int a = 42;
  #pragma omp allocate(a) align(32) allocator(Alloc)
  #if __cplusplus >= 201103L
  static_assert(alignof(a) == alignof(int));
  #endif
  return a;
}

/* 2 cases per expansion.
   2 allocator=value per expansion.
   1 alloc=32 per expansion.  */
#define INSTANTIATE_ALL_WITH_V(value)		\
  do {						\
    int v0 = nttp_alloc<value>();		\
    int v1 = nttp_alloc_align<value>();		\
    static_cast<void>(v0);			\
    static_cast<void>(v1);			\
  } while (false)

/* 4 * 2 cases, 8,
   -1 omp_default_mem_alloc without align, 7 cases.
   1 * allocator==1 (1 elided),
   2 * allocator==2,
   2 * allocator==3,
   2 * allocator==4 cases.
   4 * 1 alloc=32 cases, 4.  */
void instantiate_nttp_alloc_tests()
{
  INSTANTIATE_ALL_WITH_V(omp_default_mem_alloc);
  INSTANTIATE_ALL_WITH_V(omp_large_cap_mem_alloc);
  INSTANTIATE_ALL_WITH_V(omp_const_mem_alloc);
  INSTANTIATE_ALL_WITH_V(omp_high_bw_mem_alloc);
}

#undef INSTANTIATE_ALL_WITH_V

/* We are probably missing quite a few cases here.
   Missing cases with a param in allocator clause.  */
