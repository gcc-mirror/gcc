#include "allocate-allocator-handle.h"

/* All cases are valid and should work.  */

/* Note: the OpenMP allocate directive is not supposed to change the alignof
   of expr a, it was decided to have too many edge cases.  The static asserts
   in these cases correctly test that it remains untouched.  */

struct S0 {
  int _v;
  S0(int v) : _v(v) {}
  operator int() const { return 42; }
};

struct S1 {
  int _v[32];
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

#define INSTANTIATE_ALL_WITH_V(value)		\
  do {						\
    int v0 = nttp_align<value>();		\
    int v1 = nttp_align_alloc_0<value>();	\
    int v2 = nttp_align_alloc_1<value>();	\
  } while (false)

void instantiate_nttp_align_tests()
{
  INSTANTIATE_ALL_WITH_V(1);
  INSTANTIATE_ALL_WITH_V(2);
  INSTANTIATE_ALL_WITH_V(4);
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

#define INSTANTIATE_ALL_WITH_V(value)		\
  do {						\
    int v0 = nttp_alloc<value>();		\
    int v1 = nttp_alloc_align<value>();		\
  } while (false)

void instantiate_nttp_alloc_tests()
{
  INSTANTIATE_ALL_WITH_V(omp_default_mem_alloc);
  INSTANTIATE_ALL_WITH_V(omp_large_cap_mem_alloc);
  INSTANTIATE_ALL_WITH_V(omp_const_mem_alloc);
  INSTANTIATE_ALL_WITH_V(omp_high_bw_mem_alloc);
}

#undef INSTANTIATE_ALL_WITH_V

/* We are probably missing quite a few cases here  */
/* missing cases for alloc param */
