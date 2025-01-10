/* { dg-do compile { target c++11 } } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#include "allocate-allocator-handle.h"

/* Valid uses of lambda captures in an allocator clause.  */

/* { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc" 200 "gimple" } } */

/* { dg-final { scan-tree-dump-times "__builtin_GOMP_free" 200 "gimple" } } */

template<typename, typename>
struct is_same { static constexpr bool value = false; };

template<typename T>
struct is_same<T, T> { static constexpr bool value = true; };

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

/* Suppresses int/float cases from being optimized out, I'm not sure if this is
   going to be sufficient for all cases though.  */
int (*prevent_optimization)() = nullptr;

#define BLANK_ARGUMENT

/* Capturing with an initiaizer was added in C++14  */
#if __cplusplus >= 201402L

#define CAPTURE_LITERAL_INIT_LAMBDA(type)				\
do {									\
  auto capture_literal_init = [alloc = omp_default_mem_alloc](){	\
    type a = prevent_optimization();					\
    _Pragma("omp allocate(a) allocator(alloc)")				\
    return a;								\
  };									\
  auto result = capture_literal_init();					\
  static_assert(is_same<type, decltype(result)>::value);		\
} while (false)

void test_capture_literal_init_nondep_lambdas()
{
  /* 4 cases */
  CAPTURE_LITERAL_INIT_LAMBDA(int);
  CAPTURE_LITERAL_INIT_LAMBDA(float);
  CAPTURE_LITERAL_INIT_LAMBDA(S0);
  CAPTURE_LITERAL_INIT_LAMBDA(S1);
}

/* 1 case per instantiation */
template<typename T>
void test_capture_literal_init_dependent_lambdas()
{
  CAPTURE_LITERAL_INIT_LAMBDA(T);
}
/* 4 cases */
template void test_capture_literal_init_dependent_lambdas<int>();
template void test_capture_literal_init_dependent_lambdas<float>();
template void test_capture_literal_init_dependent_lambdas<S0>();
template void test_capture_literal_init_dependent_lambdas<S1>();

#undef CAPTURE_LITERAL_INIT_LAMBDA



#define EXPLICIT_CAPTURE_WITH_INIT_LAMBDA(type, opt_tok, capture)	\
do {									\
  auto explicit_capture_with_init = [opt_tok alloc = capture](){	\
    type a = prevent_optimization();					\
    _Pragma("omp allocate(a) allocator(alloc)")				\
    return a;								\
  };									\
  auto result = explicit_capture_with_init();				\
  static_assert(is_same<type, decltype(result)>::value);		\
} while (false)

/* 4 cases per expansion */
#define TEST_LAMBDAS_WITH_TYPE(type, opt_tok)				\
do {									\
  EXPLICIT_CAPTURE_WITH_INIT_LAMBDA(type, opt_tok, alloc);		\
  EXPLICIT_CAPTURE_WITH_INIT_LAMBDA(type, opt_tok, alloc_ref);		\
  EXPLICIT_CAPTURE_WITH_INIT_LAMBDA(type, opt_tok, alloc_param);	\
  EXPLICIT_CAPTURE_WITH_INIT_LAMBDA(type, opt_tok, alloc_ref_param);	\
} while (false)

/* 4*4 = 16 cases */
void test_capture_by_value_with_init_nondep_lambdas(omp_allocator_handle_t alloc_param,
						    omp_allocator_handle_t& alloc_ref_param)
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  omp_allocator_handle_t& alloc_ref = alloc;
  TEST_LAMBDAS_WITH_TYPE(int, BLANK_ARGUMENT);
  TEST_LAMBDAS_WITH_TYPE(float, BLANK_ARGUMENT);
  TEST_LAMBDAS_WITH_TYPE(S0, BLANK_ARGUMENT);
  TEST_LAMBDAS_WITH_TYPE(S1, BLANK_ARGUMENT);
}

/* 4*4 = 16 cases */
void test_capture_by_ref_with_init_nondep_lambdas(omp_allocator_handle_t alloc_param,
						  omp_allocator_handle_t& alloc_ref_param)
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  omp_allocator_handle_t& alloc_ref = alloc;  
  TEST_LAMBDAS_WITH_TYPE(int, &);
  TEST_LAMBDAS_WITH_TYPE(float, &);
  TEST_LAMBDAS_WITH_TYPE(S0, &);
  TEST_LAMBDAS_WITH_TYPE(S1, &);
}

/* 4 cases per instantiation */
template<typename T>
void test_capture_by_value_with_init_dependent_lambdas(omp_allocator_handle_t alloc_param,
						       omp_allocator_handle_t& alloc_ref_param)
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  omp_allocator_handle_t& alloc_ref = alloc;
  TEST_LAMBDAS_WITH_TYPE(T, BLANK_ARGUMENT);
}
/* 4*4 = 16 cases */
template void test_capture_by_value_with_init_dependent_lambdas<int>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_capture_by_value_with_init_dependent_lambdas<float>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_capture_by_value_with_init_dependent_lambdas<S0>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_capture_by_value_with_init_dependent_lambdas<S1>(omp_allocator_handle_t, omp_allocator_handle_t&);

/* 4 cases per instantiation */
template<typename T>
void test_capture_by_ref_with_init_dependent_lambdas(omp_allocator_handle_t alloc_param,
						     omp_allocator_handle_t& alloc_ref_param)
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  omp_allocator_handle_t& alloc_ref = alloc;  
  TEST_LAMBDAS_WITH_TYPE(T, &);
}
/* 4*4 = 16 cases */
template void test_capture_by_ref_with_init_dependent_lambdas<int>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_capture_by_ref_with_init_dependent_lambdas<float>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_capture_by_ref_with_init_dependent_lambdas<S0>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_capture_by_ref_with_init_dependent_lambdas<S1>(omp_allocator_handle_t, omp_allocator_handle_t&);

#undef EXPLICIT_CAPTURE_WITH_INIT_LAMBDA
#undef TEST_LAMBDAS_WITH_TYPE

#else
/* There are 4 + 4 + 16 + 16 + 16 + 16 cases in the above, we replicate that
   many so our count in dg-final isn't wrong in C++11 mode.
   The counter isn't required but it doesn't hurt and might make it easier
   to inspect the asm if we need to, that's the goal anyway.  */

#define DUMMY_ALLOC(counter)			\
do {						\
  auto dummy_ ##counter = [alloc](){		\
    int a = prevent_optimization();		\
    _Pragma("omp allocate(a) allocator(alloc)")	\
    return a;					\
  };						\
  auto result = dummy_ ##counter();		\
} while (false)

#define DUMMY_X4(counter) 	\
do {				\
  DUMMY_ALLOC(counter ##A);	\
  DUMMY_ALLOC(counter ##B);	\
  DUMMY_ALLOC(counter ##C);	\
  DUMMY_ALLOC(counter ##D);	\
} while (false)

void dummy_cases(omp_allocator_handle_t alloc)
{
  /* 4x1 for test_capture_literal_init_nondep_lambdas */
  DUMMY_X4(0);
  /* 4x1 for test_capture_literal_init_dependent_lambdas */
  DUMMY_X4(1);
  /* 4x4 for test_capture_by_value_with_init_nondep_lambdas */
  DUMMY_X4(2);
  DUMMY_X4(3);
  DUMMY_X4(4);
  DUMMY_X4(5);
  /* 4x4 for test_capture_by_value_with_init_dependent_lambdas */
  DUMMY_X4(6);
  DUMMY_X4(7);
  DUMMY_X4(8);
  DUMMY_X4(9);
  /* 4x4 for test_capture_by_ref_with_init_dependent_lambdas */
  DUMMY_X4(9);
  DUMMY_X4(10);
  DUMMY_X4(11);
  DUMMY_X4(12);
  /* 4x4 for test_capture_by_ref_with_init_nondep_lambdas */
  DUMMY_X4(13);
  DUMMY_X4(14);
  DUMMY_X4(15);
  DUMMY_X4(16);
}

#undef DUMMY_ALLOC
#undef DUMMY_X4

#endif


#define EXPLICIT_CAPTURE_LAMBDAS(type, opt_tok)			\
do {								\
  auto explicit_capture0 = [opt_tok alloc](){			\
    type a = prevent_optimization();				\
    _Pragma("omp allocate(a) allocator(alloc)")			\
    return a;							\
  };								\
  auto result0 = explicit_capture0();				\
  static_assert(is_same<type, decltype(result0)>::value);	\
								\
  auto explicit_capture1 = [opt_tok alloc_ref](){		\
    type a = prevent_optimization();				\
    _Pragma("omp allocate(a) allocator(alloc_ref)")		\
    return a;							\
  };								\
  auto result1 = explicit_capture1();				\
  static_assert(is_same<type, decltype(result1)>::value);	\
								\
  auto explicit_capture2 = [opt_tok alloc_param](){		\
    type a = prevent_optimization();				\
    _Pragma("omp allocate(a) allocator(alloc_param)")		\
    return a;							\
  };								\
  auto result2 = explicit_capture2();				\
  static_assert(is_same<type, decltype(result2)>::value);	\
								\
  auto explicit_capture3 = [opt_tok alloc_ref_param](){		\
    type a = prevent_optimization();				\
    _Pragma("omp allocate(a) allocator(alloc_ref_param)")	\
    return a;							\
  };								\
  auto result3 = explicit_capture3();				\
  static_assert(is_same<type, decltype(result3)>::value);	\
} while (false)

void test_explicit_capture_by_value_nondep_lambdas(omp_allocator_handle_t alloc_param,
						   omp_allocator_handle_t& alloc_ref_param)
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  omp_allocator_handle_t& alloc_ref = alloc;  
  EXPLICIT_CAPTURE_LAMBDAS(int, BLANK_ARGUMENT);
  EXPLICIT_CAPTURE_LAMBDAS(float, BLANK_ARGUMENT);
  EXPLICIT_CAPTURE_LAMBDAS(S0, BLANK_ARGUMENT);
  EXPLICIT_CAPTURE_LAMBDAS(S1, BLANK_ARGUMENT);
}

void test_explicit_capture_by_ref_nondep_lambdas(omp_allocator_handle_t alloc_param,
						 omp_allocator_handle_t& alloc_ref_param)
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  omp_allocator_handle_t& alloc_ref = alloc;  
  EXPLICIT_CAPTURE_LAMBDAS(int, &);
  EXPLICIT_CAPTURE_LAMBDAS(float, &);
  EXPLICIT_CAPTURE_LAMBDAS(S0, &);
  EXPLICIT_CAPTURE_LAMBDAS(S1, &);
}

template<typename T>
void test_explicit_capture_by_value_dependent_lambdas(omp_allocator_handle_t alloc_param,
						      omp_allocator_handle_t& alloc_ref_param)
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  omp_allocator_handle_t& alloc_ref = alloc;  
  EXPLICIT_CAPTURE_LAMBDAS(T, BLANK_ARGUMENT);
}
template void test_explicit_capture_by_value_dependent_lambdas<int>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_explicit_capture_by_value_dependent_lambdas<float>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_explicit_capture_by_value_dependent_lambdas<S0>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_explicit_capture_by_value_dependent_lambdas<S1>(omp_allocator_handle_t, omp_allocator_handle_t&);
						      

template<typename T>
void test_explicit_capture_by_ref_dependent_lambdas(omp_allocator_handle_t alloc_param,
						    omp_allocator_handle_t& alloc_ref_param)
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  omp_allocator_handle_t& alloc_ref = alloc;  
  EXPLICIT_CAPTURE_LAMBDAS(T, &);
}
template void test_explicit_capture_by_ref_dependent_lambdas<int>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_explicit_capture_by_ref_dependent_lambdas<float>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_explicit_capture_by_ref_dependent_lambdas<S0>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_explicit_capture_by_ref_dependent_lambdas<S1>(omp_allocator_handle_t, omp_allocator_handle_t&);

#undef EXPLICIT_CAPTURE_LAMBDA
#undef TEST_LAMBDAS_WITH_TYPE


#define DEFAULT_CAPTURE_LAMBDAS(type, capture)			\
do {								\
  auto default_capture0 = [capture](){				\
    type a = prevent_optimization();				\
    _Pragma("omp allocate(a) allocator(alloc)")			\
    return a;							\
  };								\
  auto result0 = default_capture0();				\
  static_assert(is_same<type, decltype(result0)>::value);	\
								\
  auto default_capture1 = [capture](){				\
    type a = prevent_optimization();				\
    _Pragma("omp allocate(a) allocator(alloc_ref)")		\
    return a;							\
  };								\
  auto result1 = default_capture1();				\
  static_assert(is_same<type, decltype(result1)>::value);	\
								\
  auto default_capture2 = [capture](){				\
    type a = prevent_optimization();				\
    _Pragma("omp allocate(a) allocator(alloc_param)")		\
    return a;							\
  };								\
  auto result2 = default_capture2();				\
  static_assert(is_same<type, decltype(result2)>::value);	\
								\
  auto default_capture3 = [capture](){				\
    type a = prevent_optimization();				\
    _Pragma("omp allocate(a) allocator(alloc_ref_param)")	\
    return a;							\
  };								\
  auto result3 = default_capture3();				\
  static_assert(is_same<type, decltype(result3)>::value);	\
} while (false)



void test_default_capture_by_value_nondep_lambdas(omp_allocator_handle_t alloc_param,
						  omp_allocator_handle_t& alloc_ref_param)
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  omp_allocator_handle_t& alloc_ref = alloc;  
  DEFAULT_CAPTURE_LAMBDAS(int, =);
  DEFAULT_CAPTURE_LAMBDAS(float, =);
  DEFAULT_CAPTURE_LAMBDAS(S0, =);
  DEFAULT_CAPTURE_LAMBDAS(S1, =);
}

void test_default_capture_by_ref_nondep_lambdas(omp_allocator_handle_t alloc_param,
						omp_allocator_handle_t& alloc_ref_param)
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  omp_allocator_handle_t& alloc_ref = alloc;  
  DEFAULT_CAPTURE_LAMBDAS(int, &);
  DEFAULT_CAPTURE_LAMBDAS(float, &);
  DEFAULT_CAPTURE_LAMBDAS(S0, &);
  DEFAULT_CAPTURE_LAMBDAS(S1, &);
}

template<typename T>
void test_default_capture_by_value_dependent_lambdas(omp_allocator_handle_t alloc_param,
						     omp_allocator_handle_t& alloc_ref_param)
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  omp_allocator_handle_t& alloc_ref = alloc;  
  DEFAULT_CAPTURE_LAMBDAS(T, =);
}
template void test_default_capture_by_value_dependent_lambdas<int>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_default_capture_by_value_dependent_lambdas<float>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_default_capture_by_value_dependent_lambdas<S0>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_default_capture_by_value_dependent_lambdas<S1>(omp_allocator_handle_t, omp_allocator_handle_t&);

template<typename T>
void test_default_capture_by_ref_dependent_lambdas(omp_allocator_handle_t alloc_param,
						   omp_allocator_handle_t& alloc_ref_param)
{
  omp_allocator_handle_t alloc = omp_default_mem_alloc;
  omp_allocator_handle_t& alloc_ref = alloc;  
  DEFAULT_CAPTURE_LAMBDAS(T, &);
}
template void test_default_capture_by_ref_dependent_lambdas<int>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_default_capture_by_ref_dependent_lambdas<float>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_default_capture_by_ref_dependent_lambdas<S0>(omp_allocator_handle_t, omp_allocator_handle_t&);
template void test_default_capture_by_ref_dependent_lambdas<S1>(omp_allocator_handle_t, omp_allocator_handle_t&);

#undef DEFAULT_CAPTURE_LAMBDA
#undef TEST_LAMBDAS_WITH_TYPE

/* Potential missing cases: captures that are type dependent, mutable lambdas.  */

