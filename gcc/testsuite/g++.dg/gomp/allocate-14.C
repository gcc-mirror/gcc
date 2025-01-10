/* { dg-do compile { target c++17 } } */

/* Nested lambdas */

#include "allocate-allocator-handle.h"

template<int Align>
auto lambda_00()
{
  return [](auto p0){
    return [](auto p1){
      return [](auto p2){
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b;
	  #pragma omp allocate(a) align(Align) allocator(p2)
	  return a;
	};
      };
    };
  };
}

template<int Align>
auto lambda_01()
{
  return [](auto p0){
    return [](auto p1){
      return [](auto p2){
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b; /* { dg-message "'a' declared here" } */
	  #pragma omp allocate(a) align(Align) allocator(p2)
	  /* { dg-error "variable 'a' with reference type may not appear as a list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
	  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-2 } */
	  /* { dg-error "'allocator' clause expression has type 'const int' rather than 'omp_allocator_handle_t'" "" { target *-*-* } .-3 } */
	  return a;
	};
      };
    };
  };
}

template<int Align>
auto lambda_02()
{
  return [](auto p0){
    return [](auto p1){
      return [](auto p2){
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b;
	  #pragma omp allocate(a) align(Align) allocator(p2)
	  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-1 } */
	  return a;
	};
      };
    };
  };
}

void instantiate_lambdas_0()
{
  /* valid */
  auto c00 = lambda_00<32>();
  auto c01 = c00(0);
  auto c02 = c01(0);
  auto c03 = c02(omp_default_mem_alloc);
  c03(0);
  /* invalid */
  auto c10 = lambda_01<30>(); /* { dg-message "required from here" } */
  auto c11 = c10(0); /* { dg-bogus "required from here" } */
  auto c12 = c11(0); /* { dg-bogus "required from here" } */
  auto c13 = c12(0); /* { dg-message "required from here" } */
  int a = 0;
  c13.operator()<int&>(a); /* { dg-message "required from here" } */
  /* partially instantiated (invalid) */
  auto c20 = lambda_02<30>();  /* { dg-message "required from here" } */
}



template<int Align>
auto lambda_10()
{
  return [](auto p0){
    int a = 42;
    #pragma omp allocate(a) align(Align)
    return [](auto p1){
      int a = 42;
      #pragma omp allocate(a) align(Align)
      return [](auto p2){
	int a = 42;
	#pragma omp allocate(a) align(Align)
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b;
	  #pragma omp allocate(a) align(Align) allocator(p2)
	  return a;
	};
      };
    };
  };
}

template<int Align>
auto lambda_11()
{
  return [](auto p0){
    int a = 42;
    #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
    return [](auto p1){
      int a = 42;
      #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
      return [](auto p2){
	int a = 42;
	#pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b; /* { dg-message "'a' declared here" } */
	  #pragma omp allocate(a) align(Align) allocator(p2)
	  /* { dg-error "variable 'a' with reference type may not appear as a list item in an 'allocate' directive" "" { target *-*-* } .-1 } */
	  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { target *-*-* } .-2 } */
	  /* { dg-error "'allocator' clause expression has type 'const int' rather than 'omp_allocator_handle_t'" "" { target *-*-* } .-3 } */
	  return a;
	};
      };
    };
  };
}

template<int Align>
auto lambda_12()
{
  return [](auto p0){
    int a = 42;
    #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
    return [](auto p1){
      int a = 42;
      #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
      return [](auto p2){
	int a = 42;
	#pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b;
	  #pragma omp allocate(a) align(Align) allocator(p2) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" } */
	  return a;
	};
      };
    };
  };
}

void instantiate_lambdas_1()
{
  /* valid */
  auto c00 = lambda_10<32>();
  auto c01 = c00(0);
  auto c02 = c01(0);
  auto c03 = c02(omp_default_mem_alloc);
  c03(0);
  /* invalid */
  auto c10 = lambda_11<30>(); /* { dg-message "required from here" } */
  auto c11 = c10(0); /* { dg-bogus "required from here" } */
  auto c12 = c11(0); /* { dg-bogus "required from here" } */
  auto c13 = c12(0); /* { dg-message "required from here" } */
  int a = 0;
  c13.operator()<int&>(a); /* { dg-message "required from here" } */
  /* partially instantiated (invalid) */
  auto c20 = lambda_12<30>();  /* { dg-message "required from here" } */
}
