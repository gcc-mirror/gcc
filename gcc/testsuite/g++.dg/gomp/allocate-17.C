/* { dg-do compile { target c++17 } } */

/* Nested lambdas.  */

#include "allocate-allocator-handle.h"

template<int Align>
auto lambda_0_valid()
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
auto lambda_0_bad_align()
{
  return [](auto p0){
    return [](auto p1){
      return [](auto p2){
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b;
	  #pragma omp allocate(a) align(Align) allocator(p2)
	  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } .-1 } */
	  return a;
	};
      };
    };
  };
}

template<int Align>
auto lambda_0_ref_in_directive()
{
  return [](auto p0){
    return [](auto p1){
      return [](auto p2){
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b; /* { dg-message "'a' declared here" } */
	  #pragma omp allocate(a) align(Align) allocator(p2)
	  /* { dg-error "variables with reference type may not appear in an 'allocate' directive" "" { target *-*-* } .-1 } */
	  return a;
	};
      };
    };
  };
}


template<int Align>
auto lambda_0_invalid_allocator()
{
  return [](auto p0){
    return [](auto p1){
      return [](auto p2){
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b;
	  #pragma omp allocate(a) align(Align) allocator(p2)
	  /* { dg-error "invalid conversion from 'int' to 'omp_allocator_handle_t'" "" { target *-*-* } .-1 } */
	  return a;
	};
      };
    };
  };
}

template<int Align>
auto lambda_0_all()
{
  return [](auto p0){
    return [](auto p1){
      return [](auto p2){
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b; /* { dg-message "'a' declared here" } */
	  #pragma omp allocate(a) align(Align) allocator(p2)
	  /* { dg-error "variables with reference type may not appear in an 'allocate' directive" "" { target *-*-* } .-1 } */
	  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } .-2 } */
	  /* { dg-error "invalid conversion from 'int' to 'omp_allocator_handle_t'" "" { target *-*-* } .-3 } */
	  return a;
	};
      };
    };
  };
}

template<int Align>
auto lambda_0_wrong_align_partially_instantiated()
{
  return [](auto p0){
    return [](auto p1){
      return [](auto p2){
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b;
	  #pragma omp allocate(a) align(Align) allocator(p2)
	  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } .-1 } */
	  return a;
	};
      };
    };
  };
}

template<int Align>
auto lambda_0_invalid_allocator_partially_instantiated()
{
  return [](auto p0){
    return [](auto p1){
      return [](auto p2){
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b;
	  #pragma omp allocate(a) align(Align) allocator(p2)
	  /* { dg-error "invalid conversion from 'int' to 'omp_allocator_handle_t'" "" { xfail *-*-* } .-1 } */
	  return a;
	};
      };
    };
  };
}

void instantiate_lambdas_0()
{
  {
    auto c0 = lambda_0_valid<32>();
    auto c1 = c0(0);
    auto c2 = c1(0);
    auto c3 = c2(omp_default_mem_alloc);
    c3(0);
  }
  {
    auto c0 = lambda_0_bad_align<30>(); /* { dg-message "required from here" "" { xfail *-*-* } } */
    auto c1 = c0(0); /* { dg-bogus "required from here" } */
    auto c2 = c1(0); /* { dg-bogus "required from here" } */
    auto c3 = c2(omp_default_mem_alloc); /* { dg-bogus "required from here" } */
    c3(0); /* { dg-bogus "required from here"  "" { xfail *-*-* } } */
  }
  {
    auto c0 = lambda_0_ref_in_directive<32>();
    auto c1 = c0(0);
    auto c2 = c1(0);
    auto c3 = c2(omp_default_mem_alloc);
    int a = 0;
    c3.operator()<int&>(a); /* { dg-message "required from here" } */
  }
  {
    auto c0 = lambda_0_invalid_allocator<32>();
    auto c1 = c0(0);
    auto c2 = c1(0);
    auto c3 = c2(0); /* { dg-message "required from here" "" { xfail *-*-* } } */
    c3(0); /* { dg-bogus "required from here" "" { xfail *-*-* } } */
  }
  {
    auto c0 = lambda_0_all<30>(); /* { dg-message "required from here" "" { xfail *-*-* } } */
    auto c1 = c0(0); /* { dg-bogus "required from here" } */
    auto c2 = c1(0); /* { dg-bogus "required from here" } */
    auto c3 = c2(0); /* { dg-message "required from here" "" { xfail *-*-* } } */
    int a = 0;
    c3.operator()<int&>(a); /* { dg-message "required from here" } */
  }
  {
    auto c = lambda_0_wrong_align_partially_instantiated<30>(); /* { dg-message "required from here" "" { xfail *-*-* } } */
  }
  {
    auto c0 = lambda_0_invalid_allocator_partially_instantiated<32>();
    auto c1 = c0(0);
    auto c2 = c1(0);
    auto c3 = c2(0); /* { dg-message "required from here" "" { xfail *-*-* } } */
  }
}



template<int Align>
auto lambda_1_valid()
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
auto lambda_1_bad_align()
{
  return [](auto p0){
    int a = 42;
    #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } } */
    return [](auto p1){
      int a = 42;
      #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } } */
      return [](auto p2){
	int a = 42;
	#pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } } */
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b;
	  #pragma omp allocate(a) align(Align) allocator(p2)
	  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } .-1 } */
	  return a;
	};
      };
    };
  };
}

template<int Align>
auto lambda_1_ref_in_directive()
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
	  decltype(p3) a = b; /* { dg-message "'a' declared here" } */
	  #pragma omp allocate(a) align(Align) allocator(p2)
	  /* { dg-error "variables with reference type may not appear in an 'allocate' directive" "" { target *-*-* } .-1 } */
	  return a;
	};
      };
    };
  };
}

template<int Align>
auto lambda_1_invalid_allocator()
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
	  /* { dg-error "invalid conversion from 'int' to 'omp_allocator_handle_t'" "" { target *-*-* } .-1 } */
	  return a;
	};
      };
    };
  };
}

template<int Align>
auto lambda_1_all()
{
  return [](auto p0){
    int a = 42;
    #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } } */
    return [](auto p1){
      int a = 42;
      #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } } */
      return [](auto p2){
	int a = 42;
	#pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } } */
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b; /* { dg-message "'a' declared here" } */
	  #pragma omp allocate(a) align(Align) allocator(p2)
	  /* { dg-error "variables with reference type may not appear in an 'allocate' directive" "" { target *-*-* } .-1 } */
	  /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } .-2 } */
	  /* { dg-error "invalid conversion from 'int' to 'omp_allocator_handle_t'" "" { target *-*-* } .-3 } */
	  return a;
	};
      };
    };
  };
}

template<int Align>
auto lambda_1_wrong_align_partially_instantiated()
{
  return [](auto p0){
    int a = 42;
    #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } } */
    return [](auto p1){
      int a = 42;
      #pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } } */
      return [](auto p2){
	int a = 42;
	#pragma omp allocate(a) align(Align) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } } */
	return [p2](auto p3){
	  int b = 42;
	  decltype(p3) a = b;
	  #pragma omp allocate(a) align(Align) allocator(p2) /* { dg-error "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } } */
	  return a;
	};
      };
    };
  };
}

template<int Align>
auto lambda_1_invalid_allocator_partially_instantiated()
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
	  /* { dg-error "invalid conversion from 'int' to 'omp_allocator_handle_t'" "" { xfail *-*-* } .-1 } */
	  return a;
	};
      };
    };
  };
}

void instantiate_lambdas_1()
{
  {
    auto c0 = lambda_1_valid<32>();
    auto c1 = c0(0);
    auto c2 = c1(0);
    auto c3 = c2(omp_default_mem_alloc);
    c3(0);
  }
  {
    auto c0 = lambda_1_bad_align<30>(); /* { dg-message "required from here" "" { xfail *-*-* } } */
    auto c1 = c0(0); /* { dg-bogus "required from here" "" { xfail *-*-* } } */
    auto c2 = c1(0); /* { dg-bogus "required from here" "" { xfail *-*-* } } */
    auto c3 = c2(omp_default_mem_alloc); /* { dg-bogus "required from here" "" { xfail *-*-* } } */
    c3(0); /* { dg-bogus "required from here" "" { xfail *-*-* } } */
  }
  {
    auto c0 = lambda_1_ref_in_directive<32>();
    auto c1 = c0(0);
    auto c2 = c1(0);
    auto c3 = c2(omp_default_mem_alloc);
    int a = 0;
    c3.operator()<int&>(a); /* { dg-message "required from here" } */
  }
  {
    auto c0 = lambda_1_invalid_allocator<32>();
    auto c1 = c0(0);
    auto c2 = c1(0);
    auto c3 = c2(0); /* { dg-message "required from here" "" { xfail *-*-* } } */
    c3(0); /* { dg-bogus "required from here" "" { xfail *-*-* } } */
  }
  {
    auto c0 = lambda_1_all<30>(); /* { dg-message "required from here" "" { xfail *-*-* } } */
    auto c1 = c0(0); /* { dg-bogus "required from here" "" { xfail *-*-* } } */
    auto c2 = c1(0); /* { dg-bogus "required from here" "" { xfail *-*-* } } */
    auto c3 = c2(0); /* { dg-message "required from here" } */
    int a = 0;
    c3.operator()<int&>(a); /* { dg-message "required from here" } */
  }
  {
    auto c = lambda_1_wrong_align_partially_instantiated<30>();  /* { dg-message "required from here" "" { xfail *-*-* } } */
  }
  {
    auto c0 = lambda_1_invalid_allocator_partially_instantiated<32>();
    auto c1 = c0(0);
    auto c2 = c1(0);
    auto c3 = c2(0); /* { dg-message "required from here" "" { xfail *-*-* } } */
  }
}



template<typename T>
auto lambda_2_0_valid()
{
  return [](auto){
    return [](auto){
      return [](auto){
	return [](auto){
	  int b = 42;
	  T a = b;
	  #pragma omp allocate(a)
	  return a;
	};
      };
    };
  };
}

template<typename T>
auto lambda_2_0_invalid()
{
  return [](auto){
    return [](auto){
      return [](auto){
	return [](auto){
	  int b = 42;
	  T a = b; /* { dg-message "'a' declared here" } */
	  #pragma omp allocate(a)
	  /* { dg-error "variables with reference type may not appear in an 'allocate' directive" "" { target *-*-* } .-1 } */
	  return a;
	};
      };
    };
  };
}

template<typename T>
auto lambda_2_0_invalid_partially_instantiated()
{
  return [](auto){
    return [](auto){
      return [](auto){
	return [](auto){
	  int b = 42;
	  T a = b; /* { dg-message "'a' declared here" } */
	  #pragma omp allocate(a)
	  /* { dg-error "variables with reference type may not appear in an 'allocate' directive" "" { target *-*-* } .-1 } */
	  return a;
	};
      };
    };
  };
}

template<typename>
auto lambda_2_1_valid()
{
  return [](auto){
    return [](auto){
      return [](auto p2){
	using type = decltype(p2);
	return [](auto){
	  int b = 42;
	  type a = b;
	  #pragma omp allocate(a)
	  return a;
	};
      };
    };
  };
}

template<typename>
auto lambda_2_1_invalid()
{
  return [](auto){
    return [](auto){
      return [](auto p2){
	using type = decltype(p2);
	return [](auto){
	  int b = 42;
	  type a = b; /* { dg-message "'a' declared here" } */
	  #pragma omp allocate(a)
	  /* { dg-error "variables with reference type may not appear in an 'allocate' directive" "" { target *-*-* } .-1 } */
	  return a;
	};
      };
    };
  };
}

template<typename>
auto lambda_2_1_invalid_partially_instantiated()
{
  return [](auto){
    return [](auto){
      return [](auto p2){
	using type = decltype(p2);
	return [](auto){
	  int b = 42;
	  type a = b; /* { dg-message "'a' declared here" } */
	  #pragma omp allocate(a)
	  /* { dg-error "variables with reference type may not appear in an 'allocate' directive" "" { target *-*-* } .-1 } */
	  return a;
	};
      };
    };
  };
}

void instantiate_lambdas_2()
{
  {
    auto c0 = lambda_2_0_valid<int>();
    auto c1 = c0(0);
    auto c2 = c1(0);
    auto c3 = c2(0);
    c3(0);
  }
  {
    auto c0 = lambda_2_0_invalid<int&>(); /* { dg-message "required from here" } */
    auto c1 = c0(0); /* { dg-bogus "required from here" } */
    auto c2 = c1(0); /* { dg-bogus "required from here" } */
    auto c3 = c2(0); /* { dg-bogus "required from here" } */
    c3(0); /* { dg-bogus "required from here" } */
  }
  {
    auto c0 = lambda_2_0_invalid_partially_instantiated<int&>(); /* { dg-message "required from here" } */
  }
  {
    auto c0 = lambda_2_1_valid<void>();
    auto c1 = c0(0);
    auto c2 = c1(0);
    auto c3 = c2(0);
    c3(0);
  }
  {
    auto c0 = lambda_2_1_invalid<void>();
    auto c1 = c0(0);
    auto c2 = c1(0);
    int a = 0;
    auto c3 = c2.operator()<int&>(a); /* { dg-message "required from here" } */
    c3(0); /* { dg-bogus "required from here" } */
  }
  {
    auto c0 = lambda_2_1_invalid_partially_instantiated<void>();
    auto c1 = c0(0);
    auto c2 = c1(0);
    int a = 0;
    auto c3 = c2.operator()<int&>(a); /* { dg-message "required from here" } */
  }
}

/* This is fixed by the later location wrapping patch.  */
/* { dg-bogus "'align' clause argument needs to be positive constant power of two integer expression" "" { xfail *-*-* } 0 } */
