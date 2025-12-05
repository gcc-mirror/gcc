/* { dg-do compile }  */

void f()
{
#if !defined(__cplusplus) || __cplusplus >= 201103L
  constexpr int M = 1024; // C20 + C++11
#endif
  int N, A[1];
  N = 1024;

  #pragma omp target dyn_groupprivate(0)
    ;

  #pragma omp target dyn_groupprivate(0) dyn_groupprivate(0)  // { dg-error "too many 'dyn_groupprivate' clauses" }
    ;

  #pragma omp target dyn_groupprivate(1,)  // { dg-error "expected '\\)' before ',' token" }
    ;

  #pragma omp target dyn_groupprivate(-123)  // { dg-warning "'dyn_groupprivate' value must be non-negative \\\[-Wopenmp\\\]" }
    ;

#if !defined(__cplusplus) || __cplusplus >= 201103L
  #pragma omp target dyn_groupprivate (0 * M - 1)  // { dg-warning "'dyn_groupprivate' value must be non-negative \\\[-Wopenmp\\\]" "" { target { ! c++98_only } } }
#endif
    ;

  #pragma omp target dyn_groupprivate (- 4)  // { dg-warning "'dyn_groupprivate' value must be non-negative \\\[-Wopenmp\\\]" }
    ;

  #pragma omp target dyn_groupprivate ( fallback ( other ) : N)  // { dg-error "expected 'abort', 'default_mem', or 'null' as fallback mode before 'other'" }
    // { dg-error "expected an OpenMP clause before ':' token" "" { target c++ } .-1 }
    ;

  #pragma omp target dyn_groupprivate ( A )
  // { dg-error "expected integer expression" "" { target c } .-1 }
  // { dg-error "'dyn_groupprivate' expression must be integral" "" { target c++ } .-2 }
    ;

  #pragma omp target dyn_groupprivate ( 1024. )
  // { dg-error "expected integer expression" "" { target c } .-1 }
  // { dg-error "'dyn_groupprivate' expression must be integral" "" { target c++ } .-2 }
    ;

  #pragma omp target dyn_groupprivate ( foo ( 4 ) : 10 )  // { dg-error "expected 'fallback' modifier before 'foo'" }
    ;

  #pragma omp target dyn_groupprivate ( foo2 (  ) : 10 )  // { dg-error "expected 'fallback' modifier before 'foo2'" }
    ;

  #pragma omp target dyn_groupprivate ( fallback (  ) : 10 )  // { dg-error "expected 'abort', 'default_mem', or 'null' as fallback mode before '\\)'" }
    // { dg-error "expected an OpenMP clause before ':' token" "" { target c++ } .-1 }
    ;

  #pragma omp target dyn_groupprivate ( bar : 10 )  // { dg-error "expected 'fallback' modifier before 'bar'" }
    ;

  #pragma omp target dyn_groupprivate ( fallback : 10 )  // { dg-error "expected '\\(' before ':' token" }
    // { dg-error "expected an OpenMP clause before ':' token" "" { target c++ } .-1 }
    ;

  #pragma omp target dyn_groupprivate ( fallback ( null,) : 10 )  // { dg-error "expected '\\)' before ',' token" }
    // { dg-error "expected an OpenMP clause before '\\)' token" "" { target c++ } .-1 }
    ;
}

// { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" "" { target *-*-* } 11 }
// { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" "" { target *-*-* } 14 }
// { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" "" { target *-*-* } 17 }
// { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" "" { target *-*-* } 20 }
// { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" "" { target { ! c++98_only } } 24 }
// { dg-message "sorry, unimplemented: 'dyn_groupprivate' clause" "" { target *-*-* } 28 }
