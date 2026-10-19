/* { dg-do compile }  */

// PR middle-end/127723

typedef struct S {} S;
void f(int n, int *a, double *d) {
  #pragma omp simd simdlen(scaled(int,1))
    // { dg-error "implicit declaration of function 'scaled' \\\[-Wimplicit-function-declaration\\\]" "" { target c } .-1 }
    // { dg-error "expected expression before 'int'" "" { target c } .-2 }
    // { dg-error "'simdlen' clause expression must be positive constant integer expression" "" { target c } .-3 }
    // { dg-error "expected primary-expression before 'int'" "" { target c++ } .-4 }
    // { dg-error "'scaled' was not declared in this scope" "" { target c++11 } .-5 }
    // { dg-error "a function call cannot appear in a constant-expression" "" { target c++98_only } .-6 }
  for (int i = 0; i < n; i++)
    a[i] += 5;
  #pragma omp simd simdlen(scaled(int + 3): 2)
    // { dg-error "expected '\\)' before '\\+' token" "" { target *-*-* } .-1 }
    // { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" "" { target c } .-2 }
    // { dg-error "expected ':' before '\\+' token" "" { target c++ } .-3 }
    // { dg-error "expected an OpenMP clause before ':' token" "" { target c++ } .-4 }
  for (int i = 0; i < n; i++)
    a[i] += 5;

  #pragma omp simd simdlen(scaled(4): 8)
    // { dg-error "expected specifier-qualifier-list before numeric constant" "" { target c } .-1 }
    // { dg-error "expected arithmetic type name as argument to the 'scaled' modifier" "" { target c } .-2 }
    // { dg-error "expected '\\)' before numeric constant" "" { target c } .-3 }
    // { dg-error "expected type-specifier before numeric constant" "" { target c++ } .-4 }
  for (int i = 0; i < n; i++)
    d[i] = 0.0;

  #pragma omp simd simdlen(scaled(S): 8)
    // { dg-error "expected arithmetic type name as argument to the 'scaled' modifier" "" { target *-*-* } .-1 }
    // { dg-error "expected '\\)' before numeric constant" "" { target c } .-2 }
  for (int i = 0; i < n; i++)
    d[i] = 0.0;

  #pragma omp simd simdlen(scaled(float, n): 8)
    // { dg-error "divisor expression of the 'scaled' modifier must a be positive constant integer expression" "" { target c } .-1 }
    // { dg-error "expected '\\)' before numeric constant" "" { target c } .-2 }
    // { dg-error "'n' is not a constant expression" "" { target c++11 } .-3 }
    // { dg-error "'n' cannot appear in a constant-expression" "" { target c++98_only } .-4 }
    // { dg-error "'simdlen' divisor expression of the 'scaled' modifier must be a positive constant integer expression" "" { target c++11 } .-5 }
  for (int i = 0; i < n; i++)
    d[i] = 0.0;

  #pragma omp simd simdlen(scaled(bool, 0): 8)
    // { dg-error "divisor expression of the 'scaled' modifier must a be positive constant integer expression" "" { target c } .-1 }
    // { dg-error "expected '\\)' before numeric constant" "" { target c } .-2 }
    // { dg-error "'simdlen' divisor expression of the 'scaled' modifier must be a positive constant integer expression" "" { target c++ } .-3 }
  for (int i = 0; i < n; i++)
    d[i] = 0.0;

  #pragma omp simd simdlen(scaled(bool, 0, 2): 8)
    // { dg-error "divisor expression of the 'scaled' modifier must a be positive constant integer expression" "" { target c } .-1 }
    // { dg-error "expected '\\)' before ',' token" "" { target c } .-2 }
    // { dg-error "expected '\\)' before numeric constant" "" { target c } .-3 }
    // { dg-error "expected '\\)' before ',' token" "" { target c++ } .-4 }
    // { dg-error "expected ':' before ',' token" "" { target c++ } .-5 }
    // { dg-error "expected primary-expression before ',' token" "" { target c++ } .-6 }
    // { dg-error "expected an OpenMP clause before ':' token" "" { target c++ } .-7 }
    // { dg-error "'simdlen' divisor expression of the 'scaled' modifier must be a positive constant integer expression" "" { target c++ } .-8 }
  for (int i = 0; i < n; i++)
    d[i] = 0.0;
}
