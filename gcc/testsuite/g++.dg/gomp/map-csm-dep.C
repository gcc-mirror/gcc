// can't manage to elicit the "illegal comma" but it seems that we're already getting errors for improper syntax.
// otherwise, though, the deprecation works as intended.
int
main()
  {
    float B[8];
    float C[8];
    // this should be completely fine
    #pragma omp target teams map(to: B[0:8], C[0:8])
    {}
    // this should give us a deprecation warning
    #pragma omp target teams map(close always present to: B[0:8], C[0:8]) // { dg-warning "'map' clause modifiers without comma separation is deprecated since OpenMP 5.2 \\\[-Wdeprecated-openmp\\\]" }
    {}
    // this should be completely fine
    #pragma omp target teams map(close, always, present, to: B[0:8], C[0:8])
    {}

    #pragma omp target teams map(close always,, present, to: B[0:8], C[0:8])
    {}
    // { dg-error "'close' was not declared in this scope" "" { target *-*-* } 18 }
    // { dg-error "expected '\\)' before 'always'" "" { target *-*-* } 18 }
    // { dg-error "expected primary-expression before ',' token" "" { target *-*-* } 18 }
    // { dg-error "'present' was not declared in this scope" "" { target *-*-* } 18 }
    // { dg-error "found ':' in nested-name-specifier, expected '::'" "" { target *-*-* } 18 }
    // { dg-error "'to' has not been declared; did you mean 'auto'?" "" { target *-*-* } 18 }

    #pragma omp target teams map(,close to: B[0:8], C[0:8]) 
    {}
    // { dg-error "expected primary-expression before ',' token" "" { target *-*-* } 27 }
    // { dg-error "'close' was not declared in this scope" "" { target *-*-* } 27 }
    // { dg-error "expected '\\)' before 'to'" "" { target *-*-* } 27 }
    
    #pragma omp target teams map(close,,always to: B[0:8] C[0:8])
    {}
    // { dg-error "expected primary-expression before ',' token" "" { target *-*-* } 33 }
    // { dg-error "'close' was not declared in this scope" "" { target *-*-* } 33 }
    // { dg-error "'always' was not declared in this scope" "" { target *-*-* } 33 }
    // { dg-error "expected '\\)' before 'to'" "" { target *-*-* } 33 }
    return 0;
  }
