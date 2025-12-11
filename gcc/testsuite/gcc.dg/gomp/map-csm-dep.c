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
    // this should be raising illegal comma.
    // { dg-error "'close' undeclared" "" { target *-*-* } 19 }
    #pragma omp target teams map(close always,, present, to: B[0:8], C[0:8])
    {}
    // { dg-error "expected '\\)' before 'always'" "" { target *-*-* } 19 }


    #pragma omp target teams map(,close to: B[0:8], C[0:8]) // { dg-error "expected expression before ',' token" }
    {}
    // { dg-error "'always' undeclared" "" { target *-*-* } 27 }
    #pragma omp target teams map(close,,always to: B[0:8] C[0:8]) // { dg-error "expected expression before ',' token" }
    {}
    return 0;
  }
