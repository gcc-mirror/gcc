// { dg-additional-options -fopenmp }
// { dg-require-effective-target fopenmp } 
#pragma omp declare simd // { dg-error "not immediately followed by" }

// { dg-error "-:expected unqualified-id" "" { target *-*-* } .+1 }
