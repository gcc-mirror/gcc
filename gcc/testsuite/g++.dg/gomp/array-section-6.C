// { dg-do compile { target c++26 } }

// See 'array-section-5.C' for C++23 and lower
// Same test but slightly different error messages.


// Check that OpenMP array sections with [: and :] do not get mixed up with C++26's splice specifier,
// used for reflection.
//
// See also OpenMP spec Issue 4740.


// The following checks that an additional ':' is diagnosed.
//
// NOTE: When STRIDES are permitted (e.g. for target update) that's actually
// valid but not yet implemented (at the time this test was written) - and
// it is still not valid for 'target enter data' or affinity - as used here.

void f() {
 char a1[4], a2[4], a3[4], a4[5], a5[5], b;
 char c1[4], c2[4], c3[4], c4[4], c5[4];

 #pragma omp target enter data map(b) map(to: a1[::])   // { dg-error "52: expected id-expression before '\\\]' token" }
 #pragma omp target enter data map(b) map(to: a2[: :])  // { dg-error "51: expected '\\\]' before ':\\\]' token" }
 #pragma omp target enter data map(b) map(to: a3[ ::])  // { dg-error "53: expected id-expression before '\\\]' token" }
 #pragma omp target enter data map(b) map(to: a4[:: ])  // { dg-error "53: expected id-expression before '\\\]' token" }
 #pragma omp target enter data map(b) map(to: a5[ :: ]) // { dg-error "54: expected id-expression before '\\\]' token" }

 #pragma omp target enter data map(b) map(to: a1[1::])
 // { dg-error "51: expected '\\\]' before '::' token" "" { target *-*-* } .-1 }
 // { dg-error "51: expected '\\)' before '::' token" "" { target *-*-* } .-2 }
 // { dg-error "53: expected an OpenMP clause before '\\\]' token" "" { target *-*-* } .-3 }

 #pragma omp target enter data map(b) map(to: a2[1:: ])
 // { dg-error "51: expected '\\\]' before '::' token" "" { target *-*-* } .-1 }
 // { dg-error "51: expected '\\)' before '::' token" "" { target *-*-* } .-2 }
 // { dg-error "54: expected an OpenMP clause before '\\\]' token" "" { target *-*-* } .-3 }

 #pragma omp target enter data map(b) map(to: a3[:1:])
 // { dg-error "52: expected '\\\]' before ':\\\]' token" "" { target *-*-* } .-1 }
 // { dg-error "52: expected '\\)' before ':\\\]' token" "" { target *-*-* } .-2 }

 #pragma omp target enter data map(b) map(to: a4[ :1: ])
 // { dg-error "53: expected '\\\]' before ':' token" "" { target *-*-* } .-1 }
 // { dg-error "53: expected '\\)' before ':' token" "" { target *-*-* } .-2 }
 // { dg-error "55: expected an OpenMP clause before '\\\]' token" "" { target *-*-* } .-3 }

 #pragma omp target enter data map(b) map(to: a5[ :1:])
 // { dg-error "53: expected '\\\]' before ':\\\]' token" "" { target *-*-* } .-1 }
 // { dg-error "53: expected '\\)' before ':\\\]' token" "" { target *-*-* } .-2 }

 #pragma omp task affinity(c1[::])   // { dg-error "28: expected '\\)' before 'c1'" }
  ;
 #pragma omp task affinity(c2[: :])  // { dg-error "30: expected '\\)' before '\\\[:' token" }
  ;
 #pragma omp task affinity(c3[ ::])  // { dg-error "28: expected '\\)' before 'c3'" }
  ;
 #pragma omp task affinity(c4[:: ])  // { dg-error "28: expected '\\)' before 'c4'" }
  ;
 #pragma omp task affinity(c5[ :: ]) // { dg-error "28: expected '\\)' before 'c5'" }
  ;

 #pragma omp task affinity(c1[1::])
  ;
 // { dg-error "32: expected '\\\]' before '::' token" "" { target *-*-* } .-2 }
 // { dg-error "32: expected '\\)' before '::' token" "" { target *-*-* } .-3 }
 // { dg-error "34: expected an OpenMP clause before '\\\]' token" "" { target *-*-* } .-4 }

 #pragma omp task affinity(c2[1:: ])
  ;
 // { dg-error "32: expected '\\\]' before '::' token" "" { target *-*-* } .-2 }
 // { dg-error "32: expected '\\)' before '::' token" "" { target *-*-* } .-3 }
 // { dg-error "35: expected an OpenMP clause before '\\\]' token" "" { target *-*-* } .-4 }

 #pragma omp task affinity(c3[:1:])  // { dg-error "33: expected '\\\]' before ':\\\]' token" }
  ;

 #pragma omp task affinity(c4[ :1: ])
  ;
 // { dg-error "34: expected '\\\]' before ':' token" "" { target *-*-* } .-2 }
 // { dg-error "36: expected an OpenMP clause before '\\\]' token" "" { target *-*-* } .-3 }

 #pragma omp task affinity(c5[ :1:])  // { dg-error "34: expected '\\\]' before ':\\\]' token" }
  ;
}
