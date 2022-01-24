/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic" { target c } } */
/* { dg-options "-std=c++11 -pedantic" { target c++ } } */

\u00A8 /* { dg-error "is not valid in an identifier" "" { target c++ } } */

B\u0300

\u0300 /* { dg-error "not valid at the start of an identifier" } */

A\u0300 /* { dg-warning "not in NFC" } */

\U00010000
\U0001FFFD	/* { dg-error "is not valid in an identifier" "" { target c++ } } */
\U000E1234	/* { dg-error "is not valid in an identifier" "" { target c++ } } */
