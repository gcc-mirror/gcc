/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic -fextended-identifiers" { target c } } */
/* { dg-options "-std=c++11 -pedantic -fextended-identifiers" { target c++ } } */

\u00A8

B\u0300

\u0300 /* { dg-error "not valid at the start of an identifier" } */

A\u0300 /* { dg-warning "not in NFC" } */

\U00010000
\U0001FFFD
\U000E1234
