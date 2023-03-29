/* Test C2x (= Unicode) rules for characters in identifiers.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c2x -pedantic-errors" } */

\u00A8 /* { dg-error "is not valid in an identifier" } */

/* The requirement for NFC only applies in identifiers, not pp-numbers.  */

A\u0300 /* { dg-error "not in NFC" } */
\u00ffA\u0300 /* { dg-error "not in NFC" } */

0A\u0300 /* { dg-warning "not in NFC" } */
.1A\u0300 /* { dg-warning "not in NFC" } */
