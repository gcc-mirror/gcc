/* Test C2x (= Unicode) rules for characters in identifiers.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c2x -pedantic-errors" } */

¨

/* The requirement for NFC only applies in identifiers, not pp-numbers.  */

À /* { dg-error "not in NFC" } */
ÿÀ /* { dg-error "not in NFC" } */

0À /* { dg-warning "not in NFC" } */
.1À /* { dg-warning "not in NFC" } */
