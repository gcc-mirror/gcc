/* Test for hex floating point constants: in C99 only.  Compiler test.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

double d = 0x1.2p2; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "hexadecimal floating" "hex float error" { target *-*-* } 6 } */
double d1 = 0x1p2; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "hexadecimal floating" "hex float error" { target *-*-* } 8 } */
double d2 = 0x1...p2; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "too many decimal points" "bad hex float" { target *-*-* } 10 } */
