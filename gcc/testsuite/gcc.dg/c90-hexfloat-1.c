/* Test for hex floating point constants: in C99 only.  Compiler test.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

double d = 0x1.2p2; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "radix 16" "hex float error" { target *-*-* } 6 } */
