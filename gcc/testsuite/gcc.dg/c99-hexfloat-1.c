/* Test for hex floating point constants: in C99 only.  Compiler test.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

double d = 0x1.2p2; /* { dg-bogus "radix 16" "bogus C99 hex float error" } */
