/* Test for long long: in C99 only.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

long long foo; /* { dg-bogus "long long" "bogus long long error" } */
