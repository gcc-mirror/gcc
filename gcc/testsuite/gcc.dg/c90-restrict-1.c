/* Test for restrict: in C99 only.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

char *restrict foo; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "(parse|syntax) error|no type" "restrict not in C90" { target *-*-* } 6 } */
