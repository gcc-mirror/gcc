/* Test for _Complex: in C99 only.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

_Complex double foo; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "C" "_Complex not in C90" { target *-*-* } 6 } */
