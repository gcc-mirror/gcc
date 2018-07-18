/* Test for implicit int: in C90 only.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

extern foo; /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "type defaults" "C99 implicit int error" { target *-*-* } .-1 } */
bar (void) { } /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "return type defaults" "C99 implicit int error" { target *-*-* } .-1 } */
