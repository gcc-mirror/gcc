/* Test for implicit int: in C90 only.  Function parameters in old-style
   function definition.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

void foo (a) { } /* { dg-bogus "warning" "warning in place of error" } */
/* { dg-error "defaults" "C99 implicit int parameter error" { target *-*-* } .-1 } */
