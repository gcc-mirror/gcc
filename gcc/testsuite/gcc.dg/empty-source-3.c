/* Test diagnostic for an empty source file.  Test with
   -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

/* { dg-error "error: ISO C forbids an empty source file" "empty" { target *-*-* } 1 } */
