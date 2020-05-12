/* Test diagnostic for an empty source file.  Test with
   -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

/* { dg-error "-:ISO C forbids an empty translation unit" "empty" { target *-*-* } .+1 } */
