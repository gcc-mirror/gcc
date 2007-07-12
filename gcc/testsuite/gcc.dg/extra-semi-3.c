/* Test diagnostic for extra semicolon outside a function.  Test with
   -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

; /* { dg-error "ISO C does not allow extra ';' outside of a function" } */
