/* Test diagnostic for extra semicolon outside a function.  Test with
   -pedantic.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

; /* { dg-warning "warning: ISO C does not allow extra ';' outside of a function" } */
