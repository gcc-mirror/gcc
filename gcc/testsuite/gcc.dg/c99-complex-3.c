/* Test for _Imaginary: when imaginary types are not implemented, this
   is still a keyword and must give a syntax error if used rather than
   being treated as an identifier.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

float _Imaginary; /* { dg-error "expected" } */
