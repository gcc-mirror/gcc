/* Test for flexible array members: not permitted in unions.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

union u { int a; char b[]; }; /* { dg-error "error: flexible array member in union" } */
