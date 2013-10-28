/* Test C11 static assertions.  Non-constant-expression with -pedantic.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic" } */

_Static_assert ((int)(1.0 + 1.0), "non-constant-expression"); /* { dg-warning "not an integer constant expression" } */
