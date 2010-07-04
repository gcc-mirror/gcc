/* Test C1X static assertions.  Non-constant-expression without -pedantic.  */
/* { dg-do compile } */
/* { dg-options "-std=c1x" } */

_Static_assert ((int)(1.0 + 1.0), "non-constant-expression");
