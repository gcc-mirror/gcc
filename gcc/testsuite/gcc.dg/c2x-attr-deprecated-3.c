/* Test C2x deprecated attribute: invalid syntax.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

[[deprecated()]] int a; /* { dg-error "parentheses must be omitted if attribute argument list is empty" } */

[[deprecated(0)]] int b; /* { dg-error "expected" } */

[[deprecated("", 123)]] int c; /* { dg-error "expected" } */

[[deprecated((""))]] int d; /* { dg-error "expected" } */
