/* Test C23 nodiscard attribute: invalid syntax.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

[[nodiscard()]] int a (void); /* { dg-error "parentheses must be omitted if attribute argument list is empty" } */

[[nodiscard(0)]] int b (void); /* { dg-error "expected" } */

[[nodiscard("", 123)]] int c (void); /* { dg-error "expected" } */

[[nodiscard((""))]] int d (void); /* { dg-error "expected" } */
