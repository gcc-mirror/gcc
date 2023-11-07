/* Test C23 maybe_unused attribute: invalid syntax.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

[[maybe_unused()]] int a; /* { dg-error "does not take any arguments" } */

[[maybe_unused(0)]] int b; /* { dg-error "does not take any arguments|expected" } */

[[maybe_unused("", 123)]] int c; /* { dg-error "does not take any arguments|expected" } */

[[maybe_unused("")]] int d; /* { dg-error "does not take any arguments|expected" } */
