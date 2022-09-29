/* Test C2x noreturn attribute: invalid syntax.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

[[noreturn()]] void a(); /* { dg-error "does not take any arguments" } */

[[__noreturn__(0)]] void b(); /* { dg-error "does not take any arguments|expected" } */

[[_Noreturn("", 123)]] void c(); /* { dg-error "does not take any arguments|expected" } */

[[___Noreturn__("")]] void d(); /* { dg-error "does not take any arguments|expected" } */
