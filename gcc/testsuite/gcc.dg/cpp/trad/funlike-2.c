/* Test that nested commas and parentheses in macro arguments are
   OK.  */

/* { dg-do preprocess } */

#define f(x) x
#define g(x, y) x y

#if f((1)) != 1
# error		/* { dg-bogus "error" "nested parens 1" } */
#endif

#if f((1, 2)) != 2
# error		/* { dg-bogus "error" "nested comma 1" } */
#endif

#if g(, (1)) != 1
# error		/* { dg-bogus "error" "nested parens 2" } */
#endif

#if g((1, 2), + 3) != 5
# error		/* { dg-bogus "error" "nested comma 2" } */
#endif
