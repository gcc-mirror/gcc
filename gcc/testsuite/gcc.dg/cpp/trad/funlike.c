/* Test that undefined names evaluate to zero, that macros after a
   funlike macro are expanded, and that if it is a '(' the funlike
   macro is not treated as such.  */

/* { dg-do preprocess } */

#define f(x) x
#define h != 0
#define i
#define paren (

#if f != 0
# error		/* { dg-bogus "error" "undefined name" } */
#endif

#if f h
# error		/* { dg-bogus "error" "h not expanded" } */
#endif

#if f i
# error		/* { dg-bogus "error" "empty macro" } */
#endif

#if f paren 6) /* { dg-error "missing binary" "macro-expanded parenthesis" } */
#endif
