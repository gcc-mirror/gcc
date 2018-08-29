/* Test that function-like macros are restricted to directives, and
   that unterminated ones are warned about.  */

/* { dg-do preprocess } */

#define f(x) x

#if 2 f(/* { dg-error "-:unterminated" "unterminated macro in directive" } */
)
#endif

f( /* { dg-error "-:unterminated" "unterminated macro" } */
