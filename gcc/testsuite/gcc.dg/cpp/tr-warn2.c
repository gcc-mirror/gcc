/* K+R rejects use of function-like macros in non-function context.
   ANSI C explicitly permits this (the macro is not expanded).

   We should not warn about this during pre-expansion of arguments,
   since traditional preprocessors don't do pre-expansion, and we get
   the warning anyway during the re-scan pass if and only if it is
   appropriate.  */

/* { dg-do preprocess } */
/* { dg-options -Wtraditional } */

#define f(x) x
#define g(x) x / 2
#define h(a, b) a(b)
f(g) (3)	    /* { dg-bogus "must be used with arguments" } */
f 2		    /* { dg-warning "must be used with arguments" } */
f(g) 3		    /* { dg-warning "must be used with arguments" } */
h(f, 3)		    /* { dg-bogus "must be used with arguments" } */
