/* Test that varargs are rejected, and that we don't complain about
   macro args in skipped blocks.  */

/* { dg-do preprocess } */

#define f(x) 
#define g(x, y...)		/* { dg-error "-:macro parameter list" } */

#if 0
#define f(a,b)			/* { dg-bogus "passed 2 arguments" } */
#endif
