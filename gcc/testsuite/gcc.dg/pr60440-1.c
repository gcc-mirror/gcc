/* { dg-do compile } */
/* { dg-options "-Wreturn-type" } */

/* PR c/60440 */
/* Don't warn for a missing return when there was an error
   on the return stamtent. */

int f(int a) {
  return a + b; /* { dg-error "undeclared" } */
} /* { dg-bogus "control reaches end of non-void function" } */
