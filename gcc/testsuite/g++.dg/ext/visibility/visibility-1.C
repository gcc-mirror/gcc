/* Test visibility attribute on function definition. */
/* { dg-require-visibility "" }
/* { dg-final { scan-hidden "_Z3foov" } } */

void
__attribute__((visibility ("hidden")))
foo()
{ }
