/* Test visibility attribute on function definition. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-hidden "foo" } } */

void
__attribute__((visibility ("hidden")))
foo()
{ }
