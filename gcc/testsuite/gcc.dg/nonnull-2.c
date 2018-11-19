/* Test for the invalid use of the "nonnull" function attribute.  */
/* Origin: Jason Thorpe <thorpej@wasabisystems.com> */
/* { dg-do compile } */

extern void func1 () __attribute__((nonnull)); /* { dg-error "without arguments" } */

extern void func2 (char *) __attribute__((nonnull(2))); /* { dg-warning ".nonnull. attribute argument value .2. exceeds the number of function parameters 1" } */

extern void func3 (char *) __attribute__((nonnull (foo))); /* { dg-warning ".nonnull. attribute argument is invalid" } */
/* { dg-error ".foo. undeclared" "undeclared argument" { target *-*-* } .-1 } */

extern void func4 (int) __attribute__((nonnull(1))); /* { dg-warning ".nonnull. attribute argument value .1. refers to parameter type .int." } */

void
foo (void)
{
}
