/* Test for the invalid use of the "nonnull" function attribute.  */
/* Origin: Jason Thorpe <thorpej@wasabisystems.com> */
/* { dg-do compile } */

extern void func1 () __attribute__((nonnull)); /* { dg-error "without arguments" } */

extern void func2 (char *) __attribute__((nonnull(2))); /* { dg-error "out-of-range operand" } */

extern void func3 (char *) __attribute__((nonnull(foo))); /* { dg-error "invalid operand number" } */

extern void func4 (int) __attribute__((nonnull(1))); /* { dg-error "references non-pointer" } */

void
foo (void)
{
}
