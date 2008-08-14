/* Test that external variable whose address is taken are marked. */
/* { dg-do compile } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-hidden "foo" } } */

extern int foo __attribute__ ((visibility ("hidden")));
int *test = &foo;
