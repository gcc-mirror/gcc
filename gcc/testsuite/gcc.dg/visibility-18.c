/* Test that external variable whose address is taken are marked. */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-darwin* } } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-hidden "foo" { xfail *-*-aix* } } } */

extern int foo __attribute__ ((visibility ("hidden")));
int *test = &foo;
