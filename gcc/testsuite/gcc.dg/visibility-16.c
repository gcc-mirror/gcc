/* Test that accessed external variables are marked. */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-darwin* } { "*" } { "" } } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-hidden "foo" } } */

extern int foo __attribute__ ((visibility ("hidden")));
int f () {
  return foo;
}
