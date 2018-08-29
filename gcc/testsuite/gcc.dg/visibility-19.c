/* Test that accessed external functions are marked. */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-darwin* } } */
/* { dg-require-visibility "" } */
/* { dg-final { scan-hidden "foo" { xfail *-*-aix* } } } */

extern void foo(void) __attribute__ ((visibility ("hidden")));
typedef void (*foo_t)(void);
foo_t test = foo;
