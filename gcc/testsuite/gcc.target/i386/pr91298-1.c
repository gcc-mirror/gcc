/* PR target/91298 */
/* { dg-do assemble } */
/* { dg-options "-O2 -g -fdollars-in-identifiers" } */
/* { dg-xfail-if "No support for $ in identifiers" { *-*-solaris2.* && { ! gas } } } */

int $a[18];
int *foo (void) { return &$a[0]; }
int *bar (int x) { return &$a[x]; }
int baz (void) { return $a[0]; }
int qux (void) { return $a[4]; }
int $quux (void) { return 1; }
int corge (void) { return $quux (); }
int grault (void) { return $quux () + 1; }
typedef int (*fn) (void);
fn foobar (void) { return $quux; }
