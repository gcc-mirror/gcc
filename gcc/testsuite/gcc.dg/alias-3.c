/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-options "" } */

extern int foo();

int baz () { return foo(); }

static inline int bar () __attribute__ ((alias ("foo"))); /* { dg-error "aliased to" } */

int main () { return bar (); }
