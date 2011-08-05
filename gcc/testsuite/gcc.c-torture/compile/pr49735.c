/* { dg-require-alias "" } */
void bar (void);
static void foo (void) { bar (); }
void bar (void) __attribute__((alias("foo")));
