/* PR c/90760 */
/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-require-named-sections "" } */

void bar (void) {}
void foo (void) __attribute__ ((alias ("bar")));	/* { dg-error "section of alias 'foo' must match section of its target" } */
void foo (void) __attribute__ ((section ("baz")));
void qux (void) __attribute__ ((alias ("bar"), section ("baz")));	/* { dg-error "section of alias 'qux' must match section of its target" } */
