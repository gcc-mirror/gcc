/* PR lto/93384 */
/* { dg-lto-do link } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target shared } */
/* { dg-lto-options { { -O2 -flto -ffat-lto-objects -fpic -fno-semantic-interposition } } } */
/* { dg-extra-ld-options { -shared -flto-partition=none } } */

void bar (void);
__attribute__((noipa)) void quux (int x) { if (x == 5) bar (); }
__attribute__((noipa, noreturn)) void foo (void) { while (1) ; }
__attribute__((noinline)) void bar (void) { asm (""); quux (7); foo (); }
void baz (int x) { if (x) bar (); }
