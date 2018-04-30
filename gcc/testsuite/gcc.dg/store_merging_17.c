/* PR tree-optimization/83241 */
/* { dg-do compile { target store_merge } } */
/* { dg-options "-O2" } */

struct S { int a; short b[32]; } e;
struct T { volatile int c; int d; } f;

void
foo ()
{
  struct T g = f;
  e.b[0] = 6;
  e.b[1] = 6;
  e.b[4] = g.d;
  e.b[5] = g.d >> 16;
  e.a = 1;
}
