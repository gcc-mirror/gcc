/* PR debug/78587 */
/* { dg-do compile } */
/* { dg-additional-options "-w" } */

extern void bar (void);

void
foo (long long x)
{
  x ^= 9223372036854775808ULL;
  bar ();
}

struct S { int w[4]; } a[1], b;

void
baz ()
{
  int e = (int) baz;
  if (e <= -80)
    e = 0;
  b = a[e];
}
