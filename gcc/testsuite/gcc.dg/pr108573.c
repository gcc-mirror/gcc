/* PR debug/108573 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

unsigned g;

int bar (void);
int baz (int);

void
foo (unsigned short s, long l)
{
  unsigned u = bar ();
  s &= __builtin_add_overflow_p (0, u, 0);
  s %= g;
  s -= l >> s;
  baz (s);
}
