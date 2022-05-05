/* PR rtl-optimization/102478 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-if-conversion -Wno-div-by-zero" } */

unsigned a, b, c;

void
foo (void)
{
  c |= __builtin_expect (65535 / a, 0) && 0 / 0;
  b = 0;
}

void
bar (void)
{
  if (a <= 65535)
    __builtin_trap ();
  b = 0;
}

void
baz (void)
{
  if (a > 65535)
    b = 0;
  else
    __builtin_trap ();
}
