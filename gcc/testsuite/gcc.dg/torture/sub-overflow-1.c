/* { dg-do compile } */
/* PR tree-optimization/128143 */

int a(int c, signed char x, long long y)
{
  int r;
  if (c)
    r = __builtin_sub_overflow_p(y, x, (short) 0);
  else
    r = __builtin_sub_overflow_p(y, -40000, (short) 0);
  return r;
}

