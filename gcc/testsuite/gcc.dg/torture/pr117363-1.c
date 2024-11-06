/* { dg-do run } */
/* { dg-additional-options "-fgimple" } */

/* PR tree-optimization/117363 */

/* a != 0 ? (signed)(((unsigned)a) - 1) : 0
   Should not be transformed into doing the
   plus in a signed type which could cause an overflow.` */

__attribute__((noinline))
signed __GIMPLE ()
test2 (int n)
{
  unsigned t;
  _Bool _4;
  if (n_1(D) > 0) goto unreachable;
  else goto normal;
normal:
  t_2 = (unsigned)n_1(D);
  t_3 = t_2 - 1u;
  n_5 = (signed) t_3;
  _4 = n_1(D) != 0;
  n_6 = _4 ? n_5 : 0;
  if (n_6 > 0) goto return1;
  else goto trap;

return1:
  return n_6;

unreachable:
  __builtin_unreachable();

trap:
  __builtin_trap ();
}

int main()
{
  unsigned t = -__INT_MAX__ - 1;
  test2(t);
}
