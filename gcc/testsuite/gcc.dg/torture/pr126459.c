/* PR tree-optimization/126459 */
/* { dg-do run } */

#define INT_MAX __INT_MAX__
#define INT_MIN (-(INT_MAX) - 1)

__attribute__((noipa)) int
foo (int x)
{
  return (x != INT_MAX ? x + 1 : INT_MIN) > x;
}

__attribute__((noipa)) int
bar (int x)
{
  return (x != INT_MIN ? x - 1 : INT_MAX) < x;
}

__attribute__((noipa)) int
baz (int x)
{
  return (x != INT_MIN ? x + INT_MIN : 0) < 0;
}

int
main ()
{
  if (foo (INT_MAX) != 0 || foo (1) != 1)
    __builtin_abort ();
  if (bar (INT_MIN) != 0 || bar (2) != 1)
    __builtin_abort ();
  if (baz (INT_MIN) != 0 || baz (3) != 1)
    __builtin_abort ();
}
