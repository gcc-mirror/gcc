/* PR middle-end/30314 */

#include "../../gcc.dg/tree-ssa/pr30314.c"

int
main ()
{
  if (foo (0) != 0
      || foo (~0U / 35) != 0
      || foo (~0U / 35 + 1) != 1
      || foo (~0U) != 1)
    __builtin_abort ();
  if (bar (0) != 0
      || bar (~0UL / 35) != 0
      || bar (~0UL / 35 + 1) != 1
      || bar (~0UL) != 1)
    __builtin_abort ();
  if (baz (0) != 0
      || baz (~0U / 42) != 0
      || baz (~0U / 42 + 1) != 1
      || baz (~0U) != 1)
    __builtin_abort ();
  if (qux (0) != 0
      || qux (~0UL / 42) != 0
      || qux (~0UL / 42 + 1) != 1
      || qux (~0UL) != 1)
    __builtin_abort ();
  return 0;
}
