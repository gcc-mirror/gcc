/* PR middle-end/105777 */

#include "../../gcc.dg/tree-ssa/pr105777.c"

int
main ()
{
  if (foo (0) != 0
      || foo (__INT_MAX__ / 35) != 0
      || foo (__INT_MAX__ / 35 + 1) != 1
      || foo (__INT_MAX__) != 1
      || foo ((-__INT_MAX__ - 1) / 35) != 0
      || foo ((-__INT_MAX__ - 1) / 35 - 1) != 1
      || foo (-__INT_MAX__ - 1) != 1)
    __builtin_abort ();
  if (bar (0) != 0
      || bar (__LONG_MAX__ / 35) != 0
      || bar (__LONG_MAX__ / 35 + 1) != 1
      || bar (__LONG_MAX__) != 1
      || bar ((-__LONG_MAX__ - 1) / 35) != 0
      || bar ((-__LONG_MAX__ - 1) / 35 - 1) != 1
      || bar (-__LONG_MAX__ - 1) != 1)
    __builtin_abort ();
  if (baz (0) != 0
      || baz (__INT_MAX__ / 42) != 0
      || baz (__INT_MAX__ / 42 + 1) != 1
      || baz (__INT_MAX__) != 1
      || baz ((-__INT_MAX__ - 1) / 42) != 0
      || baz ((-__INT_MAX__ - 1) / 42 - 1) != 1
      || baz (-__INT_MAX__ - 1) != 1)
    __builtin_abort ();
  if (qux (0) != 0
      || qux (__LONG_MAX__ / 42) != 0
      || qux (__LONG_MAX__ / 42 + 1) != 1
      || qux (__LONG_MAX__) != 1
      || qux ((-__LONG_MAX__ - 1) / 42) != 0
      || qux ((-__LONG_MAX__ - 1) / 42 - 1) != 1
      || qux (-__LONG_MAX__ - 1) != 1)
    __builtin_abort ();
  if (corge (0) != 0
      || corge (__INT_MAX__ / -39) != 0
      || corge (__INT_MAX__ / -39 - 1) != 1
      || corge (__INT_MAX__) != 1
      || corge ((-__INT_MAX__ - 1) / -39) != 0
      || corge ((-__INT_MAX__ - 1) / -39 + 1) != 1
      || corge (-__INT_MAX__ - 1) != 1)
    __builtin_abort ();
  if (garply (0) != 0
      || garply (__LONG_MAX__ / -39) != 0
      || garply (__LONG_MAX__ / -39 - 1) != 1
      || garply (__LONG_MAX__) != 1
      || garply ((-__LONG_MAX__ - 1) / -39) != 0
      || garply ((-__LONG_MAX__ - 1) / -39 + 1) != 1
      || garply (-__LONG_MAX__ - 1) != 1)
    __builtin_abort ();
  if (grault (0) != 0
      || grault (__INT_MAX__ / -46) != 0
      || grault (__INT_MAX__ / -46 - 1) != 1
      || grault (__INT_MAX__) != 1
      || grault ((-__INT_MAX__ - 1) / -46) != 0
      || grault ((-__INT_MAX__ - 1) / -46 + 1) != 1
      || grault (-__INT_MAX__ - 1) != 1)
    __builtin_abort ();
  if (waldo (0) != 0
      || waldo (__LONG_MAX__ / -46) != 0
      || waldo (__LONG_MAX__ / -46 - 1) != 1
      || waldo (__LONG_MAX__) != 1
      || waldo ((-__LONG_MAX__ - 1) / -46) != 0
      || waldo ((-__LONG_MAX__ - 1) / -46 + 1) != 1
      || waldo (-__LONG_MAX__ - 1) != 1)
    __builtin_abort ();
  return 0;
}
