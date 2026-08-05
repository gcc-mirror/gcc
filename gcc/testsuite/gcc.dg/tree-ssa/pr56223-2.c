/* PR tree-optimization/56223 */
/* Verify wrapping and INT_MIN behavior of the ABSU-based replacement.  */
/* { dg-do run } */
/* { dg-options "-O2 -fwrapv" } */

#define INT_MAX __INT_MAX__
#define INT_MIN (-INT_MAX - 1)

__attribute__ ((noipa)) int
add_abs (int s, int x)
{
  if (x >= 0)
    s += x;
  else
    s -= x;
  return s;
}

__attribute__ ((noipa)) int
sub_abs (int s, int x)
{
  if (x >= 0)
    s -= x;
  else
    s += x;
  return s;
}

int
main (void)
{
  if (add_abs (10, 4) != 14)
    __builtin_abort ();

  if (add_abs (10, -4) != 14)
    __builtin_abort ();

  if (add_abs (INT_MIN, INT_MIN) != 0)
    __builtin_abort ();

  if (add_abs (INT_MAX, 1) != INT_MIN)
    __builtin_abort ();

  if (add_abs (0, INT_MIN) != INT_MIN)
    __builtin_abort ();

  if (sub_abs (10, 4) != 6)
    __builtin_abort ();

  if (sub_abs (10, -4) != 6)
    __builtin_abort ();

  if (sub_abs (INT_MIN, INT_MIN) != 0)
    __builtin_abort ();

  if (sub_abs (INT_MIN, 1) != INT_MAX)
    __builtin_abort ();

  if (sub_abs (0, INT_MIN) != INT_MIN)
    __builtin_abort ();

  return 0;
}
