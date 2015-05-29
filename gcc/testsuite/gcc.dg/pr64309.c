/* PR middle-end/64309 */
/* { dg-do run } */
/* { dg-options "-fdump-tree-original" } */

int
fn1 (int n)
{
  return ((1 << n) & 1) != 0;
}

int
fn2 (int n)
{
  return (1 & (1 << n)) != 0;
}

int
fn3 (int n)
{
  return ((1 << n) & 1) == 0;
}

int
fn4 (int n)
{
  return (1 & (1 << n)) == 0;
}

int
main (void)
{
  if (fn1 (0) != 1
      || fn1 (1) != 0
      || fn1 (2) != 0
      || fn1 (3) != 0
      || fn1 (4) != 0
      || fn1 (5) != 0)
    __builtin_abort ();

  if (fn2 (0) != 1
      || fn2 (1) != 0
      || fn2 (2) != 0
      || fn2 (3) != 0
      || fn2 (4) != 0
      || fn2 (5) != 0)
    __builtin_abort ();

  if (fn3 (0) != 0
      || fn3 (1) != 1
      || fn3 (2) != 1
      || fn3 (3) != 1
      || fn3 (4) != 1
      || fn3 (5) != 1)
    __builtin_abort ();

  if (fn4 (0) != 0
      || fn4 (1) != 1
      || fn4 (2) != 1
      || fn4 (3) != 1
      || fn4 (4) != 1
      || fn4 (5) != 1)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-not "(<<|>>)" "original" } } */
