/* PR tree-optimization/66299 */
/* { dg-do run } */
/* { dg-options "-fdump-tree-original" } */

void __attribute__ ((noinline, noclone))
test1 (int x)
{
  if ((2 << x) == 1
      || (8 << x) == 1
      || (8 << x) == 2
      || (3072 << x) == 3
      || (294912 << x) == 9
      || (45056 << x) == 11
      || (2176 << x) == 17)
    __builtin_abort ();
}

void __attribute__ ((noinline, noclone))
test2 (int x)
{
  if ((2 << x) != 1
      && (8 << x) != 1
      && (8 << x) != 2
      && (3072 << x) != 3
      && (294912 << x) != 9
      && (45056 << x) != 11
      && (2176 << x) != 17)
    ;
  else
    __builtin_abort ();
}

void __attribute__ ((noinline, noclone))
test3 (int x)
{
  if ((3 << x) == 4
      || (1 << x) == 12
      || (40 << x) == 1024
      || (2 << x) == 84
      || (3 << x) == 16384
      || (10 << x) == 6144)
    __builtin_abort ();
}

void __attribute__ ((noinline, noclone))
test4 (int x)
{
  if ((3 << x) != 4
      && (1 << x) != 12
      && (40 << x) != 1024
      && (2 << x) != 84
      && (3 << x) != 16384
      && (10 << x) != 6144)
    ;
  else
    __builtin_abort ();
}

int
main (void)
{
  test1 (0);
  test2 (1);
  test3 (1);
  test4 (2);
}

/* { dg-final { scan-tree-dump-not "(<<|==|!=)" "original" } } */
