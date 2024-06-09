/* { dg-do run { target heap_trampoline } } */
/* { dg-options "-ftrampoline-impl=heap" } */

__attribute__((noipa)) int
bar (int (*fn) (int))
{
  return fn (42) + 1;
}

int
main ()
{
  int a = 0;
  int foo (int x) { if (x != 42) __builtin_abort (); return ++a; }
  if (bar (foo) != 2 || a != 1)
    __builtin_abort ();
  if (bar (foo) != 3 || a != 2)
    __builtin_abort ();
  a = 42;
  if (bar (foo) != 44 || a != 43)
    __builtin_abort ();
  return 0;
}
