/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */
[[gnu::noipa]] void
bar (int a, int b)
{
  if (a < 0)
    __builtin_abort ();
}

[[gnu::noipa]] void
foo (int n, bool p)
{
  for (int i = n; i-- > 0;)
    {
      const int x = 1 << i;
      if (x <= 0)
	__builtin_unreachable ();
      if (p)
	bar (i, x);
    }
}

int
main ()
{
  foo (4, true);
}
/* { dg-final { scan-tree-dump "__builtin_unreachable" "vrp1" } } */

