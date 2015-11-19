/* PR rtl-optimization/68376 */

extern void abort (void);

__attribute__((noinline, noclone)) int
f1 (int x)
{
  return x < 0 ? ~x : x;
}

__attribute__((noinline, noclone)) int
f2 (int x)
{
  return x < 0 ? x : ~x;
}

__attribute__((noinline, noclone)) int
f3 (int x)
{
  return x <= 0 ? ~x : x;
}

__attribute__((noinline, noclone)) int
f4 (int x)
{
  return x <= 0 ? x : ~x;
}

int
main ()
{
  if (f1 (5) != 5 || f1 (-5) != 4 || f1 (0) != 0)
    abort ();
  if (f2 (5) != -6 || f2 (-5) != -5 || f2 (0) != -1)
    abort ();
  if (f3 (5) != 5 || f3 (-5) != 4 || f3 (0) != -1)
    abort ();
  if (f4 (5) != -6 || f4 (-5) != -5 || f4 (0) != 0)
    abort ();
  return 0;
}
