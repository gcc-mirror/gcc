/* PR tree-optimization/65418 */

__attribute__((noinline, noclone)) int
foo (int x)
{
  if (x == -216 || x == -211 || x == -218 || x == -205 || x == -223)
     return 1;
  return 0;
}

int
main ()
{
  volatile int i;
  for (i = -230; i < -200; i++)
    if (foo (i) != (i == -216 || i == -211 || i == -218 || i == -205 || i == -223))
      __builtin_abort ();
  return 0;
}
