/* PR tree-optimization/65418 */

__attribute__((noinline, noclone)) int
foo (int x)
{
  if (x == -216 || x == -132 || x == -218 || x == -146)
     return 1;
  return 0;
}

int
main ()
{
  volatile int i;
  for (i = -230; i < -120; i++)
    if (foo (i) != (i == -216 || i == -132 || i == -218 || i == -146))
      __builtin_abort ();
  return 0;
}
