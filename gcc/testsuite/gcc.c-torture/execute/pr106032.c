/* PR rtl-optimization/106032 */

__attribute__((noipa)) int
foo (int x, int *y)
{
  int a = 0;
  if (x < 0)
    a = *y;
  return a;  
}

int
main ()
{
  int a = 42;
  if (foo (0, 0) != 0 || foo (1, 0) != 0)
    __builtin_abort ();
  if (foo (-1, &a) != 42 || foo (-42, &a) != 42)
    __builtin_abort ();
  return 0;
}
