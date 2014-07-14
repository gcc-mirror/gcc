/* PR tree-optimization/61725 */

int
main ()
{
  int x;
  for (x = -128; x <= 128; x++)
    {
      int a = __builtin_ffs (x);
      if (x == 0 && a != 0)
        __builtin_abort ();
    }
  return 0;
}
