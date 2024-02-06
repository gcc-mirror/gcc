/* PR tree-optimization/113759 */

extern short t[];

int
foo (int c, int b)
{
  if (b < 0)
    __builtin_unreachable ();
  if (c <= 0)
    __builtin_unreachable ();
  int d;
  for (; c >= 0; c--) 
    {
      int a = b + c;
      d = t[a];
      t[a] = 0;
    }
  return d;
}
