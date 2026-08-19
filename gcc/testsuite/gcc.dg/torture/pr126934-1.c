/* { dg-do run } */
/* PR tree-optimization/126934 */

__attribute__((noinline))
int ff(int a, signed char c)
{
  int t = c;
  int t1 = c+60;
  if (a)
    return t/t1;
  return 1000;
}

int main ()
{
  if (ff(1,-127) != 1)
    __builtin_abort ();
  if (ff(1,1) != 0)
    __builtin_abort ();
  if (ff(1,4) != 0)
    __builtin_abort ();
}

