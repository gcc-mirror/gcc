/* PR tree-optimization/33434 */
/* { dg-do run } */
/* { dg-options "-O3" } */

int k;

void __attribute__((noinline)) f2 (int b)
{
  k = b - 1;
}

void f1 (int a, int b)
{
  f2 (b);
  a = 1;
  b = 1;
  if (a)
    while (b --)
      k = 1;
  else
    if (b != 1)
      __builtin_abort ();
}

int main (void)
{
  f1 (1, 1);
  if (k != 1)
    __builtin_abort ();
  return 0;
}
