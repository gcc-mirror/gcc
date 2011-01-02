/* PR rtl-optimization/47028 */
/* { dg-do run } */
/* { dg-options "-O -foptimize-sibling-calls -fno-forward-propagate -fno-tree-copy-prop -fno-tree-dominator-opts" } */

int
fib (int n)
{
  if (n <= 1)
    return 1;
  return fib (n - 2) + fib (n - 1);
}

int
main (void)
{
  if (fib (5) != 8)
    __builtin_abort ();
  return 0;
}
