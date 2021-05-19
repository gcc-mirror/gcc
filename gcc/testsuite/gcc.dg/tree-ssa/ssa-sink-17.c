/* PR tree-optimization/97307  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-sink-details" } */

int pure_f(int a, int b) __attribute__((pure));
int my_f(int a, int b)
{
  int x = pure_f(a, b);
  if (a > 0)
    return x;
  return a;
}

/* We should sink the call to pure_f to the if block.  */
/* { dg-final { scan-tree-dump "Sinking # VUSE" "sink1" } } */
