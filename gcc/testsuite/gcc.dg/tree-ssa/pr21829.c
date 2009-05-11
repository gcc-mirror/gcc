/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int test(int v)
{
  int x = 0;
  int u;
  for (u=0;u<2;u++)
  {
    if (u>v)
    {
      if (u%2==1)
        x++;
    }
  }  
  return x;
}

/* This should be unrolled and optimized into conditional set of return value "v < 0".  */

/* { dg-final { scan-tree-dump-not "if \\(" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
