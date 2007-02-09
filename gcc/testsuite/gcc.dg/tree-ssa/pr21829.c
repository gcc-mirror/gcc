/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phicprop-details" } */

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

/* { dg-final { scan-tree-dump-times "Original statement:.*% 2\[ \t\n]*Updated statement.*=1" 0 "phicprop3" } } */
/* { dg-final { cleanup-tree-dump "phicprop\[1-3\]" } } */

