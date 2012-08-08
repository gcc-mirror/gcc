/* Verify that no straight-line strength reduction occurs across sibling
   blocks.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

int
f (int s, int c)
{
  int a1, a2, a3, x1, x2, x3, x;

  if (c > 0)
    {
      a1 = 2 * s;
      x1 = c + a1;
    }
  else
    {
      a1 = 4 * s;
      x1 = c + a1;
    }

  a2 = 6 * s;
  x2 = c + a2;
  x = x1 + x2;
  return x;
}

/* { dg-final { scan-tree-dump-times " \\* " 3 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
