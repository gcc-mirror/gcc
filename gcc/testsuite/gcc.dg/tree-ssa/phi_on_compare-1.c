/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-phiopt1-details -fdump-tree-optimized-details-blocks" } */

void g (int);
void g1 (int);

void
f (long a, long b, long c, long d, long x)
{
  _Bool t;
  if (x)
    {
      g (a + 1);
      t = a < b;
      c = d + x;
    }
  else
    {
      g (b + 1);
      a = c + d;
      t = c > d;
    }

  if (t)
    g1 (c);

  g (a);
}

/* { dg-final { scan-tree-dump-times "replicating conditional" 2 "phiopt1" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
