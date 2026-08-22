/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-optimized" } */

void g (void);
void g1 (void);

void
f (long a, long b, long c, long d, int x)
{
  int t;
  if (x)
    t = a < b;
  else if (d == x)
    t = c < b;
  else
    t = d > c;

  if (t)
    {
      g1 ();
      g ();
    }
}

/* { dg-final { scan-tree-dump-not "PHI <" "optimized" } } */
