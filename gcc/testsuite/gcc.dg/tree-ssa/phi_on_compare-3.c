/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-dom2" } */

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

/* { dg-final { scan-tree-dump-times "Removing basic block" 1 "dom2" } } */
