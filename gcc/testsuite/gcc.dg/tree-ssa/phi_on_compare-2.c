/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-optimized" } */

void g (void);
void g1 (void);

void
f (long a, long b, long c, long d, int x)
{
  _Bool t;
  if (x)
    t = c < d;
  else
    t = a < b;

  if (t)
    {
      g1 ();
      g ();
    }
}

/* { dg-final { scan-tree-dump-not "PHI <" "optimized" } } */
