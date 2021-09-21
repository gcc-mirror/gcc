/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-vrp-thread1" } */

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

/* { dg-final { scan-tree-dump-times "Removing basic block" 1 "vrp-thread1" } } */
