/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-vrp-thread1" } */

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

/* { dg-final { scan-tree-dump-times "Removing basic block" 1 "vrp-thread1" } } */
