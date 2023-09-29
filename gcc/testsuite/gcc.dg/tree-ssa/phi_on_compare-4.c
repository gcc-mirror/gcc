/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-threadfull1-stats" } */

void g (int);
void g1 (int);

void
f (long a, long b, long c, long d, int x)
{
  int t;
  _Bool l1 = 0, l2 = 0;
  if (x)
    {
      g (a);
      c = a + b;
      t = a < b;
      l1 = 1;
    }
  else
    {
      g1 (b);
      t = c > d;
      d = c + b;
      l2 = 1;
    }

  if (t)
    {
      if (l1 | l2)
	g1 (c);
    }
  else
    {
      g (d);
      g1 (a + b);
    }
  g (c + d);
}

/* { dg-final { scan-tree-dump "Jumps threaded: 2" "threadfull1" } } */
