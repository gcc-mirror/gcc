/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-dom2 -fdump-tree-optimized-details-blocks" } */

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

/* This is actually a regression.  The backward threader cannot thread
   the above scenario, but it is being caught by the DOM threader
   which still uses the forward threader.  We should implement this
   optimization in the backward threader before killing the forward
   threader.  Similarly for the other phi_on_compare-*.c tests.  */
/* { dg-final { scan-tree-dump-times "Removing basic block" 1 "dom2" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
