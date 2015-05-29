/* A test for strength reduction of ivs with nonconstant step.  */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int bar (void);

int a[100];

void xxx (void)
{
  int iter, step = bar ();

  for (iter = 0; iter < 10; iter++)
    a[iter * step] = bar ();
}

/* The array access should be strength reduced.  But to determine the value of
   the step, we need to calculate step * sizeof (int), thus we need to be
   a bit careful about which multiplications we disallow.  */

/* { dg-final { scan-tree-dump-times "step \\* \[^0-9\]" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\[^0-9\] \\* step" 0 "optimized" } } */

