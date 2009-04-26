/* PR tree-optimization/20139
   This testcase is derived from gcc.dg/20020720-1.c.  Here we check
   that the optimization happens at tree level.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

extern double fabs (double);
extern void link_error (void);

void
foo (double x)
{
  double p, q;

  p = fabs (x);
  q = 0.0;
  if (p < q)
    link_error ();
}

/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
