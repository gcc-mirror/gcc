/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt1" } */
/* PR tree-optimization/122153 */
struct s1
{
  int t;
};
void g(struct s1*);
struct s1 f(int *a, int c, int d)
{
  struct s1 r;
  int t1;
  if (c < d)
  {
    r = (struct s1){};
    t1 = c;
  }
  else
  {
    r = (struct s1){};
    t1 = d;
  }
  g(&r);
  r.t = t1;
  return r;
}
/* the `r = {};` store should be commonialized out of the conditional
   and produce a MIN_EXPR in phiopt1. */

/* { dg-final { scan-tree-dump "MIN_EXPR" "phiopt1" } } */
