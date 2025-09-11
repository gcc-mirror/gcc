/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-details  -fdump-tree-esra-details " } */

struct s1
{
  int t[1024];
};

struct s1 f(void);

void g(int a, int b, int );
void p(struct s1);
static inline void p1(struct s1 inner)
{
  p(inner);
}

void h(struct s1 outer)
{
  p1(outer);
  g(outer.t[0], outer.t[1], outer.t[2]);
}
/* Forwprop should be able to copy prop the copy of `inner = outer` to the call of p.
   Also remove this copy. */

/* { dg-final { scan-tree-dump-times "after previous" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "Removing dead store stmt inner = outer" 1 "forwprop1" } } */

/* The extra copy that was done by inlining is removed so SRA should not decide to cause
   inner nor outer to be scalarized even for the 3 elements accessed afterwards.  */
/* { dg-final { scan-tree-dump-times "Disqualifying inner" 2 "esra" } } */
/* { dg-final { scan-tree-dump-times "Disqualifying outer" 1 "esra" } } */

