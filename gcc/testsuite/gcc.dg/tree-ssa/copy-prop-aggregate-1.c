/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-details -fdump-tree-optimized" } */
/* PR tree-optimization/121751 */


struct s1
{
  int t[1024];
};

struct s2 {
  struct s1 t;
};

struct s3
{
  struct s2 t;
};

void g(struct s3*);

void f(struct s1 s)
{
  struct s2 removeme;
  removeme.t = s;
  struct s3 t1;
  t1.t = removeme;
  g(&t1);
}


/* { dg-final { scan-tree-dump-times "after previous" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "removeme " "optimized" } } */
