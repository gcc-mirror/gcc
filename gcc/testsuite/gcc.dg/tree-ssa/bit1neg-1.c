/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct f
{
  int a:1;
};

void g(struct f *a)
{
 int t = a->a;
 t = -t;
 a->a = t;
}
void g1(struct f *a, int b)
{
 int t = b;
 t = -t;
 a->a = t;
}
/* the 2 negates should have been removed as this is basically the same
   as (-a) & 1. */
/* { dg-final { scan-tree-dump-not " = -" "optimized" } } */
