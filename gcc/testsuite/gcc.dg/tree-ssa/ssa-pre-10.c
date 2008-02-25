/* { dg-do compile } */ 
/* { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-fre-stats" } */
/* { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-fre-stats -fno-finite-math-only" { target sh*-*-* } } */
double cos (double);
void link_error();
void f(double a)
{
  double b = cos (a);
  double c = cos (a);
  if (b != c)
    link_error();
}

/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "fre"} } */
/* { dg-final { cleanup-tree-dump "fre" } } */
