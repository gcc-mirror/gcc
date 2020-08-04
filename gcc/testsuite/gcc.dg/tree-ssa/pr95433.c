/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int f(int x){return x*7==17;}
int g(int x){return x*3==15;}

/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */
/* { dg-final { scan-tree-dump "== 5;" "optimized" } } */
