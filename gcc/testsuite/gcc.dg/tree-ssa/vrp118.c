/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void eliminate_me();
void f(int x,int y){
    if (y <= 0)
      __builtin_unreachable();
    x += y;
    if (x == -__INT_MAX__ - 1)
      eliminate_me ();
}

/* { dg-final { scan-tree-dump-not "eliminate_me" "optimized" } } */
