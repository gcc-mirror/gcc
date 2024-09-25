/* { dg-do compile { target fpic } } */
/* { dg-require-alias "" } */
/* { dg-options "-O2 -fPIC -fdump-tree-optimized" } */
void do_not_optimize(int b)
{
    do_not_optimize(0);
}
void do_optimize(int b)
{
    do_optimize(0);
}

void g(int b) __attribute__((alias(("do_not_optimize"))));

/* { dg-final { scan-tree-dump "do_not_optimize .0" "optimized" } } */
/* { dg-final { scan-tree-dump-not "do_optimize .0" "optimized" } } */
