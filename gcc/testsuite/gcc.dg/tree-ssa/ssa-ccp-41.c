/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(int x)
{
    int p = x & 24;
    int r = 1 << p; 
    return r & (1<<17);
}

/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */
