/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

volatile int count;
void bar(int);
void foo()
{
 bar(count++);
}

/* { dg-final { scan-tree-dump-times "count =" 1 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
