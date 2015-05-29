/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int spinlock[2];
int main ()
{
volatile int * spinlock0;
volatile int * spinlock1;
spinlock0 = &spinlock[0];
spinlock1 = &spinlock[1];

*spinlock0 = 0;
*spinlock1 = 0;
 while (*spinlock0);
}

/* { dg-final { scan-tree-dump "={v} .*spinlock" "optimized" } } */
/* { dg-final { scan-tree-dump "spinlock.* ={v}" "optimized" } } */
