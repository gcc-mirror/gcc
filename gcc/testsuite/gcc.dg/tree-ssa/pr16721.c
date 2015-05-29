/* { dg-do compile } */ 
/* { dg-options "-O2 -fno-strict-aliasing -fdump-tree-optimized" } */

struct data {
	volatile unsigned long *addr;
} *p;

int test()
{
	*p->addr;
	return 0;
}

/* The load from p->addr should not disappear.  */
/* { dg-final { scan-tree-dump-times "\->addr" 1 "optimized"} } */
