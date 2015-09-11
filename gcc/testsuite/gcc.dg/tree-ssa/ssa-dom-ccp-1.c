/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-optimized" } */
int t(int a) __attribute__ ((const));
void abort (void);
int
ccp(int b)
{
	int a=1;
	a++;
	a++;
	a++;
	if (b)
	  abort();
	return a;
}
/* We should propagate constant 4 into return.  */
/* { dg-final { scan-tree-dump-times "return 4" 1 "optimized"} } */
