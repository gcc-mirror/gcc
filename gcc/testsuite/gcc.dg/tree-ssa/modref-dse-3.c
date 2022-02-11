/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1-details"  } */
__attribute__ ((noinline))
void write (int *a)
{
	*a=1;
	a[1]=2;
}
int test ()
{
	int a;
	a=2;
	write (&a);
	return a;
}
int test2 (int *a)
{
	*a=2;
	write (a);
	return *a;
}
/* { dg-final { scan-tree-dump-times "Deleted dead store" 2 "dse1"} } */
