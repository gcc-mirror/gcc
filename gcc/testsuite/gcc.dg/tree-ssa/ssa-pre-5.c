/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats" } */
int 
foo (int i)
{
	int a, b;
	if (i)
		a = 3, b = 2;
	else
		a = 2, b = 3;
	return a + b;
}
/* We should detect that a+b is the same along both edges, and replace it with
   5  */
/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"} } */
/* { dg-final { scan-tree-dump-times "Insertions" 0 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
