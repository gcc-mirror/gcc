/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats" } */
int main(int x)
{
	int c, y;
	if (c)
		x = 2;
	y = x + 1;
	return y;
}
/* We should eliminate one evaluation of x + 1 along the x = 2 path,
   causing one elimination.  */
/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
