/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */
int foo(void)
{
	int x, c, y;
	x = 3;
	if (c)
		x = 2;
	y = x + 1;
	return y;
}
/* We should eliminate the x+1 computation from this routine, replacing
   it with a phi of 3, 4 */
/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
