/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-details-blocks" } */
int a[8];
int t()
{
	int i;
	for (i = 0; i < 3; i++)
		if (a[i])
			break;
	return i;
}
/* Currently duplicate_loop_body_to_header_edge gets wrong computation of prob_pass_wont_exit
   which assumes that the exit condition is last in the loop.  */
/* { dg-final { scan-tree-dump-times "Invalid sum" 0 "optimized" } } */
