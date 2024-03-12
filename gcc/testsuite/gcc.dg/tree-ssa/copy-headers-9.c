/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ch-details" } */
int a[100];
void test (int m, int n)
{
	int i = 0;
	do
	{
		if (m)
			break;
		i++;
		a[i]=0;
	}
	while (i<10);
}
/* { dg-final { scan-tree-dump-times "Duplicating bb . is a win" 1 "ch2" } } */
/* { dg-final { scan-tree-dump-times "Will duplicate bb" 2 "ch2" } } */
/* { dg-final { scan-tree-dump "is now do-while loop" "ch2" } } */
