/* { dg-options "-O3 -fdump-tree-cunroll-details -fno-unroll-loops -fpeel-loops -fdump-tree-ch2-details-blocks -fno-tree-loop-distribute-patterns" } */
int a[100];
int n = 1000000;
int zeroc;
int
main()
{
	a[0]=1;
	for (int i = 0; i < n; i++)
	{
	  int j;
	  for (j = 0; a[j]; j++);
	  zeroc+=j;
	  asm __volatile__ ("":::"memory");
	}
	return 0;
}
/* { dg-final-use { scan-tree-dump "Peeled loop 2, 1 times" "cunroll" } } */
/* { dg-final-use { scan-tree-dump "Peeled likely exits: likely decreased number of iterations of loop 1" "ch2" } } */
/* { dg-final-use { scan-tree-dump "Peeled all exits: decreased number of iterations of loop 2" "ch2" } } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "ch2" } } */
