/* { dg-options "-O3 -fdump-tree-lsplit-details-blocks -fdump-tree-optimized-details-blocks" } */


void
__attribute__ ((noinline,noipa))
do_something()
{
}
void
__attribute__ ((noinline,noipa))
do_something2()
{
}

__attribute__ ((noinline,noipa))
void test1 (int n, int ga)
{
  for (int i = 0; i < 200; i++)
	  if (i < 150)
		  do_something ();
	  else
		  do_something2 ();
}
int
main(int, char **)
{
	for (int i = 0 ; i < 1000; i++)
	  test1(10, 10);
	return 0;
}
/* { dg-final-use-not-autofdo { scan-tree-dump-times "Loop split" 1 "lsplit" } } */
/* { dg-final-use-not-autofdo { scan-tree-dump-times "Invalid sum" 0 "lsplit" } } */
/* { dg-final-use-not-autofdo { scan-tree-dump-times "Invalid sum" 0 "optimized" } } */
