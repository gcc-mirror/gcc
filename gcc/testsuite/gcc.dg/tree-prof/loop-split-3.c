/* { dg-options "-O3 -fdump-tree-lsplit-details-blocks -fdump-tree-optimized-details-blocks" } */

int M = 100;
int a[1000];

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
void test1 (int n)
{
  if (n <= 0 || n > 100000)
        return; 
  for (int i = 0; i <= n; i++)
    {
      if (i < n)
	do_something ();
      if (a[i])
	do_something2();
    }
}
int
main(int, char **)
{
  for (int i = 0 ; i < 1000; i+=3)
    a[i]=1;
  for (int i = 0 ; i < 1000; i++)
    test1(M);
  return 0;
}
/* { dg-final-use-not-autofdo { scan-tree-dump-times "Loop split" 1 "lsplit" } } */
/* { dg-final-use-not-autofdo { scan-tree-dump-times "Invalid sum" 0 "lsplit" } } */
/* { dg-final-use-not-autofdo { scan-tree-dump-times "Invalid sum" 0 "optimized" } } */
