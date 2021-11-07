/* { dg-do link } */
/* { dg-options "-O2 -fdump-tree-modref1"  } */
void linker_error ();
__attribute__((noinline))
int *
test (int **a)
{
  return *a;
}
int
main()
{
	int val;
	int *a=&val;
	int *b = test (&a);
	if (b == (int *)&a)
		linker_error ();
	return 0;
}
/* { dg-final { scan-tree-dump "parm 0 flags: noclobber noescape nodirectescape not_returned_directly" "modref1"} } */
