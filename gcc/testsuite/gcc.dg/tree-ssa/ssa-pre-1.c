/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */
extern int printf (const char *, ...);
int foo(int argc, char **argv)
{
	int a;
	int b;
	int c;
	b  = argc + 1;
	c =  argc + 2;
	a = b + c;
	if (argc > 2)
	{
		c = argc + 3;
	}
	printf ("%d, %d\n", a, b + c);
}
/* We should eliminate one evaluation of b + c along the main path,
   causing one reload. */
/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
