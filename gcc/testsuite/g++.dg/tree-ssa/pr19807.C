/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int a[4];
int *x, *y, *z;

void foo(void)
{
	x = &a[3] - 1;
	y = &a[1] + 1;
	z = 1 + &a[1];
}

void bar(int i)
{
	x = &a[i] - 1;
	y = &a[i] + 1;
	z = 1 + &a[i];
}

/* { dg-final { scan-tree-dump-times "&a\\\[2\\\]" 3 "optimized" } } */

/* We want &a[D.bla + 1] and &a[D.foo - 1] in the final code, but
   tuples mean that the offset is calculated in a separate instruction.
   Simply test for the existence of +1 and -1 once, which also ensures
   the above.  If the addition/subtraction would be applied to the
   pointer we would instead see +-4 (or 8, depending on sizeof(int)).  */
/* { dg-final { scan-tree-dump-times "\\\+ -1;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\\+ 1;" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
