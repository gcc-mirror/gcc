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

/* { dg-final { scan-tree-dump-times "&MEM <int> \\\[\\\(void .\\\)&a \\\+ (?:4|8)B\\\]" 3 "optimized" } } */


void bar(int i)
{
	x = &a[i] - 1;
	y = &a[i] + 1;
	z = 1 + &a[i];
}

/* We can't get &a[i +- 1] in the final code and it is not clear we
   want this.  Instead we get to see &a[i] and pointer offsetting
   by 4 and -4U.  */
