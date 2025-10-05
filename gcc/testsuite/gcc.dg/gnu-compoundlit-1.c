/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */

int g(int n, int (*p)[n]);
int f(int n)
{
	return g(n, &(int[n]){ });
}

void h(int n)
{
	(int[n]){ 1 };		/* { dg-error "empty initializer" } */
}

void i(int n)
{
	(static int[3]){ };
	(static int[n]){ };	/* { dg-error "storage size" } */
	(constexpr int[3]){ };
	(constexpr int[n]){ };	/* { dg-error "storage size" } */
	(register int[3]){ };	/* { dg-error "register" } */
	(register int[n]){ };	/* { dg-error "register" } */
	(_Thread_local int[3]){ };	/* { dg-error "_Thread_local" } */
	(_Thread_local int[n]){ };	/* { dg-error "_Thread_local" } */
}

