/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse1-details"  } */
volatile int *ptr;
struct a {
	int a,b,c;
} a;
__attribute__((noinline))
static int init (struct a*a)
{
	a->a=0;
	a->b=1;
}
__attribute__((noinline))
static int use (struct a*a)
{
	if (a->c != 3)
		*ptr=5;
}

void
main(void)
{
	struct a a;
	init (&a);
	a.c=3;
	use (&a);
}
/* { dg-final { scan-tree-dump "Deleted dead store: init" "dse1" } } */
