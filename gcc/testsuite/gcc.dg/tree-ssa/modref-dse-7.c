/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized"  } */
struct list
{
	 struct list *next;
};
__attribute__ ((noinline))
void
kill(struct list *l, int *a)
{
	while (l)
		l=l->next;
	*a = 0;
}
void
test(struct list *l, int *a)
{
	*a=12345;
	kill (l, a);
	return;
}
/* { dg-final { scan-tree-dump-not "12345" "optimized"} } */
