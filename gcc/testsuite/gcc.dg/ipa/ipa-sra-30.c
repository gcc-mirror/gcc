/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-sra"  } */
struct list
{
	 struct list *next;
	 int val;
};
__attribute__ ((noinline))
static int reta (int *a)
{
	return *a;
}
__attribute__ ((noinline))
static int
kill(struct list *l, int *a)
{
	int v;
	while (l)
	{
		v = l->val;
		l=l->next;
	}
	return reta (a) + v;
}
int
test(struct list *l, int *a)
{
	return kill (l, a);
}
/* Loop in kill may be infinite; do not SRA.  */
/* { dg-final { scan-ipa-dump-not "Created new node kill.isra"  "sra"  } } */
