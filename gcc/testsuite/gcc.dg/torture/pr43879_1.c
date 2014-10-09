/* { dg-do run } */
/* { dg-options "-fipa-pta -fno-inline" } */
/* { dg-additional-sources "pr43879_2.c" } */

void bar(int c)
{
	static int x = 1;
	if (c != x) __builtin_abort();
	x--;
}

void baz(int *i)
{
	(*i)--;
}

struct TBL {
	int (*p)(int *i);
};
extern struct TBL tbl;

int main()
{
	int c = 1;
	return tbl.p(&c);
}

