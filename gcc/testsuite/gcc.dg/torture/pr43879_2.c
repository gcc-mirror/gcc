struct TBL {
	int (*p)(int *i);
};

extern void bar(int i);
extern void baz(int *i);

static int foo(int *i)
{
	bar(*i);
	baz(i);
	bar(*i);
	return *i;
}

struct TBL tbl = { foo };

