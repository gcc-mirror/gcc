/* { dg-do run } */
/* { dg-options "-std=c2y -O2" } */

struct f { _Alignas(int) char buf[sizeof(int)]; };
struct f2 { struct f x; };
union g { _Alignas(int) char buf[sizeof(int)]; };

[[gnu::noinline]]
int foo(struct f *p, int *q)
{
	*q = 1;
	*p = (struct f){ };
	return *q;
}

[[gnu::noinline]]
int foo2(struct f2 *p, int *q)
{
	*q = 1;
	*p = (struct f2){ };
	return *q;
}

[[gnu::noinline]]
int bar(union g *p, int *q)
{
	*q = 1;
	*p = (union g){ };
	return *q;
}


int main()
{
	struct f p;
	if (0 != foo(&p, (void*)&p.buf))
		__builtin_abort();

	struct f2 p2;
	if (0 != foo2(&p2, (void*)&p2.x.buf))
		__builtin_abort();

	union g q;
	if (0 != bar(&q, (void*)&q.buf))
		__builtin_abort();
}
