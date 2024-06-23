/* { dg-do run } */
/* { dg-options "-std=c2y -O2" } */

struct f2 {
	struct f {
		_Alignas(int) char buf[sizeof(int)];
	} x[2];
	int i;
};

[[gnu::noinline]]
int foo2(struct f2 *p, int *q)
{
	*q = 1;
	*p = (struct f2){ };
	return *q;
}

struct g2 {
	union g {
		_Alignas(int) char buf[sizeof(int)];
	} x[2];
	int i;
};

[[gnu::noinline]]
int bar2(struct g2 *p, int *q)
{
	*q = 1;
	*p = (struct g2){ };
	return *q;
}

int main()
{
	struct f2 p2;
	if (0 != foo2(&p2, (void*)&p2.x[0].buf))
		__builtin_abort();

	struct g2 q2;
	if (0 != bar2(&q2, (void*)&q2.x[0].buf))
		__builtin_abort();
}
