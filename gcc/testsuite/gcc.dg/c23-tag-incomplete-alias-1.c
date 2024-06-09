/* { dg-do run } 
 * { dg-options "-std=c23 -O2" } */

[[gnu::noinline]]
void *alias(void *ap, void *bp, void *x, void *y)
{
	struct foo { struct bar *f; } *a = ap;
	struct bar { long x; };

	a->f = x;

	{
		struct bar;
		struct foo { struct bar *f; } *b = bp;
		struct bar { long x; };

		// after completing bar, the two struct foo should be compatible 

		b->f = y;
	}


	return a->f;
}

int main()
{
	struct bar { long x; };
	struct foo { struct bar *f; } a;
	struct bar x, y;
	if (&y != alias(&a, &a, &x, &y))
		__builtin_abort();

	return 0;
}

