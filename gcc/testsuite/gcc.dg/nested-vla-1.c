/* { dg-do run } */
/* { dg-options "-std=gnu99" } */
/* { dg-require-effective-target trampolines } */


int main()
{
	int n = 1;

	struct foo { char x[++n]; } bar(void) { struct foo r; return r; }

	if (2 != n)
		__builtin_abort();

	if (2 != sizeof(bar()))
		__builtin_abort();

	n = 1;

	struct bar { char x[++n]; } (*bar2p)(void);
	struct bar bar2(void) { struct bar r; return r; }
	bar2p = &bar2;

	if (2 != n)
		__builtin_abort();

	if (2 != sizeof((*bar2p)()))
		__builtin_abort();

	n = 1;

	struct str { char x[++n]; } *bar3(void)
	{ 
		struct str* s = __builtin_malloc(sizeof(struct str));
		if (!s)
			__builtin_abort();
		struct str t;
		*s = t;
		return s;
	}

	if (2 != n)
		__builtin_abort();

	struct str* p;

	if (2 != sizeof(*(p = bar3())))
		__builtin_abort();

	__builtin_free(p);

	n = 1;

	struct { char x[++n]; } *bar4(void) { }

	if (2 != n)
		__builtin_abort();
#if 0
	// UB
	if (2 != sizeof(*bar4()))
		__builtin_abort();
#endif
}

