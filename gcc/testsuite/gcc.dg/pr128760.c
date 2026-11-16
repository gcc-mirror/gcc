/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */
/* { dg-require-effective-target trampolines } */

int f(int n)
{
	int g()
	{
		static int (*p)() = &g;	/* { dg-error "initializer element is not constant" } */
		return n;
	}
	static int (*q)() = &g;		/* { dg-error "initializer element is not constant" } */
	return g();
}

int h(int n)
{
	auto int g();
	static int (*p)() = &g;		/* { dg-error "initializer element is not constant" } */
	int g()
	{
		return n;
	}
	static int (*q)() = &g;		/* { dg-error "initializer element is not constant" } */

	return g();
}

int i(int n)
{
	auto int g();
	static int (*p)() = &g;		/* { dg-error "initializer element is not constant" } */
	int g()
	{
		return n;
	}
	static int (*q)() = &g;		/* { dg-error "initializer element is not constant" } */

	return g();
}

int j()
{
	auto int g();
	auto int g();
	static int (*q)() = &g;		/* { dg-error "initializer element is not constant" } */
	int g()
	{
		return 0;
	}

	return g();
}

int k()
{
	extern int g();
	static int (*q)() = &g;		/* no warning */

	return g();
}


