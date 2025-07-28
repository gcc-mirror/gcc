/* { dg-do compile } */
/* { dg-options "-Wreturn-local-addr" } */

typedef int (*fun_t)();

fun_t foo()
{
	int bar() { return 1; }
	return bar;
}

fun_t foo0(int n)
{
	int bar() { return sizeof(n); }
	return bar;
}

fun_t foo1()
{
	constexpr int i = 1;
	int bar() { return i; }
	return bar;
}

fun_t foo2()
{
	static int i = 1;
	int bar() { return i; }
	return bar;
}

fun_t foo3()
{
	static int i = 1;
	int bar() { return i; }
	int bar2() { return bar(); }
	return bar2;
}

fun_t foo4()
{
	enum { E = 1 };
	int bar() { return E; }
	return bar;
}

fun_t foo5()
{
	int bar() {
		int n = 3;
		int foo() { return n; };
		return foo();
	}
	return bar;
}

