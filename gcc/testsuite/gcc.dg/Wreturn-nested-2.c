/* { dg-do compile } */
/* { dg-require-effective-target trampolines } */
/* { dg-options "-Wreturn-local-addr" } */
/* { dg-skip-if "" { *-*-darwin2* } } */

typedef int (*fun_t)();

fun_t foo4(int n)
{
	typedef int a[n];
	int bar() { return sizeof(a); }		
	return bar;				/* { dg-warning "address of nested function" } */
}

fun_t foo5(int n)
{
	int a[n];
	int bar() { return sizeof(a); }
	return bar;				/* { dg-warning "address of nested function" } */
}

fun_t foo6(int n)
{
	typedef int a[n];
	int bar() { a x; return 1; }
	return bar;				/* { dg-warning "address of nested function" } */
}

fun_t foo7(int n)
{
	typedef int a[n];
	int bar() { (a*)nullptr; return 1; }
	return bar;				/* { dg-warning "address of nested function" } */
}

fun_t foo8(int n)
{
	auto int bar();
	return bar;				/* { dg-warning "address of nested function" } */
	int bar() { return n; }
}

fun_t foo9(int n)
{
	int bar() { return n; }
	int bar2() { return bar(); }
	return bar2;				/* { dg-warning "address of nested function" } */
}

fun_t foo10(int n)
{
	int bar()
	{
		int bar2() { return n; }
		return bar2();
	}
	return bar;				/* { dg-warning "address of nested function" } */
}

fun_t foo11()
{
	constexpr int a = 1;
	int bar() { const int *ap = &a; return *ap; }
	return bar;				/* { dg-warning "address of nested function" } */
}

fun_t foo12(int n)
{
	static int (*a)[n];
	int bar() { typeof(a) b; return sizeof *b; }
	return bar;				/* { dg-warning "address of nested function" } */
}

fun_t foo13(int n)
{
	int (*bar())[n] { return nullptr; }
	int bar2() { return sizeof *(*bar)(); }
	return bar2;				/* { dg-warning "address of nested function" } */
}


