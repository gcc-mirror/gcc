/* { dg-do compile } */
/* { dg-require-effective-target trampolines } */
/* { dg-options "-O0 -Wtrampolines" } */
/* { dg-skip-if "" { *-*-darwin2* } } */



// Test that we do not create a trampoline even without optimization.

void g(int (*)());

void foo()
{
	int bar() { return 1; }
	g(bar);
}

void foo0(int n)
{
	int bar() { return sizeof(n); }
	g(bar);	
}

void foo1()
{
	constexpr int i = 1;
	int bar() { return i; }
	g(bar);	
}

void foo2()
{
	static int i = 1;
	int bar() { return i; }
	g(bar);	
}

void foo3()
{
	static int i = 1;
	int bar() { return i; }
	int bar2() { return bar(); }
	g(bar2);	
}

void foo4()
{
	enum { E = 1 };
	int bar() { return E; }
	g(bar);
}

void foo5()
{
	int bar() {
		int n = 3;
		int foo() { return n; };
		return foo();
	}
	g(bar);
}

