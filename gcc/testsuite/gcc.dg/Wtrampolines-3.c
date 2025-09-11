/* { dg-do compile } */
/* { dg-require-effective-target trampolines } */
/* { dg-options "-Wtrampolines" } */
/* { dg-skip-if "" { *-*-darwin2* } } */


// Test that we do generate trampolines where this is required.

void g(int (*)());

void foo4(int n)
{
	typedef int a[n];
	int bar() { return sizeof(a); }		/* { dg-warning "trampoline generated" } */
	g(bar);	
}

void foo5(int n)
{
	int a[n];
	int bar() { return sizeof(a); }		/* { dg-warning "trampoline generated" } */
	g(bar);	
}

void foo6(int n)
{
	typedef int a[n];
	int bar() { a x; return 1; }		/* { dg-warning "trampoline generated" } */
	g(bar);	
}

void foo7(int n)
{
	typedef int a[n];
	int bar() { (a*)nullptr; return 1; }	/* { dg-warning "trampoline generated" } */
	g(bar);	
}

void foo8(int n)
{
	auto int bar();
	g(bar);	
	int bar() { return n; }			/* { dg-warning "trampoline generated" } */
}

void foo9(int n)
{
	int bar() { return n; }
	int bar2() { return bar(); }		/* { dg-warning "trampoline generated" } */
	g(bar2);
}

void foo10(int n)
{
	int bar()				/* { dg-warning "trampoline generated" } */
	{
		int bar2() { return n; }
		return bar2();
	}
	g(bar);			
}

void foo11()
{
	constexpr int a = 1;
	int bar()				/* { dg-warning "trampoline generated" } */
	{
		const int *ap = &a;
		return *ap; 
	}
	g(bar);
}

void foo12(int n)
{
	static int (*a)[n];
	int bar() { typeof(a) b; return sizeof *b; }	/* { dg-warning "trampoline generated" } */
	g(bar);	
}

void foo13(int n)
{
	int (*bar())[n] { return nullptr; }
	int bar2() { return sizeof *(*bar)(); }		/* { dg-warning "trampoline generated" } */
	g(bar2);
}


