/*
Previous implementation raised errors for overloads using alias declarations
because they ignored the actual function name

TEST_OUTPUT:
---
fail_compilation/test18385b.d(13): Error: `test18385b.S.foo` called with argument types `(int)` matches multiple overloads exactly:
fail_compilation/test18385b.d(8):     `test18385b.S.foo(int s)`
and:
fail_compilation/test18385b.d(3):     `test18385b.foo(int s)`
fail_compilation/test18385b.d(102): Error: `test18385b.bar` called with argument types `(int)` matches multiple overloads exactly:
fail_compilation/test18385b.d(2):     `test18385b.bar(int s)`
and:
fail_compilation/test18385b.d(3):     `test18385b.foo(int s)`
---
*/
#line 1

void bar(int s) {}
void foo(int s) {}
alias bar = foo;

struct S
{
	void foo(int s) {}
	alias foo = bar;

	void useEm()
	{
		foo(1);
	}
}

// False positive in mutex.d when building druntime
class Mutex
{
	this() {}
	this() shared {}
	this(Object obj) {}
}

#line 100
void main()
{
	bar(0);
	new Mutex();
}
