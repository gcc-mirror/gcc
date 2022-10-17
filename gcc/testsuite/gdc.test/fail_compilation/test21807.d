/*
https://issues.dlang.org/show_bug.cgi?id=21807

REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/test21807.d(11): Deprecation: slice of static array temporary returned by `getSArray()` assigned to longer lived variable `this.str`
fail_compilation/test21807.d(12): Deprecation: slice of static array temporary returned by `getSArray()` assigned to longer lived variable `this.ca`
---
*/
#line 1

char[12] getSArray() pure;

class Foo
{
	string str;
	char[] ca;

	this()
	{
		str = getSArray(); // Should probably be a type error
		ca = getSArray();
	}
}

/*
TEST_OUTPUT:
---
fail_compilation/test21807.d(117): Error: function `test21807.addr(return ref int b)` is not callable using argument types `(int)`
fail_compilation/test21807.d(117):        cannot pass rvalue argument `S(0).i` of type `int` to parameter `return ref int b`
---
*/
#line 100

struct S
{
	int i;
}

int* addr(return ref int b)
{
	return &b;
}

class Foo2
{
	int* ptr;

	this()
	{
		ptr = addr(S().i);  // struct temporary
	}
}
