/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test22298.d(18): Error: assigning scope variable `i` to `p` with longer lifetime is not allowed in a `@safe` function
fail_compilation/test22298.d(29): Error: assigning scope variable `y` to `x` with longer lifetime is not allowed in a `@safe` function
---
*/

void g(scope void delegate(scope int*) @safe cb) @safe {
	int x = 42;
	cb(&x);
}

void main() @safe {
	int* p;
	void f(scope int* i) @safe {
		p = i;
	}

	g(&f);
	// address of x has escaped g
	assert(*p == 42);
}

void f() @safe {
    mixin("scope int* x;");
    scope int* y;
    x = y;
}
