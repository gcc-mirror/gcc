/*
TEST_OUTPUT:
---
fail_compilation/templatethis.d(13): Error: cannot use `this` outside an aggregate type
fail_compilation/templatethis.d(17): Error: cannot use `this` outside an aggregate type
fail_compilation/templatethis.d(21): Error: cannot use `this` outside an aggregate type
fail_compilation/templatethis.d(23): Error: cannot use `this` outside an aggregate type
fail_compilation/templatethis.d(29): Error: cannot use `this` outside an aggregate type
fail_compilation/templatethis.d(32): Error: mixin `templatethis.t2!()` error instantiating
---
*/

template t(this T)
{
}

struct S(this T)
{
}

enum e(this T) = 1;

void f(this T)()
{
}

mixin template t2()
{
	int i(this T) = 1;
}

mixin t2;

class C
{
	mixin t2; // OK
}
