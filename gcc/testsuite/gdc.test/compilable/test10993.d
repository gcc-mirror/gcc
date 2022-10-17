module test10993;

import core.demangle : demangleType;

auto foo(T)(T a)
{
	static immutable typeof(a) q;
//	pragma(msg, "foo: " ~ typeof(q).mangleof);
	return q;
}

struct test(alias fn)
{
	bool ini = true;
	void* p;
}

auto fun()
{
	auto x = foo!()(test!(a=>a)());
//	pragma(msg, "fun: " ~ typeof(x).mangleof);

	return x;
}

void main()
{
	const x = fun();
	enum mangle_x = typeof(x).mangleof;
//	pragma(msg, "x  : " ~ mangle_x);
	auto y = cast()x;
	enum mangle_y = typeof(y).mangleof;
//	pragma(msg, "y  : " ~ mangle_y);
	enum demangle_x = demangleType(mangle_x);
	enum demangle_y = demangleType(mangle_y);
	static assert ("immutable(" ~ demangle_y ~ ")" == demangle_x);
}
