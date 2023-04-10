module foo.bar.ba;
nothrow pure @nogc @safe package(foo)
{
	void foo();
	nothrow pure @nogc @safe package(foo.bar) void foo2();
}
