module imports.c19762;

struct Foo
{
	import ice19762 : X;
	X[] x;
}

Nullable!Foo foo()
{
	Nullable!Foo output;
	return output;
}

struct Nullable(T)
{
    bool opEquals(U)(const(U) rhs) const
    if (is(typeof(this.get == rhs)))
    {
        return true;
    }

    inout(T) get() inout
    {
        return T.init;
    }
}
