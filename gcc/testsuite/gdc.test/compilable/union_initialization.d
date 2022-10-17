// https://issues.dlang.org/show_bug.cgi?id=20068

union B
{
	int i;
	int* p;
	@safe this(int* p)
	{
		// Error: cannot access pointers in @safe code that overlap other fields
		this.p = p;
	}
}

/**************************************************************/
// https://issues.dlang.org/show_bug.cgi?id=21229

struct NeedsInit
{
	int var;
	@disable this();
}

union Union
{
	NeedsInit ni;
}

union Proxy
{
	Union union_;
}

struct S
{
	Union union_;
	Proxy proxy;

	this(NeedsInit arg)
	{
		union_.ni = arg;
		proxy.union_.ni = arg;
	}
}
