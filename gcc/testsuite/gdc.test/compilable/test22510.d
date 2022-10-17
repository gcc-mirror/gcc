// https://issues.dlang.org/show_bug.cgi?id=22510

struct S
{
	int b;

    @disable this(this);
    this (scope ref inout S) inout
    {
    	this.b = b;
    }
}

void main()
{
	auto scoped_s = S(4);
	auto heap_s = new S(42);
}
