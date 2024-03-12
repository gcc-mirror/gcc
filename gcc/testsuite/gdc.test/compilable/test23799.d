// https://issues.dlang.org/show_bug.cgi?id=23799

// REQUIRED_ARGS: -betterC

struct Data
{
	Data[] range;
	string test;
}

Data[] foo()
{
	Data[] ret;
	if (__ctfe)
	{
		Data tmp;
		tmp.range ~= Data.init;
		ret ~= tmp;
	}
	return ret;
}

void func(Data dat)()
{
}

void bar(Data dat)()
{
	if (dat.test.length)
		func!(dat.range[0])();
}

extern (C) void main()
{
	static immutable data = foo();
	bar!(data[0])();
}
