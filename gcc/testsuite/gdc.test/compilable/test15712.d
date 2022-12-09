// REQUIRED_ARGS: -unittest

// https://issues.dlang.org/show_bug.cgi?id=15712
// extern(C) attribute inside extern(C) unittest is incorrectly ignored

extern(C):

unittest
{
	extern(C) static void foo() {}
	static assert(__traits(getLinkage, foo) == "C");
}
