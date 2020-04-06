// https://issues.dlang.org/show_bug.cgi?id=7886

struct A {
	static assert (__traits(derivedMembers, A).length == 0);
}
