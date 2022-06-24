// https://issues.dlang.org/show_bug.cgi?id=23169
// Issue 23169 - [DIP1000] Mangling does not distinguish return and return scope

struct Ptr
{
	int* impl;
	void* fun0() return scope {return impl;}
	void* fun1() scope return {return impl;}
	void* fun2() return {return &this;}
}

static assert(Ptr.fun0.mangleof == "_D9test231693Ptr4fun0MFNjNlZPv");
static assert(Ptr.fun1.mangleof == "_D9test231693Ptr4fun1MFNlNjZPv");
static assert(Ptr.fun2.mangleof == "_D9test231693Ptr4fun2MFNjZPv");
