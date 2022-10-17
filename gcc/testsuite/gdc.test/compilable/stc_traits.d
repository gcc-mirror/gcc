// REQUIRED_ARGS: -preview=dip1000 -preview=in
/*
TEST_OUTPUT:
---
100 tuple()
101 tuple("return", "ref")
102 tuple("ref")
103 tuple()
104 tuple("ref")
105 tuple()
106 tuple()
107 tuple("ref")
108 tuple("ref")
109 tuple("ref")
110 tuple("ref")
111 tuple()
112 tuple("ref")
113 tuple("ref")
114 tuple("ref")
115 tuple("ref")
116 tuple()
117 tuple("ref")
118 tuple("ref")
119 tuple()
120 tuple("ref")
121 tuple()
122 tuple("ref")
123 tuple("in")
124 tuple("in")
m       tuple("ref")
m-mixin tuple("ref")
m       tuple("ref")
m-mixin tuple("ref")
m       tuple("ref")
m-mixin tuple("ref")
m       tuple("return", "ref")
m-mixin tuple("return", "ref")
m       tuple("ref")
m-mixin tuple("ref")
m       tuple("ref")
m-mixin tuple("ref")
m       tuple()
m-mixin tuple()
m       tuple("ref")
m-mixin tuple("ref")
m       tuple("ref")
m-mixin tuple("ref")
m       tuple("ref")
m-mixin tuple("ref")
m       tuple("ref")
m-mixin tuple("ref")
m       tuple("ref")
m-mixin tuple("ref")
m       tuple("ref")
m-mixin tuple("ref")
m       tuple("in")
m-mixin tuple("in")
---
*/

void func(int i) {}
void func(return ref bool i) {}
void func(ref float a, int b) {}
void get(T : int)(ref T t) {}
void get()(float t) {}
void get(T)(ref T[] t) {}
void funcautoi()(auto ref int i) {}
void funcauto(T)(auto ref T a) {}
void funcin(in int i) {}

struct Foo {
	void foo(int i) {}
	void foo(ref bool i) {}
	static void sfoo(ref int i) {}
}

struct FooT(T) {
	void foo(ref T i) {}
	static void sfoo(ref T i) {}
}

class Bar {
	void bar(int i) {}
	void bar(ref bool i) {}
	static void sbar(ref int i) {}
}

class BarT(T) {
	void bar(ref T i) {}
	static void sbar(ref T i) {}
}

int i;

template match(handlers...)
{
	static foreach(h; handlers)
	{
		// should give the same result
		pragma(msg, "m       ", __traits(getParameterStorageClasses, h(i), 0));
		pragma(msg, "m-mixin ", __traits(getParameterStorageClasses, mixin("h(i)"), 0));
	}

	enum match = (){};
}

void funcT(T)(ref T t) {}

void main() {
	int i;
	bool b;
	float f;
	int[] ia;
	Foo foo;
	FooT!int foot;
	Bar bar = new Bar;
	BarT!int bart = new BarT!int;

	ref int _foo(return ref const int* p, scope int* a, out int b, lazy int c);

	// From SPEC_RUNNABLE_EXAMPLE_COMPILE:
	int* p, a;
	int _b, c;

	static assert(__traits(getParameterStorageClasses, _foo(p, a, _b, c), 1)[0] == "scope");
	static assert(__traits(getParameterStorageClasses, _foo(p, a, _b, c), 2)[0] == "out");
	static assert(__traits(getParameterStorageClasses, _foo(p, a, _b, c), 3)[0] == "lazy");

#line 100
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, func(0), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, func(b), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, func(f, i), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, func(f, i), 1));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, get(i), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, get(0.0), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, get(f), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, get(ia), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, mixin("get(i)"), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, Foo.sfoo(i), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, FooT!int.sfoo(i), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, foo.foo(0), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, foo.foo(b), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, foot.foo(i), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, Bar.sbar(i), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, BarT!int.sbar(i), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, bar.bar(0), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, bar.bar(b), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, bart.bar(i), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, funcautoi(10), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, funcautoi(i), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, funcauto(10), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, funcauto(i), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, funcin(1), 0));
	pragma(msg, __LINE__, " ", __traits(getParameterStorageClasses, funcin(i), 0));

	cast(void) match!(
		function(ref int i) => true,
		delegate(ref int i) => true,
		(ref int i) => true,
		(return ref int i) => &i,
		get,
		funcT,
		(int i) => true,
		FooT!int.sfoo,
		Foo.sfoo,
		BarT!int.sbar,
		Bar.sbar,
		funcautoi,
		funcauto,
		funcin,
	);
}
