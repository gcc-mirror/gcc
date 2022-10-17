/*
Previous implementation raised errors because it was not
aware of the special treatment for extern(C) member funtions:

Member functions receive D name mangling...
Some arguments in favour of this inconsistencies
- static => useful for callbacks
- non-static => doesn't exists in C anyways
*/

extern(C) struct ExternC
{
	void foo() {}
	void foo(int) {}

	static void bar() {}
	static void bar(int) {}
}

#line 100
void main()
{
	ExternC.bar();
	ExternC.bar(1);

	ExternC c;
	c.foo();
	c.foo(1);
}
