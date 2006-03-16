// { dg-do run }

#include <stdlib.h>

static int cnt = 0;

class Foo2
{
	public:
		Foo2() {};
		~Foo2() { if (++cnt == 2) _Exit (0); };
};

static Foo2& GetFoo2()
{
	static Foo2 foo2;
	return foo2;
}

class Foo1
{
	public:
		Foo1() {}
		~Foo1() { if (++cnt != 1) abort(); GetFoo2(); };
};

int main( int argc, const char* argv[] )
{
	static Foo1 anotherFoo;
	exit (1);
}

