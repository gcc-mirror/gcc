// Some targets (e.g. those with "set_board_info needs_status_wrapper 1"
// in their dejagnu baseboard description) require that the status is
// final when exit is entered (or main returns), and not "overruled" by a
// destructor calling _exit.  It's not really worth it to handle that.
//
// Any platform that doesn't have proper __cxa_atexit support will also fail.
//
// { dg-do run }
// { dg-require-effective-target unwrapped }
// { dg-require-effective-target cxa_atexit }

#include <stdlib.h>
extern "C" void _exit (int);

static int cnt = 0;

class Foo2
{
	public:
		Foo2() {}
		~Foo2() { if (++cnt == 2) _exit (0); }
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
		~Foo1() { if (++cnt != 1) abort(); GetFoo2(); }
};

int main( int argc, const char* argv[] )
{
	static Foo1 anotherFoo;
	exit (1);
}

