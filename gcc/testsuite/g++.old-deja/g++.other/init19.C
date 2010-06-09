// { dg-do run { xfail { { ! cxa_atexit } && { ! *-*-solaris2* } } } }
#include <stdlib.h>

#define assert(x) do { if (! (x)) abort(); } while (0)

int count = 0;

class A
{
public:
	explicit A(int i);
	~A();

	int i;

	A(const A&);
	A& operator=(const A&);
};

A::A(int i_)
	: i(i_)
{
}

A::~A()
{
	assert(++count == i);
	i = -1;
}

extern "C" {

void one()
{
	static bool second_time;
	if (second_time)
		assert(++count == 9);
	else
	{
		assert(++count == 1);
		second_time = true;
	}
	static A a(10);
	assert(a.i == 10);
}

void two()
{
	assert(++count == 7);
	static A a(8);
	assert(a.i == 8);
}

void three()
{
	assert(++count == 2);
	static A a(6);
	assert(a.i == 6);
}

void five()
{
	assert(++count == 4);
	static A a(5);
	assert(a.i == 5);
}

void four()
{
	assert(++count == 3);
	atexit(five);
}

}

A zero(11);

int main()
{
	one();
	atexit(one);
	atexit(two);
	three();
	atexit(four);
}
