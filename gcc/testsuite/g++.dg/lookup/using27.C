// PR c++/26256
// { dg-do run }

struct A
{
    typedef int type;
};

struct B
{
    typedef double type;
};

struct C : A, B
{
    using A::type;
    type d;

    void f()
    {
	type e;
	if (sizeof (type) != sizeof (A::type))
	    __builtin_abort();
    }

    void g();
};

void C::g()
{
    type x;
    if (sizeof (type) != sizeof (A::type))
	__builtin_abort();
}

int main ()
{
    if (sizeof (C::type) != sizeof (A::type))
	__builtin_abort();

    if (sizeof (C::d) != sizeof (A::type))
	__builtin_abort();

    C::type x;
    C c;
    c.f();
    c.g();
}
