// PR c++/26256
// { dg-do run }

struct A
{
    int next;
};

struct B
{
    int next;
};

struct C : public A, public B
{
    using A::next;
};

void foo(C& c) { c.next = 42; }

int main()
{
    C c;
    foo (c);
    c.B::next = 12;
    if (c.next != 42 || c.A::next != 42 || c.B::next != 12)
	__builtin_abort();
}
