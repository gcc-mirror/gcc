// PR c++/19312

struct A {};

void foo(A a)
{
    throw (A)a;
}
