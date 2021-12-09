/*
TEST_OUTPUT:
---
fail_compilation/fail19099.d(26): Error: cannot modify struct instance `a` of type `A` because it contains `const` or `immutable` members
---
*/

struct B
{
    this(this) {}
    ~this() {}
    int a;
}

struct A
{
    B b;
    immutable int a;
    this(int b) { a = b;}
}

void main()
{
    A a = A(2);
    A b = A(3);
    a = b;
}
