/*
TEST_OUTPUT:
---
fail_compilation/fail11355.d(28): Error: struct fail11355.A is not copyable because it is annotated with @disable
---
*/

T move(T)(ref T source)
{
    return T.init;          // Dummy rvalue
}

struct A
{
    ~this() {}
    @disable this(this);    // Prevent copying
}

struct B
{
    A a;
    alias a this;
}

void main()
{
    B b;
    A a = move(b);
}
