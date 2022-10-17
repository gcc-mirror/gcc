struct A
{
    int x;
}

struct B
{
    A a, b;
}
static assert(B(A(1), A(1)) != B(A(1), A(2))); // Works

struct C
{
    A a, b;
    alias a this;
}
static assert(C(A(1), A(1)) != C(A(1), A(2))); // Fails!
