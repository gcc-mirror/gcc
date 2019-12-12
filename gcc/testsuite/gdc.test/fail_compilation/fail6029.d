struct A
{
    static A a;
    alias a this;
}

void foo(A a)
{
}

void main()
{
//  foo(A);    // Error: type A is not an expression
    int s = A; // Error: type A has no value + stack overflow
}
