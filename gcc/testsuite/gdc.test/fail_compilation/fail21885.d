// https://issues.dlang.org/show_bug.cgi?id=21885

/*
TEST_OUTPUT:
---
fail_compilation/fail21885.d(24): Error: struct `fail21885.Outer` is not copyable because field `i` is not copyable
---
*/

struct Outer
{
    Inner i;
}

struct Inner
{
    @disable this(this);
}

void main()
{
    Outer o1;
    Outer o2;
    o1 = o2;
}
