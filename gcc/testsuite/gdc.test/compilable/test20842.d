// https://issues.dlang.org/show_bug.cgi?id=20842

struct A
{
    int i;
    @disable this(ref A);
}

A a = { i: 123 };

struct B
{
    int i;
    @disable this();
}

B b = { i: 123 };

union C
{
    int i;
    @disable this(ref C);
}

C c = { i: 123 };

union D
{
    int i;
    @disable this();
}

D d = { i: 123 };
