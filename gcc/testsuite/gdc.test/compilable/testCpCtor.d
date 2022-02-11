// https://issues.dlang.org/show_bug.cgi?id=19870
struct T
{
    int i;
    this(ref return scope inout typeof(this) src)
        inout @safe pure nothrow @nogc
    {
        i = src.i;
    }
}

struct S
{
    T t;
}

void main()
{
    T a;
    S b = S(a);
}
