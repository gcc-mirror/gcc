// https://issues.dlang.org/show_bug.cgi?id=22410

alias A(T...) = T;

void fun0(const A!(int, string) x = A!(1, "asdf"))
{
    static assert(is(typeof(x) == A!(const int, const string)));
}

void fun1(const A!(immutable int, string) x = A!(1, "asdf"))
{
    static assert(is(typeof(x) == A!(immutable int, const string)));
}

void fun2(shared A!(int, string) x = A!(1, "asdf"))
{
    static assert(is(typeof(x) == A!(shared int, shared string)));
}

void fun3(shared const A!(int, string) x = A!(1, "asdf"))
{
    static assert(is(typeof(x) == A!(shared const int, shared const string)));
}

void fun4(inout A!(int, const string) x = A!(1, "asdf"))
{
    static assert(is(typeof(x) == A!(inout int, inout const string)));
}

void fun5(ref const A!(int, string) x = A!(1, "asdf"))
{
    static assert(is(typeof(x) == A!(const int, const string)));
    static assert(__traits(isRef, x[0]) && __traits(isRef, x[1]));
}

// Implicitly conversion is also fixed, for example:
// from (ulong, double) to (int, float)

// Integral narrowing here, ulong(uint.max + 1UL) would fail.
void fun10(A!(uint, float) x = A!(ulong(uint.max), 3.14))
{
    static assert(is(typeof(x) == A!(uint, float)));
}

void fun11(A!(int, double) x = A!(byte(1), 2.5f))
{
    static assert(is(typeof(x) == A!(int, double)));
}

void fun12(A!(byte, float) x = A!(1, 'a'))
{
    static assert(is(typeof(x) == A!(byte, float)));
}

A!(const int, shared char) tup = A!(1, 'a');
void fun13(A!(byte, float) x = tup)
{
    static assert(is(typeof(x) == A!(byte, float)));
}
