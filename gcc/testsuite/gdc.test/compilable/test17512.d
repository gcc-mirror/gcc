// https://issues.dlang.org/show_bug.cgi?id=17512

struct A
{
    int _value;

    bool _hasValue;

    auto ref getOr(int alternativeValue)
    {
        return _hasValue ? _value : alternativeValue;
    }
}

A a;

// https://issues.dlang.org/show_bug.cgi?id=18661

struct S0(T)
{
    int a;
    auto ref immutable(int) getA() { return a; }
}

alias B = S0!int;

