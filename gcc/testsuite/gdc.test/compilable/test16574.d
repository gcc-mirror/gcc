// https://issues.dlang.org/show_bug.cgi?id=16574
template Recursive(T) if (is(T == class))
{
    // fails because T is still forward referenced
    // speculative determineSize must not set type to error
    static assert (!__traits(compiles, { new T; }));
    // known size of class
    static assert (is(typeof(T.init) == T));

    alias Recursive = T;
}

// must be resolvable
class C
{
    Recursive!C r;
}

template Recursive(T) if (is(T == struct))
{
    // fails because T is still forward referenced
    // speculative determineSize must not set type to error
    static assert (!__traits(compiles, { T t; }));
    // no size yet for struct
    static assert (!is(typeof(T.init)));

    alias Recursive = T*;
}

// must be resolvable
struct S
{
    Recursive!S r;
}
