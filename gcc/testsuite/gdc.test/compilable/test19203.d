//https://issues.dlang.org/show_bug.cgi?id=19203
struct BoolWithErr {
    bool b;
    string error;
    alias b this;
}

struct Foo {
    int popBack() { return 0; }
}

struct Bar {}

template hasPopBack(T) {
    static if (!is(typeof(T.init.popBack)))
        enum hasPopBack = BoolWithErr(false, T.stringof~" does not have popBack");
    else
        enum hasPopBack = BoolWithErr(true,"");
}

void test()
{
    static assert( hasPopBack!Foo);
    static assert(!hasPopBack!Bar);
    static assert( hasPopBack!Foo && !hasPopBack!Bar);
}
