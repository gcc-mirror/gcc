// https://issues.dlang.org/show_bug.cgi?id=19482

alias AliasSeq(T...) = T;

extern (C++, "cppns")
@("asd", 123)
private
deprecated
immutable
static foreach (i; 0 .. 1)
{
    static assert(is(typeof(i) == int));
    static assert(__traits(getLinkage, i) == "D");
    static assert(__traits(isDeprecated, i) == false);
    static assert(__traits(getAttributes, i).length == 0);
    static assert(__traits(getCppNamespaces, i).length == 0);
    static assert(__traits(getVisibility, i) == "public");

    extern int x;
    static assert(is(typeof(x) == immutable int));
    static assert(__traits(getLinkage, x) == "C++");
    static assert(__traits(isDeprecated, x) == true);
    static assert(__traits(getAttributes, x) == AliasSeq!("asd", 123));
    static assert(__traits(getCppNamespaces, x) == AliasSeq!("cppns"));
    static assert(__traits(getVisibility, x) == "private");
}

struct S
{
    @disable static foreach (j; 0 .. 1)
    {
        int y;
        static assert(__traits(isDisabled, j) == false);
        static assert(__traits(isDisabled, S.y) == true);
    }
}

const
static foreach (i, v; ['a'])
{
    static assert(is(typeof(i) == size_t));
    static assert(is(typeof(v) == char));
}

const
static foreach (i, s, f; Range())
{
    static assert(is(typeof(i) == int));
    static assert(is(typeof(s) == string));
    static assert(is(typeof(f) == float));
}

struct Range
{
    int i;
    auto front()
    {
        return Tup!(int, string, float)(123, "asd", 3.14f);
    }
    bool empty() { return i > 0; }
    void popFront() { ++i; }
}

struct Tup(T...)
{
    T fields;
    alias fields this;
}
