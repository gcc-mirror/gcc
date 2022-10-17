// https://issues.dlang.org/show_bug.cgi?id=17494
// REQUIRED_ARGS: -revert=dtorfields
struct S
{
    ~this() {}
}

class C
{
    S s;

    this() nothrow {}
}

// https://issues.dlang.org/show_bug.cgi?id=17505
struct Array
{
    int[] _payload;
    ~this()
    {
        import core.stdc.stdlib : free;
        free(_payload.ptr);
    }
}

class Scanner
{
    Array arr;
    this() @safe {}
}

// https://issues.dlang.org/show_bug.cgi?id=17506
struct TreeMap
{
    this() @disable;
    this(TTree tree) { this.tree = tree; }
    TTree tree;
}

struct TTree
{
    this() @disable;
    this(int foo) {}
    ~this() {}
}
