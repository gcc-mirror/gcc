// https://issues.dlang.org/show_bug.cgi?id=22635
// opCast prevent calling destructor for const this

struct Foo
{
    bool opCast(T : bool)() const { assert(0); }
    ~this() {}
}

struct Bar
{
    const Foo foo;
}
