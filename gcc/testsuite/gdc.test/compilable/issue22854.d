// https://issues.dlang.org/show_bug.cgi?id=22854
void test22854()
{
    static foreach (ch; SomeContainer().range) { }
}

struct SomeContainer
{
    SomeRange range() { return SomeRange(); }
    TypeWithDestructor data;
}

struct TypeWithDestructor { ~this() { } }

struct SomeRange
{
    int front() { return 0; }
    bool empty() { return true; }
    void popFront() { }
}
