// https://issues.dlang.org/show_bug.cgi?id=22854
void main()
{
    uint loops = 0;
    static foreach (i; 0 .. 5)
    {
        static foreach (ch; SomeContainer().range)
            loops++;
    }
    assert(loops == 5 * 5);
}

struct SomeContainer
{
    SomeRange range() { return SomeRange(); }
    TypeWithDestructor data;
}

struct TypeWithDestructor { ~this() { } }

struct SomeRange
{
    int count = 5;
    int front() { return count; }
    bool empty() { return count <= 0; }
    void popFront() { count--; }
}
