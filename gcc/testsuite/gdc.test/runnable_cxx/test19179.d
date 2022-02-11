// EXTRA_CPP_SOURCES: cpp19179.cpp

// https://issues.dlang.org/show_bug.cgi?id=19179

import core.stdc.stdio;

extern(C++) struct SmallStruct { int x = 10, y = 20; }

extern (C++)
SmallStruct test_small(SmallStruct s)
{
    printf("%d %d\n", s.x, s.y); // prints: invalid memory
    assert(s.x == 10);
    assert(s.y == 20);
    return s;
}

extern (C++)
void test_small_noret(SmallStruct s)
{
    printf("%d %d\n", s.x, s.y); // prints: 10 20
    assert(s.x == 10);
    assert(s.y == 20);
}

extern (C++) void cppmain();

int main()
{
    cppmain();
    return 0;
}
