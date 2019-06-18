/*
REQUIRED_ARGS: -betterC -unittest
PERMUTE_ARGS:
EXTRA_SOURCES: extra-files/moreBettercUnittests.d
*/

import moreBettercUnittests;

unittest
{
    sum |= 0x1;
}

unittest
{
    sum |= 0x10;
}

extern (C) int main()
{
    uint count;

    static foreach (alias unit; __traits(getUnitTests, bettercUnittest))
    {
        unit();
        count++;
    }

    static foreach (alias unit; __traits(getUnitTests, moreBettercUnittests))
    {
        unit();
        count++;
    }

    assert(count == 4);
    assert(sum == 0x1111);
    return sum == 0x1111 ? 0 : 1;
}
