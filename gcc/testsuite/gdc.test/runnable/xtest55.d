// RUNNABLE_PHOBOS_TEST
// PERMUTE_ARGS:

import core.memory, std.stdio;

Stuff* stuff1;

struct Stuff {
    uint num;
}

int main()
{
    stuff1 = new Stuff;
    stuff1.num = 1;
    auto bar = new byte[1024 * 1024];
    auto stuff2 = new Stuff;
    stuff2.num = 2;
    writeln(stuff1, "\t", stuff2);  // Same address.
    assert(stuff1 != stuff2);
    writeln(stuff1.num, "\t", stuff2.num);  // Both 2.
    assert(stuff1.num == 1);
    return 0;
}

