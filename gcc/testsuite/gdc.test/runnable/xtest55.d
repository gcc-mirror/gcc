// PERMUTE_ARGS:

import core.memory, core.stdc.stdio;

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
    printf("%p\t%p\n", stuff1, stuff2);     // Same address.
    assert(stuff1 != stuff2);
    printf("%d\t%d\n", stuff1.num, stuff2.num);  // Both 2.
    assert(stuff1.num == 1);
    return 0;
}
