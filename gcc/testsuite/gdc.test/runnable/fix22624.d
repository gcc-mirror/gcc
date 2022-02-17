// https://issues.dlang.org/show_bug.cgi?id=22624
// EXTRA_FILES: imports/imp22624.c

import core.stdc.stdio;
import imports.imp22624;

struct S
{
    B b;
    ulong y = 0x1234_0000_5678;
}

int main()
{
    S s;
    //printf("%llx\n", s.y);
    assert(s.y == 0x1234_0000_5678);
    return 0;
}
