
// https://issues.dlang.org/show_bug.cgi?id=23618

import core.stdc.stdio;

uint test1()
{
    ushort ee = 1028;
    ee <<= 5U;
    ee >>= 5U;
    assert(ee == 1028);
    //printf("%x, %d\n", ee, ee);
    return ee;
}

uint test2()
{
    ubyte ee = 4;
    ee <<= 5U;
    ee >>= 5U;
    //printf("%x, %d\n", ee, ee);
    assert(ee == 4);
    return ee;
}

void main()
{
    test1();
    test2();
}
