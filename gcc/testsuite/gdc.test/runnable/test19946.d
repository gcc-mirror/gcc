// https://issues.dlang.org/show_bug.cgi?id=19946

import core.stdc.stdio;

template Tests(TY)
{
    void test1()
    {
	TY[24] ba;
	ba[0..23] = 0x40;
	check1(ba[]);
    }

    void check1(TY[] ba)
    {
	foreach (i; 0 .. 23)
	{
	    //printf("ba[%d] = 0x%02x\n", i, ba[i]);
	    assert(ba[i] == 0x40);
	}
	assert(ba[23] == 0);
    }
}

int main()
{
    Tests!byte.test1();
    Tests!short.test1();
    Tests!int.test1();
    Tests!long.test1();

    Tests!ubyte.test1();
    Tests!ushort.test1();
    Tests!uint.test1();
    Tests!ulong.test1();
    return 0;
}
