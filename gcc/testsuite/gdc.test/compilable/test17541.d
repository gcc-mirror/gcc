/* EXTRA_SOURCES: imports/test17541_2.d imports/test17541_3.d
 */

// https://issues.dlang.org/show_bug.cgi?id=17541

module one;

import two;
import three;

struct BB
{
    enum MAX_NUM_FIBERS = 4096;

    TWOR!1 t;
    TT!(int) tt;
    auto foo()
    {
        tt.insertabcdefg(1);
    }
}

BB bb;

@nogc bar()
{
    bb.foo();
}
