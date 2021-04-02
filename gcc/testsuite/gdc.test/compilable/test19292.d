// https://issues.dlang.org/show_bug.cgi?id=19292

mixin("enum a = ", 87, ";");
static assert(a == 87);

int test()
{
    mixin("enum x = ", 7, ";");
    return mixin("1", x, 2U);
}

void testit()
{
    static assert(test() == 172);
}
