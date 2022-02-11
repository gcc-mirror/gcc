// https://issues.dlang.org/show_bug.cgi?id=20868
// REQUIRED_ARGS: -preview=dip1000

void scoped (scope void delegate() dg)
{
    static void delegate()[] dgs;
    dgs ~= dg;  // error
}
