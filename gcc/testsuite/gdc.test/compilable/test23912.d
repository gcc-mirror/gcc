// https://issues.dlang.org/show_bug.cgi?id=23912
// REQUIRED_ARGS: -preview=dip1000

struct Test
{
    string val;

    this(return scope string val) scope @safe {}
    ~this() scope @safe {}
}

void giver(scope string input) @safe
{
    accepts(Test(input));
}

void accepts(scope Test test) @safe {}
