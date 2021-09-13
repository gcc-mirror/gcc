// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=100882
// { dg-additional-options "-fmain" }
// { dg-do run }

__gshared int counter = 0;
struct S100882
{
    this(int) { counter++; }
    ~this() { counter++; }
}
static S100882 s;
static this()
{
    s = cast(shared) S100882(0);
    assert(counter == 2);
}

auto test100882()
{
    return cast(shared) S100882(0);
}
