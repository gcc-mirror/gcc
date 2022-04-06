// REQUIRED_ARGS: -checkaction=context -preview=dip1000

// Issue 22919 - [dip1000] -checkaction=context gives "assigned to `__assertOp2` with longer lifetime" (
// https://issues.dlang.org/show_bug.cgi?id=22919

@safe:
struct S
{
    int* p;
    ref S get() scope return {return this;}
}

void main()
{
    scope S arr = S();
    assert(arr == arr.get());
}
