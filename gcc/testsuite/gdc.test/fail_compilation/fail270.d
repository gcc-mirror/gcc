/*
TEST_OUTPUT:
---
fail_compilation/fail270.d(12): Error: string slice `[1 .. 0]` is out of bounds
fail_compilation/fail270.d(12): Error: mixin `fail270.Tuple!int.Tuple.Tuple!()` error instantiating
fail_compilation/fail270.d(14): Error: mixin `fail270.Tuple!int` error instantiating
---
*/

struct Tuple(TList...)
{
    mixin .Tuple!((TList[1 .. $])) tail;
}
mixin Tuple!(int);
