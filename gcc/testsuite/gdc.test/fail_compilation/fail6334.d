/*
TEST_OUTPUT:
---
fail_compilation/fail6334.d(13): Error: static assert:  `0` is false
fail_compilation/fail6334.d(11):        instantiated from here: `mixin T2!();`
---
*/

mixin template T1()
{
    mixin T2;                       //compiles if these lines
    mixin T2!(a, bb, ccc, dddd);    //are before T2 declaration
    mixin template T2() { static assert(0); }
}

void main()
{
    mixin T1;
}
