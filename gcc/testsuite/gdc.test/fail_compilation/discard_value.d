/*
TEST_OUTPUT:
---
fail_compilation/discard_value.d(24): Error: the result of the equality expression `3 is 3` is discarded
fail_compilation/discard_value.d(25): Error: the result of the equality expression `null !is null` is discarded
fail_compilation/discard_value.d(26): Error: the result of the equality expression `v == 0` is discarded
fail_compilation/discard_value.d(27): Error: the result of the equality expression `v == 0` is discarded
fail_compilation/discard_value.d(28): Error: `!__equals("", "")` has no effect
fail_compilation/discard_value.d(29): Error: the result of the equality expression `"" == ""` is discarded
fail_compilation/discard_value.d(30): Error: the result of the equality expression `fun().i == 4` is discarded
fail_compilation/discard_value.d(30):        note that `fun().i` may have a side effect
fail_compilation/discard_value.d(33): Error: the result of the equality expression `slice == slice[0..0]` is discarded
---
*/

struct S { int i; }

S fun() { return S(42); }

int v;

void main()
{
    3 is 3;
    null !is null;
    true && v == 0;
    true || v == 0;
    "" != "";
    "" == ""; // https://issues.dlang.org/show_bug.cgi?id=24359
    fun().i == 4; // https://issues.dlang.org/show_bug.cgi?id=12390

    int[] slice = [0, 1];
    slice == slice[0 .. 0];
}
