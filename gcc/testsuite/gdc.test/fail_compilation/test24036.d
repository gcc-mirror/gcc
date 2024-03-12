/*
https://issues.dlang.org/show_bug.cgi?id=24036
Issue 24036 - assert message in CTFE becomes `['m', 'e', 's', 's', 'a', 'g', 'e'][0..7]` if produced using std.format.format

TEST_OUTPUT:
---
fail_compilation/test24036.d(19): Error: message
fail_compilation/test24036.d(21):        called from here: `(*function () pure nothrow @safe => 42)()`
---
*/

auto format()
{
    return ['m', 'e', 's', 's', 'a', 'g', 'e'][0 .. 7];
}

immutable ctfeThing = ()
{
    assert(0, format());
    return 42;
}();
