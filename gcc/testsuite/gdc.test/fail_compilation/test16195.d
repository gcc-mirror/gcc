/*
 * TEST_OUTPUT:
---
fail_compilation/test16195.d(13): Error: delete p is not @safe but is used in @safe function test
---
 */


// https://issues.dlang.org/show_bug.cgi?id=16195

@safe pure nothrow @nogc void test(int* p)
{
    delete p;
}
