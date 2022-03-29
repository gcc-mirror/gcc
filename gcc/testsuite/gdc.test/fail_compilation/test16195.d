/*
 * TEST_OUTPUT:
---
fail_compilation/test16195.d(14): Error: the `delete` keyword is obsolete
fail_compilation/test16195.d(14):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
---
 */


// https://issues.dlang.org/show_bug.cgi?id=16195

@safe pure nothrow @nogc void test(int* p)
{
    delete p;
}
