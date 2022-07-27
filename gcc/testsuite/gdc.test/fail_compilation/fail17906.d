// REQUIRED_ARGS: -de
/* TEST_OUTPUT:
---
fail_compilation/fail17906.d(12): Error: the `delete` keyword is obsolete
fail_compilation/fail17906.d(12):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
---
*/
// https://issues.dlang.org/show_bug.cgi?id=18647
deprecated void main ()
{
    Object o = new Object;
    delete o;
}
