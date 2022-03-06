// REQUIRED_ARGS: -de
/* TEST_OUTPUT:
---
fail_compilation/fail17906.d(11): Error: The `delete` keyword is obsolete.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
---
*/
// https://issues.dlang.org/show_bug.cgi?id=18647
deprecated void main ()
{
    Object o = new Object;
    delete o;
}
