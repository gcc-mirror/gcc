/*
TEST_OUTPUT:
---
fail_compilation/faildeleteaa.d(11): Error: The `delete` keyword is obsolete.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
---
*/

void main()
{
    int[int] aa = [1 : 2];
    delete aa[1];
}
