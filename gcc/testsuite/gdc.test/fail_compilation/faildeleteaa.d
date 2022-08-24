/*
TEST_OUTPUT:
---
fail_compilation/faildeleteaa.d(12): Error: the `delete` keyword is obsolete
fail_compilation/faildeleteaa.d(12):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
---
*/

void main()
{
    int[int] aa = [1 : 2];
    delete aa[1];
}
