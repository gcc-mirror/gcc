/*
TEST_OUTPUT:
---
fail_compilation/fail2361.d(14): Error: the `delete` keyword is obsolete
fail_compilation/fail2361.d(14):        use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead
---
*/

class C {}

void main()
{
    immutable c = new immutable(C);
    delete c;
}
