/*
TEST_OUTPUT:
---
fail_compilation/fail2361.d(13): Error: The `delete` keyword is obsolete.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
---
*/

class C {}

void main()
{
    immutable c = new immutable(C);
    delete c;
}
