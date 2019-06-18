/*
TEST_OUTPUT:
---
fail_compilation/fail2361.d(14): Deprecation: The `delete` keyword has been deprecated.  Use `object.destroy()` (and `core.memory.GC.free()` if applicable) instead.
fail_compilation/fail2361.d(14): Error: cannot modify `immutable` expression `c`
---
*/

class C {}

void main()
{
    immutable c = new immutable(C);
    delete c;
}
