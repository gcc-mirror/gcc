TEST_OUTPUT:
---
fail_compilation/test17284.d(1): Error: no identifier for declarator `TEST_OUTPUT`
fail_compilation/test17284.d(1): Error: declaration expected, not `:`
fail_compilation/test17284.d(12): Error: unmatched closing brace
---
*/

// https://issues.dlang.org/show_bug.cgi?id=17284

class C { }
union U { C c; int i; }

@safe void func(T)(T t)
{
        t.c = new C;
}

pragma(msg, typeof(func!U));

