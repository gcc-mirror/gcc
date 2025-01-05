// See also: fail20000.d
/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/cpp_cast.d(19): Error: cast from `cpp_cast.I` to `cpp_cast.C` not allowed in safe code
fail_compilation/cpp_cast.d(19):        No dynamic type information for extern(C++) classes
fail_compilation/cpp_cast.d(21): Deprecation: cast from `cpp_cast.C` to `cpp_cast.D` not allowed in safe code
fail_compilation/cpp_cast.d(21):        No dynamic type information for extern(C++) classes
---
*/
extern(C++) interface I { void f(); }
extern(C++) class C : I { void f() { } }
extern(C++) class D : C { }

void main() @safe
{
    I i;
    C c = cast(C) i; // unsafe
    i = cast(I) c; // OK
    c = cast(D) c; // reinterpret cast
    c = cast(C) new D; // OK
}
