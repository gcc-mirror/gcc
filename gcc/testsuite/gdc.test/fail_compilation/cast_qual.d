/*
REQUIRED_ARGS: -preview=dip1000 -de
TEST_OUTPUT:
---
fail_compilation/cast_qual.d(15): Deprecation: cast from `const(int)` to `int` cannot be used as an lvalue in @safe code
fail_compilation/cast_qual.d(17): Deprecation: cast from `const(int)` to `int` cannot be used as an lvalue in @safe code
---
*/

@safe:

void main() {
    const int i = 3;
    int j = cast() i; // OK
    int* p = &cast() i; // this should not compile in @safe code
    *p = 4; // oops
    cast() i = 5; // NG
    auto q = &cast(const) j; // OK, int* to const int*
}
