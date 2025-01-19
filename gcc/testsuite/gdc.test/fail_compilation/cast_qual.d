/*
REQUIRED_ARGS: -preview=dip1000 -de
TEST_OUTPUT:
---
fail_compilation/cast_qual.d(17): Deprecation: using the result of a cast from `const(int)` to `int` as an lvalue will become `@system` in a future release
fail_compilation/cast_qual.d(19): Deprecation: using the result of a cast from `const(int)` to `int` as an lvalue will become `@system` in a future release
fail_compilation/cast_qual.d(25): Error: cast from `const(Object)` to `object.Object` is not allowed in a `@safe` function
fail_compilation/cast_qual.d(25):        Incompatible type qualifier
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

void test() {
    const Object co;
    auto o = cast() co;
}
