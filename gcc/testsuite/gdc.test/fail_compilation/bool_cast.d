/*
REQUIRED_ARGS: -de -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/bool_cast.d(17): Deprecation: cast from `ubyte[]` to `bool[]` will become `@system` in a future release
fail_compilation/bool_cast.d(17):        Source element may have bytes which are not 0 or 1
fail_compilation/bool_cast.d(22): Deprecation: cast from `int*` to `bool*` will become `@system` in a future release
fail_compilation/bool_cast.d(22):        Source element may have bytes which are not 0 or 1
fail_compilation/bool_cast.d(24): Deprecation: cast from `bool*` to `byte*` will become `@system` in a future release
fail_compilation/bool_cast.d(24):        Target element could be assigned a byte which is not 0 or 1
---
*/

void main() @safe
{
    ubyte[] a = [2, 4];
    auto b = cast(bool[]) a; // reinterprets a's data
    auto c = cast(bool[]) [2, 4]; // OK, literal cast applies to each element
    auto d = cast(const(byte)[]) b; // OK, result's elements are const

    int i = 2;
    auto p = cast(bool*) &i;
    bool v;
    auto bp = cast(byte*) &v;
    *bp = 2; // v is now invalid
}
