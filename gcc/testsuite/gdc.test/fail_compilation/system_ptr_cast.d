/*
REQUIRED_ARGS: -preview=dip1000 -de
TEST_OUTPUT:
---
fail_compilation/system_ptr_cast.d(20): Deprecation: cast from `S*` to `int*` will become `@system` in a future release
fail_compilation/system_ptr_cast.d(20):        Source element type has unsafe bit patterns and target element type is mutable
fail_compilation/system_ptr_cast.d(24): Deprecation: cast from `int*` to `S*` will become `@system` in a future release
fail_compilation/system_ptr_cast.d(24):        Target element type has unsafe bit patterns
---
*/

struct S
{
    @system int i;
}

void main() @safe
{
    S s;
    auto p = cast(int*) &s;
    *p = 8;

    int i = 8;
    auto ps = cast(S*) &i;
}
