/*
TEST_OUTPUT:
---
fail_compilation/ctfe10995.d(19): Error: cannot read uninitialized variable `a` in CTFE
fail_compilation/ctfe10995.d(25): Error: cannot read uninitialized variable `a` in CTFE
---
*/
struct T
{
    short a = void;
}

T foo()
{
    auto t = T.init;
    return t;
}

enum i = foo().a;

struct T2
{
    short a = void;
}
enum i2 = T2.init.a;
