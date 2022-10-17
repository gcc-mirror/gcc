/* REQUIRED_ARGS: -preview=fieldwise
TEST_OUTPUT:
---
fail_compilation/test16284.d(24): Error: reinterpretation through overlapped field `s` is not allowed in CTFE
fail_compilation/test16284.d(27):        called from here: `test()`
fail_compilation/test16284.d(27):        while evaluating: `static assert(test())`
---
*/

// https://issues.dlang.org/show_bug.cgi?id=16284

struct S {}

struct T
{
    union {int i; S s;}
    this(uint dummy) { s = S.init; }
}

bool test()
{
    auto t1 = T(0);
    auto t2 = T(0);
    return t1 == t2;
}

static assert(test());
