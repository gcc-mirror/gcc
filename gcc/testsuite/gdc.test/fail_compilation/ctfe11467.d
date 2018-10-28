/*
TEST_OUTPUT:
---
fail_compilation/ctfe11467.d(15): Error: overlapping slice assignment [0..4] = [1..5]
fail_compilation/ctfe11467.d(24):        called from here: test11467a()
fail_compilation/ctfe11467.d(24):        while evaluating: `static assert(test11467a())`
fail_compilation/ctfe11467.d(21): Error: overlapping slice assignment [1..5] = [0..4]
fail_compilation/ctfe11467.d(25):        called from here: test11467b()
fail_compilation/ctfe11467.d(25):        while evaluating: `static assert(test11467b())`
---
*/
int test11467a()
{
    auto a = [0, 1, 2, 3, 4];
    a[0 .. 4] = a[1 .. 5];
    return 1;
}
int test11467b()
{
    auto a = [0, 1, 2, 3, 4];
    a[1 .. 5] = a[0 .. 4];
    return 1;
}
static assert(test11467a());
static assert(test11467b());

/*
TEST_OUTPUT:
---
fail_compilation/ctfe11467.d(41): Error: overlapping slice assignment [0..4] = [1..5]
fail_compilation/ctfe11467.d(50):        called from here: test11467c()
fail_compilation/ctfe11467.d(50):        while evaluating: `static assert(test11467c())`
fail_compilation/ctfe11467.d(47): Error: overlapping slice assignment [1..5] = [0..4]
fail_compilation/ctfe11467.d(51):        called from here: test11467d()
fail_compilation/ctfe11467.d(51):        while evaluating: `static assert(test11467d())`
---
*/
int test11467c()
{
    auto a = "abcde".dup;
    a[0 .. 4] = a[1 .. 5];
    return 1;
}
int test11467d()
{
    auto a = "abcde".dup;
    a[1 .. 5] = a[0 .. 4];
    return 1;
}
static assert(test11467c());
static assert(test11467d());

