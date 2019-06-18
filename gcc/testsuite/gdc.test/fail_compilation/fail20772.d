/*
TEST_OUTPUT:
---
fail_compilation/fail20772.d(20): Error: cannot pass types with postblits or copy constructors as variadic arguments
fail_compilation/fail20772.d(21): Error: cannot pass types with postblits or copy constructors as variadic arguments
---
*/
extern void variadic(...);

struct S20772
{
    int field;
    this(int) { }
    this(ref S20772 o) { }
}

void test()
{
    auto v = S20772(0);
    variadic(v,
             S20772(1));
}
