/*
TEST_OUTPUT:
---
fail_compilation/fail20771.d(19): Error: cannot pass types with postblits or copy constructors as variadic arguments
fail_compilation/fail20771.d(20): Error: cannot pass types with postblits or copy constructors as variadic arguments
---
*/
extern void variadic(...);

struct S20771
{
    int field;
    this(this) { }
}

void test()
{
    auto v = S20771(0);
    variadic(v,
             S20771(1));
}
