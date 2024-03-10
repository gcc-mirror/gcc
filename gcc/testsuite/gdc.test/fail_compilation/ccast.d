/*
TEST_OUTPUT:
---
fail_compilation/ccast.d(11): Error: C style cast illegal, use `cast(byte)i`
fail_compilation/ccast.d(24): Error: C style cast illegal, use `cast(foo)5`
fail_compilation/ccast.d(26): Error: C style cast illegal, use `cast(void*)5`
---
*/

int i;
byte b = (byte)i;

void bar(int x);

void main()
{
    (&bar)(5); // ok
    auto foo = &bar;
    (foo = foo)(5); // ok
    (*foo)(5); // ok

    (foo)(5); // ok
    (bar)(5); // ok
    (foo)5;

    (void*)5;
    (void*)(5); // semantic implicit cast error
}
