/*
TEST_OUTPUT:
---
fail_compilation/scope_class.d(12): Error: functions cannot return `scope scope_class.C`
---
*/



scope class C { int i; }    // Notice the use of `scope` here

C increment(C c)
{
    c.i++;
    return c;
}

void main()
{
    scope C c = new C();
    c.increment();
}
