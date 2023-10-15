/*
TEST_OUTPUT:
---
fail_compilation/fail18892.d(22): Error: no property `foo` for `a` of type `fail18892.MT`
fail_compilation/fail18892.d(13):        struct `MT` defined here
fail_compilation/fail18892.d(23): Error: no property `foo` for `MT` of type `fail18892.MT`
fail_compilation/fail18892.d(13):        struct `MT` defined here
---
*/

// https://issues.dlang.org/show_bug.cgi?id=18892

struct MT
{
    int _payload;
    alias _payload this;
}

void main()
{
    MT a;
    a.foo = 3;
    MT.foo = 3;
}
