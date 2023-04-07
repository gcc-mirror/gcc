//https://issues.dlang.org/show_bug.cgi?id=23607
/*
TEST_OUTPUT:
---
fail_compilation/test23607.d(15): Error: template `to(T)()` does not have property `bad`
fail_compilation/test23607.d(16): Error: template `to(T)()` does not have property `bad`
---
*/

template to(T)
{
    void to(T)(){}
}

alias comb = to!int.bad!0;
auto combe = to!int.bad!0;
