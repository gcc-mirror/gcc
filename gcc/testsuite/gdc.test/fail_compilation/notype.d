struct S(int var = 3) {
    int a;
}
S s;

alias A() = int;
A a;

enum e() = 5;
e val;

interface I()
{
}
I i;

template t()
{
}
t tv;

/*
TEST_OUTPUT:
---
fail_compilation/notype.d(4): Error: template struct `notype.S(int var = 3)` is used as a type without instantiation; to instantiate it use `S!(arguments)`
fail_compilation/notype.d(7): Error: template `notype.A()` is used as a type
fail_compilation/notype.d(10): Error: template `notype.e()` is used as a type
fail_compilation/notype.d(15): Error: template interface `notype.I()` is used as a type without instantiation; to instantiate it use `I!(arguments)`
fail_compilation/notype.d(20): Error: template `notype.t()` is used as a type
---
*/
