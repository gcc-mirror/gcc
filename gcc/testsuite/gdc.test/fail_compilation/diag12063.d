/*
TEST_OUTPUT:
---
fail_compilation/diag12063.d(21): Error: cannot check `diag12063.Bar.b` value for overflow
fail_compilation/diag12063.d(18): Error: no property `max` for type `Foo`, perhaps `import std.algorithm;` is needed?
fail_compilation/diag12063.d(21): Error: cannot generate value for `diag12063.Bar.b`
fail_compilation/diag12063.d(21): Error: operator `+` is not defined for type `Bar`
fail_compilation/diag12063.d(16):        perhaps overload the operator with `auto opBinary(string op : "+")(int rhs) {}`
fail_compilation/diag12063.d(31): Error: cannot check `diag12063.b` value for overflow
fail_compilation/diag12063.d(31): Error: no operator `==` for type `S`
fail_compilation/diag12063.d(24):        perhaps overload it with `bool opEquals(int other) const {}`
fail_compilation/diag12063.d(40): Error: enum member `diag12063.d` initialization with `__anonymous.c+1` causes overflow for type `Q`
---
*/

struct Foo {}

enum Bar : Foo
{
    a = Foo(),
    b // no max, can't +1
}

struct S {
    S opBinary(string s: "+")(int) => this;
    enum max = 1; // wrong type
}

enum {
    a = S(),
    b // can't do S() == 1
}

struct Q {
    enum max = Q();
}

enum {
    c = Q(),
    d // overflow detected
}

struct R {
    int i;
    R opBinary(string s: "+")(int) => this;
    enum max = R(1);
}

enum ER
{
    e = R(),
    f // OK
}
