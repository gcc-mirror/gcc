/*
TEST_OUTPUT:
---
fail_compilation/ctor_attr.d(26): Error: none of the overloads of `this` can construct a mutable object with argument types `(int)`
fail_compilation/ctor_attr.d(16):        Candidates are: `ctor_attr.S.this(int x) const`
fail_compilation/ctor_attr.d(18):                        `ctor_attr.S.this(string x)`
fail_compilation/ctor_attr.d(17):                        `this()(int x) shared`
fail_compilation/ctor_attr.d(28): Error: none of the overloads of `foo` are callable using a mutable object with argument types `(int)`
fail_compilation/ctor_attr.d(20):        Candidates are: `ctor_attr.S.foo(int x) immutable`
fail_compilation/ctor_attr.d(21):                        `ctor_attr.S.foo(string x)`
---
*/

struct S
{
    this(int x) const {}
    this()(int x) shared {}
    this(string x) {}

    void foo(int x) immutable  {}
    void foo(string x) {}
}

void f()
{
   auto s = S(1);
   S t;
   t.foo(1);
}
