/*
TEST_OUTPUT:
---
fail_compilation/diag12063.d(11): Error: no property `max` for type `Foo`, perhaps `import std.algorithm;` is needed?
fail_compilation/diag12063.d(14): Error: incompatible types for `(Foo()) + (1)`: `Bar` and `int`
---
*/

struct Foo {}

enum Bar : Foo
{
    a = Foo(),
    b
}
