/*
TEST_OUTPUT:
---
fail_compilation/diag18574.d(16): Error: `diag18574.Test`: multiple class inheritance is not supported. Use multiple interface inheritance and/or composition.
fail_compilation/diag18574.d(16):        `diag18574.Bar` has no fields, consider making it an `interface`
fail_compilation/diag18574.d(16):        `diag18574.Baz` has fields, consider making it a member of `diag18574.Test`
fail_compilation/diag18574.d(16): Error: `diag18574.Test`: base type must be `interface`, not `int`
---
*/
// https://issues.dlang.org/show_bug.cgi?id=18574

class Foo {}
class Bar {}
class Baz { int a; }

class Test : Foo, Bar, Baz, int {}
