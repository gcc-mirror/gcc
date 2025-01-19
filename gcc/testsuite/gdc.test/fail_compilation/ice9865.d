/*
EXTRA_FILES: imports/ice9865b.d
TEST_OUTPUT:
---
fail_compilation/ice9865.d(10): Error: alias `ice9865.Baz` recursive alias declaration
fail_compilation/ice9865.d(11):        error on member `ice9865.Foo.f`
---
*/

public import imports.ice9865b : Baz;
struct Foo { Baz f; }
