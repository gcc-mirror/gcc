/*
EXTRA_FILES: imports/ice9865b.d
TEST_OUTPUT:
---
fail_compilation/ice9865.d(9): Error: alias `ice9865.Baz` recursive alias declaration
---
*/

public import imports.ice9865b : Baz;
struct Foo { Baz f; }
