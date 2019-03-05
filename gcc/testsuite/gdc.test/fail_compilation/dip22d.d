/*
REQUIRED_ARGS: -transition=import
TEST_OUTPUT:
---
fail_compilation/dip22d.d(12): Error: imports.dip22d.Foo at fail_compilation/imports/dip22d.d(3) conflicts with imports.dip22e.Foo at fail_compilation/imports/dip22e.d(3)
fail_compilation/dip22d.d(12): Error: module dip22d struct imports.dip22d.Foo is private
---
*/
import imports.dip22d;
import imports.dip22e;

Foo foo;
