/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/dip22a.d(21): Deprecation: imports.dip22a.Klass.bar is not visible from module dip22a
fail_compilation/dip22a.d(21): Error: class imports.dip22a.Klass member `bar` is not accessible
fail_compilation/dip22a.d(22): Deprecation: imports.dip22a.Struct.bar is not visible from module dip22a
fail_compilation/dip22a.d(22): Error: struct imports.dip22a.Struct member `bar` is not accessible
fail_compilation/dip22a.d(23): Error: imports.dip22a.bar is not visible from module dip22a
fail_compilation/dip22a.d(23): Error: function `imports.dip22a.bar` is not accessible from module `dip22a`
fail_compilation/dip22a.d(24): Error: imports.dip22a.Template!int.bar is not visible from module dip22a
fail_compilation/dip22a.d(24): Error: function `imports.dip22a.Template!int.bar` is not accessible from module `dip22a`
fail_compilation/dip22a.d(25): Deprecation: imports.dip22a.bar is not visible from module dip22a
fail_compilation/dip22a.d(25): Error: function `imports.dip22a.bar` is not accessible from module `dip22a`
---
*/
import imports.dip22a;

void test()
{
    new Klass().bar();
    Struct().bar();
    imports.dip22a.bar();
    Template!int.bar();
    12.bar();
}
