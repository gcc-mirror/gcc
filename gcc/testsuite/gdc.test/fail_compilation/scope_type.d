/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/scope_type.d(11): Deprecation: `scope` as a type constraint is deprecated.  Use `scope` at the usage site.
---
*/


scope class C { }
scope interface I { }
//scope struct S { }
