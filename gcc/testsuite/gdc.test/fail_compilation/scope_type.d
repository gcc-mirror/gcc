/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/scope_type.d(11): Error: `scope` as a type constraint is obsolete.  Use `scope` at the usage site.
---
*/


scope class C { }
scope interface I { }
//scope struct S { }
