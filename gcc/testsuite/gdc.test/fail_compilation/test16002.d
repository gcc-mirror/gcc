/*
REQUIRED_ARGS:
PERMUTE_ARGS:
TEST_OUTPUT:
---
fail_compilation/test16002.d(100): Error: undefined identifier `imports.nonexistent`
fail_compilation/test16002.d(101): Error: undefined identifier `imports.nonexistent`
---
*/

module test.fail_compilation.test16002;

#line 100
enum A = is(imports.nonexistent == package);
enum B = is(imports.nonexistent == module);
