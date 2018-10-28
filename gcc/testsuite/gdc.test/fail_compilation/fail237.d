/*
TEST_OUTPUT:
---
fail_compilation/fail237.d(11): Error: undefined identifier `a` in module `fail237`
fail_compilation/fail237.d(11):        while evaluating: `static assert(module fail237.a!().b)`
---
*/

// Issue 581 - Error message w/o line number in dot-instantiated template

static assert(.a!().b);
