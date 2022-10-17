/*
TEST_OUTPUT:
---
fail_compilation/fail19915.d(8): Error: undefined identifier `c` in module `fail19915`
fail_compilation/fail19915.d(9): Error: template instance `fail19915.a!int` error instantiating
---
*/
class a (b) { align.c d; }
alias a!(int) e;
