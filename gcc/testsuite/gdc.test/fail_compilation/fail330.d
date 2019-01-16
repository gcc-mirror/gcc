/*
TEST_OUTPUT:
---
fail_compilation/fail330.d(9): Error: variable fail330.fun.result cannot modify result 'result' in contract
---
*/

int fun()
out(result) { result = 2; }
body { return 1; }
