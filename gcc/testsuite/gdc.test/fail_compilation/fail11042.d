/*
TEST_OUTPUT:
---
fail_compilation/fail11042.d(8): Error: undefined identifier `error`, did you mean class `Error`?
fail_compilation/fail11042.d(9): Error: undefined identifier `error`, did you mean class `Error`?
---
*/
static if ({ return true  || error; }()) {} // NG
static if ({ return false && error; }()) {} // NG
