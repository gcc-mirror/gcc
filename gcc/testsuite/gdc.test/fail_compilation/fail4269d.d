/*
TEST_OUTPUT:
---
fail_compilation/fail4269d.d(9): Error: undefined identifier `Y`
---
*/

static if(is(typeof(X6.init))) {}
alias Y X6;

void main() {}
