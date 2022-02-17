/*
TEST_OUTPUT:
---
fail_compilation/fail4269e.d(10): Error: semicolon needed to end declaration of `Y` instead of `X5`
fail_compilation/fail4269e.d(10): Error: no identifier for declarator `X5`
---
*/

static if(is(typeof(X5.init))) {}
typedef Y X5;

void main() {}
