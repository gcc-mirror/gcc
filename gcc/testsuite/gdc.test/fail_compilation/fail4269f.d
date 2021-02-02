/*
TEST_OUTPUT:
---
fail_compilation/fail4269f.d(9): Error: `alias X16 = X16;` cannot alias itself, use a qualified name to create an overload set
---
*/

static if(is(typeof(X16))) {}
alias X16 X16;

void main() {}
