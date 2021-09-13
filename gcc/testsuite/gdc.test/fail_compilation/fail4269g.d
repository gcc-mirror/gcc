/*
TEST_OUTPUT:
---
fail_compilation/fail4269g.d(10): Error: alias `fail4269g.Xg` cannot alias an expression `d[1]`
---
*/

int[2] d;
static if(is(typeof(Xg.init))) {}
alias d[1] Xg;

void main() {}
