/*
TEST_OUTPUT:
---
fail_compilation/fail8009.d(9): Error: template `fail8009.filter` cannot deduce function from argument types `!()(void)`
fail_compilation/fail8009.d(8):        Candidate is: `filter(R)(scope bool delegate(ref BAD!R) func)`
---
*/
void filter(R)(scope bool delegate(ref BAD!R) func) { }
void main() { filter(r => r); }

