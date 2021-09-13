/*
TEST_OUTPUT:
---
fail_compilation/fail4206.d(9): Error: initializer must be an expression, not `s`
---
*/

struct s {}
enum var = s;

void main() {}
