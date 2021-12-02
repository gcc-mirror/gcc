/*
EXTRA_FILES: ice18803b.d
TEST_OUTPUT:
---
fail_compilation/ice18803b.d(9): Error: (expression) expected following `static if`
fail_compilation/ice18803b.d(9): Error: declaration expected following attribute, not end of file
---
*/
void main() { import ice18803b; }
