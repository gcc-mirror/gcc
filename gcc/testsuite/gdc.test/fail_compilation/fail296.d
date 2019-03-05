/*
TEST_OUTPUT:
---
fail_compilation/fail296.d(10): Error: can only * a pointer, not a 'int'
---
*/

// Issue 3117 - dmd crash by *1

void main(){ *1; }
