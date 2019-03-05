/*
TEST_OUTPUT:
---
fail_compilation/fail114.d(12): Error: forward reference to 'funcA'
---
*/

// Issue 371 - ICE on mutual recursive typeof in function declarations

void funcA(typeof(&funcB) p) {}

void funcB(typeof(&funcA) p) {}
