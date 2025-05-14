/*
TEST_OUTPUT:
---
fail_compilation/fail11751.d(10): Error: missing exponent
fail_compilation/fail11751.d(10): Error: semicolon expected following auto declaration, not `ABC`
fail_compilation/fail11751.d(10): Error: variable name expected after type `ABC`, not `;`
---
*/

auto x = 0x1.FFFFFFFFFFFFFpABC;
