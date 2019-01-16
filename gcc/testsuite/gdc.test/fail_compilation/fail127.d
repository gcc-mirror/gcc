/*
TEST_OUTPUT:
---
fail_compilation/fail127.d(9): Error: a struct is not a valid initializer for a char[][]
fail_compilation/fail127.d(10): Error: a struct is not a valid initializer for a string[]
---
*/

char[][] Level2Text1 = {"LOW", "MEDIUM", "HIGH"};
string[] Level2Text2 = {"LOW", "MEDIUM", "HIGH"};    // for D2
