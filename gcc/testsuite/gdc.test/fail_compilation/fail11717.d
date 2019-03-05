/*
TEST_OUTPUT:
---
fail_compilation/fail11717.d(13): Error: cannot interpret array literal expression [1, 2, 3, 4] + [1, 2, 3, 4] at compile time
---
*/

// https://issues.dlang.org/show_bug.cgi?id=11717

enum int[4] A = [1,2,3,4];
enum int[4] B = [1,2,3,4];
// Internal Compiler Error: non-constant value [1, 2, 3, 4]
enum int[4] C = A[] + B[];

