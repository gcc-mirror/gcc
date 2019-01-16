/*
TEST_OUTPUT:
---
fail_compilation/fail11532.d(17): Error: cannot pass static arrays to extern(C) vararg functions
fail_compilation/fail11532.d(18): Error: cannot pass dynamic arrays to extern(C) vararg functions
fail_compilation/fail11532.d(19): Error: cannot pass static arrays to extern(C++) vararg functions
fail_compilation/fail11532.d(20): Error: cannot pass dynamic arrays to extern(C++) vararg functions
---
*/

extern(C) void cvararg(int, ...);
extern(C++) void cppvararg(int, ...);

void main()
{
    int[2] arr = [0x99999999, 0x88888888];
    cvararg(0, arr);
    cvararg(0, arr[]);
    cppvararg(0, arr);
    cppvararg(0, arr[]);
}
