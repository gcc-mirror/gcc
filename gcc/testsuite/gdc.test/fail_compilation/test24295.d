// REQUIRED_ARGS: -betterC

/*
TEST_OUTPUT:
---
fail_compilation/test24295.d(12): Error: expression `new int[](1$?:32=u|64=LU$)` allocates with the GC and cannot be used with switch `-betterC`
---
*/

void f()
{
   int[] overlaps = new int[1];
}
