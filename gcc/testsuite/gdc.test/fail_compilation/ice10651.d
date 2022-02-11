/*
TEST_OUTPUT:
---
fail_compilation/ice10651.d(11): Error: can only throw class objects derived from `Throwable`, not type `int*`
---
*/

void main()
{
    alias T = int;
    throw new T();  // ICE
}
