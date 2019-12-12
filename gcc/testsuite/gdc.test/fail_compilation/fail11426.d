/*
TEST_OUTPUT:
---
fail_compilation/fail11426.d(15): Error: cannot implicitly convert expression `udarr` of type `uint[]` to `int[]`
fail_compilation/fail11426.d(16): Error: cannot implicitly convert expression `usarr` of type `uint[1]` to `int[]`
fail_compilation/fail11426.d(18): Error: cannot implicitly convert expression `udarr` of type `uint[]` to `int[]`
fail_compilation/fail11426.d(19): Error: cannot implicitly convert expression `usarr` of type `uint[1]` to `int[]`
---
*/
void main()
{
    uint[]  udarr;
    uint[1] usarr;

    int[1] arr1; arr1 = udarr;  // Error, OK
    int[1] arr2; arr2 = usarr;  // Error, OK

    int[1] arr3 = udarr;    // accepted, BAD!
    int[1] arr4 = usarr;    // accepted, BAD!
}
