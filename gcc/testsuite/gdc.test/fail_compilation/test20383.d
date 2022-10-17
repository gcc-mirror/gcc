/* TEST_OUTPUT:
---
fail_compilation/test20383.d(11): Error: invalid array operation `cast(int[])data[] & [42]` (possible missing [])
---
*/

// https://issues.dlang.org/show_bug.cgi?id=20383

ubyte[1] ice(ubyte[1] data)
{
    ubyte[1] result = data[] & [42];
    return result;
}
