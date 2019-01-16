/*
TEST_OUTPUT:
---
fail_compilation/fail25.d(14): Error: need 'this' for 'yuiop' of type 'int'
---
*/

class Qwert
{
    int yuiop;

    static int asdfg()
    {
        return Qwert.yuiop + 105;
    }
}
