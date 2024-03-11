/*
TEST_OUTPUT:
---
fail_compilation/fail25.d(14): Error: accessing non-static variable `yuiop` requires an instance of `Qwert`
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
