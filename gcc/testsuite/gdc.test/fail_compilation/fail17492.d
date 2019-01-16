/* TEST_OUTPUT:
---
fail_compilation/fail17492.d(19): Error: class fail17492.C.testE.I already exists at fail_compilation/fail17492.d(12). Perhaps in another function with the same name?
---
https://issues.dlang.org/show_bug.cgi?id=17492
*/

class C
{
    void testE()
    {
        class I
        {
        }
    }

    void testE()
    {
        class I
        {
        }
    }
}
