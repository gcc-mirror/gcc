/*
TEST_OUTPUT:
---
fail_compilation/diag4528.d(14): Error: function `diag4528.Foo.pva` `private` functions cannot be `abstract`
fail_compilation/diag4528.d(15): Error: function `diag4528.Foo.pka` `package` functions cannot be `abstract`
fail_compilation/diag4528.d(16): Error: function `diag4528.Foo.pvsa` `static` functions cannot be `abstract`
fail_compilation/diag4528.d(17): Error: function `diag4528.Foo.pksa` `static` functions cannot be `abstract`
fail_compilation/diag4528.d(18): Error: function `diag4528.Foo.pbsa` `static` functions cannot be `abstract`
---
*/

class Foo
{
    private abstract void pva();
    package abstract void pka();
    private static abstract void pvsa();
    package static abstract void pksa();
    public static abstract void pbsa();
}
