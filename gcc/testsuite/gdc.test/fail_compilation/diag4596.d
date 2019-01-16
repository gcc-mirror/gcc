/*
TEST_OUTPUT:
---
fail_compilation/diag4596.d(15): Error: this is not an lvalue
fail_compilation/diag4596.d(16): Error: 1 ? this : this is not an lvalue
fail_compilation/diag4596.d(18): Error: super is not an lvalue
fail_compilation/diag4596.d(19): Error: 1 ? super : super is not an lvalue
---
*/

class NoGo4596
{
    void fun()
    {
        this = new NoGo4596;
        (1?this:this) = new NoGo4596;

        super = new Object;
        (1?super:super) = new Object;
    }
}
