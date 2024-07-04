/*
TEST_OUTPUT:
---
fail_compilation/diag4596.d(15): Error: cannot modify expression `this` because it is not an lvalue
fail_compilation/diag4596.d(16): Error: conditional expression `1 ? this : this` is not a modifiable lvalue
fail_compilation/diag4596.d(18): Error: cannot modify expression `super` because it is not an lvalue
fail_compilation/diag4596.d(19): Error: conditional expression `1 ? super : super` is not a modifiable lvalue
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
