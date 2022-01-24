// EXTRA_FILES: imports/test15785.d
/*
TEST_OUTPUT:
---
fail_compilation/test15785b.d(14): Error: `imports.test15785.Base.T` is not visible from module `test15785b`
fail_compilation/test15785b.d(15): Error: `imports.test15785.Base.T` is not visible from module `test15785b`
fail_compilation/test15785b.d(16): Error: `imports.test15785.IBase2.T` is not visible from module `test15785b`
---
*/
import imports.test15785;

class Derived : Base, IBase2
{
    typeof(super).T t;
    Base.T t2;
    IBase2.T t3;
}
