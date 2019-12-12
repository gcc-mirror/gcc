// REQUIRED_ARGS: -de
// PERMUTE_ARGS:
/*
TEST_OUTPUT:
---
---
*/
import imports.test15785;

class Derived : Base, IBase2
{
    override void foo()
    {
        super.foo();
        bar();
        // Base.bar(); // doesn't work yet due to a bug in checkAccess
        faz();
        // IBase2.faz(); // doesn't work yet due to a bug in checkAccess
    }

    super.T t;
}
