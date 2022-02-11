// REQUIRED_ARGS: -de
// EXTRA_FILES: imports/test15785.d
// PERMUTE_ARGS:
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

    typeof(super).T t;
}
