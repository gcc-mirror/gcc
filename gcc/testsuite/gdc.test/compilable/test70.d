// EXTRA_FILES: imports/test70.d
import imports.test70 : foo;

void foo(int) // overloads with selective import
{
}

void bar()
{
    foo();
}
