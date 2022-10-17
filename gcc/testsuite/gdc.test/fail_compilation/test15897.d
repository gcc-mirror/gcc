// REQUIRED_ARGS: -de
// EXTRA_FILES: imports/test15897.d
/*
TEST_OUTPUT:
---
fail_compilation/test15897.d(19): Error: no property `create` for `cat` of type `imports.test15897.Cat`
---
*/
module test15897;
import imports.test15897;

class Animal
{
    private void create() {}
}

void foo(Cat cat)
{
    cat.create();
}
