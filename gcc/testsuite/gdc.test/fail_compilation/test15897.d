// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/test15897.d(18): Error: no property `create` for type `imports.test15897.Cat`
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
