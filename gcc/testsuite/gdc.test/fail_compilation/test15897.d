// REQUIRED_ARGS: -de
/*
TEST_OUTPUT:
---
fail_compilation/test15897.d(18): Deprecation: test15897.Animal.create is not visible from class Cat
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
