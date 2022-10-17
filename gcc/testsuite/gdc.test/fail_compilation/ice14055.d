/*
TEST_OUTPUT:
---
fail_compilation/ice14055.d(16): Error: uninitialized variable `foo` cannot be returned from CTFE
---
*/

struct S
{
    static returnsFoo()
    {
        uint[1] foo = void;
        return foo;
    }

    static enum fooEnum = returnsFoo();
    static uint[1] fooArray = fooEnum[];
}
