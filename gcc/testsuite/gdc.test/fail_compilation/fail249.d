/*
TEST_OUTPUT:
---
fail_compilation/fail249.d(16): Error: invalid `foreach` aggregate `bar()` of type `void`
---
*/

module main;

public void bar()
{
}

void main()
{
    foreach (Object o; bar())
    {
        debug Object foo = null; //error
    }
}
