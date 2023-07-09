/*
REQUIRED_ARGS:
TEST_OUTPUT:
---
fail_compilation/fail4559.d(13): Error: use `{ }` for an empty statement, not `;`
fail_compilation/fail4559.d(19): Error: use `{ }` for an empty statement, not `;`
fail_compilation/fail4559.d(21): Error: use `{ }` for an empty statement, not `;`
---
*/

void foo()
{
    int x;;
    enum A
    {
        a,
        b,
        c
    };

    void bar() {};
}
