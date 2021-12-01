/* TEST_OUTPUT:
---
fail_compilation/test10.d(10): Error: found `else` without a corresponding `if`, `version` or `debug` statement
---
*/

void test(int i)
{
    ++i;
    else
        ++i;
}
