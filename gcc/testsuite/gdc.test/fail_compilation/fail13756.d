/*
TEST_OUTPUT:
---
fail_compilation/fail13756.d(14): Error: `foreach`: index parameter `ref k` must be type `const(int)`, not `int`
fail_compilation/fail13756.d(17): Error: cannot implicitly convert expression `__applyArg0` of type `int` to `string`
fail_compilation/fail13756.d(19): Error: cannot implicitly convert expression `__applyArg1` of type `int` to `char`
fail_compilation/fail13756.d(20): Error: `foreach`: value parameter `ref val` must be type `int`, not `dchar`
---
*/

void maiin()
{
    int[int] aa = [1:2];
    foreach (ref int k, v; aa)
    {
    }
    foreach (string key, val; aa) {}

    foreach (key, char val; aa) {}
    foreach (key, ref dchar val; aa) {}
}
