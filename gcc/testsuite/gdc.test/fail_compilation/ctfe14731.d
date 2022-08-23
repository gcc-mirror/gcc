/*
TEST_OUTPUT:
---
fail_compilation/ctfe14731.d(16): Error: cannot implicitly convert expression `split("a b")` of type `string[]` to `string`
fail_compilation/ctfe14731.d(17): Error: cannot implicitly convert expression `split("a b")` of type `string[]` to `string`
---
*/

string[] split(string a)
{
    return [a];
}

void main()
{
    enum string list1 = "a b".split();
         string list2 = "a b".split();
}
