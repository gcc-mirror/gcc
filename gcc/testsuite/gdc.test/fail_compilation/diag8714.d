/*
TEST_OUTPUT:
---
fail_compilation/diag8714.d(9): Error: function `diag8714.foo` circular dependency. Functions cannot be interpreted while being compiled
fail_compilation/diag8714.d(15):        called from here: `foo("somestring")`
---
*/

string foo(string f)
{
    if (f == "somestring")
    {
        return "got somestring";
    }
    return bar!(foo("somestring"));
}

template bar(string s)
{
    enum bar = s;
}
