/*
TEST_OUTPUT:
---
fail_compilation/fail162.d(25): Error: template fail162.testHelper cannot deduce function from argument types !()(string, string), candidates are:
fail_compilation/fail162.d(10):        fail162.testHelper(A...)()
fail_compilation/fail162.d(30): Error: template instance fail162.test!("hello", "world") error instantiating
---
*/

template testHelper(A ...)
{
    char[] testHelper()
    {
        char[] result;
        foreach (t; a)
        {
            result ~= "int " ~ t ~ ";\r\n";
        }
        return result;
    }
}

template test(A...)
{
    const char[] test = testHelper(A);
}

int main(char[][] args)
{
    mixin(test!("hello", "world"));
    return 0;
}
