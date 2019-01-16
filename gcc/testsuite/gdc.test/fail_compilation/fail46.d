// PERMUTE_ARGS: -inline
/*
TEST_OUTPUT:
---
fail_compilation/fail46.d(19): Error: need 'this' for 'bug' of type 'int()'
---
*/

struct MyStruct
{
    int bug()
    {
        return 3;
    }
}

int main()
{
    assert(MyStruct.bug() == 3);
    return 0;
}
