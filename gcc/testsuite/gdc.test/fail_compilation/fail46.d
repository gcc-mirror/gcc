// PERMUTE_ARGS: -inline
/*
TEST_OUTPUT:
---
fail_compilation/fail46.d(19): Error: calling non-static function `bug` requires an instance of type `MyStruct`
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
