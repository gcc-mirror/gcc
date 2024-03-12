/*
 * REQUIRED_ARGS: -de
 * TEST_OUTPUT:
---
fail_compilation/skip.d(23): Error: `switch` skips declaration of `with` temporary
fail_compilation/skip.d(28):        declared here
fail_compilation/skip.d(45): Error: `switch` skips declaration of variable `skip.test14532.n`
fail_compilation/skip.d(47):        declared here
---
 */
// https://issues.dlang.org/show_bug.cgi?id=10524

struct S
{
    int field;
}

void test10524()
{
    int a = 1;
    S struct_with_long_name;

    switch( a )
    {
        case 0:
            struct_with_long_name.field = 444; // ok
            break;
        with( struct_with_long_name )
        {
            case 1:
                field = 555; // segfault
                break;
        }

        default:
            break;
    }
}

// https://issues.dlang.org/show_bug.cgi?id=14532

void test14532()
{
    char ch = '!';
    switch (ch)
    {
            int n = 42;
        case '!':
            assert(n == 42);
            break;

      default:
    }
}
