/*
 * REQUIRED_ARGS: -de
 * TEST_OUTPUT:
---
fail_compilation/skip.d(21): Error: `switch` skips declaration of `with` temporary at fail_compilation/skip.d(26)
fail_compilation/skip.d(43): Error: `switch` skips declaration of variable `skip.test14532.n` at fail_compilation/skip.d(45)
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
