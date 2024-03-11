/*
REQUIRED_ARGS: -checkaction=context
TEST_OUTPUT:
---
fail_compilation/fail145.d(14): Error: `assert(i && (i < 0))` failed
fail_compilation/fail145.d(27):        called from here: `bar(7)`
---
*/

//import core.stdc.stdio : printf;

int bar(int i)
{
    assert(i && i < 0);
    foreach_reverse (k, v; "hello")
    {
        i <<= 1;
        if (k == 2)
            break;
        i += v;
    }
    return i;
}

void main()
{
    static b = bar(7);
    auto c = bar(7);
    //printf("b = %d, %d\n", b, c);
    assert(b && b == 674);
}
