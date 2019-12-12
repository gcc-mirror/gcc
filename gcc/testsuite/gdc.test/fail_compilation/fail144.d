/*
TEST_OUTPUT:
---
fail_compilation/fail144.d(13): Error: "message"
fail_compilation/fail144.d(26):        called from here: bar(7)
---
*/

//import core.stdc.stdio : printf;

int bar(int i)
{
    assert(i < 0, "message");
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
    assert(b == 674);
}
