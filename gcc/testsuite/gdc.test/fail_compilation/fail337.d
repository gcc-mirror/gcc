/*
TEST_OUTPUT:
---
fail_compilation/fail337.d(13): Error: static assert:  `0` is false
fail_compilation/fail337.d(26):        instantiated from here: `bar!()`
fail_compilation/fail337.d(33):        100 recursive instantiations from here: `foo!196`
fail_compilation/fail337.d(41):        253 recursive instantiations from here: `baz!300`
---
*/

template bar()
{
    static assert(0);
}

template foo(int N)
{
    static if (N > 0)
    {
        static if (N & 1)
            alias foo!(N - 3) foo;
        else
            alias foo!(N - 1) foo;
    }
    else
        alias bar!() foo;
}

template baz(int M)
{
    static if (M < 50)
    {
        alias foo!(M * 4) baz;
    }
    else
        alias baz!(M - 1) baz;
}

void main()
{
    int x = baz!(300);
}
