// https://issues.dlang.org/show_bug.cgi?id=19672

struct S
{
    ulong c;
    bool b;                  // removing this prevents bug
}

// increase the struct size at least to 17 bytes also prevents the bug.

void main()
{
    S[1] a = [S(42)];
    assert(a[0].c == 42); /* Passes. */
    f(a);
}

void f(S[1] a)
{
    assert(a[0].c == 42); /* Fails. */
}
