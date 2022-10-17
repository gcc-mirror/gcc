// https://issues.dlang.org/show_bug.cgi?id=18746

struct S {}

S f(ref int i)
{
    ++i;
    return S();
}

void main()
{
    int i = 2;
    assert(f(i) == S());
    assert(i == 3); /* failed before patch, i = 2; should pass */
}
