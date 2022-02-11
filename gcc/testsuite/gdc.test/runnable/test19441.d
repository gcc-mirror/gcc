// https://issues.dlang.org/show_bug.cgi?id=19441
struct S1
{
    int a;
    long b;
    alias a this;
}

struct S2
{
    S1 v;
    alias v this;
}

void main()
{
    auto x = S2(S1(1, 12345678));
    assert(x.a == 1 &&  x.b == 12345678); // prints: 1 12345678
    S1 y;
    y = x;
    assert(y.a == 1 && y.b == 12345678);
    y = x.v;
    assert(y.a == 1 && y.b == 12345678);
}
