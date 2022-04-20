/***************************************************/

struct S1
{
    int x;
    ~this() {}
}

__gshared S1* s1ptr;

S1 test1a()
{
    auto result = S1(123);
    (() @trusted { result.x++; s1ptr = &result; })();
    return result;
}

void test1()
{
    auto r = test1a();
    assert(r.x == 124);
    assert(&r == s1ptr);
}

/***************************************************/

void main()
{
    test1();
}
