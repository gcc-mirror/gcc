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
// https://github.com/dlang/dmd/issues/20567

struct S2
{
    int x;
    this(ref S2 s) { x = s.x; }
}

S2 returnRval(ref S2 arg1, ref S2 arg2, int i)
{
    return i ? arg1 : arg2;
}

void test2()
{
    S2 s1, s2;
    s1.x = 3;
    s2.x = 4;
    S2 s = returnRval(s1, s2, 0);
    assert(s.x == 4);
    s = returnRval(s1, s2, 1);
    assert(s.x == 3);
}

/***************************************************/

void main()
{
    test1();
    test2();
}
