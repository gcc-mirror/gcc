// https://github.com/dlang/dmd/issues/20473
// REQUIRED_ARGS: -preview=bitfields

struct S1
{
    char[] slice;
    bool flag:1;
}

void test1()
{
    auto a = S1(['1','2'], false);
    auto b = S1(['1','2'], true);
    auto c = S1(['1','2'], true);
    assert(a != b);
    assert(b == c);
    assert(typeid(S1).getHash(&a) != typeid(S1).getHash(&b));
    assert(typeid(S1).getHash(&b) == typeid(S1).getHash(&c));
}

struct S2
{
    bool flag:1;
    Object o;
}

void test2()
{
    auto o = new Object;
    auto a = S2(false, o);
    auto b = S2(true, o);
    auto c = S2(true, o);
    assert(a != b);
    assert(b == c);
    assert(typeid(S2).getHash(&a) != typeid(S2).getHash(&b));
    assert(typeid(S2).getHash(&b) == typeid(S2).getHash(&c));
}

struct Wrapper3
{
    void[] wrapped;
}
struct S3
{
    bool flag:1;
    Wrapper3 wrapper;
}

void test3()
{
    auto a = S3(false, Wrapper3([1,2,3]));
    auto b = S3(true, Wrapper3([1,2,3]));
    auto c = S3(true, Wrapper3([1,2,3]));
    assert(a != b);
    assert(b == c);
    assert(typeid(S3).getHash(&a) != typeid(S3).getHash(&b));
    assert(typeid(S3).getHash(&b) == typeid(S3).getHash(&c));
}

struct Wrapper4
{
    Object wrapped;
}
struct S4
{
    bool flag:1;
    Wrapper4 wrapper;
}

void test4()
{
    auto o = new Object;
    auto a = S4(false, Wrapper4(o));
    auto b = S4(true, Wrapper4(o));
    auto c = S4(true, Wrapper4(o));
    assert(a != b);
    assert(b == c);
    assert(typeid(S4).getHash(&a) != typeid(S4).getHash(&b));
    assert(typeid(S4).getHash(&b) == typeid(S4).getHash(&c));
}

enum Wrapper5 : string
{
    empty = ""
}
struct S5
{
    bool flag:1;
    Wrapper5 wrapper;
}

void test5()
{
    auto a = S5(false, Wrapper5.empty);
    auto b = S5(true, Wrapper5.empty);
    auto c = S5(true, Wrapper5.empty);
    assert(a != b);
    assert(b == c);
    assert(typeid(S5).getHash(&a) != typeid(S5).getHash(&b));
    assert(typeid(S5).getHash(&b) == typeid(S5).getHash(&c));
}

void main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
}
