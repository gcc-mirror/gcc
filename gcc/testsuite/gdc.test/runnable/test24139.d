// https://issues.dlang.org/show_bug.cgi?id=24139

struct S1
{
    int x;
    extern(C++) ~this() { assert(&this == s1); }
}

extern(C++) struct S2
{
    int x;
    ~this() { assert(&this == s2); }
}

S1* s1;
S2* s2;

void main()
{
    s1 = new S1;
    s2 = new S2;

    typeid(S1).destroy(s1);
    typeid(S2).destroy(s2);
}
