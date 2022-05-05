// https://issues.dlang.org/show_bug.cgi?id=22175

struct Struct
{
    short a = 24, b = 25, c = 26, d = 27;
    ubyte e = 28;
}

Struct foo() { Struct s; s.a = 60; s.b = 61; s.c = 62, s.d = 63; s.e = 64; return s; }

Struct test(int i) {
    Struct var = i ? Struct() : foo();
    Struct nest() { return var; }
    return nest();
}

int main()
{
    auto s = test(0);
    assert(s.a == 60);
    assert(s.b == 61);
    assert(s.c == 62);
    assert(s.d == 63);
    assert(s.e == 64);
    s = test(1);
    assert(s.a == 24);
    assert(s.b == 25);
    assert(s.c == 26);
    assert(s.d == 27);
    assert(s.e == 28);
    return 0;
}
