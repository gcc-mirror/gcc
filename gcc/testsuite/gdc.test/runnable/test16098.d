
// https://issues.dlang.org/show_bug.cgi?id=16098

/*********************************************/

void testDynamicClosure()
{
    byte a;
    align(128) byte b;
    assert((cast(size_t) &b) % 128 == 0);
    b = 37;

    byte foo() { return b; }
    dg = &foo;
    assert(dg() == 37);
}

__gshared byte delegate() dg;

/*********************************************/

void testStaticClosure()
{
    byte aa;
    align(128) byte b;
    assert((cast(size_t) &b) % 128 == 0);
    b = 73;

    byte foo() { return b; }
    assert(foo() == 73);
}

/*********************************************/

void test3()
{
    struct S
    {
	align(32) int b;
    }
}

/*********************************************/

align(16)
struct Cent
{
    ulong lo;  // low 64 bits
    ulong hi;  // high 64 bits
}

enum Cent One = { 1 };

Cent inc(Cent c) {  return add(c, One); }

Cent add(Cent c1, Cent c2) { const Cent ret = { 3, 2 }; return ret; }

void test4()
{
    const Cent C10_0 = { 0, 10 };
    const Cent Cm10_0 = inc(C10_0);
}

/*********************************************/

int main()
{
    testDynamicClosure();
    testStaticClosure();
    test3();
    test4();
    return 0;
}
