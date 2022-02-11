import core.vararg;
import core.stdc.stdio;

/*********************************************************/

void ifthen(bool cond, lazy void dg)
{
    if (cond)
        dg();
}

void ifthen(bool cond, lazy void dgthen, lazy void dgelse)
{
    if (cond)
        dgthen();
    else
        dgelse();
}

void dotimes(int i, lazy int dg)
{
    for (int j = 0; j < i; j++)
        dg();
}

void switcher(bool delegate()[] cases...)
{
    foreach (c; cases)
    {
        if (c())
            break;
    }
}

bool scase(bool b, lazy void dg)
{
    if (b)
    {
        dg();
        return true;
    }
    return false;
}

bool sdefault(lazy void dg)
{
    dg();
    return true;
}

void whiler(lazy bool cond, lazy void bdy)
{
    while (cond())
        bdy();
}

void test1()
{
    int x = 3;
    dotimes(5, printf("%d\n", ++x));

    ifthen(true, printf("yes\n"));
    ifthen(false, printf("no\n"));

    ifthen(true, printf("yes\n"), printf("no\n"));
    ifthen(false, printf("yes\n"), printf("no\n"));

    int v = 2;
    switcher(
        scase(v == 1, printf("it is 1\n")),
        scase(v == 2, printf("it is 2\n")),
        scase(v == 3, printf("it is 3\n")),
        sdefault( printf("it is default\n"))
    );

    whiler( x < 100,
        (){ printf("%d\n", x); x *= 2; }()
    );
}

/*********************************************************/

void fooa(lazy void dg)
{
    dg();
}

void test2()
{
    fooa(cast(void)null);
}

/*********************************************************/

void dotimes3(int count, lazy void exp)
{
    for (int i = 0; i < count; i++)
       exp;
}

void bar3(...)
{
    assert(_arguments.length == 1);
    assert(va_arg!int(_argptr) == 14);
}

void arr3(...)
{
    assert(_arguments.length == 1);
    assert(va_arg!(int[])(_argptr) == [1,0,0,0,0,0,0,0,0,9]);
}

void abc3(int* p)
{
    assert(*p == 3);
}

void test3()
{
    int x = 3;
    dotimes3(10, abc3(&x));
    dotimes3(10, ++x);
    dotimes3(1, bar3(++x));

    int[10] a = new int[10];
    a[0] = 1;
    a[$ - 1] = 9;
    dotimes3(3, arr3(a[0..$]));
}

/*********************************************************/

int p4;

void foo4(void* delegate()[] dgs...)
{
    assert(dgs.length == 4);
    assert(dgs[0]() == cast(void*)&p4);
    assert(dgs[1]() == cast(void*)&p4);
    assert(dgs[2]() == null);
    assert(dgs[3] == null);
}

void test4()
{
    void *abc()
    {
        return cast(void*)&p4;
    }

    foo4(&abc, cast(void* delegate())&abc, null, cast(void* delegate())null);
}

/*********************************************************/

bool nextis(void delegate() dgpositive = {})
{
    return true;
}

bool looping(lazy bool condition)
{
    return true;
}

void test5()
{
    looping(nextis({}));
    looping(nextis({}));
}

/*********************************************************/

void foo6(lazy int expr, ...)
{
    char[] tmp_msg = va_arg!(char[])(_argptr);
    if (cast(int)(tmp_msg.ptr)=="food_for_thought".length)
         assert(0, "length is in the pointer!");
    assert(tmp_msg=="food_for_thought");
}

int bar6() { return 3; }

void test6()
{
    foo6(bar6(),"food_for_thought");
}

/*********************************************************/

void foo7(long delegate()[] dg...)
{
    assert(dg[0]() == 1024);
    assert(dg[1]() == 1024);
}

void bar7(lazy long n)
{
    assert(n == 1024);
}

void test7()
{
    int n = 1024;
    foo7(n, n);
    bar7(n);
}

/*********************************************************/

struct Bug5750 { int a, b; }
pure Bug5750 bug5750(lazy int y) {
    Bug5750 retval;
    retval.a = y;
    retval.b = y;
    return retval;
}

pure void test5750a() {
    auto z1 = bug5750(3);
    assert(z1 == Bug5750(3, 3));
    auto z2 = bug5750(z1.a);
    assert(z2 == Bug5750(3, 3));
    auto z3 = bug5750(z1.a+z2.a+3);
    assert(z3 == Bug5750(9, 9));
    auto z4 = bug5750(++z1.a);    // expected???
    assert(z4 == Bug5750(4, 5));
    assert(z1 == Bug5750(5, 3));
}

int bug5750Global = 7;
void test5750b() {
    auto z4 = bug5750(bug5750Global);
    assert(z4 == Bug5750(7, 7));
    auto z5 = bug5750(++bug5750Global);
    assert(z5 == Bug5750(8, 9));  // expected???
    assert(bug5750Global == 9);
}
// Note: also need to make up some fail-compilation tests.

/*********************************************************/

T calcLazy6682(T)(lazy T n)
{
    return n;
}
int purefunc6682() pure
{
    return calcLazy6682(1);
}
void test6682()
{
    assert(purefunc6682() == 1);
}

/*********************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9109

void test9109()
{
    void foo(int delegate()[] dgs ...)
    {
        assert(dgs[0]() + dgs[1]() == 1 + 13);
    }

    int x = 10;
    int delegate() dg;
    foo({ return 1; }, { return 3+x; }, dg, null);
}

/*********************************************************/
// https://issues.dlang.org/show_bug.cgi?id=15835

class C15835 {}

string fun15835(lazy string s)
{
    return s;
}

void test15835()
{
    auto c = new C15835;
    auto s = typeid(c).name;
    assert(fun15835(typeid(c).name) == s);

    auto a = [c];
    assert(fun15835(typeid(a[0]).name) == s);
}

/*********************************************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();

    test5750a();
    test5750b();
    test6682();
    test9109();
    test15835();

    printf("Success\n");
    return 0;
}
