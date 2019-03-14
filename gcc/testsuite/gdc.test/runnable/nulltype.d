extern (C) int printf(const(char*) fmt, ...);

alias typeof(null) null_t;

/**********************************************/

void test1()
{
    null_t null1;
    typeof(null) null2;

    static assert(is(typeof(null1) == typeof(null)));
    static assert(is(typeof(null2) == typeof(null)));

    static assert(is(typeof(null1) == null_t));
    static assert(is(typeof(null2) == null_t));
}

/**********************************************/

interface I{}
class C{}

int f(null_t)   { return 1; }
int f(int[])    { return 2; }
int f(C)        { return 3; }

void test2()
{
    static assert(is(null_t : C));
    static assert(is(null_t : I));
    static assert(is(null_t : int[]));
    static assert(is(null_t : void*));
    static assert(is(null_t : int**));

    static assert(!is(null_t == C));
    static assert(!is(null_t == I));
    static assert(!is(null_t == int[]));
    static assert(!is(null_t == void*));
    static assert(!is(null_t == int**));

    static assert(is(null_t == null_t));

    assert(f(null) == 1);
}

/**********************************************/
// 5899

auto f5899(bool b)
{
    if (b)
        return new Object;
    else
        return null;
}
static assert(is(typeof(f5899) R == return) && is(R == Object));
pragma(msg, typeof(f5899));

auto g5899(bool b)
{
    if (b)
        return new int;
    else
        return null;
}
static assert(is(typeof(g5899) R == return) && is(R == int*));
pragma(msg, typeof(g5899));

auto h5899(bool b)
{
    if (b)
        return [1];
    else
        return null;
}
static assert(is(typeof(h5899) R == return) && is(R == int[]));
pragma(msg, typeof(h5899));

/**********************************************/
// 7278

struct Foo7278(string s)
{
    string var;
    void func()
    {
        string local = var;
    }
}

void test7278()
{
    Foo7278!null a;
    Foo7278!null b;
}

/**********************************************/
// 8221

class A8221
{
    A8221 foo() { return this; }
}
class B8221: A8221
{
    override typeof(null) foo() { return null; } // error
}
void test8221()
{
    auto a = new A8221();
    assert(a.foo() is a);
    auto b = new B8221();
    assert(b.foo() is null);
    a = b;
    assert(a.foo() is null);
}

/***************************************************/
// 8589

void test8589()
{
    static typeof(null) retnull() { return null; }

    void test(bool result, T)()
    {
        void f(T function() dg) { assert(!dg()); }

        static assert((is(typeof(null) function() : T function())) == result);
        static assert(is(typeof( f(&retnull) )) == result);
        static assert(is(typeof( f(()=>null) )) == result);
        static if (result)
        {
            f(&retnull);
            f(()=>null);
        }
    }
    test!(true,  int*)();
    test!(true,  Object)();
    test!(false, int[int])();
    test!(false, int[])();
    test!(false, void delegate())();
}

/**********************************************/
// 9385

void test9385()
{
    assert((null ? true : false) == false);
    if (null) assert(0);
    assert(!null);
}

/**********************************************/
// 12203

void test12203()
{
    typeof(null) v;
    void foo(float) {}
    void delegate(float) dg = &foo;
    assert(dg !is null);

    dg = v; // Error: e2ir: cannot cast v of type typeof(null) to type void delegate(float)

    assert(dg  is null);
}

/**********************************************/

int main()
{
    test1();
    test2();
    test7278();
    test8221();
    test8589();
    test9385();
    test12203();

    printf("Success\n");
    return 0;
}
