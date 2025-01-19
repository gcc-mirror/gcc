/* PERMUTE_ARGS: -preview=rvaluerefparam
/* testing __rvalue */

import core.stdc.stdio;

/********************************/

int foo(int) { printf("foo(int)\n"); return 1; }
int foo(ref int) { printf("foo(ref int)\n"); return 2; }

void test1()
{
    int s;
    assert(foo(s) == 2);
    assert(foo(__rvalue(s)) == 1);
}

/********************************/

struct S
{
  nothrow:
    ~this() { printf("~this() %p\n", &this); }
    this(ref S) { printf("this(ref S)\n"); }
    void opAssign(S) { printf("opAssign(S)\n"); }
}

void test2()
{
    S s;
    S t;

    t = __rvalue(s);
}

/********************************/

struct S3
{
    int a, b, c;

    this(S3) {}
    this(ref S3) {}
}

void test3()
{
    S3 s;
    S3 x = s; // this line causes the compiler to crash
}

/********************************/

struct S4
{
    void* p;

    this(ref S4) { }

    this(S4 s)
    {
        assert(&s is &x); // confirm the rvalue reference
    }
}

__gshared S4 x;

void test4()
{
    S4 t = __rvalue(x);
}

/********************************/

struct S5
{
    this(S5 s) { printf("this(S5 s)\n"); }
    this(ref inout S5 s) inout { printf("this(ref inout S5 s) inout\n"); }
}

void test5()
{
    S5 t;
    S5 t1 = t;
    S5 t2 = __rvalue(t);
}

/********************************/

int moveCtor, copyCtor, moveAss, copyAss;

struct S6
{
    this(S6 s) { ++moveCtor; }
    this(ref S6 s) { ++copyCtor; }
    void opAssign(S6 s) { ++moveAss; }
    void opAssign(ref S6 s) { ++copyAss; }
}

void test6()
{
    S6 x;
    S6 s = x;
//    printf("S6 s = x;            moveCtor %d copyCtor %d moveAss %d copyAss %d\n", moveCtor, copyCtor, moveAss, copyAss);
    assert(copyCtor == 1);
    S6 s2 = __rvalue(x);
//    printf("S6 s2 = __rvalue(x); moveCtor %d copyCtor %d moveAss %d copyAss %d\n", moveCtor, copyCtor, moveAss, copyAss);
    assert(moveCtor == 1);
    s2 = s;
//    printf("s2 = s;             moveCtor %d copyCtor %d moveAss %d copyAss %d\n", moveCtor, copyCtor, moveAss, copyAss);
    assert(copyAss == 1);
    s2 = __rvalue(s);
//    printf("s2 = __rvalue(s);   moveCtor %d copyCtor %d moveAss %d copyAss %d\n", moveCtor, copyCtor, moveAss, copyAss);
    assert(moveAss == 1);
    assert(copyCtor == 1 && moveCtor == 1 && copyAss == 1 && moveAss == 1);
}

/********************************/
// https://github.com/dlang/dmd/pull/17050#issuecomment-2543370370

struct MutableString(size_t E) {}

struct StringTest
{
    alias toString this;
    const(char)[] toString() const pure
        => null;

    this(StringTest rhs) {}
    this(ref inout StringTest rhs) inout pure {}

    this(typeof(null)) inout pure {}
    this(size_t Embed)(MutableString!Embed str) inout {}

    ~this() {}
}


void test7()
{
    StringTest s = StringTest(null);
}

/********************************/
// https://github.com/dlang/dmd/issues/20562

struct S8
{
    int a,b;
    this(S8) { printf("this(S)\n"); b = 4; }
    this(ref S8) { printf("this(ref S)\n"); assert(0); }
}


S8 returnRval(ref S8 arg)
{
static if (0)
{
    /*  __copytmp2 =  0 ;
        _D7rvalue51S6__ctorMFNcKSQxQrZQg call  (arg param  #__copytmp2);
        * __HID1 streq 1 __copytmp2;
        __HID1;
     */
    return arg;
}
else static if (1)
{
    /*  * __HID1 streq 1 * arg;
        __HID1;
     */
    return __rvalue(arg); // should move-construct the NRVO value
}
else
{
    /*  * t =  0 ;
        t;
        _TMP0 =  t;
        _D7rvalue51S6__ctorMFNcSQwQqZQg call  (arg param  _TMP0);
        t;
     */
    S8 t = __rvalue(arg);
    return t;
}
}


void test8()
{
   S8 s;
   S8 t = returnRval(s);
   printf("t.b: %d\n", t.b);
   assert(t.b == 4);
}

/********************************/

struct T9
{
    int i;
    inout this(ref inout T9 t) { this.i = t.i - 1; printf("this(ref T9)\n"); }
    inout this(inout T9 t)     { this.i = t.i + 1; printf("this(T9)\n"); }
}

struct S9
{
    T9 t;
    //inout this(return ref scope inout S9 t);// { this.i = t.i - 1; printf("this(ref T9)\n"); }
    //@system inout this(return scope inout S9 t);//     { this.i = t.i + 1; printf("this(T9)\n"); }
}

void test9()
{
    S9 s;
    s.t.i = 3;
    S9 u = s;
    printf("u.t.i = %d\n", u.t.i);
    assert(u.t.i == 2);

    S9 v = __rvalue(u);
    printf("v.t.i = %d\n", v.t.i);
    assert(v.t.i == 3);
}

/********************************/
// https://github.com/s-ludwig/taggedalgebraic/issues/75

struct T10
{
    string s;
    this(T10) {}
    this(string v) { s = v; }
}

struct S10
{
    T10 p;
}

void test10()
{
    S10 s = S10(T10("hello"));
    assert(s.p.s == "hello");
}

/********************************/

int main()
{
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();
    test8();
    test9();

    return 0;
}
