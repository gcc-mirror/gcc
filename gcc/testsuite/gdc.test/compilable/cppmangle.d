// EXTRA_FILES: cppmangle2.d
// Test C++ name mangling.
// https://issues.dlang.org/show_bug.cgi?id=4059
// https://issues.dlang.org/show_bug.cgi?id=5148
// https://issues.dlang.org/show_bug.cgi?id=7024
// https://issues.dlang.org/show_bug.cgi?id=10058

import core.stdc.stdio;

extern (C++) int foob(int i, int j, int k);

class C
{
    extern (C++) int bar(int i, int j, int k)
    {
        printf("this = %p\n", this);
        printf("i = %d\n", i);
        printf("j = %d\n", j);
        printf("k = %d\n", k);
        return 1;
    }
}


extern (C++)
int foo(int i, int j, int k)
{
    printf("i = %d\n", i);
    printf("j = %d\n", j);
    printf("k = %d\n", k);
    assert(i == 1);
    assert(j == 2);
    assert(k == 3);
    return 1;
}

void test1()
{
    foo(1, 2, 3);

    auto i = foob(1, 2, 3);
    assert(i == 7);

    C c = new C();
    c.bar(4, 5, 6);
}

version (Posix)
{
    static assert(foo.mangleof == "_Z3fooiii");
    static assert(foob.mangleof == "_Z4foobiii");
    static assert(C.bar.mangleof == "_ZN1C3barEiii");
}
version (Win32)
{
    static assert(foo.mangleof == "?foo@@YAHHHH@Z");
    static assert(foob.mangleof == "?foob@@YAHHHH@Z");
    static assert(C.bar.mangleof == "?bar@C@@UAEHHHH@Z");
}
version (Win64)
{
    static assert(foo.mangleof == "?foo@@YAHHHH@Z");
    static assert(foob.mangleof == "?foob@@YAHHHH@Z");
    static assert(C.bar.mangleof == "?bar@C@@UEAAHHHH@Z");
}

/****************************************/

extern (C++)
interface D
{
    int bar(int i, int j, int k);
}

extern (C++) D getD();

void test2()
{
    D d = getD();
    int i = d.bar(9,10,11);
    assert(i == 8);
}

version (Posix)
{
    static assert (getD.mangleof == "_Z4getDv");
    static assert (D.bar.mangleof == "_ZN1D3barEiii");
}

/****************************************/

extern (C++) int callE(E);

extern (C++)
interface E
{
    int bar(int i, int j, int k);
}

class F : E
{
    extern (C++) int bar(int i, int j, int k)
    {
        printf("F.bar: i = %d\n", i);
        printf("F.bar: j = %d\n", j);
        printf("F.bar: k = %d\n", k);
        assert(i == 11);
        assert(j == 12);
        assert(k == 13);
        return 8;
    }
}

void test3()
{
    F f = new F();
    int i = callE(f);
    assert(i == 8);
}

version (Posix)
{
    static assert (callE.mangleof == "_Z5callEP1E");
    static assert (E.bar.mangleof == "_ZN1E3barEiii");
    static assert (F.bar.mangleof == "_ZN1F3barEiii");
}

/****************************************/

extern (C++) void foo4(char* p);

void test4()
{
    foo4(null);
}

version (Posix)
{
    static assert(foo4.mangleof == "_Z4foo4Pc");
}

/****************************************/

extern(C++)
{
  struct foo5 { int i; int j; void* p; }

  interface bar5{
    foo5 getFoo(int i);
  }

  bar5 newBar();
}

void test5()
{
  bar5 b = newBar();
  foo5 f = b.getFoo(4);
  printf("f.p = %p, b = %p\n", f.p, cast(void*)b);
  assert(f.p == cast(void*)b);
}

version (Posix)
{
    static assert(bar5.getFoo.mangleof == "_ZN4bar56getFooEi");
    static assert (newBar.mangleof == "_Z6newBarv");
}

/****************************************/

extern(C++)
{
    struct S6
    {
        int i;
        double d;
    }
    S6 foo6();
}

extern (C) int foosize6();

void test6()
{
    S6 f = foo6();
    printf("%d %d\n", foosize6(), cast(int)S6.sizeof);
    assert(foosize6() == S6.sizeof);
    assert(f.i == 42);
    printf("f.d = %g\n", f.d);
    assert(f.d == 2.5);
}

version (Posix)
{
    static assert (foo6.mangleof == "_Z4foo6v");
}

/****************************************/

extern (C) int foo7();

struct S
{
    int i;
    long l;
}

void test7()
{
    printf("%d %d\n", foo7(), cast(int)S.sizeof);
    assert(foo7() == S.sizeof);
}

/****************************************/

extern (C++) void foo8(const char *);

void test8()
{
    char c;
    foo8(&c);
}

version (Posix)
{
    static assert(foo8.mangleof == "_Z4foo8PKc");
}

/****************************************/
// https://issues.dlang.org/show_bug.cgi?id=4059

struct elem9 { }

extern(C++) void foobar9(elem9*, elem9*);

void test9()
{
    elem9 *a;
    foobar9(a, a);
}

version (Posix)
{
    static assert(foobar9.mangleof == "_Z7foobar9P5elem9S0_");
}

/****************************************/
// https://issues.dlang.org/show_bug.cgi?id=5148

extern (C++)
{
    void foo10(const char*, const char*);
    void foo10(const int, const int);
    void foo10(const char, const char);

    struct MyStructType { }
    void foo10(const MyStructType s, const MyStructType t);

    enum MyEnumType { onemember }
    void foo10(const MyEnumType s, const MyEnumType t);
}

void test10()
{
    char* p;
    foo10(p, p);
    foo10(1,2);
    foo10('c','d');
    MyStructType s;
    foo10(s,s);
    MyEnumType e;
    foo10(e,e);
}

// https://issues.dlang.org/show_bug.cgi?id=19504
extern(C++) struct Class19504 {
    pragma(mangle, "HOHOHO")
    ~this();
}
static assert(Class19504.__xdtor.mangleof == "HOHOHO");

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=10058

extern (C++)
{
    void test10058a(void*) { }
    void test10058b(void function(void*)) { }
    void test10058c(void* function(void*)) { }
    void test10058d(void function(void*), void*) { }
    void test10058e(void* function(void*), void*) { }
    void test10058f(void* function(void*), void* function(void*)) { }
    void test10058g(void function(void*), void*, void*) { }
    void test10058h(void* function(void*), void*, void*) { }
    void test10058i(void* function(void*), void* function(void*), void*) { }
    void test10058j(void* function(void*), void* function(void*), void* function(void*)) { }
    void test10058k(void* function(void*), void* function(const (void)*)) { }
    void test10058l(void* function(void*), void* function(const (void)*), const(void)* function(void*)) { }
}

version (Posix)
{
    static assert(test10058a.mangleof == "_Z10test10058aPv");
    static assert(test10058b.mangleof == "_Z10test10058bPFvPvE");
    static assert(test10058c.mangleof == "_Z10test10058cPFPvS_E");
    static assert(test10058d.mangleof == "_Z10test10058dPFvPvES_");
    static assert(test10058e.mangleof == "_Z10test10058ePFPvS_ES_");
    static assert(test10058f.mangleof == "_Z10test10058fPFPvS_ES1_");
    static assert(test10058g.mangleof == "_Z10test10058gPFvPvES_S_");
    static assert(test10058h.mangleof == "_Z10test10058hPFPvS_ES_S_");
    static assert(test10058i.mangleof == "_Z10test10058iPFPvS_ES1_S_");
    static assert(test10058j.mangleof == "_Z10test10058jPFPvS_ES1_S1_");
    static assert(test10058k.mangleof == "_Z10test10058kPFPvS_EPFS_PKvE");
    static assert(test10058l.mangleof == "_Z10test10058lPFPvS_EPFS_PKvEPFS3_S_E");
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=11696

class Expression;
struct Loc {}

extern(C++)
class CallExp
{
    static void test11696a(Loc, Expression, Expression);
    static void test11696b(Loc, Expression, Expression*);
    static void test11696c(Loc, Expression*, Expression);
    static void test11696d(Loc, Expression*, Expression*);
}

version (Posix)
{
    static assert(CallExp.test11696a.mangleof == "_ZN7CallExp10test11696aE3LocP10ExpressionS2_");
    static assert(CallExp.test11696b.mangleof == "_ZN7CallExp10test11696bE3LocP10ExpressionPS2_");
    static assert(CallExp.test11696c.mangleof == "_ZN7CallExp10test11696cE3LocPP10ExpressionS2_");
    static assert(CallExp.test11696d.mangleof == "_ZN7CallExp10test11696dE3LocPP10ExpressionS3_");
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=13337

extern(C++, N13337a.N13337b.N13337c)
{
  struct S13337{}
  void foo13337(S13337 s);
}

extern(C++, `N13337a`, `N13337b`, `N13337c`)
{
    struct S13337_2{}
    void foo13337_2(S13337 s);
    void foo13337_3(S13337_2 s);
}

version (Posix)
{
    static assert(foo13337.mangleof == "_ZN7N13337a7N13337b7N13337c8foo13337ENS1_6S13337E");
    static assert(foo13337_2.mangleof == "_ZN7N13337a7N13337b7N13337c10foo13337_2ENS1_6S13337E");
    static assert(foo13337_3.mangleof == "_ZN7N13337a7N13337b7N13337c10foo13337_3ENS1_8S13337_2E");
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=15789

extern (C++) void test15789a(T...)(T args);

void test15789()
{
    test15789a(0);
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=7030

extern(C++)
{
    struct Struct7030
    {
        void foo(int) const;
        void bar(int);
        static __gshared int boo;
    }
}

version (Posix)
{
    static assert(Struct7030.foo.mangleof == "_ZNK10Struct70303fooEi");
    static assert(Struct7030.bar.mangleof == "_ZN10Struct70303barEi");
    static assert(Struct7030.boo.mangleof == "_ZN10Struct70303booE");
}

/****************************************/

// Special cases of Itanium mangling

extern (C++, std)
{
    struct pair(T1, T2)
    {
        void swap(ref pair other);
    }

    struct allocator(T)
    {
        uint fooa() const;
        uint foob();
    }

    struct basic_string(T1, T2, T3)
    {
        uint fooa();
    }

    struct basic_istream(T1, T2)
    {
        uint fooc();
    }

    struct basic_ostream(T1, T2)
    {
        uint food();
    }

    struct basic_iostream(T1, T2)
    {
        uint fooe();
    }

    struct char_traits(T)
    {
        uint foof();
    }

    struct vector (T);

    struct test18957 {}
}

extern (C++, `std`)
{
    struct pair(T1, T2)
    {
        void swap(ref pair other);
    }

    struct allocator(T)
    {
        uint fooa() const;
        uint foob();
    }

    struct basic_string(T1, T2, T3)
    {
        uint fooa();
    }

    struct basic_istream(T1, T2)
    {
        uint fooc();
    }

    struct basic_ostream(T1, T2)
    {
        uint food();
    }

    struct basic_iostream(T1, T2)
    {
        uint fooe();
    }

    struct char_traits(T)
    {
        uint foof();
    }

    struct vector (T);

    struct Struct18957 {}
}

extern(C++)
{
    // Nspace
    std.allocator!int func_18957_1(std.allocator!(int)* v);
    // CPPNamespaceAttribute
    allocator!int func_18957_2(allocator!(int)* v);
    X func_18957_2(X)(X* v);
}

extern (C++)
{
    void func_20413(pair!(int, float), pair!(float, int));
}

version (Posix)
{
    // https://issues.dlang.org/show_bug.cgi?id=17947
    static assert(std.pair!(void*, void*).swap.mangleof == "_ZNSt4pairIPvS0_E4swapERS1_");
    static assert(std.allocator!int.fooa.mangleof == "_ZNKSaIiE4fooaEv");
    static assert(std.allocator!int.foob.mangleof == "_ZNSaIiE4foobEv");
    static assert(std.basic_string!(char,int,uint).fooa.mangleof == "_ZNSbIcijE4fooaEv");
    static assert(std.basic_string!(char, std.char_traits!char, std.allocator!char).fooa.mangleof == "_ZNSs4fooaEv");
    static assert(std.basic_istream!(char, std.char_traits!char).fooc.mangleof == "_ZNSi4foocEv");
    static assert(std.basic_ostream!(char, std.char_traits!char).food.mangleof == "_ZNSo4foodEv");
    static assert(std.basic_iostream!(char, std.char_traits!char).fooe.mangleof == "_ZNSd4fooeEv");

    static assert(func_18957_1.mangleof == `_Z12func_18957_1PSaIiE`);
    static assert(func_18957_2!(std.allocator!int).mangleof == `_Z12func_18957_2ISaIiEET_PS1_`);


    static assert(pair!(void*, void*).swap.mangleof == "_ZNSt4pairIPvS0_E4swapERS1_");
    static assert(allocator!int.fooa.mangleof == "_ZNKSaIiE4fooaEv");
    static assert(allocator!int.foob.mangleof == "_ZNSaIiE4foobEv");
    static assert(basic_string!(char,int,uint).fooa.mangleof == "_ZNSbIcijE4fooaEv");
    static assert(basic_string!(char, char_traits!char, allocator!char).fooa.mangleof == "_ZNSs4fooaEv");
    static assert(basic_istream!(char, char_traits!char).fooc.mangleof == "_ZNSi4foocEv");
    static assert(basic_ostream!(char, char_traits!char).food.mangleof == "_ZNSo4foodEv");
    static assert(basic_iostream!(char, char_traits!char).fooe.mangleof == "_ZNSd4fooeEv");

    static assert(func_18957_2.mangleof == `_Z12func_18957_2PSaIiE`);
    static assert(func_18957_2!(allocator!int).mangleof == `_Z12func_18957_2ISaIiEET_PS1_`);

    static assert(func_20413.mangleof == `_Z10func_20413St4pairIifES_IfiE`);
}

/**************************************/

alias T36 = int ********** ********** ********** **********;

extern (C++) void test36(T36, T36*) { }

version (Posix)
{
    static assert(test36.mangleof == "_Z6test36PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPiPS12_");
}

/*****************************************/
// https://issues.dlang.org/show_bug.cgi?id=17772

extern(C++, SPACE)
int test37(T)(){ return 0;}

extern(C++, `SPACE`)
int test37(T)(){ return 0;}

version (Posix) // all non-Windows machines
{
    static assert(SPACE.test37!int.mangleof == "_ZN5SPACE6test37IiEEiv");
    static assert(test37!int.mangleof == "_ZN5SPACE6test37IiEEiv");
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=15388

extern (C++) void test15388(typeof(null));

version (Posix)
{
    static assert(test15388.mangleof == "_Z9test15388Dn");
}
version (Windows)
{
    static assert(test15388.mangleof == "?test15388@@YAX$$T@Z");
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=14086

extern (C++) class Test14086
{
    this();
    ~this();
}
extern (C++) class Test14086_2
{
    final ~this();
}
extern (C++) struct Test14086_S
{
    this(int);
    ~this();
}

version(Posix)
{
    static assert(Test14086.__ctor.mangleof == "_ZN9Test14086C1Ev");
    static assert(Test14086.__dtor.mangleof == "_ZN9Test14086D1Ev");
    static assert(Test14086_2.__dtor.mangleof == "_ZN11Test14086_2D1Ev");
    static assert(Test14086_S.__ctor.mangleof == "_ZN11Test14086_SC1Ei");
    static assert(Test14086_S.__dtor.mangleof == "_ZN11Test14086_SD1Ev");
}
version(Win32)
{
    static assert(Test14086.__ctor.mangleof == "??0Test14086@@QAE@XZ");
    static assert(Test14086.__dtor.mangleof == "??1Test14086@@UAE@XZ");
    static assert(Test14086_2.__dtor.mangleof == "??1Test14086_2@@QAE@XZ");
    static assert(Test14086_S.__ctor.mangleof == "??0Test14086_S@@QAE@H@Z");
    static assert(Test14086_S.__dtor.mangleof == "??1Test14086_S@@QAE@XZ");
}
version(Win64)
{
    static assert(Test14086.__ctor.mangleof == "??0Test14086@@QEAA@XZ");
    static assert(Test14086.__dtor.mangleof == "??1Test14086@@UEAA@XZ");
    static assert(Test14086_2.__dtor.mangleof == "??1Test14086_2@@QEAA@XZ");
    static assert(Test14086_S.__ctor.mangleof == "??0Test14086_S@@QEAA@H@Z");
    static assert(Test14086_S.__dtor.mangleof == "??1Test14086_S@@QEAA@XZ");
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=18888

extern (C++)
struct T18888(T)
{
    void fun();
}

extern (C++)
struct S18888(alias arg = T18888)
{
    alias I = T18888!(arg!int);
}

version(Posix)
{
    static assert(S18888!().I.fun.mangleof == "_ZN6T18888IS_IiEE3funEv");
}
version(Win32)
{
    static assert(S18888!().I.fun.mangleof == "?fun@?$T18888@U?$T18888@H@@@@QAEXXZ");
}
version(Win64)
{
    static assert(S18888!().I.fun.mangleof == "?fun@?$T18888@U?$T18888@H@@@@QEAAXXZ");
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=18890

extern (C++) class C18890
{
    ~this() {}
}
extern (C++) class C18890_2
{
    ~this() {}
    extern (C++) struct Agg
    {
        ~this() {}
    }
    Agg s;
}

version (Posix)
{
    static assert(C18890.__dtor.mangleof == "_ZN6C18890D1Ev");
    static assert(C18890.__xdtor.mangleof == "_ZN6C18890D1Ev");
    static assert(C18890_2.__dtor.mangleof == "_ZN8C18890_26__dtorEv");
    static assert(C18890_2.__xdtor.mangleof == "_ZN8C18890_2D1Ev");
}
version (Win32)
{
    static assert(C18890.__dtor.mangleof == "??1C18890@@UAE@XZ");
    static assert(C18890.__xdtor.mangleof == "??_GC18890@@UAEPAXI@Z");
    static assert(C18890_2.__dtor.mangleof == "?__dtor@C18890_2@@UAEXXZ");
    static assert(C18890_2.__xdtor.mangleof == "??_GC18890_2@@UAEPAXI@Z");
}
version (Win64)
{
    static assert(C18890.__dtor.mangleof == "??1C18890@@UEAA@XZ");
    static assert(C18890.__xdtor.mangleof == "??_GC18890@@UEAAPEAXI@Z");
    static assert(C18890_2.__dtor.mangleof == "?__dtor@C18890_2@@UEAAXXZ");
    static assert(C18890_2.__xdtor.mangleof == "??_GC18890_2@@UEAAPEAXI@Z");
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=18891

extern (C++) class C18891
{
    ~this();
    extern (C++) struct Agg
    {
        ~this() {}
    }
    Agg s;
}

version (Posix)
{
    static assert(C18891.__dtor.mangleof == "_ZN6C18891D1Ev");
    static assert(C18891.__xdtor.mangleof == "_ZN6C18891D1Ev");
}
version (Win32)
{
    static assert(C18891.__dtor.mangleof == "??1C18891@@UAE@XZ");
    static assert(C18891.__xdtor.mangleof == "??_GC18891@@UAEPAXI@Z");
}
version (Win64)
{
    static assert(C18891.__dtor.mangleof == "??1C18891@@UEAA@XZ");
    static assert(C18891.__xdtor.mangleof == "??_GC18891@@UEAAPEAXI@Z");
}

/**************************************/
// Test C++ operator mangling

extern (C++) struct TestOperators
{
    int opCast(T)();
    int opBinary(string op)(int x);
    int opUnary(string op)();
    int opOpAssign(string op)(int x);
    int opIndex(int x);
    bool opEquals(int x);
    int opCall(int, float);
    int opAssign(int);
}

version (Posix)
{
    static assert(TestOperators.opUnary!"*".mangleof     == "_ZN13TestOperatorsdeEv");
    static assert(TestOperators.opUnary!"++".mangleof    == "_ZN13TestOperatorsppEv");
    static assert(TestOperators.opUnary!"--".mangleof    == "_ZN13TestOperatorsmmEv");
    static assert(TestOperators.opUnary!"-".mangleof     == "_ZN13TestOperatorsngEv");
    static assert(TestOperators.opUnary!"+".mangleof     == "_ZN13TestOperatorspsEv");
    static assert(TestOperators.opUnary!"~".mangleof     == "_ZN13TestOperatorscoEv");
    static assert(TestOperators.opBinary!">>".mangleof   == "_ZN13TestOperatorsrsEi");
    static assert(TestOperators.opBinary!"<<".mangleof   == "_ZN13TestOperatorslsEi");
    static assert(TestOperators.opBinary!"*".mangleof    == "_ZN13TestOperatorsmlEi");
    static assert(TestOperators.opBinary!"-".mangleof    == "_ZN13TestOperatorsmiEi");
    static assert(TestOperators.opBinary!"+".mangleof    == "_ZN13TestOperatorsplEi");
    static assert(TestOperators.opBinary!"&".mangleof    == "_ZN13TestOperatorsanEi");
    static assert(TestOperators.opBinary!"/".mangleof    == "_ZN13TestOperatorsdvEi");
    static assert(TestOperators.opBinary!"%".mangleof    == "_ZN13TestOperatorsrmEi");
    static assert(TestOperators.opBinary!"^".mangleof    == "_ZN13TestOperatorseoEi");
    static assert(TestOperators.opBinary!"|".mangleof    == "_ZN13TestOperatorsorEi");
    static assert(TestOperators.opOpAssign!"*".mangleof  == "_ZN13TestOperatorsmLEi");
    static assert(TestOperators.opOpAssign!"+".mangleof  == "_ZN13TestOperatorspLEi");
    static assert(TestOperators.opOpAssign!"-".mangleof  == "_ZN13TestOperatorsmIEi");
    static assert(TestOperators.opOpAssign!"/".mangleof  == "_ZN13TestOperatorsdVEi");
    static assert(TestOperators.opOpAssign!"%".mangleof  == "_ZN13TestOperatorsrMEi");
    static assert(TestOperators.opOpAssign!">>".mangleof == "_ZN13TestOperatorsrSEi");
    static assert(TestOperators.opOpAssign!"<<".mangleof == "_ZN13TestOperatorslSEi");
    static assert(TestOperators.opOpAssign!"&".mangleof  == "_ZN13TestOperatorsaNEi");
    static assert(TestOperators.opOpAssign!"|".mangleof  == "_ZN13TestOperatorsoREi");
    static assert(TestOperators.opOpAssign!"^".mangleof  == "_ZN13TestOperatorseOEi");
    static assert(TestOperators.opCast!int.mangleof      == "_ZN13TestOperatorscviEv");
    static assert(TestOperators.opAssign.mangleof        == "_ZN13TestOperatorsaSEi");
    static assert(TestOperators.opEquals.mangleof        == "_ZN13TestOperatorseqEi");
    static assert(TestOperators.opIndex.mangleof         == "_ZN13TestOperatorsixEi");
    static assert(TestOperators.opCall.mangleof          == "_ZN13TestOperatorsclEif");
}
version (Win32)
{
    static assert(TestOperators.opUnary!"*".mangleof     == "??DTestOperators@@QAEHXZ");
    static assert(TestOperators.opUnary!"++".mangleof    == "??ETestOperators@@QAEHXZ");
    static assert(TestOperators.opUnary!"--".mangleof    == "??FTestOperators@@QAEHXZ");
    static assert(TestOperators.opUnary!"-".mangleof     == "??GTestOperators@@QAEHXZ");
    static assert(TestOperators.opUnary!"+".mangleof     == "??HTestOperators@@QAEHXZ");
    static assert(TestOperators.opUnary!"~".mangleof     == "??STestOperators@@QAEHXZ");
    static assert(TestOperators.opBinary!">>".mangleof   == "??5TestOperators@@QAEHH@Z");
    static assert(TestOperators.opBinary!"<<".mangleof   == "??6TestOperators@@QAEHH@Z");
    static assert(TestOperators.opBinary!"*".mangleof    == "??DTestOperators@@QAEHH@Z");
    static assert(TestOperators.opBinary!"-".mangleof    == "??GTestOperators@@QAEHH@Z");
    static assert(TestOperators.opBinary!"+".mangleof    == "??HTestOperators@@QAEHH@Z");
    static assert(TestOperators.opBinary!"&".mangleof    == "??ITestOperators@@QAEHH@Z");
    static assert(TestOperators.opBinary!"/".mangleof    == "??KTestOperators@@QAEHH@Z");
    static assert(TestOperators.opBinary!"%".mangleof    == "??LTestOperators@@QAEHH@Z");
    static assert(TestOperators.opBinary!"^".mangleof    == "??TTestOperators@@QAEHH@Z");
    static assert(TestOperators.opBinary!"|".mangleof    == "??UTestOperators@@QAEHH@Z");
    static assert(TestOperators.opOpAssign!"*".mangleof  == "??XTestOperators@@QAEHH@Z");
    static assert(TestOperators.opOpAssign!"+".mangleof  == "??YTestOperators@@QAEHH@Z");
    static assert(TestOperators.opOpAssign!"-".mangleof  == "??ZTestOperators@@QAEHH@Z");
    static assert(TestOperators.opOpAssign!"/".mangleof  == "??_0TestOperators@@QAEHH@Z");
    static assert(TestOperators.opOpAssign!"%".mangleof  == "??_1TestOperators@@QAEHH@Z");
    static assert(TestOperators.opOpAssign!">>".mangleof == "??_2TestOperators@@QAEHH@Z");
    static assert(TestOperators.opOpAssign!"<<".mangleof == "??_3TestOperators@@QAEHH@Z");
    static assert(TestOperators.opOpAssign!"&".mangleof  == "??_4TestOperators@@QAEHH@Z");
    static assert(TestOperators.opOpAssign!"|".mangleof  == "??_5TestOperators@@QAEHH@Z");
    static assert(TestOperators.opOpAssign!"^".mangleof  == "??_6TestOperators@@QAEHH@Z");
    static assert(TestOperators.opCast!int.mangleof      == "??BTestOperators@@QAEHXZ");
    static assert(TestOperators.opAssign.mangleof        == "??4TestOperators@@QAEHH@Z");
    static assert(TestOperators.opEquals.mangleof        == "??8TestOperators@@QAE_NH@Z");
    static assert(TestOperators.opIndex.mangleof         == "??ATestOperators@@QAEHH@Z");
    static assert(TestOperators.opCall.mangleof          == "??RTestOperators@@QAEHHM@Z");
}
version (Win64)
{
    static assert(TestOperators.opUnary!"*".mangleof     == "??DTestOperators@@QEAAHXZ");
    static assert(TestOperators.opUnary!"++".mangleof    == "??ETestOperators@@QEAAHXZ");
    static assert(TestOperators.opUnary!"--".mangleof    == "??FTestOperators@@QEAAHXZ");
    static assert(TestOperators.opUnary!"-".mangleof     == "??GTestOperators@@QEAAHXZ");
    static assert(TestOperators.opUnary!"+".mangleof     == "??HTestOperators@@QEAAHXZ");
    static assert(TestOperators.opUnary!"~".mangleof     == "??STestOperators@@QEAAHXZ");
    static assert(TestOperators.opBinary!">>".mangleof   == "??5TestOperators@@QEAAHH@Z");
    static assert(TestOperators.opBinary!"<<".mangleof   == "??6TestOperators@@QEAAHH@Z");
    static assert(TestOperators.opBinary!"*".mangleof    == "??DTestOperators@@QEAAHH@Z");
    static assert(TestOperators.opBinary!"-".mangleof    == "??GTestOperators@@QEAAHH@Z");
    static assert(TestOperators.opBinary!"+".mangleof    == "??HTestOperators@@QEAAHH@Z");
    static assert(TestOperators.opBinary!"&".mangleof    == "??ITestOperators@@QEAAHH@Z");
    static assert(TestOperators.opBinary!"/".mangleof    == "??KTestOperators@@QEAAHH@Z");
    static assert(TestOperators.opBinary!"%".mangleof    == "??LTestOperators@@QEAAHH@Z");
    static assert(TestOperators.opBinary!"^".mangleof    == "??TTestOperators@@QEAAHH@Z");
    static assert(TestOperators.opBinary!"|".mangleof    == "??UTestOperators@@QEAAHH@Z");
    static assert(TestOperators.opOpAssign!"*".mangleof  == "??XTestOperators@@QEAAHH@Z");
    static assert(TestOperators.opOpAssign!"+".mangleof  == "??YTestOperators@@QEAAHH@Z");
    static assert(TestOperators.opOpAssign!"-".mangleof  == "??ZTestOperators@@QEAAHH@Z");
    static assert(TestOperators.opOpAssign!"/".mangleof  == "??_0TestOperators@@QEAAHH@Z");
    static assert(TestOperators.opOpAssign!"%".mangleof  == "??_1TestOperators@@QEAAHH@Z");
    static assert(TestOperators.opOpAssign!">>".mangleof == "??_2TestOperators@@QEAAHH@Z");
    static assert(TestOperators.opOpAssign!"<<".mangleof == "??_3TestOperators@@QEAAHH@Z");
    static assert(TestOperators.opOpAssign!"&".mangleof  == "??_4TestOperators@@QEAAHH@Z");
    static assert(TestOperators.opOpAssign!"|".mangleof  == "??_5TestOperators@@QEAAHH@Z");
    static assert(TestOperators.opOpAssign!"^".mangleof  == "??_6TestOperators@@QEAAHH@Z");
    static assert(TestOperators.opCast!int.mangleof      == "??BTestOperators@@QEAAHXZ");
    static assert(TestOperators.opAssign.mangleof        == "??4TestOperators@@QEAAHH@Z");
    static assert(TestOperators.opEquals.mangleof        == "??8TestOperators@@QEAA_NH@Z");
    static assert(TestOperators.opIndex.mangleof         == "??ATestOperators@@QEAAHH@Z");
    static assert(TestOperators.opCall.mangleof          == "??RTestOperators@@QEAAHHM@Z");
}

import cppmangle2;
extern(C++, Namespace18922)
{
    // Nspace
    void func18922(cppmangle2.Namespace18922.Struct18922) {}
    // CPPNamespaceAttribute
    void func18922_1(Struct18922) {}
}

extern(C++, `Namespace18922`)
{
    // Nspace
    void func18922_2(cppmangle2.Namespace18922.Struct18922) {}
    // CPPNamespaceAttribute
    void func18922_3(Struct18922) {}
}

version (Posix)
{
    static assert(func18922.mangleof == "_ZN14Namespace189229func18922ENS_11Struct18922E");
    static assert(func18922_1.mangleof == "_ZN14Namespace1892211func18922_1ENS_11Struct18922E");
    static assert(func18922_2.mangleof == "_ZN14Namespace1892211func18922_2ENS_11Struct18922E");
    static assert(func18922_3.mangleof == "_ZN14Namespace1892211func18922_3ENS_11Struct18922E");
}
else version(Windows)
{
    static assert(func18922.mangleof == "?func18922@Namespace18922@@YAXUStruct18922@1@@Z");
    static assert(func18922_1.mangleof == "?func18922_1@Namespace18922@@YAXUStruct18922@1@@Z");
    static assert(func18922_2.mangleof == "?func18922_2@Namespace18922@@YAXUStruct18922@1@@Z");
    static assert(func18922_3.mangleof == "?func18922_3@Namespace18922@@YAXUStruct18922@1@@Z");
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=18957
// extern(C++) doesn't mangle 'std' correctly on posix systems

version (Posix)
{
    // https://godbolt.org/z/C5T2LQ
    /+
    namespace std
    {
    struct test18957 {};
    }
    void test18957(const std::test18957& t) {}
    +/
    extern (C++) void test18957(ref const(Struct18957) t) {}

    static assert(test18957.mangleof == "_Z9test18957RKSt11Struct18957");
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=19043
// Incorrect mangling for extern(C++) const template parameter on windows

extern(C++) struct test19043(T) {}

extern(C++) void test19043a(test19043!(const(char)) a) {}
extern(C++) void test19043b(T)(T a) {}
version(Windows)
{
    static assert(test19043a.mangleof == "?test19043a@@YAXU?$test19043@$$CBD@@@Z");
    static assert(test19043b!(test19043!(const(char))).mangleof ==
      "??$test19043b@U?$test19043@$$CBD@@@@YAXU?$test19043@$$CBD@@@Z");
}

// https://issues.dlang.org/show_bug.cgi?id=16479
//  Missing substitution while mangling C++ template parameter for functions
version (Posix) extern (C++)
{
    // Make sure aliases are still resolved
    alias Alias16479 = int;
    Alias16479 func16479_0 (FuncT1) (FuncT1, Alias16479);
    static assert(func16479_0!(int).mangleof == `_Z11func16479_0IiEiT_i`);

    // Simple substitution on return type
    FuncT1* func16479_1 (FuncT1) ();
    static assert(func16479_1!(int).mangleof == `_Z11func16479_1IiEPT_v`);

    // Simple substitution on parameter
    void    func16479_2 (FuncT1) (FuncT1);
    static assert(func16479_2!(int).mangleof == `_Z11func16479_2IiEvT_`);

    // Make sure component substition is prefered over template parameter
    FuncT1* func16479_3 (FuncT1) (FuncT1);
    static assert(func16479_3!(int).mangleof == `_Z11func16479_3IiEPT_S0_`);

    struct Array16479 (Arg) { Arg* data; }
    struct Array16479_2 (Arg, int Size) { Arg[Size] data; }
    struct Value16479 (int Value1, int Value2) { int data; }

    // Make sure template parameter substitution happens on templated return
    Array16479!(FuncT2) func16479_4 (FuncT1, FuncT2) (FuncT1);
    static assert(func16479_4!(int, float).mangleof
                  == `_Z11func16479_4IifE10Array16479IT0_ET_`);

    // Make sure template parameter substitution happens with values
    Value16479!(Value2, Value1)* func16479_5 (int Value1, int Value2) ();
    static assert(func16479_5!(1, 1).mangleof
                  == `_Z11func16479_5ILi1ELi1EEP10Value16479IXT0_EXT_EEv`);

    // But make sure it's not substituting *too many* values
    Value16479!(1, 1)* func16479_6 (int Value1, int Value2) ();
    static assert(func16479_6!(1, 1).mangleof
                  == `_Z11func16479_6ILi1ELi1EEP10Value16479ILi1ELi1EEv`);

    // Or too many types
    Array16479!(int) func16479_7 (FuncT1, FuncT2) (FuncT1);
    static assert(func16479_7!(int, int).mangleof
                  == `_Z11func16479_7IiiE10Array16479IiET_`);

    // Also must check the parameters for template param substitution
    void func16479_8 (FuncT1) (Array16479!(FuncT1));
    static assert(func16479_8!(int).mangleof
                  == `_Z11func16479_8IiEv10Array16479IT_E`);

    // And non-substitution
    void func16479_9 (FuncT1) (Array16479!(int));
    static assert(func16479_9!(int).mangleof
                  == `_Z11func16479_9IiEv10Array16479IiE`);

    // Now let's have a bit of fun with alias parameters,
    // starting with C functions
    // TODO: Why is this mangled by g++:
    /*
      extern "C"
      {
        void externC16479 (int);
      }

      template<void (*Print)(int)>
      void func16479_10 ();

      void foo () { func16479_10<externC16479>(); }
     */
    extern (C) void externC16479 (int);
    void func16479_10 (alias Print) ();
    static assert(func16479_10!(externC16479).mangleof
                  == `_Z12func16479_10IXadL_Z12externC16479EEEvv`);

    /**
     * Let's not exclude C++ functions
     * Note:
     *   Passing a function as template parameter has an implicit
     *   `&` operator prepended to it, so the following code:
     * ---
     * void CPPPrinter16479(const char*);
     * template<void (*Print)(const char*)> void func16479_11 ();
     * void foo () { func16479_11<CPPPrinter16479>(); }
     * ---
     * Gets mangled as `func16479_11<&CPPPrinter16479>()` would,
     * which means the expression part of the template argument is
     * mangled as `XadL_Z[...]E` not `XL_Z[...]E`
     * (expressions always begin with a code anyway).
     */
    extern(C++) void CPPPrinter16479(const(char)*);
    extern(C++, Namespace16479) void CPPPrinterNS16479(const(char)*);
    extern(C++, `Namespace16479`) void CPPPrinterNS16479_1(const(char)*);
    void func16479_11 (alias Print) ();
    static assert(func16479_11!(CPPPrinter16479).mangleof
                  == `_Z12func16479_11IXadL_Z15CPPPrinter16479PKcEEEvv`);
    static assert(func16479_11!(CPPPrinterNS16479).mangleof
                  == `_Z12func16479_11IXadL_ZN14Namespace1647917CPPPrinterNS16479EPKcEEEvv`);
    static assert(func16479_11!(CPPPrinterNS16479_1).mangleof
                  == `_Z12func16479_11IXadL_ZN14Namespace1647919CPPPrinterNS16479_1EPKcEEEvv`);

    // Functions are fine, but templates are finer
    // ---
    // template<template<typename, int> class Container, typename T, int Val>
    // Container<T, Val> func16479_12 ();
    // ---
    Container!(T, Val) func16479_12 (alias Container, T, int Val) ();
    static assert(func16479_12!(Array16479_2, int, 42).mangleof
                  == `_Z12func16479_12I12Array16479_2iLi42EET_IT0_XT1_EEv`);

    // Substitution needs to happen on the most specialized type
    // Iow, `ref T identity (T) (ref T v);` should be mangled as
    // `_Z8identityIiET_*S1_*`, not as `_Z8identityIiET_*RS0_*`
    ref FuncT1 func16479_13_1 (FuncT1) (ref FuncT1);
    FuncT1*    func16479_13_2 (FuncT1) (FuncT1*);
    void       func16479_13_3 (FuncT1) (FuncT1*, FuncT1*);
    FuncT1**   func16479_13_4 (FuncT1) (FuncT1*, FuncT1);
    FuncT1     func16479_13_5 (FuncT1) (FuncT1*, FuncT1**);
    static assert(func16479_13_1!(int).mangleof == `_Z14func16479_13_1IiERT_S1_`);
    static assert(func16479_13_2!(float).mangleof == `_Z14func16479_13_2IfEPT_S1_`);
    static assert(func16479_13_3!(int).mangleof == `_Z14func16479_13_3IiEvPT_S1_`);
    static assert(func16479_13_4!(int).mangleof == `_Z14func16479_13_4IiEPPT_S1_S0_`);
    static assert(func16479_13_5!(int).mangleof == `_Z14func16479_13_5IiET_PS0_PS1_`);

    // Opaque types result in a slightly different AST
    vector!T* func16479_14 (T) (T v);
    static assert(func16479_14!(int).mangleof == `_Z12func16479_14IiEPSt6vectorIT_ES1_`);

    struct Foo16479_15 (T);
    struct Baguette16479_15 (T);
    struct Bar16479_15 (T);
    struct FooBar16479_15 (A, B);
    void inst16479_15_2 (A, B) ();
    void inst16479_15_3 (A, B, C) ();

    static assert(inst16479_15_2!(Bar16479_15!int, int).mangleof
                  == `_Z14inst16479_15_2I11Bar16479_15IiEiEvv`);
    static assert(inst16479_15_2!(int, Bar16479_15!int).mangleof
                  == `_Z14inst16479_15_2Ii11Bar16479_15IiEEvv`);
    static assert(inst16479_15_2!(Bar16479_15!int, FooBar16479_15!(Bar16479_15!int, Foo16479_15!(Bar16479_15!(Foo16479_15!int)))).mangleof
                  == `_Z14inst16479_15_2I11Bar16479_15IiE14FooBar16479_15IS1_11Foo16479_15IS0_IS3_IiEEEEEvv`);
    static assert(inst16479_15_3!(int, Bar16479_15!int, FooBar16479_15!(Bar16479_15!int, Foo16479_15!(Bar16479_15!(Foo16479_15!int)))).mangleof
                  == `_Z14inst16479_15_3Ii11Bar16479_15IiE14FooBar16479_15IS1_11Foo16479_15IS0_IS3_IiEEEEEvv`);

    static import cppmangle2;
    cppmangle2.Struct18922* func16479_16_1 (T) (T*);
    static assert(func16479_16_1!int.mangleof == `_Z14func16479_16_1IiEPN14Namespace1892211Struct18922EPT_`);
    T* func16479_16_2 (T) (T*);
    static assert(func16479_16_2!int.mangleof == `_Z14func16479_16_2IiEPT_S1_`);
    static assert(func16479_16_2!(cppmangle2.vector!int).mangleof == `_Z14func16479_16_2ISt6vectorIiEEPT_S3_`);
    static assert(func16479_16_2!(cppmangle2.vector!int).mangleof
                  == func16479_16_2!(cppmangle2.vector!int).mangleof);
    cppmangle2.vector!T* func16479_16_3 (T) (T*);
    static assert(func16479_16_3!int.mangleof == `_Z14func16479_16_3IiEPSt6vectorIiEPT_`);

    extern(C++, `fakestd`) {
        extern (C++, `__1`) {
            struct allocator16479 (T);
            struct vector16479(T, alloc = allocator16479!T);
        }
    }
    vector16479!(T, allocator16479!T)* func16479_17_1(T)();
    vector16479!(T)* func16479_17_2(T)();
    static assert(func16479_17_1!int.mangleof == `_Z14func16479_17_1IiEPN7fakestd3__111vector16479IT_NS1_14allocator16479IS3_EEEEv`);
    static assert(func16479_17_2!int.mangleof == `_Z14func16479_17_2IiEPN7fakestd3__111vector16479IT_NS1_14allocator16479IS3_EEEEv`);

    // Make sure substitution takes place everywhere in template arg list
    extern(C++, "ns") void func16479_18_1(T, X)(int, X, T, float);
    extern(C++, "ns") void func16479_18_2(T, X)(X, int, T, float);
    static assert(func16479_18_1!(double, char).mangleof == `_ZN2ns14func16479_18_1IdcEEviT0_T_f`);
    static assert(func16479_18_2!(double, char).mangleof == `_ZN2ns14func16479_18_2IdcEEvT0_iT_f`);
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=19278
// extern(C++, "name") doesn't accept expressions

extern(C++, "hello" ~ "world")
{
    void test19278();
}
enum NS = "lookup";
extern(C++, (NS))
{
    void test19278_2();
}
alias AliasSeq(Args...) = Args;
alias Tup = AliasSeq!("hello", "world");
extern(C++, (Tup))
{
    void test19278_3();
    __gshared size_t test19278_var;
}
extern(C++, (AliasSeq!(Tup, "yay")))
{
    void test19278_4();
}
version(Win64)
{
    static assert(test19278.mangleof == "?test19278@helloworld@@YAXXZ");
    static assert(test19278_2.mangleof == "?test19278_2@lookup@@YAXXZ");
    static assert(test19278_3.mangleof == "?test19278_3@world@hello@@YAXXZ");
    static assert(test19278_4.mangleof == "?test19278_4@yay@world@hello@@YAXXZ");
    static assert(test19278_var.mangleof == "?test19278_var@world@hello@@3_KA");
}
else version(Posix)
{
    static assert(test19278.mangleof == "_ZN10helloworld9test19278Ev");
    static assert(test19278_2.mangleof == "_ZN6lookup11test19278_2Ev");
    static assert(test19278_3.mangleof == "_ZN5hello5world11test19278_3Ev");
    static assert(test19278_4.mangleof == "_ZN5hello5world3yay11test19278_4Ev");
    static assert(test19278_var.mangleof == "_ZN5hello5world13test19278_varE");
}

/**************************************/
// https://issues.dlang.org/show_bug.cgi?id=18958
// Issue 18958 - extern(C++) wchar, dchar mangling not correct

version(Posix)
    enum __c_wchar_t : dchar;
else version(Windows)
    enum __c_wchar_t : wchar;
alias wchar_t = __c_wchar_t;
extern (C++) void test_char_mangling(char, wchar, dchar, wchar_t);
version (Posix)
{
    static assert(test_char_mangling.mangleof == "_Z18test_char_manglingcDsDiw");
}
version (Win64)
{
    static assert(test_char_mangling.mangleof == "?test_char_mangling@@YAXD_S_U_W@Z");
}

// https://github.com/dlang/dmd/pull/10021/files#r294055424
version (Posix)
{
    extern(C++, PR10021_NS) struct PR10021_Struct(T){}
    extern(C++) void PR10021_fun(int i)(PR10021_Struct!int);
    static assert(PR10021_fun!0.mangleof == `_Z11PR10021_funILi0EEvN10PR10021_NS14PR10021_StructIiEE`);
}

// https://github.com/dlang/dmd/pull/10021#discussion_r294095749
version (Posix)
{
    extern(C++, "a", "b")
    struct PR10021_Struct2
    {
        void func();
        void func2(PR10021_Struct2*);
    }
    static assert(PR10021_Struct2.func.mangleof == `_ZN1a1b15PR10021_Struct24funcEv`);
    static assert(PR10021_Struct2.func2.mangleof == `_ZN1a1b15PR10021_Struct25func2EPS1_`);
}

/// https://issues.dlang.org/show_bug.cgi?id=20022
version (Posix)
{
    extern(C++, `ns20022`) enum Enum20022_1 { A = 1, }
    extern(C++) void fun20022_1(Enum20022_1);
    extern(C++, `ns20022`) void fun20022_2(Enum20022_1);

    extern(C++, ns20022)
    {
        enum Enum20022_2 { A = 1, }
        void fun20022_5(Enum20022_1);
        void fun20022_6(Enum20022_2);
    }
    extern(C++) void fun20022_3(Enum20022_2);
    extern(C++, `ns20022`) void fun20022_4(Enum20022_2);

    static assert(fun20022_1.mangleof == `_Z10fun20022_1N7ns2002211Enum20022_1E`);
    static assert(fun20022_2.mangleof == `_ZN7ns2002210fun20022_2ENS_11Enum20022_1E`);

    static assert(fun20022_3.mangleof == `_Z10fun20022_3N7ns2002211Enum20022_2E`);
    static assert(fun20022_4.mangleof == `_ZN7ns2002210fun20022_4ENS_11Enum20022_2E`);
    static assert(fun20022_5.mangleof == `_ZN7ns2002210fun20022_5ENS_11Enum20022_1E`);
    static assert(fun20022_6.mangleof == `_ZN7ns2002210fun20022_6ENS_11Enum20022_2E`);
}

// https://issues.dlang.org/show_bug.cgi?id=20094
version (Posix)
{
    extern(C++, "ns20094")
    {
        struct xvector20094 (T) {}
        alias V20094 = xvector20094!(ubyte);
    }

    extern(C++) void test20094(xvector20094!(V20094)* v);
    static assert(test20094.mangleof == `_Z9test20094PN7ns2009412xvector20094INS0_IhEEEE`);
}

// https://issues.dlang.org/show_bug.cgi?id=20223
version (Posix)
{
    extern(C++)
    {
        int test20223_1(T)(int function(const(T)* value));
        int test20223_2(T)(int function(ref const(T) value));

        struct Struct20223_1 {}
        struct Struct20223_2 {}
        int test20223_3(ref const Struct20223_1, Struct20223_2*, Struct20223_2*);
        int test20223_4(ref const Struct20223_1, const ref Struct20223_2, Struct20223_2*);

        struct Struct20223_3 (T) {}
        void test20223_5(ref Struct20223_1, ref Struct20223_3!(const(char)*),
                         ref Struct20223_3!(const(char)*));
    }
    static assert(test20223_1!int.mangleof == `_Z11test20223_1IiEiPFiPKT_E`);
    static assert(test20223_2!int.mangleof == `_Z11test20223_2IiEiPFiRKT_E`);
    static assert(test20223_1!(int*).mangleof == `_Z11test20223_1IPiEiPFiPKT_E`);
    static assert(test20223_2!(int*).mangleof == `_Z11test20223_2IPiEiPFiRKT_E`);
    static assert(test20223_3.mangleof == `_Z11test20223_3RK13Struct20223_1P13Struct20223_2S3_`);
    static assert(test20223_4.mangleof == `_Z11test20223_4RK13Struct20223_1RK13Struct20223_2PS2_`);
    static assert(test20223_5.mangleof == `_Z11test20223_5R13Struct20223_1R13Struct20223_3IPKcES5_`);
}

// https://issues.dlang.org/show_bug.cgi?id=20224
version (Posix)
{
    extern(C++) public int test20224_1(T)(set20224!T set);  // ok
    extern(C++) public int test20224_2(T)(ref set20224!T set);  // segfault

    extern(C++) struct set20224 (T)
    {
        void test ()
        {
            test20224_1!T(this);
            test20224_2!T(this);  // segfaults
        }
    }

    extern(D) void func20224 ()
    {
        set20224!int x;
    }
}

/**************************************/

version (Posix)
{
    extern (C++) struct Loc2 {};
    extern (C++) class FuncDeclaration
    {
        static FuncDeclaration create(ref const Loc2, ref const Loc2);
    };
    extern (C++) FuncDeclaration FuncDeclaration_create(ref const Loc2, ref const Loc2);

    static assert(FuncDeclaration_create.mangleof == `_Z22FuncDeclaration_createRK4Loc2S1_`);
    static assert(FuncDeclaration.create.mangleof == `_ZN15FuncDeclaration6createERK4Loc2S2_`);
}

enum Enum19542 = func19542!(int).mangleof;

extern(C++, `bar`)
{
    void func19542(T)();
}

// https://issues.dlang.org/show_bug.cgi?id=20700
// Only testing on WIn64 because the mangling includes 'E',
// and the bug can be tested on either platform
version (Win64) extern(C++)
{
    void test20700_1(Struct20700);
    extern(C++, class) struct Struct20700 {}
    void test20700_2(Struct20700);

    // Note: Needs to be `V` (`class`), not `U` (`struct`)
    static assert(test20700_1.mangleof == `?test20700_1@@YAXVStruct20700@@@Z`);
    static assert(test20700_2.mangleof == `?test20700_2@@YAXVStruct20700@@@Z`);

    // Test that the scope is not "sticky" on the arguments
    void test20700_3(TStruct20700_1!DefaultClass20700_1);
    extern(C++, class) struct TStruct20700_1 (T1, T2 = DefaultStruct20700_1) {}
    extern(C++, class) struct DefaultStruct20700_1 {}
    extern(C++, struct) class DefaultClass20700_1 {}
    static assert(test20700_3.mangleof == `?test20700_3@@YAXV?$TStruct20700_1@PEAUDefaultClass20700_1@@VDefaultStruct20700_1@@@@@Z`);

    // Each test needs to be independent symbol to trigger a new semantic pass
    void test20700_4(TStruct20700_2!(DefaultClass20700_2, DefaultStruct20700_2));
    extern(C++, struct) class TStruct20700_2 (T1, T2 = DefaultClass20700_2) {}
    extern(C++, class) struct DefaultStruct20700_2 {}
    extern(C++, struct) class DefaultClass20700_2 {}
    static assert(test20700_4.mangleof == `?test20700_4@@YAXPEAU?$TStruct20700_2@PEAUDefaultClass20700_2@@VDefaultStruct20700_2@@@@@Z`);
}

/*****************************************/

alias noreturn = typeof(*null);

extern (C++)
{
    alias fpcpp = noreturn function();
    int funccpp(fpcpp);

    version (Posix)
        static assert(funccpp.mangleof == "_Z7funccppPFvvE");

    version (Win32)
        static assert(funccpp.mangleof == "?funccpp@@YAHP6AXXZ@Z");

    version (Win64)
        static assert(funccpp.mangleof == "?funccpp@@YAHP6AXXZ@Z");
}
