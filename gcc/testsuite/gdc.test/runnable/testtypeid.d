
import core.vararg;
import core.stdc.stdio;

/******************************************************/

void test2()
{
    assert(typeid(int) == typeid(int));
    assert(typeid(int) != typeid(uint));
}

/******************************************************/

class FOO3 { }

FOO3 foox3;

void foo3(int x, ...)
{
    printf("%zd arguments\n", _arguments.length);
    for (int i = 0; i < _arguments.length; i++)
    {
        //writeln(_arguments[i].toString());

        if (_arguments[i] is typeid(int))
        {
            int j = va_arg!int(_argptr);
            printf("\t%d\n", j);
            assert(j == 2);
        }
        else if (_arguments[i] == typeid(long))
        {
            long j = va_arg!long(_argptr);
            printf("\t%lld\n", j);
            assert(j == 3);
        }
        else if (_arguments[i] is typeid(double))
        {
            double d = va_arg!double(_argptr);
            printf("\t%g\n", d);
            assert(d == 4.5);
        }
        else if (_arguments[i] is typeid(FOO3))
        {
            FOO3 f = va_arg!FOO3(_argptr);
            printf("\t%p\n", f);
            assert(f is foox3);
        }
        else
            assert(0);
    }
}

void test3()
{
    FOO3 f = new FOO3();

    printf("\t%p\n", f);
    foox3 = f;

    foo3(1,2,3L,4.5,f);
    foo3(1,2,3L,4.5,f);
}

/******************************************************/

void test4()
{
    TypeInfo ti;

    ti = typeid(float[]);
    assert(!(ti is null));
    ti = typeid(double[]);
    assert(!(ti is null));
    ti = typeid(real[]);
    assert(!(ti is null));

    ti = typeid(void);
    assert(!(ti is null));
    ti = typeid(void[]);
    assert(!(ti is null));
    ti = typeid(bool[]);
    assert(!(ti is null));
}

/******************************************************/

void test6()
{
    TypeInfo ti = typeid(void*);
    assert(!(ti is null));
    assert(ti.tsize==(void*).sizeof);
    assert(ti.toString()=="void*");
}

/******************************************************/

void test7()
{
    TypeInfo ti = typeid(bool*);
    assert(!(ti is null));
    assert(ti.tsize==(bool*).sizeof);
    assert(ti.toString()=="bool*");
}

/******************************************************/

void test8()
{
    TypeInfo ti = typeid(byte*);
    assert(!(ti is null));
    assert(ti.tsize==(byte*).sizeof);
    assert(ti.toString()=="byte*");
}

/******************************************************/

void test9()
{
    TypeInfo ti = typeid(byte[]);
    assert(!(ti is null));
    assert(ti.tsize==(byte[]).sizeof);
    assert(ti.toString()=="byte[]");
}

/******************************************************/

void test10()
{
    TypeInfo ti = typeid(short*);
    assert(!(ti is null));
    assert(ti.tsize==(short*).sizeof);
    assert(ti.toString()=="short*");
}

/******************************************************/

void test11()
{
    TypeInfo ti = typeid(ushort*);
    assert(!(ti is null));
    assert(ti.tsize==(ushort*).sizeof);
    assert(ti.toString()=="ushort*");
}

/******************************************************/

void test12()
{
    TypeInfo ti = typeid(int*);
    assert(!(ti is null));
    assert(ti.tsize==(int*).sizeof);
    assert(ti.toString()=="int*");
}

/******************************************************/

void test13()
{
    TypeInfo ti = typeid(uint*);
    assert(!(ti is null));
    assert(ti.tsize==(uint*).sizeof);
    assert(ti.toString()=="uint*");
}

/******************************************************/

void test14()
{
    TypeInfo ti = typeid(ulong*);
    assert(!(ti is null));
    assert(ti.tsize==(ulong*).sizeof);
    assert(ti.toString()=="ulong*");
}

/******************************************************/

void test15()
{
    TypeInfo ti = typeid(long*);
    assert(!(ti is null));
    assert(ti.tsize==(long*).sizeof);
    assert(ti.toString()=="long*");
}

/******************************************************/

void test16()
{
    TypeInfo ti = typeid(float*);
    assert(!(ti is null));
    assert(ti.tsize==(float*).sizeof);
    assert(ti.toString()=="float*");
}

/******************************************************/

void test19()
{
    TypeInfo ti = typeid(double*);
    assert(!(ti is null));
    assert(ti.tsize==(double*).sizeof);
    assert(ti.toString()=="double*");
}

/******************************************************/

void test22()
{
    TypeInfo ti = typeid(real*);
    assert(!(ti is null));
    assert(ti.tsize==(real*).sizeof);
    assert(ti.toString()=="real*");
}

/******************************************************/

void test25()
{
    TypeInfo ti = typeid(char*);
    assert(!(ti is null));
    assert(ti.tsize==(char*).sizeof);
    assert(ti.toString()=="char*");
}

/******************************************************/

void test26()
{
    TypeInfo ti = typeid(wchar*);
    assert(!(ti is null));
    assert(ti.tsize==(wchar*).sizeof);
    assert(ti.toString()=="wchar*");
}

/******************************************************/

void test27()
{
    TypeInfo ti = typeid(dchar*);
    assert(!(ti is null));
    assert(ti.tsize==(dchar*).sizeof);
    assert(ti.toString()=="dchar*");
}

/******************************************************/

enum MyEnum { A, B }

void test28()
{
    TypeInfo ti = typeid(MyEnum);
    assert(!(ti is null));
    assert(ti.tsize==(MyEnum).sizeof);
    assert(ti.toString()=="testtypeid.MyEnum");
}

/******************************************************/

void test29()
{
    alias void function() func;
    TypeInfo ti = typeid(func);
    assert(ti !is null);
    assert(ti.tsize == func.sizeof);
}

/******************************************************/

void test30()
{
    alias int delegate() del;
    TypeInfo ti = typeid(del);
    assert(ti !is null);
    assert(ti.tsize == del.sizeof);
}

/******************************************************/

void test31()
{
    TypeInfo ti = typeid(void);
    assert(!(ti is null));
    assert(ti.tsize == void.sizeof);
    assert(ti.toString()=="void");
}

/******************************************************/

class Foo32 { int x = 3; }
class Bar32 { long y = 4; }

void printargs(int x, ...)
{
    printf("%zd arguments\n", _arguments.length);
    for (int i = 0; i < _arguments.length; i++)
    {
        //writeln(_arguments[i].toString());

        if (_arguments[i] == typeid(int))
        {
            int j = va_arg!int(_argptr);
            printf("\t%d\n", j);
        }
        else if (_arguments[i] == typeid(long))
        {
            long j = va_arg!long(_argptr);
            printf("\t%lld\n", j);
        }
        else if (_arguments[i] == typeid(double))
        {
            double d = va_arg!double(_argptr);
            printf("\t%g\n", d);
        }
        else if (_arguments[i] == typeid(Foo32))
        {
            Foo32 f = va_arg!Foo32(_argptr);
            assert(f.x == 3);
            printf("\t%p\n", f);
        }
        else if (_arguments[i] == typeid(Bar32))
        {
            Bar32 b = va_arg!Bar32(_argptr);
            assert(b.y == 4);
            printf("\t%p\n", b);
        }
        else
            assert(0);
    }
}

void test32()
{
    Foo32 f = new Foo32();
    Bar32 b = new Bar32();

    printf("%p\n", f);
    printargs(1, 2, 3L, 4.5, f, b);
}

/******************************************************/

void test33()
{
}

/******************************************************/

void test34()
{
    class C { }
    C c;
    auto a = typeid(C).info;
}

/******************************************************/

void test35()
{
    auto ti = typeid(shared(int));

    auto sti = cast(TypeInfo_Shared)ti;
    assert(sti);

    // allow both next and base as field names in TypeInfo_Const
    static if (is(typeof(&sti.base) == TypeInfo*))
        assert(sti.base == typeid(int));
    else
        assert(sti.next == typeid(int));
}

/******************************************************/

void test36()
{
    int i;
    assert(typeid(i++) == typeid(int));
    assert(i == 1);
    assert(typeid(i + 1) == typeid(int));
}

/******************************************************/

class A37 {}
class B37 : A37 {}

void test37()
{
    auto a = new B37;
    //writeln(typeid(A));
    assert(typeid(a) == typeid(B37));
}

/******************************************************/

void test38()
{
    static if (is(cent))
    {
        TypeInfo ti = typeid(cent*);
        assert(!(ti is null));
        assert(ti.tsize==(cent*).sizeof);
        assert(ti.toString()=="cent*");
    }
}

/******************************************************/

void test39()
{
    static if (is(ucent))
    {
        TypeInfo ti = typeid(ucent*);
        assert(!(ti is null));
        assert(ti.tsize==(ucent*).sizeof);
        assert(ti.toString()=="ucent*");
    }
}

/******************************************************/

void test40()
{
    static if (is(cent))
    {
        cent i;
        assert(typeid(i++) == typeid(cent));
        assert(i == 1);
        assert(typeid(i + 1) == typeid(cent));
    }
}

/******************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9442

class C
{
    this()
    {
        c = this;
        auto x = typeid(c);         // NG
        auto y = typeid(this.c);    // ok
    }

    C c;
}

void test9442()
{
    auto c = new C();
}

/******************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10451

struct Foo10451;

struct Bar10451
{
    Foo10451*[] foos;
}

void test10451()
{
    Foo10451*[] foos = [];
    foos ~= null;
    foos = new Foo10451*[2];
}

/******************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11010

struct S11010 { S11010* p; }

class C11010 { C11010 p; }
class D11010 : C11010 {}

void test11010()
{
    TypeInfo ti;

    S11010 s;
    ti = typeid(s.p);
    assert(cast(TypeInfo_Pointer)ti !is null);
    assert(ti.toString() == "testtypeid.S11010*");

    C11010 c = new C11010();
    c.p = new D11010();
    ti = typeid(c.p);
    assert(cast(TypeInfo_Class)ti !is null);
    assert(ti.toString() == "testtypeid.D11010");
}

/******************************************************/
// https://issues.dlang.org/show_bug.cgi?id=13045

void test13045a()
{
    static struct S
    {
        int[] a;
    }

    auto s1 = S([1,2]);
    auto s2 = S([1,2]);
    assert(s1 !is s2);
    assert(s1 == s2);
    assert(typeid(S).getHash(&s1) == typeid(S).getHash(&s2));   // should succeed
}

void test13045b()
{
    bool thrown(T)(lazy T cond)
    {
        import core.exception;
        try
            cond();
        catch (Error e)
            return true;
        return false;
    }

    struct S
    {
        size_t toHash() const nothrow @safe
        {
            // all getHash call should reach here
            throw new Error("");
        }
    }
    struct T
    {
        S s;
    }
    S s;
    assert(thrown(typeid(S).getHash(&s)));      // OK
    S[1] ssa;
    assert(thrown(typeid(S[1]).getHash(&ssa))); // OK
    S[] sda = [S(), S()];
    assert(thrown(typeid(S[]).getHash(&sda)));  // OK

    T t;
    assert(thrown(typeid(T).getHash(&t)));      // OK <- NG
    T[1] tsa;
    assert(thrown(typeid(T[1]).getHash(&tsa))); // OK <- NG
    T[] tda = [T(), T()];
    assert(thrown(typeid(T[]).getHash(&tda)));  // OK <- NG
}

/******************************************************/
// https://issues.dlang.org/show_bug.cgi?id=15680

void test15680()
{
    auto tid = typeid(null);
    auto iz = tid.initializer();
    assert(iz.length == typeof(null).sizeof);
    assert(iz.ptr is null || *(cast(void**)iz.ptr) is null);
}

/******************************************************/

int main()
{
    test2();
    test3();
    test4();
    test6();
    test7();
    test8();
    test9();
    test10();
    test11();
    test12();
    test13();
    test14();
    test15();
    test16();
    test19();
    test22();
    test25();
    test26();
    test27();
    test28();
    test29();
    test30();
    test31();
    test32();
    test33();
    test34();
    test35();
    test36();
    test37();
    test38();
    test39();
    test40();
    test9442();
    test10451();
    test11010();
    test13045a();
    test13045b();
    test15680();

    return 0;
}
