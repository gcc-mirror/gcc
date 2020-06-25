// { dg-options "-I $srcdir/gdc.dg -I $srcdir/gdc.dg/imports -Wno-psabi" }
// { dg-additional-sources "imports/gdc27.d imports/gdc231.d" }
// { dg-do compile }

import core.simd;
import gcc.attribute;

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=27

import imports.gdc27;

interface I_B : I_A
{
    void b();
}

abstract class C_B : C_A, I_B
{
    abstract void b();
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=108

@attribute("forceinline")
void forceinline108()
{
}

@attribute("noinline")
void noinline108()
{
}

@attribute("flatten")
void flatten108()
{
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=170

import imports.gdc170;

void test170()
{
    foo!void.foo1!void();
    foo!void.foo2!void();
    foo!void.foo3();
    foo!void.foo3!void();
    foo!void.foo4();
    foo!void.foo4!void();
    foo!void.foo5!void(null);
    foo!void.foo6!void(null);
    foo!void.foo7(null);
    foo!void.foo7!void(null);
    foo!void.foo8(null);
    foo!void.foo8!void(null);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=212

template hasElaborateAssign212(S)
{
    enum hasElaborateAssign212 = is(typeof(S.init.opAssign(rvalueOf212!S))) ||
        is(typeof(lvalueOf212!S)) ;
}

T rvalueOf212(T)();

T lvalueOf212(T)();


template TypeTuple212(TList...)
{
    alias TypeTuple212 = TList;
}

template Tuple212()
{
    struct Tuple212
    {
        void opAssign(R)(R)
        {
            if (hasElaborateAssign212!R)
            {
            }
        }
    }
}

ref emplaceRef212()
{
    static if (!hasElaborateAssign212!(Tuple212!()))
        chunk;
}

class TaskPool212
{
    void reduce()
    {
        Tuple212!() seed = void;
        Tuple212!()[] results;
        foreach(i; TypeTuple212!(0, 1))
            results[i] = seed;
    }
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=213

struct S213
{
    int4 vec;
}

void test213()
{
    S213 s, b;

    assert(s == b);
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=218

struct S218a
{
    this(int* pdata_)
    {
        pdata = pdata_;
    }

    void opIndexAssign(int, size_t) { }
    int* pdata;
};

struct S218
{
    S218a getS218a()
    {
        return S218a(data.ptr);
    }

    int[] data;
    int[] tab2;
};

S218 f()
{
    S218 r;

    for(int i = 0; i < 1; ++i)
        r.getS218a()[0] = 0;

    return r;
}

S218 var;

static this()
{
    var = f();
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=223

struct S223
{
    long[8] field;
}

class C223
{
    long[8] field;
}

S223 test223_1();
real test223_2();
string[long[8]] test223_3();
C223 test223_4();
long test223_5();
long[] test223_6();
long[8] test223_7();
C223[8] test223_8();
void delegate() test223_9();

bool test223()
{
    return test223_1() == test223_1() &&
           test223_1() is test223_1() &&
           test223_2() == test223_2() &&
           test223_2() is test223_2() &&
           test223_3() == test223_3() &&
           test223_3() is test223_3() &&
           test223_4() == test223_4() &&
           test223_4() is test223_4() &&
           test223_5() == test223_5() &&
           test223_5() is test223_5() &&
           test223_6() == test223_6() &&
           test223_6() is test223_6() &&
           test223_7() == test223_7() &&
           test223_7() is test223_7() &&
           test223_8() == test223_8() &&
           test223_8() is test223_8() &&
           test223_9() == test223_9() &&
           test223_9() is test223_9();
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=231

import imports.gdc231;

class Range231 : Widget231
{
    override void* getStruct()
    {
        return null;
    }
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=239

import imports.gdc239;

class C239
{
    C239a *foo;
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=240

interface I204
{
      void f();
}

class C204 : I204
{
      void f();
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=241

import imports.gdc241a;
public import imports.gdc241b : S241, C241, E241, N241;

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=242

struct S242a
{
    enum M = S242a();
    void iter() { }
}

void test242a()
{
    return S242a.M.iter;
}

struct S242b
{
    enum M = S242b();
    void iter() { }
}

void test242b()
{
    S242b.M.iter;
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=251

import imports.gdc251a;
import imports.gdc251b : C251;

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=253

import imports.gdc253;

class C253 : C253a
{
    void test253() { }
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=255

class C255
{
    void f2()
    {
        class C1
        {
            void f1()
            {
                void f0()
                {
                    class C0
                    {
                        void test255()
                        {
                            f2();
                        }
                    }
                }
            }
        }
    }
}

class C255a
{
    void f3()
    {
        class C1
        {
            void f2()
            {
                void f1()
                {
                    void f0()
                    {
                        class C0
                        {
                            void test255a()
                            {
                                f3();
                            }
                        }
                    }
                }
            }
        }
    }
}

class C255b
{
    void f4()
    {
        class C2
        {
            void f3()
            {
                void f2()
                {
                    class C1
                    {
                        void f1()
                        {
                            void f0()
                            {
                                class C0
                                {
                                    void test255b()
                                    {
                                        f4();
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=256

import imports.gdcpkg256 : gdc256;

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=261

void test261()
{
    class C1
    {
        void f1()
        {
            class C2
            {
                void f2()
                {
                    auto v = &f1;
                }
            }
        }
    }
}

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=280

struct RBNode280
{
    RBNode280* _parent;

    @property left(RBNode280*)
    {
        _parent = &this;
    }
}

class RedBlackTree280
{
    RBNode280* _end;
    RBNode280* _begin;

    this(int[] elems...)
    {
        _end = new RBNode280;

        foreach (e; elems)
        {
            _end.left = _begin;
        }
    }
}

__gshared s = new RedBlackTree280('h');

/******************************************/
// https://bugzilla.gdcproject.org/show_bug.cgi?id=284

alias v284 = __vector(int[2]);

v284 test284(v284 a, ...)
{
    return a + a;
}
