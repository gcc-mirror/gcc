/*
GCC 5.1 introduced new implementations of std::string and std::list:

https://gcc.gnu.org/onlinedocs/libstdc++/manual/using_dual_abi.html

This causes e.g. std::string to be actually defined as
std::__cxx11::string.

On machines with GCC 5.1, this manifests as a linker error when
running the cppa.d / cppb.cpp test:

cppa.o: In function `_D4cppa6test14FZv':
cppa.d:(.text._D4cppa6test14FZv+0x11): undefined reference to `foo14a(std::string*)'
cppa.d:(.text._D4cppa6test14FZv+0x18): undefined reference to `foo14b(std::basic_string<int, std::char_traits<int>, std::allocator<int> >*)'
cppa.d:(.text._D4cppa6test14FZv+0x3a): undefined reference to `foo14f(std::char_traits<char>*, std::string*, std::string*)'
cppa.o: In function `_D4cppa7testeh3FZv':
cppa.d:(.text._D4cppa7testeh3FZv+0x19): undefined reference to `throwle()'
collect2: error: ld returned 1 exit status
--- errorlevel 1

When the .cpp file is compiled with g++ 5.3.0, the actual function
signatures in the cppb.o object file are:

foo14a(std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >*)
foo14b(std::__cxx11::basic_string<int, std::char_traits<int>, std::allocator<int> >*)
foo14f(std::char_traits<char>*, std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >*, std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >*)

Fortunately, it is easily possible to disable the new feature
by defining _GLIBCXX_USE_CXX11_ABI as 0 before including any standard
headers.
*/
#define _GLIBCXX_USE_CXX11_ABI 0

#include <stdio.h>
#include <assert.h>
#include <exception>
#include <cstdarg>

/**************************************/

int foo(int i, int j, int k);

int foob(int i, int j, int k)
{
    printf("i = %d\n", i);
    printf("j = %d\n", j);
    printf("k = %d\n", k);
    assert(i == 1);
    assert(j == 2);
    assert(k == 3);

    foo(i, j, k);

    return 7;
}

/**************************************/

class D *dthis;

class D
{
  public:
    virtual int bar(int i, int j, int k)
    {
    printf("this = %p\n", this);
    assert(this == dthis);
    printf("D.bar: i = %d\n", i);
    printf("D.bar: j = %d\n", j);
    printf("D.bar: k = %d\n", k);
    assert(i == 9);
    assert(j == 10);
    assert(k == 11);
    return 8;
    }
};


D* getD()
{
    D *d = new D();
    dthis = d;
    return d;
}

/**************************************/

class E
{
  public:
    virtual int bar(int i, int j, int k);
};


int callE(E *e)
{
    return e->bar(11,12,13);
}

/**************************************/

void foo4(char *p)
{
}

/**************************************/

struct foo5 { int i; int j; void *p; };

class bar5
{
public:
  virtual foo5 getFoo(int i){
    printf("This = %p\n", this);
    foo5 f;
    f.i = 1;
    f.j = 2 + i;
    f.p = (void*)this;
    return f;
  }
};

bar5* newBar()
{
  bar5* b = new bar5();
  printf("bar = %p\n", b);
  return b;
}


/**************************************/

struct A11802;
struct B11802;

class C11802
{
public:
    virtual void fun(A11802 *);
    virtual void fun(B11802 *);
};

class D11802 : public C11802
{
public:
    void fun(A11802 *);
    void fun(B11802 *);
};

void test11802x(D11802 *c)
{
    c->fun((A11802 *)0);
    c->fun((B11802 *)0);
}

/**************************************/

typedef struct
{
    int i;
    double d;
} S6;

union S6_2
{
    int i;
    double d;
};

enum S6_3
{
    A, B
};


S6 foo6(void)
{
    S6 s;
    s.i = 42;
    s.d = 2.5;
    return s;
}

S6_2 foo6_2(void)
{
    S6_2 s;
    s.i = 42;
    return s;
}

S6_3 foo6_3(void)
{
    S6_3 s = A;
    return s;
}

extern "C" { int foosize6()
{
    return sizeof(S6);
}
}

/**************************************/

typedef struct
{
    int i;
    long long d;
} S7;

extern "C" { int foo7()
{
    return sizeof(S7);
}
}

/**************************************/

struct S13955a
{
    float a;
    double b;
};

struct S13955b
{
    double a;
    float b;
};

struct S13955c
{
    float a;
    float b;
};

struct S13955d
{
    double a;
    double b;
};

void check13955(S13955a a, S13955b b, S13955c c, S13955d d);

void func13955(S13955a a, S13955b b, S13955c c, S13955d d)
{
    check13955(a, b, c, d);
}

/**************************************/

struct Struct10071
{
    void *p;
    long double r;
};

size_t offset10071()
{
    Struct10071 s;
    return (char *)&s.r - (char *)&s;
}

/**************************************/

void foo8(const char *p)
{
}

/**************************************/
// 4059

struct elem9 { };
void foobar9(elem9*, elem9*) { }

/**************************************/
// 5148

void foo10(const char*, const char*) { }
void foo10(const int, const int) { }
void foo10(const char, const char) { }
void foo10(bool, bool) { }

struct MyStructType { };
void foo10(const MyStructType s, const MyStructType t) { }

enum MyEnumType { onemember };
void foo10(const MyEnumType s, const MyEnumType t) { }

/**************************************/

namespace N11 { namespace M { void bar11() { } } }

namespace A11 { namespace B { namespace C { void bar() { } } } }

/**************************************/

void myvprintfx(const char* format, va_list);

void myvprintf(const char* format, va_list va)
{
    myvprintfx(format, va);
}

/**************************************/

class C13161
{
public:
        virtual void dummyfunc() {}
        long long val_5;
        unsigned val_9;
};

class Test : public C13161
{
public:
        unsigned val_0;
        long long val_1;
};

size_t getoffset13161()
{
    Test s;
    return (char *)&s.val_0 - (char *)&s;
}

class C13161a
{
public:
        virtual void dummyfunc() {}
        long double val_5;
        unsigned val_9;
};

class Testa : public C13161a
{
public:
        bool val_0;
};

size_t getoffset13161a()
{
    Testa s;
    return (char *)&s.val_0 - (char *)&s;
}

/****************************************************/

#if __linux__ || __APPLE__ || __FreeBSD__
#include <memory>
#include <vector>
#include <string>

#if __linux__
template struct std::allocator<int>;
template struct std::vector<int>;

void foo15()
{
    std::allocator<int>* p;
    p->deallocate(0, 0);
}

#endif

// _Z5foo14PSt6vectorIiSaIiEE
void foo14(std::vector<int, std::allocator<int> > *p) { }

void foo14a(std::basic_string<char> *p) { }
void foo14b(std::basic_string<int> *p) { }
void foo14c(std::basic_istream<char> *p) { }
void foo14d(std::basic_ostream<char> *p) { }
void foo14e(std::basic_iostream<char> *p) { }

void foo14f(std::char_traits<char>* x, std::basic_string<char> *p, std::basic_string<char> *q) { }

#endif

/**************************************/

struct S13956
{
};

void check13956(S13956 arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6);

void func13956(S13956 arg0, int arg1, int arg2, int arg3, int arg4, int arg5, int arg6)
{
    check13956(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

/**************************************/

wchar_t f13289_cpp_wchar_t(wchar_t ch)
{
    if (ch <= L'z' && ch >= L'a')
    {
        return ch - (L'a' - L'A');
    }
    else
    {
        return ch;
    }
}

#if __linux__ || __APPLE__ || __FreeBSD__ || __OpenBSD__ || __sun || __NetBSD__
unsigned short f13289_d_wchar(unsigned short ch);
wchar_t f13289_d_dchar(wchar_t ch);
#elif _WIN32
wchar_t f13289_d_wchar(wchar_t ch);
unsigned int f13289_d_dchar(unsigned int ch);
#endif

wchar_t f13289_d_wchar_t(wchar_t ch);

bool f13289_cpp_test()
{
    if (!(f13289_d_wchar_t(L'e') == L'E')) return false;
    if (!(f13289_d_wchar_t(L'F') == L'F')) return false;
#if __linux__ || __APPLE__ || __FreeBSD__ || __OpenBSD__ || __sun || __NetBSD__
    if (!(f13289_d_wchar((unsigned short)'c') == (unsigned short)'C')) return false;
    if (!(f13289_d_wchar((unsigned short)'D') == (unsigned short)'D')) return false;
    if (!(f13289_d_dchar(L'e') == L'E')) return false;
    if (!(f13289_d_dchar(L'F') == L'F')) return false;
    return true;
#elif _WIN32
    if (!(f13289_d_wchar(L'c') == L'C')) return false;
    if (!(f13289_d_wchar(L'D') == L'D')) return false;
    if (!(f13289_d_dchar((unsigned int)'e') == (unsigned int)'E')) return false;
    if (!(f13289_d_dchar((unsigned int)'F') == (unsigned int)'F')) return false;
    return true;
#else
    return false;
#endif
}

/******************************************/

long double testld(long double ld)
{
    assert(ld == 5);
    return ld + 1;
}

long double testldld(long double ld1, long double ld2)
{
    assert(ld1 == 5);
    return ld2 + 1;
}

long testl(long lng)
{
    assert(lng == 5);
    return lng + sizeof(long);
}

unsigned long testul(unsigned long ul)
{
    assert(ul == 5);
    return ul + sizeof(unsigned long);
}

/******************************************/

struct S13707
{
    void* a;
    void* b;
    S13707(void *a, void* b)
    {
        this->a = a;
        this->b = b;
    }
};

S13707 func13707()
{
    S13707 pt(NULL, NULL);
    return pt;
}

/******************************************/

template <int x> struct S13932
{
    int member;
};

void func13932(S13932<-1> s) {}

/******************************************/

namespace N13337 {
  namespace M13337 {
    struct S13337 { };
    void foo13337(S13337 s) { }
  }
}

/****************************************/
// 14195

template <typename T>
struct Delegate1 {};

template <typename R1>
struct Delegate1 < R1() > {};

template <typename T1, typename T2>
struct Delegate2 {};

template < typename R1, typename T1, typename T2, typename R2, typename T3, typename T4 >
struct Delegate2<R1(T1, T2), R2(T3, T4)> {};

void test14195a(Delegate1<void()> func) {}

void test14195b(Delegate2<int(float, double), int(float, double)> func) {}

/******************************************/
// 14200

void test14200a(int a) {};
void test14200b(float a, int b, double c) {};

/******************************************/
// 14956

namespace std {
    namespace N14956 {
	struct S14956 { };
    }
}

void test14956(std::N14956::S14956 s) { }

/******************************************/
// check order of overloads in vtable

class Statement;
class ErrorStatement;
class PeelStatement;
class ExpStatement;
class DtorExpStatement;

class Visitor
{
public:
    virtual int visit(Statement*) { return 1; }
    virtual int visit(ErrorStatement*) { return 2; }
    virtual int visit(PeelStatement*) { return 3; }
};

class Visitor2 : public Visitor
{
public:
    virtual int visit2(ExpStatement*) { return 4; }
    virtual int visit2(DtorExpStatement*) { return 5; }
};

bool testVtableCpp(Visitor2* sv)
{
    if (sv->visit((Statement*)0) != 1) return false;
    if (sv->visit((ErrorStatement*)0) != 2) return false;
    if (sv->visit((PeelStatement*)0) != 3) return false;
    if (sv->visit2((ExpStatement*)0) != 4) return false;
    if (sv->visit2((DtorExpStatement*)0) != 5) return false;
    return true;
}

Visitor2 inst;

Visitor2* getVisitor2()
{
    return &inst;
}

/******************************************/
// issues detected by fuzzer
#if _LP64
#define longlong long
#else
#define longlong long long
#endif

void fuzz1_checkValues(longlong arg10, longlong arg11, bool arg12);
void fuzz1_cppvararg(longlong arg10, longlong arg11, bool arg12)
{
    fuzz1_checkValues(arg10, arg11, arg12);
}

void fuzz2_checkValues(unsigned longlong arg10, unsigned longlong arg11, bool arg12);
void fuzz2_cppvararg(unsigned longlong arg10, unsigned longlong arg11, bool arg12)
{
    fuzz2_checkValues(arg10, arg11, arg12);
}

#if __linux__ || __APPLE__ || __FreeBSD__ || __OpenBSD__ || __sun || __NetBSD__
#define wchar unsigned short
#elif _WIN32
#define wchar wchar_t
#endif

void fuzz3_checkValues(wchar arg10, wchar arg11, bool arg12);
void fuzz3_cppvararg(wchar arg10, wchar arg11, bool arg12)
{
    fuzz3_checkValues(arg10, arg11, arg12);
}

/******************************************/

void throwit()
{
#if _WIN32
#else
    std::exception se;
    throw se;
#endif
}

/******************************************/

#if linux
#include <stdexcept>

void throwle()
{
     std::logic_error le("test");
     throw le;
}

#endif

/******************************************/
// 15579

/******************************************/
// 15579

class Base
{
public:
    //virtual ~Base() {}
    virtual void base();
    unsigned char x;
};

class Interface
{
public:
    virtual int MethodCPP() = 0;
    virtual int MethodD() = 0;
};

class Derived : public Base, public Interface
{
public:
    Derived();
    short y;
    int MethodCPP();
#if _WIN32 || _WIN64
    int MethodD();
    virtual int Method();
#else
    int MethodD() { return 3; }  // need def or vtbl[] is not generated
    virtual int Method()  { return 6; }  // need def or vtbl[] is not generated
#endif
};

void Base::base() { }
int Derived::MethodCPP() {
    printf("Derived::MethodCPP() this = %p, x = %d, y = %d\n", this, x, y);
    assert(x == 4 || x == 7);
    assert(y == 5 || y == 8);
    return 30;
}
Derived::Derived() { }


Derived *cppfoo(Derived *d)
{
    printf("cppfoo(): d = %p\n", d);
    assert(d->x == 4);
    assert(d->y == 5);
    assert(d->MethodD() == 3);
    assert(d->MethodCPP() == 30);
    assert(d->Method() == 6);

    d = new Derived();
    d->x = 7;
    d->y = 8;
    assert(d->MethodD() == 3);
    assert(d->MethodCPP() == 30);
    assert(d->Method() == 6);
    printf("d1 = %p\n", d);
    return d;
}

Interface *cppfooi(Interface *i)
{
    printf("cppfooi(): i = %p\n", i);
    assert(i->MethodD() == 3);
    assert(i->MethodCPP() == 30);

    Derived *d = new Derived();
    d->x = 7;
    d->y = 8;
    printf("d = %p, i = %p\n", d, (Interface *)d);
    return d;
}

/******************************************/
// 15610

class Base2
{
  public:
    int i;
    virtual void baser() { }
};

class Interface2
{
  public:
    virtual void f() = 0;
};

class Derived2 : public Base2, public Interface2
{
  public:
    void f();
};

void Derived2::f()
{
    printf("Derived2::f() this = %p i = %d\n", this, i);
    assert(i == 3);
}

/******************************************/
// 15455

struct X6
{
    unsigned short a;
    unsigned short b;
    unsigned char c;
    unsigned char d;
};

struct X8
{
    unsigned short a;
    X6 b;
};

void test15455b(X8 s)
{
    assert(sizeof(X6) == 6);
    assert(sizeof(X8) == 8);
    assert(s.a == 1);
    assert(s.b.a == 2);
    assert(s.b.b == 3);
    assert(s.b.c == 4);
    assert(s.b.d == 5);
}

/******************************************/
// 15372

template <typename T>
int foo15372(int value)
{
    return value;
}

void test15372b()
{
	int t = foo15372<int>(1);
}

/****************************************/
// 15576

namespace ns15576
{
    int global15576;

    namespace ns
    {
        int n_global15576;
    }
}

/****************************************/
// 15802

template <typename T>
class Foo15802
{
public:
    static int boo(int value)
    {
        return value;
    }
};

void test15802b()
{
	int t = Foo15802<int>::boo(1);
}


/****************************************/
// 16536 - mangling mismatch on OSX

#if defined(__APPLE__)
__UINTMAX_TYPE__ pass16536(__UINTMAX_TYPE__ a)
{
    return a;
}
#endif
