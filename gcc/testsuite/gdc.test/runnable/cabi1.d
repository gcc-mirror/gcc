
// EXTRA_CPP_SOURCES: cabi2.cpp

import core.stdc.stdio;
import core.stdc.config;

struct Foo1 { char c; }
struct Foo2 { short s; }
struct Foo3 { char c; short s; }
struct Foo4 { int i; }
struct Foo5 { int i, j; }
struct Foo6 { int i, j, k; }
struct S7 { float a, b; }

extern (C) Foo1 ctest1();
extern (C) Foo2 ctest2();
extern (C) Foo3 ctest3();
extern (C) Foo4 ctest4();
extern (C) Foo5 ctest5();
extern (C) Foo6 ctest6();
extern (C) S7 ctest10();

version(Windows)
    version = Windows_or_32bit;
else version(X86)
    version = Windows_or_32bit;


void test1()
{
    Foo1 f1 = ctest1();
    assert(f1.c == 3);

    Foo2 f2 = ctest2();
    assert(f2.s == 0x1234);

    Foo3 f3 = ctest3();
    assert(f3.s == 0x5678);

    Foo4 f4 = ctest4();
    assert(f4.i == 0x12345678);

    Foo5 f5 = ctest5();
    assert(f5.i == 0x12345678);
    assert(f5.j == 0x21436587);

version(Windows_or_32bit)
{
    Foo6 f6 = ctest6();
    assert(f6.i == 0x12345678);
    assert(f6.j == 0x21463587);
    assert(f6.k == 0x24163857);
}

    S7 s7 = ctest10();
    assert(s7.a == 2.5);
    assert(s7.b == 1.5);
}

/*******************************************/

extern (C)
{
    char ctest7(char);
    ubyte ctest8(ubyte);
    byte ctest9(byte);
}

void test2()
{
    assert(ctest7('a') == 'b');
    assert(ctest8(7) == 8);
    assert(ctest9(3) == 4);
}

/******************************************/

extern (C)
{
    void ctestrir(int x1, int x2, int x3, int x4, int x5, int x6, c_long_double a, int b, c_long_double c);
}

void test3()
{
    ctestrir(1,2,3,4,5,6, c_long_double(100.0), 67, c_long_double(200.0));
}

/******************************************/

extern (C) void dtestrir(int x1, int x2, int x3, int x4, int x5, int x6, c_long_double a, int b, c_long_double c)
{
    assert(a == 300.0);
    assert(b == 68);
    assert(c == 401.0);
}

extern (C) void test4();

/******************************************/

struct S11 { ubyte a, b, c; }

extern (C) S11 ctest11(ubyte x, S11, ubyte y);

void test11()
{
  version (X86)
  {
  S11 t;
  assert(S11.sizeof == 3);
  t.a = 2;
  t.b = 3;
  t.c = 4;
  auto s = ctest11(1, t, 5);
  assert(s.a == 2);
  assert(s.b == 3);
  assert(s.c == 4);
  }
}

/******************************************/

struct S12 { char a,d; char b,e; ubyte c; }

extern (C) S12 ctest12(ubyte x, S12, ubyte y);

void test12()
{
  version (X86)
  {
  S12 t;
  printf("D sz = %d\n", cast(int)S12.sizeof);
//  assert(S12.sizeof == 5);
  t.a = 2;
  t.b = 3;
  t.c = 4;
  auto s = ctest12(1, t, 5);
  assert(s.a == 2);
  assert(s.b == 3);
  assert(s.c == 4);
  }
}

/******************************************/

struct S13 { ushort a, b, c; }

extern (C) S13 ctest13(ubyte x, S13, ubyte y);

void test13()
{
  version (X86)
  {
  S13 t;
  assert(S13.sizeof == 6);
  t.a = 2;
  t.b = 3;
  t.c = 4;
  auto s = ctest13(1, t, 5);
  assert(s.a == 2);
  assert(s.b == 3);
  assert(s.c == 4);
  }
}

/******************************************/

struct S14 { char a,d,e,f; char b,g; ubyte c; }

extern (C) S14 ctest14(ubyte x, S14, ubyte y);

void test14()
{
  version (X86)
  {
  S14 t;
  assert(S14.sizeof == 7);
  t.a = 2;
  t.b = 3;
  t.c = 4;
  auto s = ctest14(1, t, 5);
  assert(s.a == 2);
  assert(s.b == 3);
  assert(s.c == 4);
  }
}

/******************************************/

struct S15 { char a,d,e,f; char b,g,h,i; ubyte c; }

extern (C) S15 ctest15(ubyte x, S15, ubyte y);

void test15()
{
  version (X86)
  {
  S15 t;
  assert(S15.sizeof == 9);
  t.a = 2;
  t.b = 3;
  t.c = 4;
  auto s = ctest15(1, t, 5);
  assert(s.a == 2);
  assert(s.b == 3);
  assert(s.c == 4);
  }
}

/******************************************/

// see https://issues.dlang.org/show_bug.cgi?id=17277
struct S16 {
  char[5] a;
  struct {
    char b;
    align(1) int c;
  }
}

extern (C) S16 ctest16(ubyte x, S16, ubyte y);

void test16()
{
  version (X86) // misaligned field
  {
  S16 t;
  assert(S16.sizeof == 10);
  assert(S16.alignof == 1);
  t.a = "hello";
  t.b = 3;
  t.c = 0x11223344;
  auto s = ctest16(1, t, 5);
  assert(s.a == "hello");
  assert(s.b == 3);
  assert(s.c == 0x11223344);
  }
}

/******************************************/

int main()
{
    test1();
    test2();
    test3();
version (Win64)
{
}
else
{
    test4();
}
    test11();
    test12();
    test13();
    test14();
    test15();
    test16();

    return 0;
}
