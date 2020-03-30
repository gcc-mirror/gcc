
#include <assert.h>
#include <stdio.h>
#include <stdint.h>

#if __cplusplus
extern "C" {
#endif

struct Foo1 { char c; };

struct Foo1 ctest1()
{
    struct Foo1 f;

    f.c = 3;
    return f;
}

struct Foo2 { short s; };

struct Foo2 ctest2()
{
    struct Foo2 f;

    f.s = 0x1234;
    return f;
}

struct Foo3 { char c; short s; };

struct Foo3 ctest3()
{
    struct Foo3 f;

    f.s = 0x5678;
    return f;
}


struct Foo4 { int i; };

struct Foo4 ctest4()
{
    struct Foo4 f;

    f.i = 0x12345678;
    return f;
}

struct Foo5 { int i, j; };

struct Foo5 ctest5()
{
    struct Foo5 f;

    f.i = 0x12345678;
    f.j = 0x21436587;
    return f;
}


struct Foo6 { int i, j, k; };

struct Foo6 ctest6()
{
    struct Foo6 f;

    f.i = 0x12345678;
    f.j = 0x21463587;
    f.k = 0x24163857;
    return f;
}

struct S7 { float a,b; };

struct S7 ctest10()
{
    struct S7 f;

    f.a = 2.5;
    f.b = 1.5;
    return f;
}

// =================================

char ctest7(char c)
{
    return c + 1;
}

unsigned char ctest8(unsigned char c)
{
    return c + 1;
}

signed char ctest9(signed char c)
{
    return c + 1;
}

/***********************************************/

void ctestrir(int x1, int x2, int x3, int x4, int x5, int x6, long double a, int b, long double c)
{
    assert(a == 100.0);
    assert(b == 67);
    assert(c == 200.0);
}

/***********************************************/

extern void dtestrir(int x1, int x2, int x3, int x4, int x5, int x6, long double a, int b, long double c);

void test4()
{
    dtestrir(1,2,3,4,5,6, 300.0, 68, 401.0);
}

/**********************************************/

typedef struct S11 {
  char a;
  char b;
  char c;
} S11;

S11 ctest11(char x, S11 s, char y) {
  printf("C sz = %d\n", (int)sizeof(S11));
  assert(sizeof(S11) == 3);
  printf("x   = %d\n", (int)x);
  printf("s.a = %d\n", (int)s.a);
  printf("s.b = %d\n", (int)s.b);
  printf("s.c = %d\n", (int)s.c);
  printf("y   = %d\n", (int)y);
  return s;
}

/**********************************************/

typedef struct S12 {
  char a,d;
  char b,e;
  char c;
} S12;

S12 ctest12(char x, S12 s, char y) {
  printf("C sz = %d\n", (int)sizeof(S12));
  assert(sizeof(S12) == 5);
  printf("x   = %d\n", (int)x);
  printf("s.a = %d\n", (int)s.a);
  printf("s.b = %d\n", (int)s.b);
  printf("s.c = %d\n", (int)s.c);
  printf("y   = %d\n", (int)y);
  return s;
}


/**********************************************/

typedef struct S13 {
  short a;
  short b;
  short c;
} S13;

S13 ctest13(char x, S13 s, char y) {
  printf("C sz = %d\n", (int)sizeof(S13));
  assert(sizeof(S13) == 6);
  printf("x   = %d\n", (int)x);
  printf("s.a = %d\n", (int)s.a);
  printf("s.b = %d\n", (int)s.b);
  printf("s.c = %d\n", (int)s.c);
  printf("y   = %d\n", (int)y);
  return s;
}


/**********************************************/

typedef struct S14 {
  char a,d,e,f;
  char b,g;
  char c;
} S14;

S14 ctest14(char x, S14 s, char y) {
  printf("C sz = %d\n", (int)sizeof(S14));
  assert(sizeof(S14) == 7);
  printf("x   = %d\n", (int)x);
  printf("s.a = %d\n", (int)s.a);
  printf("s.b = %d\n", (int)s.b);
  printf("s.c = %d\n", (int)s.c);
  printf("y   = %d\n", (int)y);
  return s;
}


/**********************************************/

typedef struct S15 {
  char a,d,e,f;
  char b,g,h,i;
  char c;
} S15;

S15 ctest15(char x, S15 s, char y) {
  printf("C sz = %d\n", (int)sizeof(S15));
  assert(sizeof(S15) == 9);
  printf("x   = %d\n", (int)x);
  printf("s.a = %d\n", (int)s.a);
  printf("s.b = %d\n", (int)s.b);
  printf("s.c = %d\n", (int)s.c);
  printf("y   = %d\n", (int)y);
  return s;
}


/**********************************************/

typedef struct S16 {
  char a[5];
#ifdef __GNUC__
  struct __attribute__((packed))
#else
  #pragma pack(push, 1)
  struct
#endif
  {
    char b;
    int c;
  };
#ifndef __GNUC__
  #pragma pack(pop)
#endif
} S16;

S16 ctest16(char x, S16 s, char y) {
  printf("C sz = %d\n", (int)sizeof(S16));
  assert(sizeof(S16) == 10);
  printf("x   = %d\n", (int)x);
  printf("s.a = %.*s\n", 5, s.a);
  printf("s.b = %d\n", (int)s.b);
  printf("s.c = %d\n", s.c);
  printf("y   = %d\n", (int)y);
  return s;
}



#if __cplusplus
}
#endif

