/* PR middle-end/93200 - spurious -Wstringop-overflow due to assignment
   vectorization to multiple members
   { dg-do compile }
   { dg-options "-O3 -Wall" } */

typedef __INT8_TYPE__  int8_t;
typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;

struct A { char b, c; };
struct B1A { int8_t i8; struct A a; };
struct B2A { int16_t i16; struct A a; };
struct B3A { int16_t i16; int8_t i8; struct A a; };
struct B4A { int64_t i64; struct A a; };

void ba1 (struct B1A *p)
{
  p->a.b = 0; p->a.c = 1;
}

void b2a (struct B2A *p)
{
  /* This results in:
     vector(2) char *vectp.14_6 = &p_2(D)->a.b;
     MEM <vector(2) char> [(char *)vectp.14_6] = { 4, 5 };  */

  p->a.b = 4;       // { dg-bogus "-Wstringop-overflow" }
  p->a.c = 5;
}

void b3a (struct B3A *p)
{
  p->a.b = 4; p->a.c = 5;
}

void b4a (struct B4A *p)
{
  /* This results in:
     vector(2) char *vectp.22_6 = &p_2(D)->a.b;
     MEM <vector(2) char> [(char *)vectp.22_6] = { 6, 7 };  */

  p->a.b = 6;       // { dg-bogus "-Wstringop-overflow" }
  p->a.c = 7;
}


struct Aa { char a[2], b[2]; };
struct B1Aa { int8_t i8; struct Aa a; };
struct B2Aa { int16_t i16; struct Aa a; };
struct B3Aa { int16_t i16; int8_t i8; struct Aa a; };
struct B4Aa { int64_t i64; struct Aa a; };

void b1aa (struct B1Aa *p)
{
  p->a.a[0] = 0; p->a.a[1] = 1;
  p->a.b[0] = 0; p->a.b[1] = 1;
}

void b2aa (struct B2Aa *p)
{
  p->a.a[0] = 2; p->a.a[1] = 3;
  p->a.b[0] = 2; p->a.b[1] = 3;
}

void b3aa (struct B3Aa *p)
{
  p->a.a[0] = 4; p->a.a[1] = 5;
  p->a.b[0] = 4; p->a.b[1] = 5;
}

void b4aa (struct B4Aa *p)
{
  /* This results in:
     vector(4) char *vectp.36_8 = &p_2(D)->a.a[0];
     MEM <vector(4) char> [(char *)vectp.36_8] = { 6, 7, 6, 7 };  */

  p->a.a[0] = 6; p->a.a[1] = 7;
  p->a.b[0] = 6; p->a.b[1] = 7;
}
