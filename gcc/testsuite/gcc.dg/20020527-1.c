/* PR c/6660
   Test whether an unnamed field with user defined type - struct or union is
   accepted.  */
/* { dg-do compile } */
/* { dg-options "-fms-extensions" } */

typedef struct {
  unsigned short a;
  unsigned short b;
} __attribute__ ((__packed__)) A;

typedef struct B_ {
  unsigned int c;
  unsigned int d;
} B;

typedef struct C_ {
  B;
  unsigned int e;
  unsigned int f;
} C;

typedef C D;

typedef struct {
  A;
  D;
  struct {
    unsigned short g;
    unsigned short h;
  } __attribute__ ((__packed__));
  union {
    int i;
    long j;
  };
  int k;
} __attribute__ ((__packed__)) E;

E x;

void foo (void)
{
  x.a = 1;
  x.b = 2;
  x.c = 3;
  x.d = 4;
  x.e = 5;
  x.f = 6;
  x.g = 7;
  x.h = 8;
  x.i = 9;
  x.j = 10;
  x.k = 11;
}
