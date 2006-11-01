/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-Wno-long-long" } */

struct A
{
  long long a;
  unsigned char b;
};

struct D
{
  unsigned char y;
  struct A x;
  unsigned char z;
};

struct E
{
  long long d;
  unsigned char e;
};

struct y
{
  struct A b2;
  struct D b3;
  struct E b4;
};

int f[sizeof(struct y)!=56?-1:1];
