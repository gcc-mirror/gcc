/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-Wno-long-long" } */

struct A
{
  long long a;
  unsigned char b;
};

struct B
{
  struct A x;
  unsigned char z;
};

struct C
{
  long d;
  unsigned char e;
};

struct z
{ 
  struct A b2;
  struct B b3;
  struct C b4;
};

int f[sizeof(struct z)!=48?-1:1];
