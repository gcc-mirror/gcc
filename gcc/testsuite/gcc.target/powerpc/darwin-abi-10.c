/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-Wno-long-long" } */

struct b
{
  long long t;
  int i;
};

struct c
{
  double d;
  int i;
};

struct n
{
  long long ll;
  int tt;
  struct c d;
  struct b h;
  int t;
};
int f[sizeof(struct n)!=48?-1:1];
