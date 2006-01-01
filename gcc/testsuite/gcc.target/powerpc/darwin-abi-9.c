/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-xfail-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-Wno-long-long" } */

struct b
{
  long long t;
  int i;
};

struct l
{
  int i;
  double d;
};
struct k
{
  int tt;
  struct l d;
  struct b h;
  int t;
};

int f[sizeof(struct k)!=36?-1:1];
