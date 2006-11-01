/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */

struct c
{
  double d;
  int i;
};

struct j
{
  int tt;
  struct c d;
  int t;
};

int f[sizeof(struct j)!=24?-1:1];
