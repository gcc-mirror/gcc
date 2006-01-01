/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-xfail-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-Wno-long-long" } */

struct a
{
  int tt;
  long long t;
  int i;
};

struct g
{
  int tt;
  struct a d;
  int t;
};

int f[sizeof(struct g)!=24?-1:1];
