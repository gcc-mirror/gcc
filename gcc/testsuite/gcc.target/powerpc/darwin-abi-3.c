/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-xfail-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-Wno-long-long" } */
struct f
{
  int i;
  long long ll;
};

int f[sizeof(struct f)!=12?-1:1];
