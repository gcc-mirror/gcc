/* { dg-do compile { target powerpc*-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-Wno-long-long" } */
struct f
{
  long long ll;
  int i;
};

int f[sizeof(struct f)!=16?-1:1];
