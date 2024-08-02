/* { dg-do compile } */

typedef union { char a[__INT_MAX__ / 4]; } T;
unsigned short f(const void *p)
{
  unsigned short v;
  *(T *)(void *)(&v) = *(const T *)p;
  return v;
}
