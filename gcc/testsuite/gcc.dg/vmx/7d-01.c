/* { dg-do compile } */
#include <altivec.h>
extern vector unsigned char a[];

vector unsigned char
f(vector unsigned char *p, int i, int b)
{
  if (b)
    return p[i];
  return vec_ld(i*16,p);
}

vector unsigned char
g(int i, int b)
{
  if (b)
    return a[i];
  return vec_ld(i*16,a);
}
