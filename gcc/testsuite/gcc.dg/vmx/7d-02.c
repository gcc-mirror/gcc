/* { dg-do compile } */
#include <altivec.h>
extern vector unsigned char a[];

void f
(vector unsigned char v, vector unsigned char *p, int i, int b)
{
  if (b)
    p[i] = v;
  else
    vec_st(v, i*16,p);
}

void g
(vector unsigned char v, int i, int b)
{
  if (b)
    a[i] = v;
  else
    vec_st(v,i*16,a);
}
