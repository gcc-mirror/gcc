/* { dg-do compile } */
#include <altivec.h>
void
f(vector unsigned char *a, vector unsigned char *b, vector unsigned char *c)
{
  int i;
  for (i = 0; i < 16; i++)
    c[i] = vec_add(a[i], b[i]);
}
