/* { dg-do run } */
/* { dg-options "-O1" } */

#include <stdlib.h>

typedef union
{
  struct
  {
    unsigned char a;
    unsigned char b;
    unsigned char c;
    unsigned char d;
  } parts;
  unsigned long whole;
} T;

T *g_t;

void bar (unsigned long x)
{
  if (x != 0)
    abort ();
}

int main ()
{
  T one;
  T two;
  T tmp1, tmp2;

  one.whole = 0xFFE0E0E0UL;
  two.whole = 0xFF000000UL;
  tmp1.parts = two.parts;
  tmp2.parts = one.parts;
  tmp2.parts.c = tmp1.parts.c;
  one.parts = tmp2.parts;

  g_t = &one;
  bar (0);
  return 0;
}
