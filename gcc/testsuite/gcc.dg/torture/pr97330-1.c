/* { dg-do compile } */
/* { dg-additional-options "-Wno-old-style-definition" } */

typedef int a;
typedef char b;
int c;
void d(e, f, dst, g, avail, h) int e;
b *f, *dst;
a g, avail;
int h;
{
  b i = *f;
  if (e)
    goto j;
  while (avail) {
    *dst = i;
  j:
    avail -= c;
  }
}
