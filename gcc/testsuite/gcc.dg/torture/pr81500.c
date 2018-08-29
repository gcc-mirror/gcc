/* { dg-do compile } */

typedef int a;
void c(int *b)
{
  int d;
  a e, f, *g, *h = b;
  for (; d; d--) {
      f = *g & 1;
      *h-- = *g-- | e;
      e = f;
  }
}
