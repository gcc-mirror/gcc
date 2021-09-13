/* { dg-do compile } */

long *a, *b;
long c;
void d(void)
{
  b = a;
  while (c) {
    *a = (__INTPTR_TYPE__)(a += (long)1 << (sizeof(long) * 8 - 10));
    c = b[0];
    b = a;
  }
}
