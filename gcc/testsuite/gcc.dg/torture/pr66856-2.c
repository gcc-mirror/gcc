/* { dg-do compile } */

typedef int uint32_t;
int c, e, f, g, h;
short *d;
uint32_t fn1(uint32_t p1, uint32_t p2)
{
  uint32_t a, b;
  a = p1 >> 3 & p2;
  b = p1 & 072;
  a |= a >> 5;
  a |= b >> 5;
  return a;
}

void fn2()
{
  uint32_t *i;
  uint32_t j;
  while (c -= 4) {
      fn1(e, j);
      fn1(f, j) * fn1(g, j) * fn1(h, j);
      *d++ = fn1(*i++, j);
      *d++ = fn1(*i++, j);
  }
}
