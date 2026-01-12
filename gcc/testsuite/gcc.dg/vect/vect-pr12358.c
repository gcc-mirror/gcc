/* { dg-do compile } */

typedef __attribute__((__vector_size__(1))) char V;
unsigned char c;
union {
  _Bool b;
  V v;
} u;

_Complex char *p;

void
foo()
{
  _Bool t = u.b;
  int t1 = t;
  char ip = __imag__ *p;
  char rp = __real__ *p;
  int t2 = rp;
  _Bool pp = t1 != t2;
  _Bool p2 = ip != 0;
  p2 |= pp;
  u.b = p2;
  u.v |= c;
}
