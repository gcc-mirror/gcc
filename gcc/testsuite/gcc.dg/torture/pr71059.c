/* { dg-do compile } */

short a, c;
union {
    unsigned f0;
    unsigned short f1;
} b;
volatile int d;
short fn1(short p1) { return p1 + a; }
void fn2()
{
  b.f0 = 0;
  for (;; b.f0 = fn1(b.f0))
    (c && b.f1) || d;
}
