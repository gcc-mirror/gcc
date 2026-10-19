/* { dg-do run } */
/* { dg-options "-O1" } */

#define a() 0
int b, c, d;
int __attribute__((noipa)) fn1() {
  unsigned long e = b = 7;
  for (; b; b--) {
    c = ~e;
    c >>= 10;
  }
  e = c & 8 ? c & ~0x3fc00 : a();
  if (e != ~0x3fc00UL)
    return 3;
  return 2;
}
int main() {
  d = fn1();
  if (d != 2)
    __builtin_abort ();
  return 0;
}
