/* { dg-do run } */
/* { dg-options "-Os" } */

int b, c, d;
char e;
short f;
const unsigned short **g;
char h(char k) {
  if (k)
    return '0';
  return 0;
}
int l() {
  b = 0;
  return 1;
}
static short m(unsigned k) {
  const unsigned short *n[65];
  g = &n[4];
  k || l();
  __INTPTR_TYPE__ a = k;
  char i = 0;
  unsigned long j = k;
  while (j--)
    *(char *)a++ = i;
  c = h(d);
  f = k;
  return 0;
}
int main() {
  long o = (e < 0) << 5;
  m(o);
  if (f != 0)
    __builtin_abort ();
  return 0;
}
