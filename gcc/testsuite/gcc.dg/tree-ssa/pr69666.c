/* { dg-do compile } */
/* { dg-options "-std=gnu17 -O2 -w" } */

int a, c, d;
float b;
void *memcpy();
int fun1(int p1, unsigned char *p2) {
  p2[p1] = b;
  return a;
}
void fun2() {
  unsigned char e[16];
  fun1(16, e);
  d = e[d];
  memcpy(&c, e, sizeof(e));
}
