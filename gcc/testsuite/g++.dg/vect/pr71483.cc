/* { dg-do compile } */
int b, c, d;
short *e;
void fn1() {
  for (; b; b--) {
    d = *e >> 2;
    *e++ = d;
    c = *e;
    *e++ = d;
  }
}
