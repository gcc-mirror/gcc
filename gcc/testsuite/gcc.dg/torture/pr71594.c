/* { dg-do compile } */
/* { dg-options "--param max-rtl-if-conversion-insns=2" } */

unsigned short a;
int b, c;
int *d;
void fn1() {
  *d = 24;
  for (; *d <= 65;) {
    unsigned short *e = &a;
    b = (a &= 0 <= 0) < (c ?: (*e %= *d));
    for (; *d <= 83;)
      ;
  }
}
