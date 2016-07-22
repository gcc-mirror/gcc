/* { dg-do compile } */
/* { dg-options "-Ofast" { target *-*-* } } */

int a, b;
unsigned long d;
void fn1() {
  unsigned long *h = &d;
line1 : {
  int i = 4;
  for (; b; i++) {
    d = ((d + 6 ?: *h) ? a : 7) && (i &= 0 >= b);
    b += a;
  }
}
  h = 0;
  for (; *h;)
    goto line1;
}
