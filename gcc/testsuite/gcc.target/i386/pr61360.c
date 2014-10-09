/* { dg-do compile } */
/* { dg-options "-march=amdfam10 -O2" } */
int a, b, c, e, f, g, h;
long *d;
__attribute__((cold)) void fn1() {
  int i = g | 1;
  for (; g; h++) {
    for (; a; e++) d[0] = c;
    if (0.002 * i) break;
    for (; b; f++) d[h] = 0;
  }
}
