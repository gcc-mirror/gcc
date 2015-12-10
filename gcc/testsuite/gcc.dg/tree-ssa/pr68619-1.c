/* { dg-do compile } */
/* { dg-options "-O2 -w" } */

extern void fn2(int);
int a, b, c;
void fn1() {
  int d;
  for (; b; b++) {
    a = 7;
    for (; a;) {
    jump:
      fn2(d ?: c);
      d = 0;
    }
    d = c;
    if (c)
      goto jump;
  }
  goto jump;
}
