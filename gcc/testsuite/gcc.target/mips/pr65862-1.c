/* { dg-do compile } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\\\$f\[0-9\]+" } } */
int a, c;
int *b, *d;
void
fn1(int p1, int *p2(void *, void *), void *p3(void *, void *, int)) {
  int n = c;
  for (;;) {
    a = 1;
    for (; a < n;) {
      *d = p1 && p2(0, (int *) ((long)p1 + 1));
      p3(0, b + p1, 0);
    }
  }
}
