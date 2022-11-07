/* { dg-do compile } */
/* { dg-options "-O1" } */

int printf(const char *, ...);
int a, b;
void c() {
  int d, e;
 L:
  a = (b && a) ^ 2756578370;
  d = ~a + (e ^ d) ^ 2756578370;
  if (!d)
    printf("%d", a);
  d = a / e;
  goto L;
}
int main() {
  if (a)
    c();
  return 0;
}

