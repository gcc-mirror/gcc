/* { dg-do compile } */
/* { dg-additional-options "-std=gnu17" } */

extern void f();
char a[1][1], b;
int main() {
  int c = -1U;
  if (b)
    f(a[c][b]);
  return 0;
}
