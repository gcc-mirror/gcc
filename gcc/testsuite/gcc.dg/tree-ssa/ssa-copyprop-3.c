/* { dg-do link } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O -fdump-tree-copyprop2" } */

#include <limits.h>
enum { a } b();
int d;
int e;
int f;
void foo();
[[gnu::noipa]]
void bar49_(void){}
[[gnu::noipa]]
void(c)(void){}
static short g(int h, int i) {
  int j = -1420678603, k = 1;
  if (h)
    for (; j < INT_MAX-18; j = j + 9) {
      f = 0;
      for (; f <= 1; c())
        k = 90;
    }
  i = k;
  for (; e; ++e) {
    if (i)
      continue;
    foo();
    i = b();
  }
  return 4;
}
int l() {
  bar49_();
  return 1;
}
int main() { d = d || g(d, l()); }

/* { dg-final { scan-tree-dump-not "foo" "copyprop2" } } */
