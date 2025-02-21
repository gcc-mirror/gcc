/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized" } */

/* PR tree-optimization/108358 */

struct a {
  int b;
  int c;
  short d;
  int e;
  int f;
};
struct g {
  struct a f;
  struct a h;
};
int i;
void foo();
void bar31_(void);
int main() {
  struct g j, l = {2, 1, 6, 1, 1, 7, 5, 1, 0, 1};
  for (; i; ++i)
    bar31_();
  j = l;
  struct g m = j;
  struct g k = m;
  if (k.h.b)
    ;
  else
    foo();
}
/* The call to foo should be optimized away. */
/* { dg-final { scan-tree-dump-not "foo " "optimized" } } */
