/* { dg-lto-do link } */
/* { dg-require-effective-target fpic } */
/* { dg-lto-options {{-O3 -flto -flto-partition=1to1 -fPIC -r -nostdlib}} } */

struct Foo { int f1, f2, f3, f4, f5; };

int x = 0;
struct Foo *foo;

inline void Bar(int n){
  foo[x].f1 = 0;
  foo[x].f2 = 0;
  foo[x].f3 = 0;
  foo[x].f4 = 0;
  foo[x].f5 = n;
}

int ei[1];
inline void Baz(int n) {
  if (ei[n] == 1)
    Bar (0);
  else if (ei[n] == 0)
    Bar (1);
}

void mumble(void) {
  for (;;)
    Baz (0);
}
