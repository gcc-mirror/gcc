/* PR tree-optimization/101223 */
/* { dg-do run } */
/* { dg-options "-O2 " } */

struct {
  int a : 1;
} b;
int c = 1, d;
int foo1() {
  for (; d < 2; d++) {
    int e = ~c, f = 0, g;
    if (e) {
      f = c;
      g = b.a;
      b.a = f;
      if (b.a >= g)
        __builtin_abort();
    }
    c = f;
    b.a = g;
  }
  return 0;
}

int foo2() {
  for (; d < 2; d++) {
    int e = ~c, f = 0, g;
    if (e) {
      f = c;
      g = b.a;
      b.a = f;
      if (g <= b.a)
        __builtin_abort();
    }
    c = f;
    b.a = g;
  }
  return 0;
}
int main ()
{
  return foo1() + foo2();
}
  
