// { dg-require-effective-target alloca }
// { dg-require-effective-target indirect_calls }
/* PR c/77754 */

int fn3();

typedef void (*fn6) (int[][fn3 ()]);
struct S {
  fn6 **fn7;
  fn6 *fn8;
  fn6 fn9;
} s;

void fn1 () {
  int a[10][fn3 ()];
  (**s.fn7) (a);
  (*s.fn8) (a);
  s.fn9 (a);
}
