/* { dg-do compile } */

struct A {
  int *x;
};
int i;
int f(int *const c) {
  struct A * b = (struct A *)(&c);
  return b->x != 0;
}
void g() { f(&i); }

