/* { dg-do link } */
/* { dg-options "-O" } */

extern void foo(void);
int a, b;
static int c;
static void f() {
  while (a)
    for (; b; b--)
      ;
}
void i() {
  if (c)
    foo();
  int *g = &c;
  {
    int **h[1] = {&g};
    f();
  }
}
int main() {
  i();
  return 0;
}
