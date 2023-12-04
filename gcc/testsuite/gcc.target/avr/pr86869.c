/* { dg-do compile } */

struct S {
  char y[2];
};

void foo(const __memx  struct S *s) {
  const char (*p)[2] = &s->y;
}
