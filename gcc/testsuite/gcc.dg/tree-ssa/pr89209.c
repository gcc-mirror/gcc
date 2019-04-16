/* { dg-do compile } */
/* { dg-options "-O2" } */

struct S {
  short a, b;
};
struct T {
  int c;
  struct S s;
};
int f ()
{
  struct T t;
  t.c = t.s.a || t.s.b;
  return t.c;
}
