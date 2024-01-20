/* { dg-do compile { target lra } } */

struct T { int x; };
int foo(void) {
  struct T v;
  asm goto("" : "+r"(v.x) : : : lab);
  return 0;
lab:
  return -5;
}
