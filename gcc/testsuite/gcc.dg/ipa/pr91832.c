/* { dg-do compile } */
/* { dg-options "-O2"  } */

struct A1 {
  char a1[1];
};

void fn2(char a);

void fn1(struct A1 *p1) {
  fn2(p1->a1[-1]);
}
