/* { dg-do compile } */
/* { dg-additional-options "-mcpu=cortex-a72" { target aarch64*-*-* } } */

class A {
public:
  unsigned char *fn1();
  int fn2();
};

class B {
  A fld1;
  int fld2;
  void fn3();
  unsigned char fld3;
};

int a;

void
B::fn3() {
  int b = fld1.fn2() / 8;
  unsigned char *c = fld1.fn1(), *d = &fld3, *e = c;
  for (; a < fld2;)
    for (int j = 0; j < b; j++)
      *d++ = e[j];
  for (; 0 < fld2;)
    for (int j = 0; j < b; j++)
      e[j] = *d++;
  for (; fld2;)
    ;
}
