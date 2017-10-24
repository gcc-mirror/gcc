extern unsigned long f, g;
extern bool h, i, j, k;
extern unsigned char l, m;
extern short n;
extern unsigned o;
struct B {
  short b0 : 27;
  long b1 : 10;
};
struct A {
  int a0 : 5;
};
struct C {
  static B c0;
};
struct D {
  static unsigned d0;
  A d1;
};
struct E {
  B e2;
  D e4;
};
struct F {
  E f2;
  short f4;
};
extern F p;
extern C q;
void foo ();
void bar ();
