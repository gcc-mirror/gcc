// PR c++/60845

class A { };
typedef A B;
void foo (B &a) {
  a.x();  // { dg-error "'B {aka class A}' has no member named 'x'" }
}
