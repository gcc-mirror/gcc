/* { dg-do compile } */
/* { dg-options "-fpack-struct -mavx" } */

struct A {
  __attribute__((__vector_size__(4 * sizeof(double)))) double data;
};
struct B {
  A operator*(B);
};
void fn1() {
  B x, y;
  x *y;
}
