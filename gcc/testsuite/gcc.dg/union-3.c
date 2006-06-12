/* PR target/27421 */
/* { dg-do compile } */

union A
{
  int i;
  void x[1];  /* { dg-error "array of voids" } */
};

void foo(union A a) {}
