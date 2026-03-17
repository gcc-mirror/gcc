/* { dg-do compile } */
/* { dg-options "" } */
/* PR c/123461 */

void foo(int n) {
  enum b typedef V[]; /* { dg-error "array type has incomplete element type" } */
  void bar(V) /* { dg-error "old-style parameter" } */
  V bar_ptr;
  {}
}
