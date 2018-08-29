// PR c++/69257

void fn1() {
  struct S *x;
  __asm ( "": :"" (*x));	// { dg-error "incomplete" }
}
