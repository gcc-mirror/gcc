// PR c++/69257

int fn1() {
  struct S *x;
  __asm ( "": :"" (*x));	// { dg-error "incomplete" }
}
