// PR c++/19811

class C; // { dg-error "forward" }

void foo(void *p) {
  delete [] ((C*)p) ; // { dg-error "" }
}
