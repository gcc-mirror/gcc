// PR c++/19811

class C; // { dg-warning "forward" }

void foo(void *p) {
  delete [] ((C*)p) ; // { dg-warning "problem|incomplete" }
}
