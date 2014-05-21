// PR c++/19811

class C; // { dg-message "forward" }

void foo(void *p) {
  delete [] ((C*)p) ; // { dg-warning "problem|incomplete" }
}
