// PR c++/27666

struct A { // { dg-message "A" }
  A(int);
};

void foo(volatile A a) {  // { dg-warning "deprecated" "" { target c++2a } }
  1 ? a : 0; // { dg-error "qualifiers|lvalue|no match" }
  1 ? 0 : a; // { dg-error "qualifiers|lvalue|no match" }
} 
