// PR c++/27666

struct A { // { dg-message "A" }
  A(int); // { dg-message "A" }
};

void foo(volatile A a) { 
  1 ? a : 0; // { dg-error "match|temporary" }
  1 ? 0 : a; // { dg-error "match|temporary" }
} 
