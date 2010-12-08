// PR c++/27666

struct A { // { dg-message "A" }
  A(int); // { dg-message "A" }
};

void foo(volatile A a) { 
  1 ? a : 0; // { dg-error "match|temporary" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 8 }
  1 ? 0 : a; // { dg-error "match|temporary" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 10 }
} 
