// PR c++/30274

struct S {
  bool x : 4;
};

S s;

void f() {
  ++s.x = false; // { dg-warning "deprecated" "" { target { ! c++1z } } }
  // { dg-error "forbidden" "" { target c++1z } 10 }
}
