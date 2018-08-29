// PR c++/30274

struct S {
  bool x : 4;
};

S s;

void f() {
  ++s.x = false; // { dg-warning "deprecated" "" { target { ! c++17 } } }
  // { dg-error "forbidden" "" { target c++17 } .-1 }
}
