// PR c++/30274

struct S {
  bool x : 4;
};

S s;

void f() {
  ++s.x = false; // { dg-warning "7:use of an operand of type .bool. in .operator\\+\\+. is deprecated" "" { target { ! c++17 } } }
  // { dg-error "forbidden" "" { target c++17 } .-1 }
}
