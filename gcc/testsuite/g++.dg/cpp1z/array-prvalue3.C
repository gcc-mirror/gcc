// PR c++/115187
// { dg-do compile { target c++17 } }

void f() {
  using T = int[2];
  delete T{};			// { dg-warning "deleting array" }
  // { dg-warning "unallocated object" "" { target *-*-* } .-1 }
}
