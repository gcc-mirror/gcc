// PR c++/30274
// { dg-do run { target c++14_down } }
// { dg-do compile { target c++17 } }

struct S {
  bool x : 4;
};

S s;

int main() {
  s.x++; // { dg-warning "deprecated" "" { target { ! c++17 } } }
  // { dg-error "forbidden" "" { target c++17 } .-1 }
  if (s.x != 1)
    return 1;
  ++s.x; // { dg-warning "deprecated" "" { target { ! c++17 } } }
  // { dg-error "forbidden" "" { target c++17 } .-1 }
  if (s.x != 1)
    return 2;
}
