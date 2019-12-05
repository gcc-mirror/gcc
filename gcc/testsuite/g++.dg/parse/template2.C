namespace N {
  template < typename T > class C : T {};
}

int main() {
  N::C(); // { dg-error "6:cannot deduce template arguments" "" { target c++17 } }
  // { dg-error "7:missing template arguments" "" { target c++14_down } .-1 }
}
