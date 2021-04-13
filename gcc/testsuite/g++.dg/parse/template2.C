namespace N {
  template < typename T > class C : T {};
}

int main() {
  N::C(); // { dg-error "8:class template argument deduction failed|no match" "" { target c++17 } }
  // { dg-error "7:missing template arguments" "" { target c++14_down } .-1 }
}
