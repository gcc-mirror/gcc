namespace N {
  template < typename T > class C : T {};
}

int main() {
  N::C(); // { dg-error "template" }
}
