namespace N {
  template <typename T>
  struct foo {};
}

int main() {
  using N::foo<double>; // { dg-error "" }
}
