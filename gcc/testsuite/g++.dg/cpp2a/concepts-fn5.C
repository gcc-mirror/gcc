// Verify we check associated constraints when resolving the address of a
// template-id.
// { dg-do compile { target c++20 } }

void id(auto) { }

template <typename>
int f() { return 0; }

template <typename T> requires requires { T::fail(); }
auto f() { T::fail(); }

int main() {
  using U = decltype(&f<int>);
  (void)&f<int>;
  id(&f<int>);
}
