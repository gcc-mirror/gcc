// PR c++/116379
// { dg-do compile { target c++14 } }

template<typename T>
struct X {
  T val;
  decltype(auto) value() { return (val); }
};

int main() {
  int i = 0;
  X<int&&> x{ static_cast<int&&>(i) };
  using type = decltype(x.value());
  using type = int&;
}
