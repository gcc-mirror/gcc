// PR c++/117054
// { dg-do compile { target c++20 } }

template<auto = []{}>
constexpr bool v = true;

template<typename>
void f() {
  [](auto) {
    if constexpr (v<>) { }
  }(0);
}

int main() {
  f<int>();
}
