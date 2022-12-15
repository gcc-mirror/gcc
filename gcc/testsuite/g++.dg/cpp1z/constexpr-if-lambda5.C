// PR c++/100295
// { dg-do compile { target c++17 } }

template<typename... Ts>
void f(Ts... ts) {
  auto lambda = [=](auto x) {
    if constexpr (sizeof((ts+x) + ...) != 0)
      (..., ts);
  };
  lambda(0);
}

int main() {
  f(0, 'a');
}
