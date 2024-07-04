// PR c++/114303
// { dg-do compile { target c++17 } }

struct A { static constexpr bool value = true; };

int main() {
  [](auto x1) {
    return [&](auto) {
      return [&](auto x3) {
        if constexpr (decltype(x3)::value) {
          static_assert(decltype(x1)::value);
        }
      }(A{});
    }(0);
  }(A{});
}
