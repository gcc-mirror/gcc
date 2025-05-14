// PR c++/99546
// { dg-do compile { target c++20 } }

int main() {
  constexpr auto b = requires { []{}; };
  static_assert(b);
  static_assert(!b);		// { dg-error "assertion failed" }
}
