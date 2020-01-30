// PR c++/90333
// { dg-do compile { target c++11 } }

auto l = [] [[nodiscard]] () -> int { return 0; };
auto l2 = []() -> int __attribute ((warn_unused_result)) { return 0; };
auto f() -> int __attribute ((warn_unused_result));
auto f() -> int { return 0; }

int main()
{
  l();				// { dg-warning "nodiscard" }
  l2();				// { dg-warning "unused_result" }
  f();				// { dg-warning "unused_result" }
}
