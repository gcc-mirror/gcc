// PR c++/104981 - ICE from convert_to_base when passing *this to promise ctor

#include <coroutine>

class Base {};

struct PromiseType;

struct Result {
 using promise_type = PromiseType;
};

struct PromiseType {
  PromiseType(const Base& parser, auto&&...) {}

  Result get_return_object() { return {}; }

  static std::suspend_never initial_suspend() { return {}; }
  static std::suspend_always final_suspend() noexcept { return {}; }
  [[noreturn]] static void unhandled_exception() { throw; }

  void return_value(int) {}
};

struct Derived : Base {
  Result f() {
   co_return 42;
  }
};

int main() {
  Derived d;
  d.f();
}
