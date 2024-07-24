// https://gcc.gnu.org/PR105475
// Test the case where we have type members instead of functions for
// from_address and address.
namespace std {
struct awaitable {
  bool await_ready() noexcept { return false; }
  void await_suspend(auto) noexcept {}
  bool await_resume() noexcept { return true; }
};

template <typename T>
struct coroutine_handle {
  struct from_address {};
  struct address {};
};

template <typename T = void> struct coroutine_traits {
  struct promise_type {
    awaitable initial_suspend() { return {}; }
    awaitable final_suspend() noexcept { return {}; }
    void return_void() {}
    T get_return_object() { return T(); }
    void unhandled_exception() {}
  };
};
} // namespace std

void foo() { co_return; }
// { dg-error "'std::coroutine_handle<void>::address' must be a non-overloaded method" "" { target *-*-* } {.-1} }
