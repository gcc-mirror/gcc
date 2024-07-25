// https://gcc.gnu.org/PR105475
namespace std {

struct handle {};

struct awaitable {
  bool await_ready() noexcept { return false; }
  void await_suspend(handle) noexcept {}
  bool await_resume() noexcept { return true; }
};

template <typename T> struct coroutine_handle {
  static handle from_address(void *address) noexcept { return {}; }
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
// { dg-error "'address' is not a member of 'std::coroutine_handle<void>'" "" { target *-*-* } {.-1} }
