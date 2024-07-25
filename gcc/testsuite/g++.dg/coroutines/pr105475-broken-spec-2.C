// https://gcc.gnu.org/PR105475
// Test the case where we specialize coroutine_handle<void> and remove
// 'address'.
namespace std {

struct awaitable {
  bool await_ready() noexcept { return false; }
  void await_suspend(auto) noexcept {}
  bool await_resume() noexcept { return true; }
};

template <typename T> struct coroutine_handle {
  static coroutine_handle from_address(void *address);
  void* address();
};

template <> struct coroutine_handle<void> {
  static coroutine_handle from_address(void *address);
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
