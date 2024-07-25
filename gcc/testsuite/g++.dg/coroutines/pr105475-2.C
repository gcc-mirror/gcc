// { dg-do compile }
// https://gcc.gnu.org/PR105475
// Test the case where we lack 'from_address' (i.e. the member required from
// coroutine_handle<void>), but not address()
namespace std {

struct awaitable {
  bool await_ready() noexcept { return false; }
  void await_suspend(auto) noexcept {}
  bool await_resume() noexcept { return true; }
};

template <typename T> struct coroutine_handle {
  void* address();
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
// { dg-error "'from_address' is not a member of 'std::coroutine_handle<std::coroutine_traits<void>::promise_type>'" "" { target *-*-* } {.-1} }
